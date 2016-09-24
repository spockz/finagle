package com.twitter.finagle.netty3.proxy

import com.twitter.finagle._
import com.twitter.finagle.client.Transporter
import com.twitter.finagle.client.Transporter.Credentials
import com.twitter.io.Charsets
import com.twitter.util.Base64StringEncoder
import java.net.{InetSocketAddress, SocketAddress}
import java.util.concurrent.atomic.AtomicReference

import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http._
import org.jboss.netty.handler.queue.BufferedWriteHandler

/**
 * An internal handler that upgrades the pipeline to delay connect-promise satisfaction until the
 * remote HTTP proxy server is ready to proxy traffic to an ultimate destination represented as
 * `host` (i.e., HTTP proxy connect procedure is successful).
 *
 * This enables "Tunneling TCP-based protocols (i.e., TLS/SSL) through Web proxy servers" [1] and
 * may be used with any TCP traffic, not only HTTP(S). See Squid documentation on this feature [2].
 *
 * [1]: http://www.web-cache.com/Writings/Internet-Drafts/draft-luotonen-web-proxy-tunneling-01.txt
 * [2]: http://wiki.squid-cache.org/Features/HTTPS
 * @param host the ultimate host a remote proxy server connects to
 * @param credentialsOption optional credentials for a proxy server
 */
private[netty3] class HttpProxyConnectHandler(
                                              host: String,
                                              credentialsOption: Option[Transporter.Credentials],
                                              httpClientCodec: ChannelHandler = new HttpClientCodec()) // exposed for testing
  extends SimpleChannelHandler
{ self =>

  private[this] val httpCodecKey: String = "http proxy client codec"

  private[this] val connectFuture = new AtomicReference[ChannelFuture](null)

  private[this] def proxyAuthorizationHeader(c: Credentials): String = {
    val bytes = "%s:%s".format(c.username, c.password).getBytes(Charsets.Utf8)
    "Basic " + Base64StringEncoder.encode(bytes)
  }

  private[this] def fail(c: Channel, t: SocketAddress => Throwable) {
    Option(connectFuture.get) foreach { _.setFailure(t(c.getRemoteAddress)) }
    Channels.close(c)
  }

  override def connectRequested(ctx: ChannelHandlerContext, e: ChannelStateEvent): Unit = {
    e match {
      case de: DownstreamChannelStateEvent =>
        if (!connectFuture.compareAndSet(null, e.getFuture)) {
          fail(ctx.getChannel, (remoteAddress : SocketAddress) => new InconsistentStateException(remoteAddress))
          return
        }

        // proxy cancellation
        val wrappedConnectFuture = Channels.future(de.getChannel, true)
        de.getFuture.addListener(new ChannelFutureListener {
          def operationComplete(f: ChannelFuture) {
            if (f.isCancelled)
              wrappedConnectFuture.cancel()
          }
        })

        // Proxy failures here so that if the connect fails, it is
        // propagated to the listener, not just on the channel.
        wrappedConnectFuture.addListener(new ChannelFutureListener {
          def operationComplete(f: ChannelFuture) {
            if (f.isSuccess || f.isCancelled) {
              return
            }

            fail(f.getChannel, (_: SocketAddress) => f.getCause)
          }
        })

        // We propagate the pipeline with a new promise thereby delaying the original connect's
        // satisfaction.
        // TODO: Figure out why the remoteAddress is `null` sometimes
        val wrappedEvent = new DownstreamChannelStateEvent(
          de.getChannel, wrappedConnectFuture,
          de.getState, de.getValue)


        super.connectRequested(ctx, wrappedEvent)
      case ue: UpstreamMessageEvent =>
        super.connectRequested(ctx, ue)
    }


  }

  override def channelConnected(ctx: ChannelHandlerContext, e: ChannelStateEvent): Unit = {
    if (connectFuture.get eq null) {
      fail(ctx.getChannel, (remoteAddress: SocketAddress) => new InconsistentStateException(remoteAddress))
      return
    }
    // Add HTTP client codec so we can talk to an HTTP proxy.
    ctx.getPipeline().addBefore(ctx.getName(), httpCodecKey, httpClientCodec)

//    val httpProxyConnectRequestFuture = Channels.future(e.getChannel, true)
    // Create new connect HTTP proxy connect request.
    val req = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.CONNECT, host)
    req.headers().set(HttpHeaders.Names.HOST, host).set(HttpHeaders.Names.CONNECTION, "keep-alive")
    credentialsOption.foreach(c =>
      req.headers().add(HttpHeaders.Names.PROXY_AUTHORIZATION, proxyAuthorizationHeader(c))
    )

    ctx.sendDownstream(new DownstreamMessageEvent(ctx.getChannel, connectFuture.get(), req, ctx.getChannel.getRemoteAddress))

    // proxy cancellations again.
    connectFuture.get.addListener(new ChannelFutureListener {
      override def operationComplete(f: ChannelFuture): Unit =
        if (f.isCancelled) {
          fail(ctx.getChannel, (remoteAddress : SocketAddress) =>
            new ChannelClosedException(remoteAddress))
        } else if (f.isSuccess) {
          // ALE: Seems redundant
          connectFuture.get().setSuccess()
        }
    })
  }

  override def messageReceived(ctx: ChannelHandlerContext, messageEvent: MessageEvent): Unit =     {
      messageEvent.getMessage match {
        case rep: DefaultHttpResponse =>
          // A remote HTTP proxy is ready to proxy traffic to an ultimate destination. We no longer
          // need HTTP proxy pieces in the pipeline.
          if (rep.getStatus == HttpResponseStatus.OK) {
            ctx.getPipeline.remove(httpCodecKey)
            ctx.getPipeline.remove(self) // drains pending writes when removed

            connectFuture.get.setSuccess()
          } else {
            val failure = (remote: SocketAddress) => new ConnectionFailedException(
              Failure(s"Unexpected status returned from an HTTP proxy server: ${rep.getStatus()}."),
              remote
            )

            fail(ctx.getChannel, failure)
          }
        case _ => ctx.sendUpstream(messageEvent)
      }
    }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent): Unit =
    fail(ctx.getChannel, (_: SocketAddress) => e.getCause)
}