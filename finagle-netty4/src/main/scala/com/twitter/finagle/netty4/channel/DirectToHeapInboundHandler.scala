package com.twitter.finagle.netty4.channel

import io.netty.buffer.{ByteBuf, ByteBufAllocator, ByteBufHolder}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}

/**
 * An inbound channel handler that copies direct byte buffers onto the JVM heap
 * and gives them a deterministic lifecycle.
 *
 * @note This handler recognizes both ByteBuf's and ByteBufHolder's (think of HTTP
 *       messages extending ByteBufHolder's in Netty).
 *
 * @note If your protocol manages ref-counting or if you are delegating ref-counting
 *       to application space you don't need this handler in your pipeline. Every
 *       other use case needs this handler or you will with very high probability
 *       incur a direct buffer leak.
 */
@Sharable
object DirectToHeapInboundHandler extends ChannelInboundHandlerAdapter {
  private[this] final def copyOnHeap(bb: ByteBuf, alloc: ByteBufAllocator): ByteBuf = {
    try alloc.heapBuffer(bb.readableBytes, bb.capacity).writeBytes(bb)
    finally bb.release()
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: Any): Unit = msg match {
    case bb: ByteBuf if bb.isDirect =>
      ctx.fireChannelRead(copyOnHeap(bb, ctx.alloc()))

    case bbh: ByteBufHolder if bbh.content().isDirect =>
      val onHeapContent = copyOnHeap(bbh.content(), ctx.alloc())
      ctx.fireChannelRead(bbh.replace(onHeapContent))

    case _ => ctx.fireChannelRead(msg)
  }
}
