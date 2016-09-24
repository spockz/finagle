package com.twitter.finagle.http.netty

import java.util

import com.twitter.finagle.Http
import com.twitter.finagle.http.RequestBuilder
import com.twitter.finagle.util.DefaultTimer
import com.twitter.io.Buf
import com.twitter.util._

/**
 * Created by alessandro on 13/07/16.
 */
object ProxyTest extends App {
  val client =
    Http.client
        .withTransport.httpProxyTo("mijn.ing.nl:443", None)
        .withTransport.tls("mijn.ing.nl")
        .newService("localhost:3128")

  val request =
  RequestBuilder().url("https://mijn.ing.nl/internetbankieren/SesamLoginServlet")
    .buildGet

//  val request =
//    RequestBuilder().url("http://www.nu.nl/")
//    .buildGet

  FuturePool.immediatePool.apply(
  Await.ready(client(request).liftToTry.within(DefaultTimer.twitter, Duration.fromSeconds(1)).foreach( {
    case Return(msg) => println(msg.encodeString())
    case Throw(t) => println(s"error!: $t")
  })))
  Thread.sleep(2000)
  println("Done sending messages")
//  Await.ready(client(requestING).foreach(msg => println(msg.encodeString())))
//  Await.ready(client(requestNu).foreach(msg => println(msg.encodeString())))
}
