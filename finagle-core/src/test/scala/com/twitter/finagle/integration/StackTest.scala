package com.twitter.finagle.integration

import com.twitter.finagle._
import com.twitter.finagle.builder.{ClientBuilder, ServerBuilder}
import com.twitter.finagle.client.{StackClient, StringClient}
import com.twitter.finagle.filter.{FailureDetector, FailureDetectionFilter}
import com.twitter.finagle.server.StringServer
import com.twitter.util.{Await, Future}
import java.net.{InetAddress, InetSocketAddress}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StackTest extends FunSuite {

  class TestCtx extends StringClient with StringServer {
    val failService =
      Service.mk[String, String] { s: String => Future.exception(Failure.rejected("unhappy")) }

    val newClientStack =
      StackClient.newStack[String, String].replace(
        StackClient.Role.prepFactory,
        (sf: ServiceFactory[String, String]) => sf.map(identity[Service[String, String]]))
  }

  test("Client/Server: Status.busy propagates from failAccrual to the top of the stack") {
    new TestCtx {
      val server = stringServer.serve(new InetSocketAddress(0), failService)
      val client =
        stringClient.withStack(newClientStack)
        .newService(Name.bound(server.boundAddress), "client")

      // marked busy by FailureAccrualFactory
      for (_ <- 0 until 6) {
        intercept[Exception](Await.result(client("hello\n")))
      }

      assert(client.status == Status.Busy)
    }
  }

  test("ClientBuilder: Status.busy propagates from failAccrual to the top of the stack") {
    new TestCtx {
      val server = ServerBuilder()
                   .codec(StringCodec)
                   .bindTo(new InetSocketAddress(InetAddress.getLoopbackAddress, 0))
                   .name("server")
                   .build(failService)

      val client = ClientBuilder()
                   .codec(StringCodec)
                   .hosts(Seq(server.boundAddress))
                   .hostConnectionLimit(1)
                   .build()

      // marked busy by FailureAccrualFactory
      for (_ <- 0 until 6) {
        intercept[Exception](Await.result(client("hello\n")))
      }

      assert(client.status == Status.Busy)
    }
  }

  test("Client/Server: Status.busy propagates from failFast to the top of the stack") {
    new TestCtx {
      val client =
        stringClient.withStack(newClientStack)
        .newService(Name.bound(new InetSocketAddress(InetAddress.getLoopbackAddress, 0)), "client")

      // marked busy by FailFastFactory
      intercept[Exception](Await.result(client("hello\n")))

      assert(client.status == Status.Busy)
    }
  }

  test("Client/Server: Failure Response is detected and transformed in failure") {
    new TestCtx {
      val server = stringServer.serve(new InetSocketAddress(0), Service.mk[String, String](Future.value))

      val detectorParam = FailureDetectionFilter.FailureDetectorParam(new FailureDetector {
        def apply(v1: Any, v2: Any): Option[Throwable] = {
          Some(new Exception)
        }
      })

      val client = stringClient.newService(Name.bound(server.boundAddress), "client")
      val crankyClient = stringClient.configured(detectorParam).newService(Name.bound(server.boundAddress), "crankyClient")


      Await.ready(client("foo") onFailure { t ⇒
        fail("The normal client should return the input", t)
      })

      Await.ready(crankyClient("bar") onSuccess { response ⇒
        fail(s"The cranky client should not respond but responded with [$response].")
      })
    }
  }
}