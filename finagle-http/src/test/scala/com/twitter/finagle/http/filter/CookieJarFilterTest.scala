package com.twitter.finagle.http.filter


import com.twitter.finagle.Service
import com.twitter.finagle.http._
import com.twitter.util.{Await, Future}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * Created by alessandro on 12/03/16.
  */
@RunWith(classOf[JUnitRunner])
class CookieJarFilterTest extends FunSuite {
  test("should send cookies again") {
    var receivedCookies: Seq[Cookie] = Seq.empty

    val cookieFilter = new CookieJarFilter
    val httpService =
      cookieFilter andThen new Service[Request, Response] {
        def apply(request: Request): Future[Response] = {
          receivedCookies ++= new CookieMap(request).values
          val response = Response(Status.Ok)

          val cookieMap = new CookieMap(response)
          val cookie: Cookie = new Cookie("test", "value")
          cookie.domain = "defaulthost"
          cookieMap.add(cookie)
          Future.value(response)
        }
      }

    Await.ready(httpService(RequestBuilder().url("http://defaulthost/path").buildGet()))
    Await.ready(httpService(RequestBuilder().url("http://defaulthost/path").buildGet()))
    assert(receivedCookies.size == 1)
  }

  test("should not send cookies from a different domain") {
    var receivedCookies: Seq[Cookie] = Seq.empty

    val cookieFilter = new CookieJarFilter
    val httpService =
      cookieFilter andThen new Service[Request, Response] {
        def apply(request: Request): Future[Response] = {
          receivedCookies ++= new CookieMap(request).values
          val response = Response(Status.Ok)

          val cookieMap = new CookieMap(response)
          val cookie = new Cookie("test", "value")
          cookie.domain = "notdefaulthost"
          cookieMap.add(cookie)
          Future.value(response)
        }
      }

    Await.ready(httpService(RequestBuilder().url("http://defaulthost/path").buildGet()))
    Await.ready(httpService(RequestBuilder().url("http://defaulthost/path").buildGet()))
    assert(receivedCookies.isEmpty)
  }

  test("should not send cookies from a different path") {
    var receivedCookies: Seq[Cookie] = Seq.empty

    val cookieFilter = new CookieJarFilter
    val httpService =
      cookieFilter andThen new Service[Request, Response] {
        def apply(request: Request): Future[Response] = {
          receivedCookies ++= new CookieMap(request).values
          val response = Response(Status.Ok)

          val cookieMap = new CookieMap(response)
          val cookie = new Cookie("test", "value")
          cookie.domain = "defaulthost"
          cookieMap.add(cookie)
          Future.value(response)
        }
      }

    Await.ready(httpService(RequestBuilder().url("http://defaulthost/path").buildGet()))
    Await.ready(httpService(RequestBuilder().url("http://defaulthost/differentPath").buildGet()))
    assert(receivedCookies.isEmpty)
    Await.ready(httpService(RequestBuilder().url("http://defaulthost/path").buildGet()))
    assert(receivedCookies.size == 1)
  }
}
