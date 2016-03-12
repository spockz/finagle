package com.twitter.finagle.http.filter

import java.util.Date

import com.twitter.finagle.http.{Cookie, CookieMap, Request, Response}
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.Future

import scala.annotation.varargs

/**
  * Created by alessandro on 12/03/16.
  */
class CookieJarFilter
  extends Filter[Request, Response, Request, Response] {

  private[this] val underlyingCookies: CookieMap = new CookieMap(Request.apply("/"))

  val cookieMeantForThisDomain: Request => ((String, Cookie)) => Boolean = req => {
    case (_, cookie) =>
      req.host.exists { host =>
        cookie.domain != null && host.endsWith(cookie.domain)
      } && req.path.startsWith(cookie.path)
  }

  /**
    * This is the method to override/implement to create your own Filter.
    *
    * @param request the input request type
    * @param service a service that takes the output request type and the input response type
    *
    */
  override def apply(request: Request, service: Service[Request, Response]): Future[Response] = {


    // How to make sure only correct cookies are send...


    // on outgoing requests, instantiate a new [[CookieMap]] on the request, add all the cookies from
    // the current [[underlyingCookies]]
    // Use this creepy stateful business... ... ... ...
    underlyingCookies.synchronized {
      val requestSpecificMap = new CookieMap(request)
      underlyingCookies.withFilter(cookieMeantForThisDomain(request)).foreach { cookiepair =>
        requestSpecificMap.add(cookiepair._2)
      }

    }

    // On the response, read the cookies, and add them to the general underlyingCookies
    service(request).foreach { response =>
      underlyingCookies.synchronized {
        val requestSpecificMap = new CookieMap(response)
        requestSpecificMap.foreach(cookiepair => {
          val cookie: Cookie = cookiepair._2

          if (cookie.domain == null && request.host.nonEmpty) {
            cookie.domain = request.host.get
          }
          if (cookie.path == null) {
            cookie.path = request.path
          }

          underlyingCookies.add(cookie)
        })
      }
    }
  }
}


