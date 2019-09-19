package com.twitter.finagle.http

import scala.collection.mutable.MapLike

class CookieMap private[finagle] (message: Message, cookieCodec: CookieCodec) extends CookieMapBase(message, cookieCodec) with MapLike[String, Cookie, CookieMap]{
  def this(message: Message) = this(message, CookieMap.cookieCodec)

  override def +=(cookie: (String, Cookie)): this.type = addCookie(cookie)
  override def -=(name: String): this.type = removeCookie(name)
  
  override def ++=(cookies: TraversableOnce[(String, Cookie)]): this.type = addCookies(cookies)
  override def --=(names: TraversableOnce[String]): this.type = removeCookies(names)
}

object CookieMap extends CookieMapCompanionBase