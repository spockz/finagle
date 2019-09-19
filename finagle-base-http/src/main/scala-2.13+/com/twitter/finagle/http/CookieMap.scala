package com.twitter.finagle.http

class CookieMap private[finagle] (message: Message, cookieCodec: CookieCodec) extends CookieMapBase(message, cookieCodec) {
  def this(message: Message) = this(message, CookieMap.cookieCodec)

  override def addOne(cookie: (String, Cookie)): this.type = addCookie(cookie)
  override def addAll(cookies: IterableOnce[(String, Cookie)]): this.type = addCookies(cookies)
  
  override def subtractOne(name: String): this.type = removeCookie(name)
  override def subtractAll(names: IterableOnce[String]): this.type = removeCookies(names)

}

object CookieMap extends CookieMapCompanionBase