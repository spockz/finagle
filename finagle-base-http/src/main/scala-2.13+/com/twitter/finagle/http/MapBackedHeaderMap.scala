package com.twitter.finagle.http

/**
 * Mutable, thread-safe [[HeaderMap]] implementation, backed by
 * a mutable [[Map[String, Header]]], where the map key
 * is forced to lower case
 */
private[http] final class MapBackedHeaderMap extends MapBackedHeaderMapBase {
  def addOne(elem: (String, String)): this.type = {
    set(elem._1, elem._2)
    this
  }

  def subtractOne(elem: String): this.type = removed(elem).asInstanceOf[this.type]
}