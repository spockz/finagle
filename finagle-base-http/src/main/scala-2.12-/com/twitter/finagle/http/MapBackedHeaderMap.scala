package com.twitter.finagle.http

import scala.collection.mutable.MapLike

/**
 * Mutable, thread-safe [[HeaderMap]] implementation, backed by
 * a mutable [[Map[String, Header]]], where the map key
 * is forced to lower case
 */
private[http] final class MapBackedHeaderMap extends MapBackedHeaderMapBase with MapLike[String, String, MapBackedHeaderMap] {
  
  def +=(kv: (String, String)): this.type = {
    set(kv._1, kv._2)
    this
  }

  def -=(key: String): this.type = removed(key).asInstanceOf[this.type]

  override def empty: MapBackedHeaderMap = new MapBackedHeaderMap
}