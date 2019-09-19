package com.twitter.finagle.http

import scala.collection.mutable.MapLike

/**
 * Mutable, thread-safe [[HeaderMap]] implementation.
 */
private[http] final class TrieHeaderMap extends TrieHeaderMapBase with MapLike[String, String, TrieHeaderMap] {
  def +=(kv: (String, String)): this.type = {
    set(kv._1, kv._2)
    this
  }

  def -=(key: String): this.type = removed(key).asInstanceOf[this.type]

  override def empty: TrieHeaderMap = new TrieHeaderMap
}