package com.twitter.finagle.http

import com.twitter.finagle.http.Rfc7230HeaderValidation.{
  ObsFoldDetected,
  ValidationFailure,
  ValidationSuccess
}
import com.twitter.logging.Logger
import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Mutable, thread-safe [[HeaderMap]] implementation.
 */
private[http] final class TrieHeaderMap extends TrieHeaderMapBase {
  def addOne(elem: (String, String)): this.type = {
    set(elem._1, elem._2)
    this
  }

  def subtractOne(elem: String): this.type = removed(elem).asInstanceOf[this.type]
}