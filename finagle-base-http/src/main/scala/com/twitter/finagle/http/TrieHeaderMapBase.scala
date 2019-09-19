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
private[http] abstract class TrieHeaderMapBase extends HeaderMap {
  //implementation methods should be final, but that 
  //creates a VerifyError: https://github.com/scala/bug/issues/11749
  import HeaderMap._

  // In general, HashSet/HashTables that are not thread safe are not
  // durable to concurrent modification and can result in infinite loops.
  // As such, we synchronize on the underlying `Headers` when performing
  // accesses to avoid this. In the common case of no concurrent access,
  // this should be cheap.
  private[this] val underlying: Headers = Headers.empty
  final def prettyPrint = underlying.pretty(0)
  //private[this] val underlying: List[Header] = Nil
  final override def foreach[U](f: ((String, String)) => U): Unit = underlying.foreach(f)

  // ---- HeaderMap -----

  final def getAll(key: String): Seq[String] = underlying.synchronized {
    var h = underlying.forNameOrNull(key)
    if (h == null) Nil
    else h.values
  }

  // Validates key and value.
  final def add(key: String, value: String): HeaderMap = {
    validateName(key)
    addUnsafe(key, foldReplacingValidateValue(key, value))
  }

  // Does not validate key and value.
  final def addUnsafe(key: String, value: String): HeaderMap = underlying.synchronized {
    underlying.addHeader(new Header(key, value))
    this
  }

  // Validates key and value.
  final def set(key: String, value: String): HeaderMap = {
    validateName(key)
    setUnsafe(key, foldReplacingValidateValue(key, value))
  }

  // Does not validate key and value.
  final def setUnsafe(key: String, value: String): HeaderMap = underlying.synchronized {
    underlying.setHeader(new Header(key, value))
    this
  }

  // Overriding this for efficiency reasons.
  final override def getOrNull(key: String): String = {
    val h = underlying.forNameOrNull(key)
    if (h == null) null
    else h.value
  }

  // ---- Map/MapLike -----

  /*final*/ def get(key: String): Option[String] = underlying.synchronized {
    Option(getOrNull(key))
  }

  final def iterator: Iterator[(String, String)] = underlying.synchronized {
    underlying.leaves.flatMap(leaf => if (leaf == null) Iterator.empty else leaf.keyValuePairs)
  }

  final def removed(key: String) = underlying.synchronized {
    underlying.clearName(key)
    this
  }

  /*final*/ override def keys: Set[String] = keysIterator.toSet

  final override def keysIterator: Iterator[String] = underlying.synchronized {
    underlying.leaves.flatMap(header => {
      def rec(uniq: List[String], todo: List[String]): List[String] = todo match {
        case Nil => uniq
        case (head :: tail) =>
          rec(head :: uniq, todo.filterNot(x => x == head))
      }
      rec(Nil, header.names)
    })
  }

  private[finagle] final override def nameValueIterator: Iterator[HeaderMap.NameValue] =
    underlying.synchronized {
      underlying.leaves.flatMap(_.iterated)
    }
}

object DefaultHeaderMap {
  def apply(args: (String, String)*) = HeaderMap(args :_*)
}