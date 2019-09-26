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
 * Mutable, thread-safe [[HeaderMap]] implementation, backed by
 * a mutable [[Map[String, Header]]], where the map key
 * is forced to lower case
 */
private[http] abstract class MapBackedHeaderMapBase extends HeaderMap {
  //implementation methods should be final, but that 
  //creates a VerifyError: https://github.com/scala/bug/issues/11749
  import HeaderMap._

  // In general, HashSet/HashTables that are not thread safe are not
  // durable to concurrent modification and can result in infinite loops.
  // As such, we synchronize on the underlying `Headers` when performing
  // accesses to avoid this. In the common case of no concurrent access,
  // this should be cheap.
  private[this] val underlying: mutable.Map[String, Header] = mutable.Map.empty
  
  //private[this] val underlying: List[Header] = Nil
  final override def foreach[U](f: ((String, String)) => U): Unit = for {
    (_, h) <- underlying
    kv <- h.keyValuePairs
  } f(kv)

  // ---- HeaderMap -----

  final def getAll(key: String): Seq[String] = underlying.synchronized {
    underlying.get(key.toLowerCase).toSeq.flatMap(h => h.values)
  }

  // Validates key and value.
  final def add(key: String, value: String): HeaderMap = {
    validateName(key)
    addUnsafe(key, foldReplacingValidateValue(key, value))
  }

  // Does not validate key and value.
  final def addUnsafe(key: String, value: String): HeaderMap = underlying.synchronized {
    val lower = key.toLowerCase
    val header = new Header(key, value)
    underlying.get(lower) match {
      case Some(h) => h.add(header)
      case None => underlying.+=((lower, header))
    }
    this
  }

  // Validates key and value.
  final def set(key: String, value: String): HeaderMap = {
    validateName(key)
    setUnsafe(key, foldReplacingValidateValue(key, value))
  }

  // Does not validate key and value.
  final def setUnsafe(key: String, value: String): HeaderMap = underlying.synchronized {
    underlying.+=((key.toLowerCase, new Header(key, value)))
    this
  }

  // ---- Map/MapLike -----

  /*final*/ def get(key: String): Option[String] = underlying.synchronized {
    underlying.get(key.toLowerCase).map(_.value)
  }

  final def iterator: Iterator[(String, String)] = underlying.synchronized {
    underlying.iterator.flatMap{ case (_, h) => h.keyValuePairs }
  }

  final def removed(key: String) = underlying.synchronized {
    underlying.-=(key.toLowerCase)
    this
  }

  /*final*/ override def keys: Set[String] = keysIterator.toSet

  final override def keysIterator: Iterator[String] = underlying.synchronized {
    underlying.valuesIterator.flatMap(header => {
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
      underlying.valuesIterator.flatMap(_.iterated)
    }
}