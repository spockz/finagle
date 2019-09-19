package com.twitter.finagle.http

import scala.collection.immutable.MapLike

abstract class ParamMap extends ParamMapBase with MapLike[String, String, ParamMap] {
  /**
   * Add a key/value pair to the map, returning a new map.
   * Overwrites all values if the key exists.
   */
  override def +[B >: String](kv: (String, B)): ParamMap = addParam(kv._1, kv._2.toString)

  /**
   * Removes a key from this map, returning a new map.
   * All values for the key are removed.
   */
  override def -(name: String): ParamMap = removeParam(name)
}

object ParamMap extends ParamMapCompanionBase


/** Empty ParamMap */
object EmptyParamMap extends ParamMap {
  val isValid = true
  def get(name: String): Option[String] = None
  def getAll(name: String): Iterable[String] = Nil
  def iterator: Iterator[(String, String)] = Iterator.empty
  override def -(name: String): ParamMap = this
}