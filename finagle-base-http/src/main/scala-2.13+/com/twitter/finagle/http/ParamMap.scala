package com.twitter.finagle.http

abstract class ParamMap extends ParamMapBase {
  override def updated[V1 >: String](key: String, value: V1): ParamMap = addParam(key, value.toString)
  override def removed(name: String): ParamMap = removeParam(name)
  def +(param: (String, String)) = addParam(param._1, param._2)
}

object ParamMap extends ParamMapCompanionBase

object EmptyParamMap extends ParamMap {
  val isValid = true
  def get(name: String): Option[String] = None
  def getAll(name: String): Iterable[String] = Nil
  def iterator: Iterator[(String, String)] = Iterator.empty
  override def removed(name: String): ParamMap = this
}