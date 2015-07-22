package com.twitter.finagle.filter

import com.twitter.finagle._
import com.twitter.util.Future

abstract class FailureDetector extends ((Any, Any) ⇒ Option[Throwable]) {

}

/**
 * Filter that can transform a correctly received response into a failed
 * response based on business logic.
 */
class FailureDetectionFilter[Req, Rep](failureDetector: FailureDetector)
  extends SimpleFilter[Req, Rep] {

  /**
   * This is the method to override/implement to create your own Filter.
   *
   * @param request the input request type
   * @param service a service that takes the output request type and
   *                the input response type
   *
   */
  def apply(request: Req, service: Service[Req, Rep]) = {
    service(request) flatMap { rep ⇒
      failureDetector(request, rep).map(Future.rawException).getOrElse(Future.value(rep))
    }
  }
}

object FailureDetectionFilter {
  val role = Stack.Role("FailureDetectionFilter")

  def module[Req, Rep]: Stackable[ServiceFactory[Req, Rep]] =
    new Stack.Module1[FailureDetectorParam, ServiceFactory[Req, Rep]] {
      val role = FailureDetectionFilter.role
      val description = "Detect responses that are considered failures and transform them accordingly"
      def make(detector: FailureDetectorParam, next: ServiceFactory[Req, Rep]) = {
        new FailureDetectionFilter[Req, Rep](detector.failureDetector).andThen(next)
      }
    }

  case class FailureDetectorParam(failureDetector: FailureDetector) {
    def mk(): (FailureDetectorParam, Stack.Param[FailureDetectorParam]) =
      (this, FailureDetectorParam.param)
  }
  object FailureDetectorParam {
    implicit val param: Stack.Param[FailureDetectorParam]= Stack.Param(FailureDetectorParam(
      new FailureDetector {
        def apply(v1: Any, v2: Any): Option[Throwable] = None
      }))
  }
}
