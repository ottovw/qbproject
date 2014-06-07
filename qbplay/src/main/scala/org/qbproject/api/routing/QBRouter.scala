package org.qbproject.api.routing

import play.core.Router
import scala.runtime.AbstractPartialFunction
import play.api.mvc.RequestHeader
import play.api.mvc.Handler
import play.api.mvc.Action

// TODO: Composite router
trait QBRouter extends QBBaseRouter with QBRouteWrapping

object QBRouter {
  def apply(_routes: List[QBRoute]): QBRouter = new QBRouter {
    override val qbRoutes: List[QBRoute] = _routes
  }
  def apply(routes: QBRoute*): QBRouter = apply(routes.toList)
}

trait QBBaseRouter extends Router.Routes {

  def qbRoutes: List[QBRoute] = Nil

  def routeExists(requestHeader: RequestHeader) = qbRoutes.exists(_.matches(prefix, requestHeader))

  def resolveHandler(requestHeader: RequestHeader): Option[Handler] = {
    qbRoutes
      .find(_.matches(prefix, requestHeader))
      .flatMap(_.getHandler(prefix, requestHeader))
  }

  /**
   * Below: Methods to satisfy Play's Router.Routes
   */
  private var path: String = ""
  def prefix = path
  def setPrefix(prefix: String) = path = prefix

  def routes = new AbstractPartialFunction[RequestHeader, Handler] {
    override def applyOrElse[A <: RequestHeader, B >: Handler](requestHeader: A, default: A => B) = {
      resolveHandler(requestHeader).getOrElse(default(requestHeader))
    }
    def isDefinedAt(requestHeader: RequestHeader) = routeExists(requestHeader)
  }

  def documentation = Seq(("", "", ""))

}

object QBRoutes {

  val GET = "GET"
  val POST = "POST"
  val PUT = "PUT"
  val DELETE = "DELETE"

  implicit def strToMethod(methodName: String): Method = new Method(methodName)
  implicit def strToBuilder(method: String)(implicit collector: RouteCollector = VoidCollector) = new Builder0(method, "", collector)
  implicit def methodToBuilder(method: Method)(implicit collector: RouteCollector = VoidCollector) = new Builder0(method, "", collector)

  import QBRouterUtil.joinPaths

  trait RouteCollector {
    def addAndReturn[R <: QBRoute](route: R): R
  }
  case object VoidCollector extends RouteCollector {
    def addAndReturn[R <: QBRoute](route: R): R = route
  }

  class Builder0(method: Method, var path: String, collector: RouteCollector = VoidCollector) {
    def /(static: String) = { path = joinPaths(path, static); this }
    def /[N](dynamic: PathParam[N]) = { new Builder1[N](method, joinPaths(path, dynamic.regexString), dynamic, collector) }
    def to(handler: => Handler): SimpleRoute0 = { collector.addAndReturn(SimpleRoute0(method, path, () => handler)) }
  }
  class Builder1[A](method: Method, var path: String, param1: PathParam[A], collector: RouteCollector = VoidCollector) {
    def /(static: String) = { path = joinPaths(path, static); this }
    def /[N](dynamic: PathParam[N]) = { new Builder2[A, N](method, joinPaths(path, dynamic.regexString), param1, dynamic, collector) }
    def to(handler: A => Handler) = { collector.addAndReturn(SimpleRoute1(method, path, param1, handler)) }
  }
  class Builder2[A, B](method: Method, var path: String, param1: PathParam[A], param2: PathParam[B], collector: RouteCollector = VoidCollector) {
    def /(static: String) = { path = joinPaths(path, static); this }
    def /[N](dynamic: PathParam[N]) = { new Builder3[A, B, N](method, joinPaths(path, dynamic.regexString), param1, param2, dynamic, collector) }
    def to(handler: (A, B) => Handler) = { collector.addAndReturn(SimpleRoute2(method, path, param1, param2, handler)) }
  }
  class Builder3[A, B, C](method: Method, var path: String, param1: PathParam[A], param2: PathParam[B], param3: PathParam[C], collector: RouteCollector = VoidCollector) {
    def /(static: String) = { path = joinPaths(path, static); this }
    def /[N](dynamic: PathParam[N]) = ???
    def to(handler: (A, B, C) => Handler) = { collector.addAndReturn(SimpleRoute3(method, path, param1, param2, param3, handler)) }
  }

  trait PathParam[A] {
    def regexString: String
    def apply(str: String): A
  }

  case object string extends PathParam[String] {
    def regexString: String = "(.*?)"
    def apply(str: String): String = str
  }
  case object int extends PathParam[Int] {
    def regexString: String = "([0-9]+)"
    def apply(str: String): Int = str.toInt
  }
}


