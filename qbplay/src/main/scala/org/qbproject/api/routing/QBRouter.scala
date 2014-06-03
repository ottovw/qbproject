package org.qbproject.api.routing

import play.core.Router
import scala.runtime.AbstractPartialFunction
import play.api.mvc.RequestHeader
import play.api.mvc.Handler
import play.api.mvc.Action

// TODO: Composite router
trait QBRouter extends QBBaseRouter with QBRouteWrapping

trait QBBaseRouter extends Router.Routes {

  def qbRoutes: List[QBRoute] = Nil

  def routeExists(requestHeader: RequestHeader) = qbRoutes.exists(_.matches(prefix, requestHeader))

  def resolveHandler(requestHeader: RequestHeader): Option[Handler] = {
    qbRoutes.find(_.matches(prefix, requestHeader))
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

trait QBRouteWrapping {
  self: QBBaseRouter =>

  def wrappers: Map[List[QBRoute], List[Handler => Handler]] = Map.empty

  override def resolveHandler(requestHeader: RequestHeader): Option[Handler] = {
    for {
      route <- qbRoutes.find(_.matches(prefix, requestHeader))
      handler <- route.getHandler(prefix, requestHeader)
      wrappedHandler <- wrapHandler(route, handler)
    } yield wrappedHandler
  }

  def wrapHandler[A](route: QBRoute, handler: Handler): Option[Handler] = handler match {
    case action: Action[A] =>
      val ws = wrappers.keys
        .find(_.map(route => route.path).contains(route.path))
        .flatMap(wrappers.get)
        .getOrElse(List.empty)
      Some(ws.foldLeft(action.asInstanceOf[Handler])((act, wrapper) => wrapper(act)))
    case _ => None
  }

  case class HandlerFunction(fn: Action[_] => Action[_]) {
    def cast: Handler => Handler = fn.asInstanceOf[Handler => Handler]
  }

  implicit class RoutesExtensionOps(routes: List[QBRoute]) {
    def wrapWith(wrapper: Action[_] => Action[_]): (List[QBRoute], List[Handler => Handler]) =
      routes -> List(HandlerFunction(wrapper).cast)

    def wrapWith(wrappers: List[Action[_] => Action[_]]): (List[QBRoute], List[Handler => Handler]) =
      routes -> wrappers.map(fn => HandlerFunction(fn).cast)
  }

  implicit class RouteExtensionOps(route: QBRoute) {
    def wrapWith(wrappers: List[Action[_] => Action[_]]): (List[QBRoute], List[Handler => Handler]) =
      List(route) -> wrappers.map(HandlerFunction(_).cast)

    def wrapWith(wrapper: Action[_] => Action[_]): (List[QBRoute], List[Handler => Handler]) =
      List(route) -> List(HandlerFunction(wrapper).cast)
  }

}

case class QBRouteBuilder(method: String, path: String) {
  def ->(handler: => Handler): QBRoute = SimpleRoute0(method, buildPath(path), () => handler)
  def ->(handler: String => Handler): QBRoute = SimpleRoute1(method, buildPath(path), handler)

  private def buildPath(path: String) = {
    if (path == "") path else QBRouterUtil.joinPaths("/", path)
  }
}

object QBRoutes {

  val root = ""

  object GET {
    def /(path: String = ""): QBRouteBuilder = QBRouteBuilder("GET", path)
  }

  object POST {
    def /(path: String = ""): QBRouteBuilder = QBRouteBuilder("POST", path)
  }

  object PUT {
    def /(path: String = ""): QBRouteBuilder = QBRouteBuilder("PUT", path)
  }
}


