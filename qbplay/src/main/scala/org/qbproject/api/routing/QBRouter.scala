package org.qbproject.api.routing

import play.core.Router
import scala.runtime.AbstractPartialFunction
import play.api.mvc.RequestHeader
import play.api.mvc.Handler
import play.api.mvc.Action

// TODO: Composite router
trait QBRouter extends Router.Routes {

  private var path: String = ""
  def prefix = path
  def setPrefix(prefix: String) = path = prefix

  def qbRoutes: List[QBRoute] = Nil

  def wrappers: Map[List[QBRoute], List[Handler => Handler]] = Map.empty

  def wrapHandler[A](route: QBRoute, handler: Handler): Option[Handler] = handler match {
    case action: Action[A] =>
      val ws = wrappers.keys
        .find(_.map(r => r.path).contains(route.path))
        .flatMap(wrappers.get)
        .getOrElse(List.empty)
      Some(ws.foldLeft(action.asInstanceOf[Handler])((act, wrapper) => wrapper(act)))
    case _ => None
  }

  def routes = new AbstractPartialFunction[RequestHeader, Handler] {
    override def applyOrElse[A <: RequestHeader, B >: Handler](requestHeader: A, default: A => B) = {

      val handler = for {
        route <- qbRoutes.find(_.matches(prefix, requestHeader))
        handler <- route.getHandler(prefix, requestHeader)
        wrappedHandler <- wrapHandler(route, handler)
      } yield wrappedHandler

      handler.getOrElse(default(requestHeader))
    }

    def isDefinedAt(requestHeader: RequestHeader) = qbRoutes.exists(_.matches(prefix, requestHeader))
  }

  def documentation = Seq(("", "", ""))

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
  def ->(handler: => Handler): QBRoute =  SimpleRoute0(method, "/" + path, () => handler)
  def ->(handler: String => Handler): QBRoute =  SimpleRoute1(method, "/" + path, handler)
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


