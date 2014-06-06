package org.qbproject.api.routing

import play.api.mvc.Handler
import play.api.mvc.RequestHeader
import play.api.mvc.Action

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