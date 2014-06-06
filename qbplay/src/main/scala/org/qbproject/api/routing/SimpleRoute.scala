package org.qbproject.api.routing

import play.api.mvc.{ RequestHeader, Handler }
import scala.util.matching.Regex

trait SimpleRoute extends QBRoute {

  def matches(prefix: String, requestHeader: RequestHeader): Boolean = {
    QBRouterUtil.cutPath(prefix, requestHeader).exists { requestPath =>
      //  println(s"MATCH: $pathPartial with $requestPath ... ${pathPartial.pattern.matcher(requestPath).matches}")
      pathPartial.pattern.matcher(requestPath).matches && requestHeader.method == method
    }
  }

  def pathPartial: Regex
  def method: String
}

case class SimpleRoute0(method: String, path: String, handler: () => Handler) extends SimpleRoute {

  val pathPartial = QBRouterUtil.joinPaths(path).r

  def getHandler(namespace: String, requestHeader: RequestHeader): Option[Handler] =
    if (matches(namespace, requestHeader)) Some(handler()) else None

  def copy(path: String = path): SimpleRoute0 = {
    new SimpleRoute0(method, path, handler)
  }
}

case class SimpleRoute1(method: String, path: String, handler: String => Handler) extends SimpleRoute {

  val pathPartial: Regex = QBRouterUtil.joinPaths(path, "/([^/]+)").r

  def getHandler(namespace: String, requestHeader: RequestHeader): Option[Handler] =
    QBRouterUtil.cutPath(namespace, requestHeader).flatMap {
      case pathPartial(id) => Some(handler(id))
      case _ => None
    }

  def copy(path: String = path): SimpleRoute1 = {
    new SimpleRoute1(method, path, handler)
  }
}