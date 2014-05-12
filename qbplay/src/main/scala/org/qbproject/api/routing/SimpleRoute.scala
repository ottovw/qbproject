package org.qbproject.api.routing

import play.api.mvc.{RequestHeader, Handler}
import scala.util.matching.Regex

trait SimpleRoute extends QBRoute {

  val MaybeSlash = "/?"
  val Id = "/([^/]+)".r


  def matches(prefix: String, requestHeader: RequestHeader): Boolean = {
    QBRouterUtil.cutPath(prefix, requestHeader).exists(
      /* println(s"MATCH: $pathPartial with $requestPath ... ${pathPartial.pattern.matcher(requestPath).matches}") */
      pathPartial.pattern.matcher(_).matches && requestHeader.method == method
    )
  }

  def pathPartial: Regex
  def method: String
}

case class SimpleRoute0(method: String, path: String, handler: () => Handler) extends SimpleRoute {

  val pathPartial = (path + MaybeSlash).r

  def getHandler(namespace: String, requestHeader: RequestHeader): Option[Handler] =
    if (matches(namespace, requestHeader)) Some(handler()) else None

  def copy(path: String = path): SimpleRoute0 = {
    new SimpleRoute0(method, path, handler)
  }
}

case class SimpleRoute1(method: String, path: String, handler: String => Handler) extends SimpleRoute {

  val pathPartial: Regex = (path + Id).r

  def getHandler(namespace: String, requestHeader: RequestHeader): Option[Handler] =
    QBRouterUtil.cutPath(namespace, requestHeader).flatMap {
      case pathPartial(id) => Some(handler(id))
      case _ => None
    }

  def copy(path: String = path): SimpleRoute1 = {
    new SimpleRoute1(method, path, handler)
  }
}