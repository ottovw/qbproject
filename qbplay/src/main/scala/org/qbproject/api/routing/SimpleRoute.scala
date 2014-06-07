package org.qbproject.api.routing

import play.api.mvc.{ RequestHeader, Handler }
import scala.util.matching.Regex
import org.qbproject.api.routing.QBRoutes._

trait SimpleRoute extends QBRoute {

  def matches(prefix: String, requestHeader: RequestHeader): Boolean = {
    QBRouterUtil.cutPath(prefix, requestHeader).exists { requestPath =>
      println(s"MATCH: $pathPartial with $requestPath ... ${pathPartial.pattern.matcher(requestPath).matches}")
      pathPartial.pattern.matcher(requestPath).matches &&
        method.matches(requestHeader.method)
    }
  }

  def pathPartial: Regex
  def method: Method
}

class Method(name: String) {
  def matches(otherMethod: String) = name == otherMethod
}

case class SimpleRoute0(method: Method, path: String, handler: () => Handler) extends SimpleRoute {
  val pathPartial = path.r
  def getHandler(namespace: String, requestHeader: RequestHeader): Option[Handler] =
    if (matches(namespace, requestHeader)) Some(handler()) else None
  def copy(path: String = path): SimpleRoute0 = {
    new SimpleRoute0(method, path, handler)
  }
}

case class SimpleRoute1[A](method: Method, path: String, pm1: PathParam[A], handler: A => Handler) extends SimpleRoute {
  val pathPartial: Regex = path.r
  def getHandler(namespace: String, requestHeader: RequestHeader): Option[Handler] =
    QBRouterUtil.cutPath(namespace, requestHeader).flatMap {
      case pathPartial(p1) => Some(handler(pm1(p1)))
      case _ => None
    }

  def copy(path: String = path): SimpleRoute1[A] = {
    new SimpleRoute1(method, path, pm1, handler)
  }
}
case class SimpleRoute2[A, B](method: Method, path: String, pm1: PathParam[A], pm2: PathParam[B], handler: (A, B) => Handler) extends SimpleRoute {
  val pathPartial: Regex = path.r
  def getHandler(namespace: String, requestHeader: RequestHeader): Option[Handler] =
    QBRouterUtil.cutPath(namespace, requestHeader).flatMap {
      case pathPartial(p1, p2) => Some(handler(pm1(p1), pm2(p2)))
      case _ => None
    }

  def copy(path: String = path): SimpleRoute2[A, B] = {
    new SimpleRoute2(method, path, pm1, pm2, handler)
  }
}
case class SimpleRoute3[A, B, C](method: Method, path: String, pm1: PathParam[A], pm2: PathParam[B], pm3: PathParam[C], handler: (A, B, C) => Handler) extends SimpleRoute {
  val pathPartial: Regex = path.r
  def getHandler(namespace: String, requestHeader: RequestHeader): Option[Handler] =
    QBRouterUtil.cutPath(namespace, requestHeader).flatMap {
      case pathPartial(p1, p2, p3) => Some(handler(pm1(p1), pm2(p2), pm3(p3)))
      case _ => None
    }

  def copy(path: String = path): SimpleRoute3[A, B, C] = {
    new SimpleRoute3(method, path, pm1, pm2, pm3, handler)
  }
}