package org.qbproject.api.routing

import play.api.mvc.RequestHeader

object QBRouterUtil {

  /**
   * Drops the namespace prefix of the request header's path, if it matches.
   *
   * @param prefix
   *            the namespace prefix to be dropped
   * @param requestHeader
   *            the request header whose namespace prefix should be dropped
   * @return the path of the request header without the matched prefix, if the prefix matched, None otherwise
   */
  def cutPath(prefix: String, requestHeader: RequestHeader): Option[String] = {
    if (requestHeader.path.startsWith(prefix)) {
      Some(requestHeader.path.drop(prefix.length))
    } else {
      None
    }
  }

  /**
   * Joins parts of a URL path to one path.
   *
   * @param pathParts
   */
  def joinPaths(pathParts: String*): String = {
    pathParts.map(_.trim).filterNot(_ == "")
      .foldLeft("")((url, next) => {
        val priorHasSlash = url.endsWith("/")
        val nextHasSlash = next.startsWith("/")

        (priorHasSlash, nextHasSlash) match {
          case (true, false) => url + next
          case (false, true) => url + next
          case (true, true) => url + next.substring(1)
          case (false, false) => url + "/" + next
        }
      })
  }

  /**
   * Prefixes all given path with the given prefix.
   */
  object namespace {

    def apply(prefix: String)(routes: => List[QBRoute]): List[QBRoute] = {
      routes.map(copyRoute(prefix, _))
    }

    def apply(prefix: String, route: => QBRoute): QBRoute = copyRoute(prefix, route)

    private def copyRoute(prefix: String, route: QBRoute): QBRoute = {
      route.copy(path = QBRouterUtil.joinPaths("/", prefix, route.path))
    }

  }

}