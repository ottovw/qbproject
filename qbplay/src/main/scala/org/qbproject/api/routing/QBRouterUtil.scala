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
   * Prefixes all given path with the given prefix.
   */
  object namespace {
    def apply(prefix: String)(routes: => List[QBRoute]): List[QBRoute] = routes.map(r => r.copy(path = prefix + r.path))
  }

}