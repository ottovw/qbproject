package org.qbproject.api.routing

case class QBRouterEndpoint(_qbRoutes: List[QBRoute]) extends QBRouter {
  override val qbRoutes = _qbRoutes
}

object QBRouterEndpoint {
  def apply(routes: QBRoute*): QBRouterEndpoint = QBRouterEndpoint(routes.toList)
}

