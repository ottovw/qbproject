package org.qbproject.api.routing

import org.specs2.mutable._
import org.qbproject.api.routing.QBRoutes._
import play.api.mvc._
import org.qbproject.api.routing.QBRouterUtil.namespace

class RouterSpec extends Specification {

  "Router" should {

    val prefix = "/my/prefix"

    "prefix route" in {
      val route = SimpleRoute0("GET", "a", null)
      val copy = route.copy(path = "hey/" + route.path)
      println(copy.pathPartial)
      true must beTrue
    }

    "move routes into namespace" in {
      val hello = Action {
        play.api.mvc.Results.Ok("Hello world")
      }
      val route = GET / "a" -> hello
      val updatedRoute = namespace("/foo") { List(route) }
      updatedRoute(0).path must beEqualTo("/foo/a")
    }

  }
}