package org.qbproject.api.routing

import org.specs2.mutable._
import org.qbproject.api.routing.QBRoutes.{ GET => qbGET, POST => qbPOST, _ }
import org.qbproject.api.routing.QBRouterUtil.namespace
import play.core.Router
import play.api.mvc._
import play.api.test.WithApplication
import play.api.test.FakeRequest
import play.api.test.PlaySpecification
import play.api.test.FakeApplication

class QBBaseRouterSpec extends PlaySpecification {

  object TestController extends Controller {

    // Actions
    def root = Action { Ok("root") }
    def foo = Action { Ok("foo") }
    def bar = Action { Ok("bar") }
    def echo(str: String) = Action { Ok(str) }

    // Routes (qb Prefixes due to import conflict)
    val rootRoute = qbGET / "" -> root
    val fooRoute = qbGET / "foo" -> foo
    val barRoute = qbPOST / "bar" -> bar
    val echoRoute = qbGET / "echo" -> echo _

    val nsRoute = namespace("ns", fooRoute)
    val nsRoute2 = namespace("ns2", nsRoute)

    val allRoutes = List(rootRoute, fooRoute, barRoute, echoRoute, nsRoute, nsRoute2)

    val FakeAppWithRouter = new FakeApplication {
      override lazy val routes: Option[Router.Routes] =
        Some(QBRouterEndpoint(allRoutes))
    }
  }

  "QBBaseRouter" should {

    "resolve a simple GET route" in new WithApplication(TestController.FakeAppWithRouter) {
      val result = route(FakeRequest(GET, "/foo"))
      result.map(contentAsString) must beSome("foo")
    }

    "resolve a simple root GET route" in new WithApplication(TestController.FakeAppWithRouter) {
      val result = route(FakeRequest(GET, ""))
      result.map(contentAsString) must beSome("root")
    }

    "resolve a simple POST route" in new WithApplication(TestController.FakeAppWithRouter) {
      val result = route(FakeRequest(POST, "/bar"))
      result.map(contentAsString) must beSome("bar")
    }

    "resolve route with dynamic part" in new WithApplication(TestController.FakeAppWithRouter) {
      val result = route(FakeRequest(GET, "/echo/blub"))
      result.map(contentAsString) must beSome("blub")
    }

    "resolve a simple route with namespace" in new WithApplication(TestController.FakeAppWithRouter) {
      val result = route(FakeRequest(GET, "/ns/foo"))
      result.map(contentAsString) must beSome("foo")
    }

    "resolve a simple route with two namespaces" in new WithApplication(TestController.FakeAppWithRouter) {
      val result = route(FakeRequest(GET, "/ns2/ns/foo"))
      result.map(contentAsString) must beSome("foo")
    }

  }

}

