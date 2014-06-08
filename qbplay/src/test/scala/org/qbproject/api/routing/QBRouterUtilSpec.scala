package org.qbproject.api.routing

import org.specs2.mutable._
import org.qbproject.api.routing._
import play.api.mvc._

class QBRouterUtilSpec extends Specification {

  "#joinPaths" should {
    "join paths with no slahes" in {
      QBRouterUtil.joinPaths("/foo", "bar") must equalTo("/foo/bar")
    }

    "join multiple paths" in {
      QBRouterUtil.joinPaths("/foo", "bar", "/dude") must equalTo("/foo/bar/dude")
    }

    // TODO do we want this behavior?
    "join paths with empty next part" in {
      QBRouterUtil.joinPaths("/foo", "") must equalTo("/foo")
    }

    "join paths with root next part" in {
      QBRouterUtil.joinPaths("/foo/", "/") must equalTo("/foo/")
    }

    "join paths with root slash" in {
    	QBRouterUtil.joinPaths("/", "") must equalTo("/")
    }
  }
  
}