package org.qbproject.api

import org.specs2.mutable.Specification
import org.qbproject.schema._
import org.qbproject.api.schema.{ValidationRule, QBStringImpl, QBTypeMapper, QBSchema}
import QBSchema._
import play.api.libs.json._

object SchemaExtensionSpec extends Specification {

  class QBImage(rules: Set[ValidationRule[JsString]]) extends QBStringImpl(rules)
  def image = new QBImage(Set.empty)

  "Schema extension test" should {
    "allow to map extension types" in {
      val schema = cls("img" -> image)
      val instance = Json.obj("img" -> "otto.png")

      QBTypeMapper[QBImage]().map(schema)(instance) {
        case JsString(path) => JsString("public/images/" + path)
      }.get must beEqualTo(Json.obj("img" -> "public/images/otto.png"))
    }
  }
}