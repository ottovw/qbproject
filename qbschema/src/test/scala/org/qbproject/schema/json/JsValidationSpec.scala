package org.qbproject.schema.json

import org.specs2.mutable.Specification
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json._
import org.qbproject.schema._
import org.qbproject.api.schema.{QBClass, QBPartialValidator, QBValidator, QBSchema}
import QBSchema._
import scala.math.BigDecimal.int2bigDecimal
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object JsValidationSpec extends Specification {

  "QBValidator" should {

    "should accept correct JSON" in {
      val schema = cls(
        "o" -> cls(
          "s" -> string(minLength(5))))
      val instance = Json.obj(
        "o" -> Json.obj(
          "s" -> "hallo"))
      QBValidator.validate(schema)(instance).asOpt.isDefined must beTrue
    }

    "should decline invalid JSON" in {
      val schema = cls(
        "o" -> cls(
          "s" -> string(minLength(5))))
      val instance = Json.obj(
        "o" -> Json.obj(
          "s" -> "hall"))
      QBValidator.validate(schema)(instance).asOpt.isDefined must beFalse
    }

    "should emit same JSON as read if no transformation is present" in {
      val schema = cls(
        "o" -> string,
        "e" -> number)
      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23)

      QBValidator.validate(schema)(instance).asOpt.get must beEqualTo(instance)
    }

    "test schema adapter" in {
      val schema = cls(
        "o" -> string,
        "e" -> number,
        "d" -> default(number, JsNumber(42)))
      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23)

      QBValidator.validate(schema)(instance).asOpt.get \ "d" must beEqualTo(JsNumber(42))
    }

    "test optional base type without default" in {
      val schema = cls(
        "o" -> string,
        "e" -> number,
        "d" -> optional(number))
      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23)

      QBValidator.validate(schema)(instance).asOpt.isDefined must beTrue
    }

    "test optional object without default " in {
      val schema = cls(
        "o" -> string,
        "e" -> number,
        "d" -> optional(cls("i" -> string)))
      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23)

      QBValidator.validate(schema)(instance).asOpt.isDefined must beTrue
    }

    "test nested optional object without default " in {
      val schema = cls(
        "o" -> string,
        "e" -> number,
        "d" -> optional(cls("i" -> optional(string))))
      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23)

      QBValidator.validate(schema)(instance).asOpt.isDefined must beTrue
    }

    "test optional without default" in {
      val schema = cls(
        "s" -> string,
        "n" -> number,
        "o" -> optional(number))
      val instance = Json.obj(
        "s" -> "o",
        "n" -> 23)

      QBValidator.validate(schema)(instance).asOpt.isDefined must beTrue
    }

    "test optional without default should not yield JsUndefined" in {
      val schema = cls(
        "s" -> string,
        "n" -> number,
        "o" -> optional(number))
      val instance = Json.obj(
        "s" -> "o",
        "n" -> 23)

      QBValidator.validate(schema)(instance).asOpt must beSome.which(_.fields.size == 2)
    }

    "test optional with already set value" in {
      val schema = cls(
        "o" -> string,
        "e" -> number,
        "d" -> optional(number, JsNumber(11)))

      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23,
        "d" -> 14)

      QBValidator.validate(schema)(instance).asOpt.get \ "d" must beEqualTo(JsNumber(14))
    }

    "test optional with default" in {
      val schema = cls(
        "o" -> string,
        "e" -> number,
        "d" -> optional(number, JsNumber(11)))
      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23)

      val result = QBValidator.validate(schema)(instance)
      result.asOpt.get \ "d" must beEqualTo(JsNumber(11))
    }

    "test optional nested in an optional object where parent is not set" in {
      val schema = cls(
        "o" -> string,
        "e" -> number,
        "d" -> optional(cls(
          "x" -> optional(string)
        )))
      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23)

      QBValidator.validate(schema)(instance).asOpt must beSome
    }

    "test optional nested in an optional object where child is not set" in {
      val schema = cls(
        "o" -> string,
        "e" -> number,
        "d" -> optional(cls(
          "x" -> optional(string)
        )))

      val instance = Json.obj(
        "o" -> "o",
        "e" -> 23,
        "d" -> cls())

      QBValidator.validate(schema)(instance).asOpt must beSome
    }

    "test optional when value is null" in {
      val schema = cls(
        "o" -> string,
        "e" -> optional(number)
      )

      val instance = JsObject(Seq(
        "o" -> JsString(""),
        "e" -> JsNull
      ))

      val result = QBValidator.validate(schema)(instance)
      result.asOpt must beSome
    }

    "test optional when value is JsUndefined" in {
      val schema = cls(
        "o" -> string,
        "e" -> optional(number)
      )

      val instance = JsObject(Seq(
        "o" -> JsString(""),
        "e" -> JsUndefined("")
      ))

      QBValidator.validate(schema)(instance).asOpt must beSome
    }

    "test multipleOf with valid integer" in {
      val schema = cls(
        "i" -> integer(multipleOf(3)))
      val instance = Json.obj(
        "i" -> 9)
      val result = QBValidator.validate(schema)(instance)
      result.asOpt must beSome
    }

    "test multipleOf with invalid integer" in {
      val schema = cls(
        "i" -> integer(multipleOf(3)))
      val instance = Json.obj(
        "i" -> 10)
      val result = QBValidator.validate(schema)(instance)
      result.asOpt must beNone
    }

    "test parsing a boolean" in {
      val schema = cls(
        "bool" -> bool)
      val input = Json.obj(
        "bool" -> true)

      val result = QBValidator.validate(schema)(input)
      result.asOpt must beEqualTo(Some(input))
    }

    "test oneOf object validation" in {

      val schema = cls(
        "i" -> QBSchema.oneOf(
          cls("x" -> number),
          cls("x" -> string)))

      val result = QBValidator.validate(schema)(Json.obj("i" -> Json.obj("x" -> "wat")))
      result.asOpt must beSome
    }

    "test oneOf object violation" in {
      val o = cls(
        "i" -> QBSchema.oneOf(cls("x" -> number), cls("x" -> string)))
      QBValidator.validate(o)(Json.obj("j" -> Json.obj("xx" -> "wat"))).asOpt must beNone
    }

    "test recursive definition" in {
      lazy val recSchema: QBClass = cls(
        "author" -> string,
        "message" -> string,
        "replies" -> arr(recSchema))
      val data = Json.obj(
        "author" -> "Dude",
        "message" -> "Hallo?",
        "replies" -> Json.arr(Json.obj(
          "author" -> "Dudezzzz",
          "message" -> "Hi!",
          "replies" -> Json.arr())))

      QBValidator.validate(recSchema)(data).asOpt.isDefined must beTrue
    }

    "test valid minProperties" in {
      val schema = cls(List(
        "i" -> integer,
        "j" -> integer),
        minProperties(1))
      val instance = Json.obj(
        "i" -> 9,
        "j" -> 10)
      val result = QBValidator.validate(schema)(instance)
      result.asOpt must beSome
    }

    "test minProperties violation" in {
      val schema = cls(List(
        "i" -> integer,
        "j" -> integer),
        minProperties(3))
      val instance = Json.obj(
        "i" -> 9,
        "j" -> 10)
      val result = QBValidator.validate(schema)(instance)
      result.asOpt must beNone
    }

    "test valid maxProperties" in {
      val schema = cls(List(
        "i" -> integer,
        "j" -> integer),
        maxProperties(3))
      val instance = Json.obj(
        "i" -> 9,
        "j" -> 10)
      val result = QBValidator.validate(schema)(instance)
      result.asOpt must beSome
    }

    "test maxProperties violation" in {
      val schema = cls(List(
        "i" -> integer,
        "j" -> integer),
        maxProperties(1))
      val instance = Json.obj(
        "i" -> 9,
        "j" -> 10)
      val result = QBValidator.validate(schema)(instance)
      result.asOpt must beNone
    }

    "test tolerant numbers conversion" in {
      val schema = cls(List(
        "i" -> integer,
        "j" -> number))
      val instance = Json.obj(
        "i" -> 9,
        "j" -> "10")
      QBValidator.validate(schema)(instance).asOpt must beSome
    }

    "test tolerant integer conversion" in {
      val schema = cls(List(
        "i" -> integer,
        "j" -> integer))
      val instance = Json.obj(
        "i" -> 9,
        "j" -> "10")
      QBValidator.validate(schema)(instance).asOpt must beSome
    }

    "test error case when tolerant number conversion gets non valid number" in {
      val schema = cls(List(
        "i" -> integer,
        "j" -> number))
      val instance = Json.obj(
        "i" -> 9,
        "j" -> "10aaaa")
      QBValidator.validate(schema)(instance).asOpt must beNone
    }

    "test tolerant boolean conversion false" in {
      val schema = cls(List(
        "i" -> integer,
        "j" -> bool))
      val instance = Json.obj(
        "i" -> 9,
        "j" -> "fALSE")
      val result = QBValidator.validate(schema)(instance)
      println(result)
      result.asOpt must beSome
    }

    "test tolerant boolean conversion with true" in {
      val schema = cls(List(
        "i" -> integer,
        "j" -> bool))
      val instance = Json.obj(
        "i" -> 9,
        "j" -> "true")
      QBValidator.validate(schema)(instance).asOpt must beSome
    }

    "test error case when tolerant boolean conversion gets non valid boolean" in {
      val schema = cls(List(
        "i" -> integer,
        "j" -> bool))
      val instance = Json.obj(
        "i" -> 9,
        "j" -> "MaybeTrue")
      QBValidator.validate(schema)(instance).asOpt must beNone
    }

    "test partial validation" in {
      val schema = cls(
        "multipleOf3" -> integer(multipleOf(3)),
        "num" -> number,
        "str" -> string)
      val instance = Json.obj(
        "multipleOf3" -> 9)
      val result = QBPartialValidator.validate(schema)(instance)
      result.asOpt must beSome
    }
  }

}