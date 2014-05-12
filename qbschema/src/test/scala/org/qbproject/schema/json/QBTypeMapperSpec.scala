package org.qbproject.schema.json

import org.specs2.mutable.Specification
import org.qbproject.schema._
import org.qbproject.api.schema._
import QBSchema._
import play.api.libs.json._
import play.api.libs.json.extensions.JsExtensions
import java.util.Date
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.qbproject.schema.json.mapper.JsTypeMapperBuilder
import org.qbproject.api.schema.QBTypeMapper
import play.api.libs.json.JsString
import play.api.libs.json.JsNumber
import play.api.libs.json.JsObject

object QBTypeMapperSpec extends Specification {

  "Mapping over types" should {

    val schema = cls(
      "o" -> string,
      "x" -> arr(cls(
        "d" -> integer,
        "e" -> integer)))

    val instance = Json.obj(
      "o" -> "foo",
      "x" -> List(Json.obj(
        "d" -> 4,
        "e" -> 5)))

    "should find all integers" in {
      val schema = cls("s" -> string, "i" -> integer)
      val instance = Json.obj("s" -> "foo", "i" -> 3)
      val matchedPaths = QBTypeMapper[QBInteger]().matchedPaths(schema)(instance)
      matchedPaths.get.size must beEqualTo(1)
    }

    "find all, also nested, integers" in {
      val schema = cls("o" -> string, "i" -> integer, "x" -> cls("e" -> integer))
      val instance = Json.obj("o" -> "foo", "i" -> 3, "x" -> Json.obj("e" -> 4))
      val matchedPaths = QBTypeMapper[QBInteger]().matchedPaths(schema)(instance)
      matchedPaths.get.size must beEqualTo(2)
      matchedPaths.get(1) must beEqualTo(JsPath() \ "x" \ "e")
      matchedPaths.get(1) must beEqualTo(JsPath() \ "x" \ "e")
    }

    "find integers in an array" in {
      val schema = cls("o" -> string, "x" -> arr(cls("d" -> integer, "e" -> integer)))
      val instance = Json.obj("o" -> "foo", "x" -> List(Json.obj("d" -> 4, "e" -> 5)))
      val matchedPaths = QBTypeMapper[QBInteger]().matchedPaths(schema)(instance)
      matchedPaths.get.size must beEqualTo(2)
    }

    "find and increment integers in an array" in {

      val schema = cls(
        "o" -> string,
        "x" -> arr(
          cls("d" -> integer,
            "e" -> integer)))

      val instance = Json.obj(
        "o" -> "foo",
        "x" -> List(Json.obj(
          "d" -> 4,
          "e" -> 5)))

      val matchedPaths = QBTypeMapper[QBInteger]().matchedPaths(schema)(instance)
      val updatedObject = matchedPaths.get.foldLeft(instance)((o, path) => {
        println(o.get(path))
        o.set((path, JsNumber(o.get(path).as[JsNumber].value + 1))).asInstanceOf[JsObject]
      })
      (updatedObject \ "x")(0) \ "d" must beEqualTo(JsNumber(5))
      (updatedObject \ "x")(0) \ "e" must beEqualTo(JsNumber(6))
    }

    "find and increment integers in an array via map" in {
      val updatedObject = QBTypeMapper[QBInteger]().map(schema)(instance) {
        case JsNumber(n) => JsNumber(n + 1)
      }.get

      (updatedObject \ "x")(0) \ "d" must beEqualTo(JsNumber(5))
      (updatedObject \ "x")(0) \ "e" must beEqualTo(JsNumber(6))
    }

    "find and increment integers in an array via builder" in {
      val now = new Date().toString
      val schema = cls(
        "o" -> string,
        "x" -> arr(cls(
          "d" -> dateTime,
          "e" -> integer)))

      val instance = Json.obj(
        "o" -> "foo",
        "x" -> List(Json.obj(
          "d" -> Json.obj("$date" -> now),
          "e" -> 5)))

      val updatedSchema = schema
        .map[QBDateTime](attr => cls("$date" -> dateTime))

      val builder = new JsTypeMapperBuilder(updatedSchema).map[QBClass] {
        case o: JsObject if o.fieldSet.exists(_._1 == "$date") => o.fieldSet.find(_._1 == "$date").get._2
        case o => o
      }

      val updatedObject = builder.go(instance)

      (updatedObject \ "x")(0) \ "d" must beEqualTo(JsString(now))
    }

    "find and increment integers in an array via builder directly" in {

      val now = new Date().toString

      val schema = cls(
        "x" -> arr(dateTime))

      val instance = Json.obj(
        "x" -> List(Json.obj("$date" -> now)))

      val updatedSchema = schema
        .map[QBDateTime](qbType => cls("$date" -> dateTime))

      val builder = new JsTypeMapperBuilder(updatedSchema).map[QBClass] {
        case o: JsObject if o.fieldSet.exists(_._1 == "$date") => o.fieldSet.find(_._1 == "$date").get._2
        case o => o
      }

      val updatedObject = builder.go(instance)

      (updatedObject \ "x")(0) must beEqualTo(JsString(now))
    }

    "find and uppercase all strings via toUpperCase" in {
      val updatedObject = QBTypeMapper[QBString]().map(schema)(instance)(toUpperCase).get
      (updatedObject \ "o") must beEqualTo(JsString("FOO"))
    }

    "find and convert numbers to strings" in {
      val updatedObject = QBTypeMapper[QBInteger]().map(schema)(instance) {
        case JsNumber(n) => JsString(n.intValue().toString)
      }.get

      (updatedObject \ "x")(0) \ "d" must beEqualTo(JsString("4"))
    }

    "find and uppercase all strings and increment all ints via mapping builder" in {
      val mappingBuilder = new JsTypeMapperBuilder(schema).map[QBString] {
        case JsString(s) => JsString(s.toUpperCase)
      }.map[QBInteger] {
        case JsNumber(n) => JsNumber(n + 1)
      }
      val updatedObject = mappingBuilder.go(instance)
      (updatedObject \ "o") must beEqualTo(JsString("FOO"))
      (updatedObject \ "x")(0) \ "d" must beEqualTo(JsNumber(5))
      (updatedObject \ "x")(0) \ "e" must beEqualTo(JsNumber(6))
    }
    
    "convert datetime and posixtime dates to string" in {
      val date = new DateTime(2000,1,1,1,1)
      val expected = "01.01.2000"

      val dateString = date.toString()
      val dateMillis = date.getMillis

      val sampleSchema = cls(
        "d" -> dateTime, 
        "e" -> posixTime)

      val sampleJson = Json.obj(
        "d" -> dateString, 
        "e" -> dateMillis)

      val expectedJson = Json.obj(
        "d" -> expected, 
        "e" -> expected)

      def formatDate(date: DateTime) = DateTimeFormat.forPattern("dd.MM.yyyy").print(date)
      val transformer = new JsTypeMapperBuilder(sampleSchema)
        .map[QBDateTime] {
          case JsString(dateTime) => JsString(formatDate(DateTime.parse(dateTime)))
          case j => j
        }.map[QBPosixTime] {
          case JsNumber(time) => JsString(formatDate(new DateTime(time.toLong)))
          case j => j
        }

      transformer.go(sampleJson) must beEqualTo(expectedJson)
    } 

    "distinguish posixdates and numbers" in {
      val date = new DateTime(2000,1,1,1,1)
      val dateMillis = date.getMillis

      val sampleSchema = cls(
        "e" -> posixTime,
        "n" -> number)

      val sampleJson = Json.obj(
        "e" -> dateMillis,
        "n" -> 1)

      val expectedJson = Json.obj(
        "e" -> "01.01.2000", 
        "n" -> 2)

      def formatDate(date: DateTime) = DateTimeFormat.forPattern("dd.MM.yyyy").print(date)
      val transformer = new JsTypeMapperBuilder(sampleSchema)
        .map[QBPosixTime] {
          case JsNumber(time) => JsString(formatDate(new DateTime(time.toLong)))
          case j => j
        }.map[QBNumber] {
          case JsNumber(num) => JsNumber(num + 1)
          case j => j
        }

      transformer.go(sampleJson) must beEqualTo(expectedJson)
    } 
  }
}