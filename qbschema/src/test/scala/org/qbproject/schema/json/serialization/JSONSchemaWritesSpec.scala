package org.qbproject.schema.json.serialization

import play.api.libs.json.Json
import org.qbproject.api.schema.{QBType, QBSchema}
import QBSchema._
import org.qbproject.schema._
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object JSONSchemaWritesSpec extends Specification {

  "QBSchema" should {

    "to JSON schema with min rule" in {
      val qbSchema = qbClass(
        "n" -> qbNumber(min(10)))
      val jsonSchema = Json.toJson(qbSchema)
      (jsonSchema \ "n" \ "minimum").toString must contain("10")
    }

    "to JSON schema with max rule" in {
      val qbSchema = qbClass(
        "n" -> qbNumber(max(10)))
      val jsonSchema = Json.toJson(qbSchema)
      //      val j = Json.fromJson(jsonSchema)
      //      val schema: QBValue = j.get
      (jsonSchema \ "n" \ "maximum").toString must contain("10")
    }

    "to JSON schema with min and max rule" in {
      val qbSchema = qbClass(
        "n" -> qbNumber(min(5), max(10)))
      val jsonSchema = Json.toJson(qbSchema)
      (jsonSchema \ "n" \ "minimum").toString must contain("5")
      (jsonSchema \ "n" \ "maximum").toString must contain("10")
    }

    "to JSON schema with integer only" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> qbInteger)
      val jsonSchema = Json.toJson(qbSchema)
      val j = Json.fromJson(jsonSchema)(valueReader)
      val schema: QBType = j.get
      true must beTrue
    }

    "to JSON schema with number only" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> qbNumber)
      val jsonSchema = Json.toJson(qbSchema)
      val j = Json.fromJson(jsonSchema)(valueReader)
      val schema: QBType = j.get
      true must beTrue
    }

    "to JSON schema with number with min" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> qbNumber(min(10.0)))
      val jsonSchema = Json.toJson(qbSchema)
      val j = Json.fromJson(jsonSchema)(valueReader)
      val schema: QBType = j.get
      (jsonSchema \ "age" \ "minimum").toString must contain("10")
    }

    "to JSON schema with number with max" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> qbNumber(max(20)))
      val jsonSchema = Json.toJson(qbSchema)
      val j = Json.fromJson(jsonSchema)(valueReader)
      val schema: QBType = j.get
      (jsonSchema \ "age" \ "maximum").toString must contain("20")
    }

    "to JSON schema with number with min and isExclusive" in {
      val qbSchema = qbClass(
        "firstName" -> qbString,
        "lastName" -> qbString,
        "age" -> qbNumber(exclusiveMin(10.0)))
      val jsonSchema = Json.toJson(qbSchema)
      val j = Json.fromJson(jsonSchema)(valueReader)
      val schema: QBType = j.get
      (jsonSchema \ "age" \ "minimum").toString must contain("10")
      (jsonSchema \ "age" \ "exclusiveMinimum ").toString must contain("true")
    }
  }

}