package org.qbproject.api.mongo

import org.specs2.mutable.Specification
import play.api.libs.json.{ JsString, Json, JsSuccess, JsNumber }
import org.qbproject.api.mongo.{MongoTransformer, MongoSchemaExtensions}
import MongoSchemaExtensions._
import org.qbproject.schema._
import org.qbproject.api.schema.{QBClass, QBSchema}
import QBSchema._
import reactivemongo.bson.BSONObjectID
import org.joda.time.DateTime

object MongoTransformerSpec extends Specification {

  "MongoTransformer" should {

    val schema = cls("o" -> objectId, "d" -> dateTime, "e" -> posixTime)
    val id = BSONObjectID.generate.stringify
    val date = new DateTime().toString()
    val time = System.currentTimeMillis() / 1000L

    "support writes" in {
      val i = Json.obj("o" -> id, "d" -> date, "e" -> time)
      val mongoTransformer = new MongoTransformer(schema)
      val result = mongoTransformer.write(i)
      result.get must beEqualTo(Json.obj(
        "o" -> Json.obj("$oid" -> id),
        "d" -> Json.obj("$date" -> date),
        "e" -> Json.obj("$date" -> time)))
    }

    "support writes with multiple nested" in {
      val schema = cls("o" -> objectId, "d" -> dateTime, "e" -> posixTime, "i" -> cls("x" -> objectId))
      val i = Json.obj("o" -> id, "d" -> date, "e" -> time, "i" -> Json.obj("x" -> id))
      val mongoTransformer = new MongoTransformer(schema)
      val result = mongoTransformer.write(i)
      result.get must beEqualTo(Json.obj(
        "o" -> Json.obj("$oid" -> id),
        "d" -> Json.obj("$date" -> date),
        "e" -> Json.obj("$date" -> time),
        "i" -> Json.obj("x" -> Json.obj("$oid" -> id))))
    }

    "support reads" in {
      val i = Json.obj(
        "o" -> Json.obj("$oid" -> id),
        "d" -> Json.obj("$date" -> date),
        "e" -> Json.obj("$date" -> time))
      val expected = Json.obj("o" -> id, "d" -> date, "e" -> time)
      val mongoTransformer = new MongoTransformer(schema)
      val res = mongoTransformer.read(i)
      res.get must beEqualTo(expected)
    }

    "support reads with array" in {
      val schema = cls(
        "o" -> arr(objectId),
        "d" -> dateTime,
        "e" -> posixTime)

      val i = Json.obj(
        "o" -> Json.arr(Json.obj("$oid" -> id)),
        "d" -> Json.obj("$date" -> date),
        "e" -> Json.obj("$date" -> time))
      val expected = Json.obj("o" -> Json.arr(id), "d" -> date, "e" -> time)
      val mongoTransformer = new MongoTransformer(schema)
      val result = mongoTransformer.read(i)
      result.get must beEqualTo(expected)
    }

    "support nested reads" in {
      val schema = cls("o" -> objectId, "d" -> cls("o2" -> objectId), "e" -> posixTime)

      val i = Json.obj(
        "o"  -> Json.obj("$oid" -> id),
        "d" -> Json.obj("o2" -> Json.obj("$oid" -> id)),
        "e" -> Json.obj("$date" -> time))
      val expected = Json.obj("o" -> id, "d" -> Json.obj("o2" -> id), "e" -> time)
      val mongoTransformer = new MongoTransformer(schema)
      mongoTransformer.read(i).get must beEqualTo(expected)
    }

    "support multiple, nested reads" in {

      val schema = cls("o" -> objectId, "d" -> dateTime, "e" -> posixTime)


      val i = Json.obj(
        "o" -> Json.obj(
          "$oid" -> id
        ),
        "d" -> Json.obj("$date" -> date),
        "e" -> Json.obj("$date" -> time),
        "f" -> Json.obj(
          "g" -> Json.obj(
            "date" -> Json.obj("$date" -> date),
            "time" -> Json.obj("$date" -> time)
          )
        )
      )
      val expected = Json.obj(
        "o" -> id,
        "d" -> date,
        "e" -> time,
        "f" -> Json.obj(
          "g" -> Json.obj(
            "date" -> date,
            "time" -> time)))
      val mongoTransformer = new MongoTransformer(schema
        ++ cls(
        "f" -> cls(
          "g" -> cls(
            "date" -> dateTime,
            "time" -> posixTime))))
      val result = mongoTransformer.read(i)

      result.asOpt.isDefined must beTrue
      result.get must beEqualTo(expected)
    }

    "support two same elements, which needs to be transformed" in {
      val input = Json.obj(
        "o" -> Json.obj(
          "$oid" -> id
        ),
        "d" -> Json.obj("$date" -> date),
        "e" -> Json.obj("$date" -> time),
        "f" -> Json.obj(
          "g" -> Json.obj(
            "date" -> Json.obj("$date" -> date),
            "date2" -> Json.obj("$date" -> date),
            "time" -> Json.obj("$date" -> time))))

      val expected = Json.obj(
        "o" -> id,
        "d" -> date,
        "e" -> time,
        "f" -> Json.obj(
          "g" -> Json.obj(
            "date" -> date,
            "date2" -> date,
            "time" -> time)))
      val mongoTransformer = new MongoTransformer(schema ++ cls(
        "f" -> cls(
          "g" -> cls(
            "date" -> dateTime,
            "date2" -> dateTime,
            "time" -> posixTime))))

      val result = mongoTransformer.read(input)
      result.asOpt.isDefined must beTrue
      result.get must beEqualTo(expected)
    }

    val core = cls(
      "id" -> objectId,
      "lastModified" -> dateTime,
      "creationDate" -> dateTime)

    val companyCore = core ++ cls(
      "companyId" -> integer,
      "companyStatus" -> enum("active", "inactive"))

    val coordinates = cls(
      "lat" -> number,
      "lng" -> number)

    val companyInfo = cls(
      "name" -> string,
      "location" -> cls(
        "coordinates" -> coordinates))

    val dbCompany: QBClass = companyCore ++ cls(
      "company" -> companyInfo)

    "support write complex" in {
      val company = Json.obj(
        "id" -> id,
        "lastModified" -> date,
        "creationDate" -> date,
        "companyId" -> 1,
        "companyStatus" -> "active",
        "company" -> Json.obj(
          "name" -> "ACME",
          "location" -> Json.obj(
            "coordinates" -> Json.obj(
              "lat" -> 12.34,
              "lng" -> 23.45))))
      val mongoTransformer = new MongoTransformer(dbCompany)
      val jsValue = mongoTransformer.write(company)
      jsValue.asOpt must beSome
    }

    "fail validation" in {
      val date = new DateTime().toString()

      val company = Json.obj(
        "id" -> BSONObjectID.generate.stringify,
        "lastModified" -> date,
        "creationDate" -> date,
        "companyId" -> 1,
        "companyStatus" -> "active",
        "company" -> Json.obj(
          "location" -> Json.obj(
            "lat" -> 12.34,
            "lng" -> 23.45)))
      val mongoTransformer = new MongoTransformer(schema)
      val jsValue = mongoTransformer.write(company)
      jsValue.asOpt must beNone
    }

    "support optional writes with present field" in {
      val schema = cls("o" -> objectId, "d" -> optional(dateTime), "e" -> posixTime)
      val instance = Json.obj("o" -> id, "d" -> date, "e" -> time)
      val mongoTransformer = new MongoTransformer(schema)
      val result = mongoTransformer.write(instance)
      result.get must beEqualTo(Json.obj(
        "o" -> Json.obj("$oid" -> id),
        "d" -> Json.obj("$date" -> date),
        "e" -> Json.obj("$date" -> time)))
    }

    "support optional reads with present field" in {
      val schema = cls("o" -> objectId, "d" -> optional(dateTime), "e" -> posixTime)
      val instance = Json.obj("o" -> Json.obj(
        "$oid" -> id),
        "d" -> Json.obj("$date" -> date),
        "e" -> Json.obj("$date" -> time))
      val expected = Json.obj("o" -> id, "d" -> date, "e" -> time)
      val mongoTransformer = new MongoTransformer(schema)
      mongoTransformer.read(instance).get must beEqualTo(expected)
    }

    "support optional writes with missing field" in {
      val schema = cls("o" -> objectId, "d" -> optional(dateTime), "e" -> posixTime)
      val instance = Json.obj("o" -> id, "e" -> time)
      val mongoTransformer = new MongoTransformer(schema)
      val result = mongoTransformer.write(instance)
      result.get must beEqualTo(Json.obj(
        "o" -> Json.obj("$oid" -> id),
        "e" -> Json.obj("$date" -> time)))
    }

    "support optional reads with present field" in {
      val schema = cls("o" -> objectId, "d" -> optional(dateTime), "e" -> posixTime)
      val instance = Json.obj("o" -> Json.obj(
        "$oid" -> id),
        "e" -> Json.obj("$date" -> time))
      val expected = Json.obj("o" -> id, "e" -> time)
      val mongoTransformer = new MongoTransformer(schema)
      mongoTransformer.read(instance).get must beEqualTo(expected)
    }

    "support default writes with present field" in {
      val schema = cls("o" -> objectId, "d" -> default(dateTime, JsString(date)), "e" -> posixTime)
      val instance = Json.obj("o" -> id, "d" -> date, "e" -> time)
      val mongoTransformer = new MongoTransformer(schema)
      val result = mongoTransformer.write(instance)
      result.get must beEqualTo(Json.obj(
        "o" -> Json.obj("$oid" -> id),
        "d" -> Json.obj("$date" -> date),
        "e" -> Json.obj("$date" -> time)))
    }

    "support default reads with present field" in {
      val schema = cls("o" -> objectId, "d" -> default(dateTime, JsString(date)), "e" -> posixTime)
      val instance = Json.obj("o" -> Json.obj(
        "$oid" -> id),
        "d" -> Json.obj("$date" -> date),
        "e" -> Json.obj("$date" -> time))
      val expected = Json.obj("o" -> id, "d" -> date, "e" -> time)
      val mongoTransformer = new MongoTransformer(schema)
      mongoTransformer.read(instance).get must beEqualTo(expected)
    }

    /**
     * Extension to MongoTransfomer
     */
    "rewrite _id to id" in {
      val schema = cls("id" -> objectId)
      val instance = Json.obj("_id" -> Json.obj("$oid" -> "52eb6c66e4b08a001831aa9a"))
      val expected = Json.obj("id" -> "52eb6c66e4b08a001831aa9a")
      val mongoTransformer = new MongoTransformer(schema)
      mongoTransformer.read(instance).get must beEqualTo(expected)
    }

    "rewrite id to _id" in {
      val schema = cls("id" -> objectId)
      val instance = Json.obj("id" -> "52eb6c66e4b08a001831aa9a")
      val expected = Json.obj("_id" -> Json.obj("$oid" -> "52eb6c66e4b08a001831aa9a"))
      val mongoTransformer = new MongoTransformer(schema)
      mongoTransformer.write(instance).get must beEqualTo(expected)
    }

  }

}