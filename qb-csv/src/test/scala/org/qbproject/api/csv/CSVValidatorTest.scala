package org.qbproject.api.csv

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import java.io.ByteArrayInputStream
import play.api.libs.json._
import org.qbproject.api.schema.{QBClass, QBType, QBValidator, QBSchema}
import QBSchema._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class CSVValidatorTest extends Specification {

  val testSchema = cls(
    "id" -> string,
    "name" -> string,
    "email" -> string,
    "age" -> readOnly(number),
    "tags" -> arr(string))

  val testData = """id;name;email;age;tags[0];tags[1]
    1;Eddy;eddy@qb.org;28;yolo;quake
    2;Otto;otto@qb.org;26;ginger;"""

  "CSV Adapter" should {

    def parse(value: QBType, testData: String, adapter: CSVSchemaAdapter = new CSVSchemaAdapter {}) = {
      val parser = new CSVColumnUtil(row => adapter.adapt(value.asInstanceOf[QBClass])(row))
      parser.parse(new ByteArrayInputStream(testData.getBytes("utf-8")), ';', '"')(identity)
    }

    "read basic csv" in {
      val result = parse(testSchema, testData)
      result.size must beEqualTo(2)
      result(0).get.get must beEqualTo(Json.obj(
        "id" -> "1",
        "name" -> "Eddy",
        "email" -> "eddy@qb.org",
        "age" -> 28,
        "tags" -> Json.arr("yolo", "quake")))
    }

    "read basic csv with default" in {
      val testSchema = cls(
        "id" -> string,
        "name" -> string,
        "email" -> string,
        "age" -> readOnly(number),
        "tags" -> arr(string),
        "sex" -> default(string, JsString("f")))
      val result = parse(testSchema, testData)
      result.size must beEqualTo(2)
      val r = QBValidator.validate(testSchema)(result(0).get.get.asInstanceOf[JsObject])
      r.get must beEqualTo(Json.obj(
        "id" -> "1",
        "name" -> "Eddy",
        "email" -> "eddy@qb.org",
        "age" -> 28,
        "tags" -> Json.arr("yolo", "quake"),
        "sex" -> "f"))
    }

    "read basic csv with missing optional" in {
      val testSchema = cls(
        "id" -> string,
        "name" -> string,
        "email" -> string,
        "age" -> readOnly(number),
        "tags" -> arr(string),
        "sex" -> optional(string, JsString("f")))
      val testData = """id;name;email;age;tags[0];tags[1];sex
        1;Eddy;eddy@qb.org;28;yolo;quake
        2;Otto;otto@qb.org;26;ginger"""
      val result = parse(testSchema, testData)

      result.size must beEqualTo(2)
      // TODO: get get get
      QBValidator.validate(testSchema)(result(0).get.get.asInstanceOf[JsObject]).get must beEqualTo(Json.obj(
        "id" -> "1",
        "name" -> "Eddy",
        "email" -> "eddy@qb.org",
        "age" -> 28.0,
        "tags" -> Json.arr("yolo", "quake"),
        "sex" -> "f"))
    }

    "read basic csv with present optional" in {
      val testSchema = cls(
        "id" -> string,
        "name" -> string,
        "email" -> string,
        "age" -> readOnly(number),
        "sex" -> optional(string, JsString("f")),
        "tags" -> arr(string)
        )
      val testData = """id;name;email;age;sex;tags[0];tags[1];
        1;Eddy;eddy@qb.org;28;m;yolo;quake
        2;Otto;otto@qb.org;26;f;ginger"""
      val result = parse(testSchema, testData)
      result.size must beEqualTo(2)
      // TODO: get get get
      QBValidator.validate(testSchema)(result(0).get.get.asInstanceOf[JsObject]).get must beEqualTo(Json.obj(
        "id" -> "1",
        "name" -> "Eddy",
        "email" -> "eddy@qb.org",
        "age" -> 28.0,
        "sex" -> "m",
        "tags" -> Json.arr("yolo", "quake")
        ))
    }

//    "convert path to String" in {
//      resolvePath((JsPath \ "hallo" \ "dude")(1)) must beEqualTo("hallo.dude[1]")
//    }

//    "works for #pathExists" in {
//
//      val row = new CSVRow(List("1", "", ""), List("A", "B"))
//
//      pathExists((JsPath \ "A"))(row) must beTrue
//      pathExists((JsPath \ "B"))(row) must beFalse
//      pathExists((JsPath \ "C"))(row) must beFalse
//    }

    "read csv with boolean" in {
      val data = "bool;\ntrue"
      val schema = cls("bool" -> bool)

      val result = parse(schema, data)

      result must have size 1
      result(0).get.get must beEqualTo(Json.obj(
        "bool" -> true))
    }

    "allow tranformer plugins" in {
      val rangeData = """id;range
         1;"2-3""""
      val rangeSchema = cls(
        "id" -> number,
        "range" -> cls(
          "start" -> number,
          "end" -> number))

      val range = "([0-9]+)\\s*-\\s*([0-9]+)".r

      val transformer = new CSVAdaptedValidator(List(
        ("range.start", "range", (x: String) => x match { case range(start, end) => start }),
        ("range.end", "range", (x: String) => x match { case range(start, end) => end })))

      val result = parse(rangeSchema, rangeData, transformer)

      result.size must beEqualTo(1)
      result(0).get.get must beEqualTo(Json.obj(
        "id" -> 1,
        "range" -> Json.obj(
          "start" -> 2,
          "end" -> 3)))
    }

    "allow arrays with simple value in one column" in {
      val csv = "array\n\"a\nb\""
      val schema = cls(
        "array" -> arr(string))

      def arrayAdapter(index: Int)(value: String): String = {
        value.split("\n")(index)
      }

      // TODO Argh, how ugly -.-
      val transformer = new CSVAdapter(List(
        ("array", "array", (value: String) => value),
        ("array[0]", "array", arrayAdapter(0) _),
        ("array[1]", "array", arrayAdapter(1) _)))

      val result = parse(schema, csv, transformer)

      result must have size 1
      result(0).get.get must beEqualTo(Json.obj(
        "array" -> List("a", "b")))
    }

    "allow arrays with multiple values in one column" in {
      val csv = "array\n\"1//2\n3//4\""
      val schema = cls(
        "array" -> arr(cls(
          "first" -> string,
          "second" -> string)))

      def arrayAdapter(line: Int, part: Int)(value: String) = {
        val results = value.split("\n")(line).split("//").map(_.trim)
        results(part)
      }

      // TODO Argh, how ugly -.-
      val transformer = new CSVAdapter(List(
        ("array", "array", (value: String) => value),
        ("array[0].first", "array", arrayAdapter(0, 0) _),
        ("array[0].second", "array", arrayAdapter(0, 1) _),
        ("array[1].first", "array", arrayAdapter(1, 0) _),
        ("array[1].second", "array", arrayAdapter(1, 1) _)))

      val result = parse(schema, csv, transformer)

      result must have size 1
      result(0).get.get must beEqualTo(Json.obj(
        "array" -> List(Json.obj(
          "first" -> "1",
          "second" -> "2"), Json.obj(
          "first" -> "3",
          "second" -> "4"))))
    }

  }
}