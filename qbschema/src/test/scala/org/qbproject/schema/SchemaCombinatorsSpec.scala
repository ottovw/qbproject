package org.qbproject.schema

import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.qbproject.api.schema._

@RunWith(classOf[JUnitRunner])
object SchemaCombinatorsSpec extends Specification {

  import QBSchema._

  "Schema combinators" should {

    val schema = qbClass(
      "o" -> qbClass(
        "n" -> qbClass(
          "s" -> qbString(minLength(5)),
          "t" -> qbInteger,
          "v" -> qbNumber)))

    "add a field to an existing path" in {
      val updatedSchema = schema ++ ("o.n", "e" -> qbNumber)
      updatedSchema.follow[QBNumber]("o.n.e") must beAnInstanceOf[QBNumber]
    }

    "add a field to an existing path directly" in {
      val schema = qbClass("x" -> qbString)
      val updatedSchema = schema ++ ("e" -> qbNumber)
      updatedSchema.attributes.size must beEqualTo(2)
    }

    "add multiple fields to an existing path" in {
      val updatedSchema = schema ++ ("o", "e" -> qbNumber, "xx" -> qbString)
      val resolved = updatedSchema.follow[QBClass]("o")
      resolved.attributes.size must beEqualTo(3)
    }

    "remove a field from an existing path" in {
      val updatedSchema = schema - "o.n"
      updatedSchema.attributes.size must beEqualTo(1)
    }

    "remove a field directly" in {
      val schema = qbClass("i" -> qbString, "j" -> qbString)
      val updatedSchema = schema - "i"
      updatedSchema.attributes.size must beEqualTo(1)
    }

    "remove multiple fields directly" in {
      val schema = qbClass("i" -> qbString, "j" -> qbString, "x" -> qbString)
      val updatedSchema = schema -- ("i", "j")
      updatedSchema.attributes.size must beEqualTo(1)
    }

    "remove multiple fields from an existing path" in {
      val updatedSchema = schema -- ("o.n.s", "o.n.t")
      val resolved = updatedSchema.follow[QBClass]("o.n")
      resolved.attributes.size must beEqualTo(1)
    }

    "make a field optional on an existing path" in {
      val updatedSchema = schema ? "o.n"
      val resolved = updatedSchema.follow[QBClass]("o")
      val n = resolved.attributes.find(_.name == "n")
      // TODO: provide convenience methods via implicit classes
      n.get.annotations.find(_.isInstanceOf[QBOptionalAnnotation]) must beSome
    }

    "make multiple fields optional on an existing path" in {
      val updatedSchema = schema ? ("o.n.s", "o.n.t")
      val resolved = updatedSchema.follow[QBClass]("o.n")
      val n = resolved.attributes.find(_.name == "s")
      val t = resolved.attributes.find(_.name == "t")
      n.get.annotations.find(_.isInstanceOf[QBOptionalAnnotation]) must beSome
      t.get.annotations.find(_.isInstanceOf[QBOptionalAnnotation]) must beSome
    }

    "make a field read-only on an existing path" in {
      val updatedSchema = schema readOnly "o.n"
      val resolved = updatedSchema.follow[QBClass]("o")
      val n = resolved.attributes.find(_.name == "n")
      n.get.annotations.find(_.isInstanceOf[QBReadOnlyAnnotation]) must beSome
    }

    "make multiple fields read-only on an existing path" in {
      val updatedSchema = schema readOnly ("o.n.s", "o.n.t")
      val resolved = updatedSchema.follow[QBClass]("o.n")
      val n = resolved.attributes.find(_.name == "s")
      val t = resolved.attributes.find(_.name == "t")
      n.get.annotations.find(_.isInstanceOf[QBReadOnlyAnnotation]) must beSome
      t.get.annotations.find(_.isInstanceOf[QBReadOnlyAnnotation]) must beSome
    }

    "not be able to remove a field from an non-existing leaf path" in {
      schema - "o.x" must throwA[RuntimeException]("field.does.not.exist")
    }

    "not be able to remove a field from an non-existing path" in {
      schema - "o.x.s" must throwA[RuntimeException]("field.does.not.exist")
    }

    "not be able to add a field to a non-existing path" in {
      schema + ("o.x", "e" -> qbNumber) must throwA[RuntimeException]("field.does.not.exist")
    }

    "be able to add a field" in {
      val schema = qbClass("x" -> qbString)
      val updatedSchema = schema + ("e" -> qbNumber)
      updatedSchema.attributes.size must beEqualTo(2)
    }

    "be able to rename a field on an existing path" in {
      val updatedSchema = schema rename ("o.n", "e")
      val resolved = updatedSchema.follow[QBClass]("o.e")
      resolved must beAnInstanceOf[QBClass]
    }

    "be able to rename a field" in {
      val schema = qbClass("x" -> qbString)
      val updatedSchema = schema rename ("x", "y")
      val resolved = updatedSchema.follow[QBString]("y")
      resolved must beAnInstanceOf[QBString]
    }

    "be able to make a field optional" in {
      val schema = qbClass("x" -> qbString)
      val updatedSchema = schema ? "x"
      val attr = updatedSchema.attributes.find(_.name == "x")
      attr must beSome.which(_.annotations.exists(_.isInstanceOf[QBOptionalAnnotation]))
    }

    "not be able to rename a field on an non-existing path" in {
      schema rename ("o.x", "A") must throwA[RuntimeException]("field.does.not.exist")
    }

    "be able to keep fields on a existing path" in {
      val updatedSchema = schema keep ("o.n", List("s"))
      val resolved = updatedSchema.follow[QBClass]("o.n")
      resolved.attributes.size must beEqualTo(1)
    }

    "be able to keep fields on a root path" in {
      val updatedSchema = schema keep ("", List("o"))
      val resolved = updatedSchema.follow[QBClass]("o")
      resolved.attributes.size must beEqualTo(1)
    }

    "not be able to keep fields on a non-existing path" in {
      schema keep ("o.x", List("s")) must throwA[RuntimeException]
    }

    "be able to keep fields without path" in {
      val schema = qbClass(
        "a" -> qbInteger,
        "b" -> qbString)
      val updatedSchema = schema keep List("a")
      updatedSchema.attributes.size must beEqualTo(1)
    }

    "not be able to remove a fields from an existing flat path" in {
      val schema = qbClass("o" -> qbString)
      val updatedSchema = schema - "o"
      updatedSchema.follow[QBClass]("o") must throwA[RuntimeException]
    }

    "add objects" in {
      val schema = qbClass("o" -> qbString)
      val updated = schema ++ qbClass(
        "a" -> qbInteger)
      updated.attributes must have size 2
    }

    "override existing field" in {
      val schema = qbClass("o" -> qbString)
      val temp = schema ++ qbClass(
        "a" -> qbInteger)
      val updated = temp + ("a" -> qbString)
      updated.attributes must have size 2
      updated.attributes.exists(_.qbType.isInstanceOf[QBString]) must beTrue
    }

    "rename attribute by type" in {
      val schema = qbClass("o" -> qbString)
      val updated = schema.mapOverAttributes(_.qbType.isInstanceOf[QBString])(attr => QBAttribute("o2", attr.qbType)).asInstanceOf[QBClass]
      updated.attributes.exists(_.name == "o2") must beTrue
    }

    "rename attribute" in {
      val schema = qbClass("o" -> qbString)
      val updated = schema.mapOverAttributes(_.name == "o")(attr => QBAttribute("o2", attr.qbType)).asInstanceOf[QBClass]
      updated.attributes.exists(_.name == "o2") must beTrue
    }

    "rename object attribute" in {
      val schema = qbClass("o" -> qbClass("s" -> qbString))
      val updated = schema.mapOverAttributes(_.name == "o")(attr => QBAttribute("o2", attr.qbType)).asInstanceOf[QBClass]
      updated.attributes.exists(_.name == "o2") must beTrue
    }

    "add attribute" in {
      val schema = qbClass(
        "o" -> qbClass(
          "n" -> optional(qbClass(
            "s" -> qbString(minLength(5)),
            "t" -> qbInteger,
            "v" -> qbNumber))))

      val updated = update[QBClass](List("o", "n"), schema, obj => {
        val fields = obj.attributes
        QBClassImpl(fields :+ QBAttribute("vv", qbNumber))
      })

      val buildDesc = resolve(List("o", "n", "vv"), updated)
      buildDesc._2 must beAnInstanceOf[QBNumber]
    }

    "should support equals" in {
      val otherSchema = qbClass(
        "o" -> qbClass(
          "n" -> qbClass(
            "s" -> qbString(minLength(5)),
            "t" -> qbInteger,
            "v" -> qbNumber)))
      schema must beEqualTo(otherSchema)
      schema.isEquals(otherSchema) must beTrue
      schema.equals(otherSchema) must beTrue
    }

    "emit true when comparing equal schemas" in {
      val otherSchema = qbClass(
        "o" -> qbClass(
          "n" -> qbClass(
            "s" -> qbString(minLength(5)),
            "t" -> qbInteger,
            "v" -> qbNumber)))
      schema must beEqualTo(otherSchema)
      schema.isEquals(otherSchema) must beTrue
      schema.equals(otherSchema) must beTrue
    }

    "emit true when comparing equal schemas with arrays" in {
      val schemaA = qbClass(
        "a" -> qbString,
        "b" -> optional(qbInteger),
        "c" -> qbClass("d" -> qbBoolean),
        "e" -> qbList(qbInteger)
      )

      val schemaB = qbClass(
        "a" -> qbString,
        "b" -> optional(qbInteger),
        "c" -> qbClass("d" -> qbBoolean),
        "e" -> qbList(qbInteger)
      )
      
      schemaA.isEquals(schemaB) must beTrue
      schemaA.equals(schemaB) must beTrue
    }

    "emit false when comparing different schemas" in {
      val otherSchema = qbClass(
        "o" -> qbClass(
          "n" -> qbClass(
            "s" -> qbString(minLength(5)),
            "t" -> qbInteger)))
      schema.isEquals(otherSchema) must beFalse
      schema.equals(otherSchema) must beFalse
    }

    "emit false when comparing equal schemas but with different annotation" in {
      val otherSchema = qbClass(
        "o" -> qbClass(
          "n" -> qbClass(
            "s" -> qbString(minLength(5)),
            "t" -> optional(qbInteger),
            "v" -> qbNumber)))
      schema.isEquals(otherSchema) must beFalse
      schema.equals(otherSchema) must beFalse
    }

    "should determine whether a schema is part of another schema" in {
      val otherSchema = qbClass(
        "n" -> qbClass(
          "s" -> qbString(minLength(5)),
          "t" -> qbInteger,
          "v" -> qbNumber))
      otherSchema.isSubSetOf(schema) must beTrue
    }

    "should determine whether a schema is part of itself" in {
      schema.isSubSetOf(schema) must beTrue
    }

    "should determine whether a schema is not part of another schema" in {
      val otherSchema = qbClass(
        "n" -> qbClass(
          "s" -> qbString(minLength(5)),
          "t" -> qbInteger,
          "v" -> qbNumber))
      schema.isSubSetOf(otherSchema) must beFalse
    }
  }
}