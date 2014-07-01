package org.qbproject.api.schema

import play.api.libs.json._
import play.api.libs.json.JsObject
import scala.reflect.ClassTag
import org.qbproject.schema.visitor.QBPath
import org.qbproject.schema.json._
import org.qbproject.schema.json.serialization.JSONSchemaReads
import org.qbproject.schema.json.serialization.JSONSchemaAnnotationWrites
import org.qbproject.schema.json.processors.JsDefaultValueProcessor
import org.qbproject.schema.json.mapper.{JsTypeMapper, JsTypeMapperOps}

object QBSchema
  extends QBSchemaOps 
  with QBSchemaDSL
  with JSONSchemaReads 
  with JSONSchemaAnnotationWrites
  with JsTypeMapperOps

trait QBValidator extends JsDefaultValueProcessor with JsValidationVisitor {
  def validateJsValue(schema: QBType)(input: JsValue): JsResult[JsValue] =
    process(schema, QBPath(), input)

  def validate(schema: QBClass)(input: JsObject): JsResult[JsObject] =
    process(schema, QBPath(), input).asInstanceOf[JsResult[JsObject]]
}

trait PartialValidator { self: QBValidator =>
  override def ignoreMissingFields = true
}

object QBValidator extends QBValidator
object QBPartialValidator extends QBValidator with PartialValidator

case class QBJson(json: JsObject, schema: QBClass)

case class QBTypeMapper[A <: QBType : ClassTag]() extends JsTypeMapper[A]
