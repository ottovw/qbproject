package org.qbproject.schema.json.processors

import play.api.libs.json._
import org.qbproject.schema._
import org.qbproject.schema.visitor._
import org.qbproject.schema.QBSchemaUtil._
import org.qbproject.api.schema.{QBOptionalAnnotation, QBAttribute}

/**
 * Handles optional annotations.
 */
class JsOptionalAnnotationProcessor extends AnnotationProcessor {

  /**
   * @inheritdoc
   *
   * @param attr
   *             the current attribute in scope
   * @param input
   *             the attribute value
   * @param path
   *             the current path
   * @param jsObject
   *             the parent JsObject
   * @return a JsResult containing a result of type O
   */
  def process(attr: QBAttribute, input: Option[JsValue], path: QBPath, jsObject: JsObject): Option[JsValue] = {
    val optionalAnnotation = attr.annotations.collectFirst { case optional: QBOptionalAnnotation => optional }
    input.flatMap(x => {
      optionalAnnotation.fold[Option[JsValue]](None)(annotation =>
        if (jsObject.keys.contains(attr.name) && isNotNullOrUndefined(x)) {
          Some(x)
        } else {
          annotation.fallBack.fold[Option[JsValue]](None)(Some(_))
        })
    })
  }
}