package org.qbproject.schema.json.processors

import org.qbproject.schema._
import org.qbproject.schema.visitor._
import play.api.libs.json._
import org.qbproject.api.schema.{QBDefaultAnnotation, QBAttribute}

/**
 * Handles default value annotations.
 */
class JsDefaultAnnotationProcessor extends AnnotationProcessor {

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
    attr.annotations
      .collectFirst { case default: QBDefaultAnnotation => default }
      .fold {
      input
    } { defaultAnnotation =>
      Some(defaultAnnotation.value)
    }
  }

}