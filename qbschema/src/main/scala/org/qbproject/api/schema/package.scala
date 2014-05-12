package org.qbproject.api

import play.api.libs.json._
import org.qbproject.api.schema.QBClass
import org.qbproject.schema.QBSchemaUtil


package object schema {

  implicit def toJsObject(qbJs: QBJson): JsObject = qbJs.json
  implicit def toQBObject(qbJs: QBJson): QBClass = qbJs.schema

  implicit class QBTypeExtensionOps(qbType: QBType) {
    def prettyPrint: String = QBSchemaUtil.prettyPrint(qbType)
  }
}