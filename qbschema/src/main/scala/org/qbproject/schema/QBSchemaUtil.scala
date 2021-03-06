package org.qbproject.schema

import play.api.libs.json._
import org.qbproject.api.schema.{QBArray, QBClass, QBType}

object QBSchemaUtil {

  def isNotNullOrUndefined(input: JsValue) = input match {
    case _: JsNull.type => false
    case _: JsUndefined => false
    case _ => true
  }

  /**
   * Used for formatting error messages.
   *
   * @param jsValue
   *           the JsValue which to create a string representation for
   * @return a string representation of the given JsValue
   */
  def mapJsValueToTypeName(jsValue: JsValue): String = jsValue match {
    case _: JsNumber => "number"
    case _: JsString => "string"
    case _: JsBoolean => "boolean"
    case _: JsObject => "object"
    case _: JsArray => "array"
    case _: JsUndefined => "undefined"
    case JsNull => "null"
    case _ => "<no type>"
  }

  def prettyPrint(qbType: QBType, indent: Int = 0): String = qbType match {
    case obj: QBClass => "{\n" +
      obj.attributes.map { field =>
        " " * (indent + 2)  + field.name + ": " +
          prettyPrint(field.qbType, indent + 2) + "\n"}.mkString +
        " " * indent + "}"
    case arr: QBArray => "[" + prettyPrint(arr.items, indent) + "]"
    case q => q.toString
  }
}
