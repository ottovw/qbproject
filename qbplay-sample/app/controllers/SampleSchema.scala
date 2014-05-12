package controllers

import scala.collection.mutable.Map
import play.api.libs.json._
import org.qbproject.api.schema.QBSchema

object SampleSchema {

  import QBSchema._

  val project = cls(
    "id" -> string,
    "name" -> string(minLength(10), maxLength(20)),
    "title" -> default(string, JsString("hallo")),
    "bugs" -> arr(string))

  val person = cls(
    "id" -> string,
    "name" -> string,
    "email" -> string,
    "age" -> number,
    "tags" -> arr(string)
  )

  //  val state = Choice("open", "closed", "assigned")

  //  val comment : QBValue = obj(
  //    "author" -> string,
  //    "message" -> string,
  //    "replies" -> arr(comment)
  //  )

  val bug = cls(
    "id" -> string,
    "title" -> string(minLength(10)),
    //    "state" -> state,
    "reporter" -> string,
    "assignee" -> string,
    "comments" -> cls(
      "author" -> string,
      "message" -> string),
    "dueDate" -> number,
    "version" -> cls(
      "start" -> number,
      "end" -> number))

  val max = cls(
    "name" -> string,
    "age" -> number(min(10)),
    "brother" -> string
  ) - "name"

}