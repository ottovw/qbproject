package org.qbproject.api.csv

import CSVColumnUtil._
import java.io.InputStream
import scala.collection.JavaConversions._
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime
import scala.Array.canBuildFrom
import au.com.bytecode.opencsv.CSVReader
import java.io.InputStreamReader

class CSVColumnUtil[A <: Any](
  factory: CSVRow => A) {

  def parse[B](input: InputStream, seperator: Char, quoteChar: Char)(f: List[Try[A]] => List[B] = (x: List[Try[A]]) => x.collect { case Success(s) => s }): List[B] = {
    val reader = new CSVReader(new InputStreamReader(input, "utf-8"), seperator, quoteChar)
    val (headers :: rawRows) = reader.readAll().toList.map(_.map(_.trim).toList)

    val rows = rawRows.map(row => CSVRow(row, headers))

    f(convertRows(rows))
  }

  def convertRows(rows: List[CSVRow]): List[Try[A]] = {
    rows.view.zipWithIndex.map {
      row =>
        val rowResult = Try {
          factory(row._1)
        }
        if (rowResult.isFailure) {
          val error = rowResult.asInstanceOf[Failure[_]]
          val errorMessage = "error in row " + (row._2 + 2) + " : " + error.exception.getMessage
          //          Logger.info("[CSVImporter] " + error.exception.getStackTraceString)
          //          Logger.info("[CSVImporter] " + errorMessage)
          //
          //          // TODO remove
          error.exception.printStackTrace()
          Failure(CSVException(row._2 + 2, error.exception))
        } else
          rowResult
    }.toList
  }
}

case class CSVException(rowNr: Int, e: Throwable) extends RuntimeException

object CSVColumnUtil {

  def apply[A <: AnyRef](factory: CSVRow => A) = new CSVColumnUtil(factory)

  case class CSVRow(row: List[String], headers: List[String]) {
    def getColumnData(colName: String): String = {
      val index = headers.indexOf(colName)
      if (index == -1) {
        throw new RuntimeException("column: " + colName + " not found.")
      }
      row(index).trim
    }
  }

  def getColumnData(colName: String)(implicit row: CSVRow): String = row.getColumnData(colName)

  def getColumnDataAsList(colName: String)(implicit row: CSVRow) = {
    getColumnData(colName).split("\n").map(_.trim).filterNot(_ == "").toList
  }

  implicit class oneOfWrapper[A](wrapped: A) {
    def oneOf(colNames: String*)(body: A => A)(implicit row: CSVRow): A = {
      if (colNames.filterNot(isColumnEmpty).size != 1) throw new RuntimeException("oneOf rule violated: " + colNames)
      body(wrapped)
    }
  }

  /**
   *
   */

  def asString(colName: String)(implicit row: CSVRow): String = {
    getColumnData(colName)
  }

  def asStringList(colName: String)(implicit row: CSVRow): List[String] = {
    getColumnDataAsList(colName)
  }

  def asInt(colName: String)(implicit row: CSVRow): Int = {
    getColumnData(colName).toInt
  }

  def asDouble(colName: String)(implicit row: CSVRow): Double = {
    getColumnData(colName).toDouble
  }

  def asDate(colName: String, pattern: String)(implicit row: CSVRow): DateTime = {
    DateTimeFormat.forPattern(pattern).parseDateTime(getColumnData(colName))
  }

  def asBoolean(colName: String)(implicit row: CSVRow): Boolean = asBoolean(colName, "true", "false")
  def asBoolean(colName: String, trueToken: String = "true", falseToken: String = "false")(implicit row: CSVRow): Boolean = {
    getColumnData(colName) match {
      case t if t == trueToken => true
      case f if f == falseToken => false
      case _ => throw new RuntimeException(s"Invalid value in colName")
    }
  }

  def isColumnEmpty(colName: String)(implicit row: CSVRow) = row.headers.indexOf(colName) match {
    case -1 => true
    case x => row.row(x).isEmpty
  }

  def optional[A](colName: String)(f: String => A)(implicit row: CSVRow): Option[A] = {
    if (isColumnEmpty(colName)) None
    else Some(f(colName))
  }

  def isValidOptionGroup(options: Seq[Option[_]]) = {
    if (options.forall(_.isDefined)) true
    else if (options.forall(_.isEmpty)) false
    else throw new RuntimeException("Invalid option group")
  }

  def optionGroup[A](options: Option[_]*)(f: => A) = {
    if (isValidOptionGroup(options))
      Some(f)
    else None
  }

  def asEnum[E <: Enumeration](colName: String, enum: E)(implicit row: CSVRow): E#Value = asEnum(enum)(colName)(row)
  def asEnum[E <: Enumeration](enum: E)(colName: String)(implicit row: CSVRow): E#Value = {
    try { enum.withName(getColumnData(colName)) } catch { case _: Throwable => throw new RuntimeException("invalid enum - " + colName) }
  }

}