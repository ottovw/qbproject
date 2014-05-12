package org.qbproject.api.schema

import play.api.libs.json._
import play.api.libs.json.JsArray
import play.api.libs.json.JsString
import scala.Some
import play.api.libs.json.JsNumber
import play.api.libs.json.JsObject

/**
 * QB DSL.
 */
trait QBSchemaDSL {

  implicit def wrapper2doubleRule(rule: DoubleRuleWrapper) = rule.rule
  implicit def tuple2attribute(tuple: (String, QBType)) = tuple._2 match {
    case annotatedType: AnnotatedQBType => QBAttribute(tuple._1, annotatedType.qbType, annotatedType.annotations)
    case _ => QBAttribute(tuple._1, tuple._2)
  }

  /**
   * Classes.
   */
  def cls(els: List[(String, QBType)])(): QBClass =
    buildClass(els)

  def cls(els: (String, QBType)*): QBClass =
    buildClass(els.toList)

  def cls(els: List[(String, QBType)], rules: ValidationRule[JsObject]*): QBClass =
    buildClass(els, rules.toSet)

  private def buildClass(attributes: List[(String,QBType)], rules: Set[ValidationRule[JsObject]] = Set.empty): QBClass = {
     findDuplicates(attributes.map(_._1))(identity) match {
      case Nil => QBClassImpl(attributes.toList.map(tuple2attribute), rules)
      case duplicates=> throw new RuntimeException("qb.duplicate.fields - " + duplicates.mkString(","))
    }
  }

  private def findDuplicates[A, B](list: List[B])(criterion: (B) => A): Seq[A] = {
    (list.groupBy(criterion) filter { case (_, l) => l.size > 1 } keys).toSeq
  }

  /**
   * Array Rules
   */
  def arr(dataType: => QBType): QBArray = QBArrayImpl(dataType)
  def arr(dataType: => QBType, rules: ValidationRule[JsArray]*): QBArray = QBArrayImpl(dataType, rules.toSet)

  /**
   * String Rules
   */
  def string(rules: ValidationRule[JsString]*): QBString = QBStringImpl(rules.toSet)
  def string = QBStringImpl()

  def text = string
  def nonEmptyText = string(minLength(1))

  def minLength(n: Int): MinLengthRule = new MinLengthRule(n)
  def maxLength(n: Int): MaxLengthRule = new MaxLengthRule(n)
  def length(lower: Int, upper: Int): ValidationRule[JsString] = {
    new CompositeRule[JsString] {
      val rules: Set[ValidationRule[JsString]] = Set(minLength(lower), maxLength(upper))
    }
  }

  def enum(values: String*) = string(new EnumRule(values.toList))

  def pattern(regex: String, errMessage: String = "qb.invalid.pattern") = new RegexRule(regex) {
    override val errorMessage = errMessage
  }
  def email = string(pattern("""\b[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""", "invalid.email"))

  def dateTime = new QBDateTimeImpl(Set(DateTimeRule))

  // TODO: DSL should not allow specifying doubles when posixTime type is used 
  def posixTime = new QBPosixTimeImpl(Set(PosixTimeRule))

  /**
   * Number Rules
   */
  def number: QBNumberClass = QBNumberClass()
  def number(rules: ValidationRule[JsNumber]*): QBNumber = QBNumberClass(rules.toSet)

  def min(n: Double) = MinRule(n, false)
  def min(n: Int): DoubleRuleWrapper = DoubleRuleWrapper(MinRule(n, false))

  def max(n: Double) = MaxRule(n, false)
  def max(n: Int) = DoubleRuleWrapper(MaxRule(n, false))

  def exclusiveMin(n: Int) = DoubleRuleWrapper(MinRule(n, true))
  def exclusiveMin(n: Double) = MinRule(n, true)

  def exclusiveMax(n: Int) = DoubleRuleWrapper(MaxRule(n, true))
  def exclusiveMax(n: Double) = MaxRule(n, true)

  def multipleOf(n: Double): MultipleOfRule = MultipleOfRule(n)
  def multipleOf(n: Int) = DoubleRuleWrapper(MultipleOfRule(n))

  def range(lower: Double, upper: Double): ValidationRule[JsNumber] = {
    new CompositeRule[JsNumber] {
      val rules: Set[ValidationRule[JsNumber]] = Set(min(lower), max(upper))
    }
  }

  def range(lower: Int, upper: Int) = DoubleRuleWrapper(new CompositeRule[JsNumber] {
    val rules: Set[ValidationRule[JsNumber]] = Set(min(lower.toDouble), max(upper.toDouble))
  })

  def integer: QBIntegerImpl = QBIntegerImpl()
  def integer(rules: DoubleRuleWrapper*): QBIntegerImpl = QBIntegerImpl(rules.map(_.rule).toSet)

  /**
   * Boolean Rules
   */
  def bool: QBBoolean = QBBooleanImpl()

  /**
   * Array Rules
   */
  def unique: ValidationRule[JsArray] = UniquenessRule()

  /**
   * Object Rules
   */
  def oneOf(values: QBClass*) = QBOneOfImpl(values.toList)
  def minProperties(min: Int) = MinPropertiesRule(min)
  def maxProperties(max: Int) = MaxPropertiesRule(max)

  def id = string

  /**
   * DSL helper class
   */
  case class AnnotatedQBType(qbType: QBType, annotations: List[QBAnnotation]) extends QBType

  /**
   * Annotations
   */
  def default(qbType: QBType, default: JsValue): AnnotatedQBType =
    AnnotatedQBType(qbType, List(new QBDefaultAnnotation(default)))

  /**
   * Mark the attribute as optional.
   */
  def optional(qbType: QBType): AnnotatedQBType =
    AnnotatedQBType(qbType, List(QBOptionalAnnotation()))

  /**
   * Mark the attribute as optional with a default value that is used
   * in case the attribute is not present.
   */
  def optional(qbType: QBType, defaultValue: JsValue): AnnotatedQBType =
    AnnotatedQBType(qbType, List(QBOptionalAnnotation(Some(defaultValue))))

  /**
   * Mark the attribute as read-only.
   */
  def readOnly(qbType: QBType): AnnotatedQBType =
    AnnotatedQBType(qbType, List(new QBReadOnlyAnnotation))

}
