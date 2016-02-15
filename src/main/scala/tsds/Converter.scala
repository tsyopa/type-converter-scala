/**
  * Useful with Anorm.
  * 
  * Example:
  * {{{
  *   class Person(
  *       var name: String = "",
  *       var age: Int = 0,
  *       // ... much more fields
  *   )
  *
  *   DB.withConnection { implicit conn =>
  *     val persons: List[Person] = SQL("""
  *         SELECT ...
  *         FROM ...
  *     """)().map(row => Converter.extract(new Person, row.asMap).toList
  *   }
  * }}}
  */
package tsds

import java.text.SimpleDateFormat
import java.util.Date

import reflect.runtime.universe.{Try => _, _}
import scala.util.Try


object Converter {

  def extract[A](obj: A, data: Map[String, Any]): A = {
    val index = collection.mutable.Map[String, (String, String)]()
    data.keySet.foreach(k => index += cleanKey(k) + "_$eq" -> (k, cleanKey(k)))
    obj.getClass.getMethods.foreach { m =>
      index.get(m.getName).foreach { k =>
        val newValue = Converter.unOptionize(data.get(k._1).orNull)
        val fieldName = k._2
        val clazz: Class[_] = m.getParameterTypes.apply(0)
        if (Converter.isOptionClass(clazz)) {
          val unwrappedClazz = Converter.classOfOptionField(obj.getClass, fieldName)
          val newValueConverted = Option(Converter.as(newValue, unwrappedClazz))
          m.invoke(obj, newValueConverted.asInstanceOf[AnyRef])
        } else {
          val newValueConverted = Converter.as(newValue, clazz)
          m.invoke(obj, newValueConverted.asInstanceOf[AnyRef])
        }
      }
    }
    obj
  }

  def as[A: TypeTag](v: Any, c: Class[A]): A = {
    c match {
      case q if q == classOf[String] => asString(v).asInstanceOf[A]
      case q if q == classOf[Long] => asLong(v).asInstanceOf[A]
      case q if q == classOf[Int] => asInt(v).asInstanceOf[A]
      case _ => v.asInstanceOf[A]
    }
  }

  def unOptionize(value: Any): Any = value match {
    case v: Option[_] => v.orNull
    case v => v
  }

  def isOptionClass(clazz: Class[_]): Boolean = {
    val optClass = classOf[Option[_]]
    clazz match {
      case `optClass` => true
      case _ => false
    }
  }

  def classOfOptionField(cl: Class[_], field: String): Class[_] = {
    val mirror = runtimeMirror(this.getClass.getClassLoader)
    val method = mirror.classSymbol(cl).toType.decls.filter(_.name.toString == field).head.asMethod
    mirror.runtimeClass(method.returnType.asInstanceOf[TypeRefApi].args.head.typeSymbol.asClass)
  }

  def asInt(v: Any): Int = v match {
    case v: Int => v
    case v: Integer => v.intValue
    case v: Long => v.toInt
    case v: Byte => v.toInt
    case v: Char => v.toInt
    case v: Double => scala.math.round(v).toInt
    case v: Float => scala.math.round(v)
    case v: BigInt => v.intValue()
    case v: BigDecimal => v.intValue()
    case v: String => v.toInt
    case v: Boolean => if (v) 1 else 0
    case _ => throw new UnknownConversionAttemptException
  }

  def asLong(v: Any): Long = v match {
    case v: Int => v.toLong
    case v: Integer => v.longValue()
    case v: Long => v.toLong
    case v: Byte => v.toLong
    case v: Char => v.toLong
    case v: Double => scala.math.round(v)
    case v: Float => scala.math.round(v).toLong
    case v: BigInt => v.longValue()
    case v: BigDecimal => v.longValue()
    case v: String => v.toLong
    case v: Boolean => if (v) 1 else 0
    case _ => throw new UnknownConversionAttemptException
  }

  def asString(v: Any): String = v.toString

  def asDate(v: Any): Date = v match {
    case v: java.sql.Date => v.asInstanceOf[Date]
    case v: java.sql.Timestamp => v.asInstanceOf[Date]
    case v: Long => new Date(v)
    case v: Int => new Date(v.toLong)
    case v: String => tryParseDate(v)
    case _ => throw new UnknownConversionAttemptException
  }

  private val formats = Seq(
    new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z"), // RSS 2.0
    new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssz"), // ISO 8601
    new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"), //
    new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")
  )

  private def tryParseDate(dateStr: String): Date = {
    formats.map(f => Try(f.parse(dateStr))).filter(_.isSuccess).head.getOrElse(null)
  }

  private def cleanKey(s: String): String = {
    val parts: Array[String] = s.split("\\.").last.split("_")
    val head = parts.head
    val res = parts.map(s => s.capitalize)
    res.update(0, head)
    res.mkString("")
  }

  private class UnknownConversionAttemptException extends Exception
}
