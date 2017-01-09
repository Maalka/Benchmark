package models

import java.util.Locale

import org.joda.time.format._
import org.joda.time._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Try}


case class CSVcompute(parameters: List[List[String]]) {

  val dateFormatGeneration: DateTimeFormatter = DateTimeFormat.forPattern("MM-dd-yy").withPivotYear(1950).withLocale(Locale.US)

  val dateFormats = List(
    "MM/dd/yy",
    "dd/MM/yy",
    "MM-dd-yy",
    "dd-MMM-yy",
    "dd MMM yy",
    "MMM dd yy",
    "dd/MM/yyyy",
    "MM/dd/yyyy",
    "dd MM yyyy",
    "dd MMM yyyy",
    "dd-MMM-yyyy",
    "dd-MM-yyyy").map(p => (p, DateTimeFormat.forPattern(p)))


  val goodEntries = parameters.map {
    _ match {
      case List(a,b,c,d,e) => List(normalizeDate(a), normalizeDate(b), Try{c.toDouble}, Try{d.toDouble}, e)
    }
  }.collect {
      case List(Some(a:DateTime),Some(b:DateTime),Success(c:Double),Success(d:Double),e:String) => CSVLine(a,b,c,d,e)
  }


  def normalizeDate(dateStr: String): Option[DateTime] = {
    val trimmedDate = dateStr.trim
    if(trimmedDate.isEmpty) None
    else {
      dateFormats.toStream.map { case (pattern, fmt) =>
        //println(s"Pattern: $pattern")
        Try(DateTime.parse(trimmedDate,fmt))
      }.find(_.isSuccess).map{ t =>
       t.get}
    }
  }
}


case class CSVLine(startDate: DateTime, endDate: DateTime, Temperature:Double, value: Double, unit: String)



