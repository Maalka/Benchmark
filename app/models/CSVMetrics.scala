package models

import org.joda.time.DateTime

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


/**
  * Created by rimukas on 12/19/16.
  */


case class CSVcompute(parameters: Any) {

  val allLines = List[CSVLine]

  val value = parameters match {
    case List(startDate: DateTime, endDate: DateTime, value: Double, unit: String) => CSVLine(startDate, endDate, value, unit)
  }

  def getOutput: Future[CSVLine] = {
    Future{
      value
    }
  }

}

case class CSVLine(startDate: DateTime, endDate: DateTime, value: Double, unit: String)
