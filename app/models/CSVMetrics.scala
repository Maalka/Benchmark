package models

import org.joda.time.DateTime

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by rimukas on 12/19/16.
  */

case class CSVcompute(parameters: List[List[String]]) {

  def getOutput: Future[List[CSVLine]] = {

    Future {
      parameters.map(line => {
          CSVLine(
            line.head.asInstanceOf[DateTime],
            line(1).asInstanceOf[DateTime],
            line(2).asInstanceOf[Double],
            line(3))
      })
    }
  }

}

case class CSVLine(startDate: DateTime, endDate: DateTime, value: Double, unit: String)
