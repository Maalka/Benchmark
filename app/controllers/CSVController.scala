package controllers

/**
  * Created by rimukas on 12/19/16.
  */

import com.github.tototoshi.csv.CSVReader
import com.google.inject.Inject
import models._
import play.api.cache.CacheApi
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions


trait CSVActions {
  this: Controller =>

  def upload = Action.async(parse.multipartFormData) {implicit request =>

    request.body.file("attachment").map { upload =>
      Future {
        import java.io.File
        val filename = upload.filename
        val uploadedFile = upload.ref.moveTo(new File(s"/tmp/upload/$filename"))
        val reader = CSVReader.open(uploadedFile)
        val csvOutput: CSVcompute = CSVcompute(reader.all())
        csvOutput.getOutput.map {out => Console.println("line: out" + out)}
        Ok("File uploaded") }
    }.getOrElse {
      Future{
        Ok("File is missing")
      }
    }



  }
}
class CSVController @Inject() (val cache: CacheApi) extends Controller with Security with Logging with CSVActions