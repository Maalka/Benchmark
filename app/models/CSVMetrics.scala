package models

import java.io.InputStream

import play.api.libs.json.{JsValue, Json}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Play

import scala.reflect.io.File

/**
  * Created by rimukas on 12/19/16.
  */




case class CSVcompute(parameters: Any) {




  def getOutput: Future[String] = {
    Future{"yeh"}
  }




}

