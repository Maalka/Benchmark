/**
 * Created by rimukas on 10/12/15.
 */


package controllers

import com.google.inject.Inject
import play.api.cache.CacheApi
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.Future
import scala.math._
import squants.energy.{Gigajoules, KBtus, Energy, KilowattHours}
import squants.space._

import models.EUIMetrics
import models.EUICalculator
import scala.util.control.NonFatal
import scala.util.{ Success, Failure, Try}


import scala.concurrent.ExecutionContext.Implicits.global


trait BaselineActions {
  this: Controller =>

  private def tryToOption[A](objs: List[Try[A]]): List[Option[A]] = {
    objs.map {
      case Success(e) => Option(e)
      case Failure(th) => None
    }
  }

  case class EitherFuture(name: String, future: Future[Any])

  case class EitherFutureResult(name: String, either: Either[String, JsValue])

  implicit def energyToJSValue(b: Energy): JsValue = Json.toJson(b.value)
  implicit def listTryEnergyToJSValue(v: List[Try[Energy]]): JsValue = Json.toJson(tryToOption[Energy](v).map(_.map(_.value)))

  def futureFailed(f:Future[Any]):Boolean = {f.failed.value.isDefined}

  def eitherFutures(futures: Seq[EitherFuture]): Seq[Future[EitherFutureResult]] = {




    val errors = futures.filter {
      case EitherFuture(a,b) => b.failed.value.isDefined
    }

    val results = futures.filter {
      case EitherFuture(a,b) => b.failed.value.isEmpty
    }

    Console.println("Size of Errors:  " + errors.size)
    Console.println("Size of Results:  " + results.size)


    results.foreach{f=>{
      Console.println(f.name)
      Console.println(f.future.onComplete(println)) //pretty much always empty
      Console.println("rrrrrrrrr!")
    }}

    errors.foreach{f=>{
      Console.println(f.name)
      Console.println(f.future.failed.value) //sometimes empty, if future doesn't complete
      Console.println("eeeeeeee!")
    }}

    val err = errors.map { f => Future{EitherFutureResult(f.name, Left("Could not recognize input type"))}}

    val res = results.map { f => f.future.map {
      case v: Energy => EitherFutureResult(f.name, Right(v))
      case v: Double => EitherFutureResult(f.name, Right(Json.toJson(v)))
      case v: Int => EitherFutureResult(f.name, Right(Json.toJson(v)))
      case v: Energy => EitherFutureResult(f.name, Right(v))
      case v: List[Try[Energy]] => EitherFutureResult(f.name, Right(v))
      case None => EitherFutureResult(f.name, Left("Could not recognize input type"))
    }
    }
    // FOR THE FOLLOWING SAMPLE CURL CALL:
    // curl -XPOST "http://localhost:9000/baseline" -d"@adult_education.json" -H "Content-Type: application/json" | python -m json.tool
    //if err is returned AND the futures are fully completed, then the correct errors json entry will be returned,
    //but if the futures are not fully completed, then it won't fail but it also won't return any errors
    err
    //if res is returned, then it will fail SOMETIMES when the futures aren't completed in time, need to somehow
    //figure out how to make sure the futures are fully completed before separating between successes and failures
    //res
    }



  def makeBaseline() = Action.async(parse.json) { implicit request =>
    implicit def energyToJSValue(b: Energy): JsValueWrapper = Json.toJsFieldJsValueWrapper(b.value)


    val getBaseline: EUIMetrics = EUIMetrics(request.body)

    val energyCalcs: EUICalculator = EUICalculator(request.body)

    val futures = Future.sequence(eitherFutures(Seq(

      EitherFuture("siteEnergy", energyCalcs.getSiteEnergy),
      EitherFuture("totalSiteEnergy", energyCalcs.getTotalSiteEnergy),
      EitherFuture("sourceEnergy", energyCalcs.getSourceEnergy),
      EitherFuture("totalSourceEnergy", energyCalcs.getTotalSourceEnergy),
      EitherFuture("totalSourceEnergyNoPoolNoParking", energyCalcs.getTotalSourceEnergyNoPoolNoParking),
      EitherFuture("expectedSourceEUI", getBaseline.expectedSourceEUI),
      EitherFuture("sourceEUI", getBaseline.sourceEUI),
      EitherFuture("ES", getBaseline.ES)

    )))

    futures.map { r =>
      val errors = r.collect {
        case EitherFutureResult(n, Left(s)) => Json.obj(n -> s)
      }
      val results = r.collect {
        case EitherFutureResult(n, Right(s)) => Json.obj(n -> s)
      }
      Ok(Json.obj(
        "results" -> results,
        "errors" -> errors
      ))
    }

  }
}
/*

      EitherFuture("siteEnergy", energyCalcs.getSiteEnergy),
      EitherFuture("totalSiteEnergy", energyCalcs.getTotalSiteEnergy),
      EitherFuture("sourceEnergy", energyCalcs.getSourceEnergy),
      EitherFuture("totalSourceEnergy", energyCalcs.getTotalSourceEnergy),
      EitherFuture("totalSourceEnergyNoPoolNoParking", energyCalcs.getTotalSourceEnergyNoPoolNoParking),
      EitherFuture("poolEnergy", energyCalcs.getPoolEnergy),
      EitherFuture("parkingEnergy", energyCalcs.getParkingEnergy),

      EitherFuture("medianSiteEUI", getBaseline.medianSiteEUI),
      EitherFuture("medianSiteEnergy", getBaseline.medianSiteEnergy),
      EitherFuture("medianSourceEUI", getBaseline.medianSourceEUI),
      EitherFuture("medianSourceEnergy", getBaseline.medianSourceEnergy),

      EitherFuture("targetSourceEUI", getBaseline.targetSourceEUI),
      EitherFuture("targetSourceEnergy", getBaseline.targetSourceEnergy),
      EitherFuture("targetSiteEUI", getBaseline.targetSiteEUI),
      EitherFuture("targetSiteEnergy", getBaseline.targetSiteEnergy),

      EitherFuture("percentBetterSourceEUI", getBaseline.percentBetterSourceEUI),
      EitherFuture("percentBetterSourceEnergy", getBaseline.percentBetterSourceEnergy),
      EitherFuture("percentBetterSiteEUI", getBaseline.percentBetterSiteEUI),
      EitherFuture("percentBetterSiteEnergy", getBaseline.percentBetterSiteEnergy),
      EitherFuture("percentBetterES", getBaseline.percentBetterES),

      EitherFuture("expectedSourceEUI", getBaseline.expectedSourceEUI),
      EitherFuture("sourceEUI", getBaseline.sourceEUI),
      EitherFuture("ES", getBaseline.ES)

      EitherFuture("siteEnergy", {
        for(t<-energyCalcs.getSiteEnergy)yield t
      }),

      EitherFuture("totalSiteEnergy", {
        for(t<-energyCalcs.getTotalSiteEnergy)yield t
      }),
      EitherFuture("sourceEnergy", {
        for(t<-energyCalcs.getSourceEnergy)yield t
      }),
      EitherFuture("totalSourceEnergy", {
        for(t<-energyCalcs.getTotalSourceEnergy)yield t
      }),

      EitherFuture("expectedSourceEUI", {
        for(t<-getBaseline.expectedSourceEUI)yield t
      }),
      EitherFuture("sourceEUI", {
        for(t<-getBaseline.sourceEUI)yield t
      }),
      EitherFuture("ES", {
        for(t<-getBaseline.ES)yield t
      })


    val p = results.map { f => f.future.map {
        case v: Energy => EitherFutureResult(f.name, Right(v))
        case v: Double => EitherFutureResult(f.name, Right(Json.toJson(v)))
        case v: Int => EitherFutureResult(f.name, Right(Json.toJson(v)))
        case v: Energy => EitherFutureResult(f.name, Right(v))
        case v: List[Try[Energy]] => EitherFutureResult(f.name, Right(v))
        case None => EitherFutureResult(f.name, Left("Could not recognize input type"))
      }
    }
    p
    }


val test = {


      for {
        siteEnergy <- energyCalcs.getSiteEnergy

        totalSiteEnergy <- energyCalcs.getTotalSiteEnergy

        sourceEnergy <- energyCalcs.getSourceEnergy
        totalSourceEnergy <- energyCalcs.getTotalSourceEnergy

        poolEnergy <- energyCalcs.getPoolEnergy
        parkingEnergy <- energyCalcs.getParkingEnergy
        totalSourceEnergyNoPoolNoParking <- energyCalcs.getTotalSourceEnergyNoPoolNoParking

        expectedSourceEUI <- getBaseline.expectedSourceEUI
        sourceEUI <- getBaseline.sourceEUI
        es <- getBaseline.ES
      } yield {
        val pop = Seq(
        EitherFuture("siteEnergy", energyCalcs.getSiteEnergy),
        EitherFuture("totalSiteEnergy", energyCalcs.getTotalSiteEnergy),
        EitherFuture("sourceEnergy", energyCalcs.getSourceEnergy),
        EitherFuture("totalSourceEnergy", energyCalcs.getTotalSourceEnergy),
        EitherFuture("poolEnergy", energyCalcs.getPoolEnergy),
        EitherFuture("expectedSourceEUI", getBaseline.expectedSourceEUI),
        EitherFuture("sourceEUI", getBaseline.sourceEUI),
        EitherFuture("ES", getBaseline.ES)
        )
        val errors = pop.filter {
          case EitherFuture(a, b) => b.failed.value.isDefined
        }

        val results = pop.filter {
          case EitherFuture(a, b) => b.failed.value.isEmpty
        }
        Console.println("Size of Errors:  " + errors.size)
        Console.println("Size of Results:  " + results.size)


        results.foreach { f => {
          Console.println(f.name)
          Console.println(f.future.onComplete(println))
          Console.println("ddddddd!")
        }
        }

        errors.foreach { f => {
          Console.println(f.name)
          Console.println(f.future.failed.value)
          Console.println("ddddddd!")
        }
        }
      }
    }

  def eitherFutures(futures: Seq[EitherFuture]): Seq[Future[EitherFutureResult]] = {

    futures.map { f => f.future.failed.value match {
      case Some(a) => Future {
        EitherFutureResult(f.name, Left(a.getOrElse("Error Getting" + f.name).toString))
      }
      case None => f.future.map {
        case v: Energy => EitherFutureResult(f.name, Right(v))
        case v: Double => EitherFutureResult(f.name, Right(Json.toJson(v)))
        case v: Int => EitherFutureResult(f.name, Right(Json.toJson(v)))
        case v: Energy => EitherFutureResult(f.name, Right(v))
        case v: List[Try[Energy]] => EitherFutureResult(f.name, Right(v))
        case None => EitherFutureResult(f.name, Left("Could not recognize input type"))
      }
    }
    }
  }

      EitherFuture("targetSourceEUI", getBaseline.targetSourceEUI),
      EitherFuture("targetSourceEnergy", getBaseline.targetSourceEnergy),
      EitherFuture("targetSiteEUI", getBaseline.targetSiteEUI),
      EitherFuture("targetSiteEnergy", getBaseline.targetSiteEnergy),

      EitherFuture("percentBetterSourceEUI", getBaseline.percentBetterSourceEUI),
      EitherFuture("percentBetterSourceEnergy", getBaseline.percentBetterSourceEnergy),
      EitherFuture("percentBetterSiteEUI", getBaseline.percentBetterSiteEUI),
      EitherFuture("percentBetterSiteEnergy", getBaseline.percentBetterSiteEnergy),
      EitherFuture("percentBetterES", getBaseline.percentBetterES),

      EitherFuture("expectedSourceEUI", getBaseline.expectedSourceEUI),
      EitherFuture("sourceEUI", getBaseline.sourceEUI),


      siteEnergy <- energyCalcs.getSiteEnergy.map { m => tryToOption(m) }
      totalSiteEnergy <- energyCalcs.getTotalSiteEnergy

      sourceEnergy <- energyCalcs.getSourceEnergy.map { m => tryToOption(m) }
      totalSourceEnergy <- energyCalcs.getTotalSourceEnergy

      poolEnergy <- energyCalcs.getPoolEnergy
      parkingEnergy <- energyCalcs.getParkingEnergy
      totalSourceEnergyNoPoolNoParking <- energyCalcs.getTotalSourceEnergyNoPoolNoParking

      expectedSourceEUI <- getBaseline.expectedSourceEUI
      sourceEUI <- getBaseline.sourceEUI
      es <- getBaseline.ES

      targetSourceEUI <- getBaseline.targetSourceEUI
      targetSiteEUI <- getBaseline.targetSiteEUI
      targetSourceEnergy <- getBaseline.targetSourceEnergy
      targetSiteEnergy <- getBaseline.targetSiteEnergy

      medianSourceEUI <- getBaseline.medianSourceEUI
      medianSiteEUI <- getBaseline.medianSiteEUI
      medianSiteEnergy <- getBaseline.medianSiteEnergy
      medianSourceEnergy <- getBaseline.medianSourceEnergy

      percentBetterSourceEUI <- getBaseline.percentBetterSourceEUI
      percentBetterSiteEUI <- getBaseline.percentBetterSiteEUI
      percentBetterSourceEnergy <- getBaseline.percentBetterSourceEnergy
      percentBetterSiteEnergy <- getBaseline.percentBetterSiteEnergy
      percentBetterES <- getBaseline.percentBetterES
    } yield {
        Json.obj(
          "siteEnergy" -> siteEnergy.map(_.map(_.value)),
          "totalSiteEnergy" -> totalSiteEnergy,

          "sourceEnergy" -> sourceEnergy.map(_.map(_.value)),
          "totalSourceEnergy" -> totalSourceEnergy,

          "poolEnergy" -> poolEnergy,
          "parkingEnergy" -> parkingEnergy,
          "totalSourceEnergyNoPoolNoParking" -> totalSourceEnergyNoPoolNoParking,

          "expectedSourceEUI" -> expectedSourceEUI,
          "sourceEUI" -> sourceEUI,
          "es" -> es,

          "targetSourceEUI" -> targetSourceEUI,
          "targetSiteEUI" -> targetSiteEUI,
          "targetSourceEnergy" -> targetSourceEnergy,
          "targetSiteEnergy" -> targetSiteEnergy,

          "medianSourceEUI" -> medianSourceEUI,
          "medianSiteEUI" -> medianSiteEUI,
          "medianSiteEnergy" -> medianSiteEnergy,
          "medianSourceEnergy" -> medianSourceEnergy,

          "percentBetterSourceEUI" -> percentBetterSourceEUI,
          "percentBetterSiteEUI" -> percentBetterSiteEUI,
          "percentBetterSourceEnergy" -> percentBetterSourceEnergy,
          "percentBetterSiteEnergy" -> percentBetterSiteEnergy,
          "percentBetterES" -> percentBetterES
        )
      }
    futures.map { res =>
      Ok(res)
    }
  }
}
*/
class BaselineController @Inject() (val cache: CacheApi) extends Controller with Security with Logging with BaselineActions