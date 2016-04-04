import play.api.ApplicationLoader.Context
import play.api.cache.EhCacheComponents
import play.api.mvc.{RequestHeader, EssentialAction, EssentialFilter}
import play.api.routing.Router
import play.api.{Logger, Application, ApplicationLoader, BuiltInComponentsFromContext}
import play.filters.gzip.GzipFilter
import router.Routes

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext

class AppLoader extends ApplicationLoader {
  override def load(context: Context): Application = {
    Logger.configure(context.environment)
    new AppComponents(context).application
  }
}

class HTTPRequestLoggingFilter extends EssentialFilter {
  def apply(nextFilter: EssentialAction) = new EssentialAction {
    def apply(requestHeader: RequestHeader) = {

      val startTime = System.currentTimeMillis

      nextFilter(requestHeader).map { result =>

        val endTime = System.currentTimeMillis
        val requestTime = endTime - startTime

        Logger.info(s"${requestHeader.method} ${requestHeader.uri}" +
          s" took ${requestTime}ms and returned ${result.header.status}")

        result.withHeaders(
          "Request-Time" -> requestTime.toString
        )
      }
    }
  }
}

class AppComponents(context: Context) extends BuiltInComponentsFromContext(context) with EhCacheComponents {

  lazy val applicationController = new controllers.Application(defaultCacheApi)
  lazy val baselineController = new controllers.BaselineController(defaultCacheApi)
//  lazy val usersController = new controllers.Users(defaultCacheApi)
  lazy val assets = new controllers.Assets(httpErrorHandler)

  // Routes is a generated class
  //override def router: Router = new Routes(httpErrorHandler, applicationController, baselineController, usersController, assets)
  override def router: Router = new Routes(httpErrorHandler, applicationController, baselineController, assets)
  val gzipFilter = new GzipFilter(shouldGzip =
    (request, response) => {
      val contentType = response.headers.get("Content-Type")
      contentType.exists(_.startsWith("text/html")) || request.path.endsWith("jsroutes.js")
    })

  override lazy val httpFilters: Seq[EssentialFilter] = Seq(gzipFilter, new HTTPRequestLoggingFilter())
}
