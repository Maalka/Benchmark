import play.api.ApplicationLoader.Context
import play.api.cache.EhCacheComponents
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import play.api.{Logger, Application, ApplicationLoader, BuiltInComponentsFromContext}
import play.filters.gzip.GzipFilter
import router.Routes

class AppLoader extends ApplicationLoader {
  override def load(context: Context): Application = {
    Logger.configure(context.environment)
    new AppComponents(context).application
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

  override lazy val httpFilters: Seq[EssentialFilter] = Seq(gzipFilter)
}
