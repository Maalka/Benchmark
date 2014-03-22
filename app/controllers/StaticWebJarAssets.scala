package controllers

import play.api.Play
import play.api.Play.current

/**
 * See http://www.jamesward.com/2014/03/20/webjars-now-on-the-jsdelivr-cdn
 */
object CdnWebJarAssets extends WebJarAssets(Assets) {

  def getUrl(file: String) = {
    val maybeContentUrl = Play.configuration.getString("contentUrl")

    maybeContentUrl.map { contentUrl =>
        contentUrl + controllers.routes.CdnWebJarAssets.at(file).url
    } getOrElse controllers.routes.CdnWebJarAssets.at(file).url
  }

}

import play.api.mvc.Call

/**
 * This is copied from `org.webjars.play.RequireJS` to override `setupJavaScript`
 * in order to retrieve WebJars from the jsDelivr CDN.
 */
object RequireJS {

  def setup(main: String): String = {

    // We have to use reflection because the reverse routing is not available in the library project
    def nastyReflectedRoute(path: String, routerName: String): Call = {
      // We have to use the current Play app's classloader otherwise we won't be able to find the reverse routers
      val c = Class.forName("controllers.Reverse" + routerName, true, play.api.Play.current.classloader)
      val m = c.getMethod("at", classOf[String])
      m.invoke(c.newInstance(), path).asInstanceOf[Call]
    }

    // !!! Changed: Use CdnWebJarAssets for everything
    def nastyReflectedWebJarAssetsRoute(path: String): Call = nastyReflectedRoute(path, "CdnWebJarAssets")
    def nastyReflectedAssetsRoute(path: String): Call = nastyReflectedRoute(path, "Assets")

    // !!! Changed: Retrieve RequireJS from CDN in PROD mode
    val setupJavaScript: String = {
      val baseUrl = nastyReflectedWebJarAssetsRoute("").url
      val path = Play.configuration.getString("contentUrl") map { contentUrl =>
        contentUrl + baseUrl
      } getOrElse baseUrl
      org.webjars.RequireJS.getSetupJavaScript(path)
    }

    val mainRoute = nastyReflectedAssetsRoute(main)

    val requireRoute = nastyReflectedWebJarAssetsRoute(controllers.WebJarAssets.locate("require.min.js"))

    s"""<script>
      |    // this stuff must be done before require.js is loaded
      |    $setupJavaScript
      |</script>
      |<script data-main="$mainRoute" src="$requireRoute"></script>""".stripMargin
  }
}
