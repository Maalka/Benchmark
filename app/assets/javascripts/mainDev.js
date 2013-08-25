(function(requirejs) {
  "use strict";

  // -- DEV RequireJS config --
  requirejs.config({
    // Packages = top-level folders; loads a contained file named "main.js"
    packages: ["common", "main", "user", "dashboard"],
    shim: {
      "jquery": {
        exports: "$"
      },
      "angular" : {
        exports : "angular",
        deps: ["jquery"]
      },
      "angular-cookies": ["angular"],
      "app" : ["angular"]
    },
    paths: {
      // You can also define a module here, e.g. a local module that doesn't support RequireJS
      // or map a longer path to a shorter name
    },
    priority: ["angular"]
  });

  requirejs.onError = function(err) {
    console.log(err);
  };

  /*
   * Custom naming for WebJars.
   * Define all WebJars deps with a generic name, so they can be referenced transparently
   * This is important so the mainProd file stays as simple as possible.
   */

  define("_", ["webjars!underscore.js"], function() {return _;});
  define("jquery", ["webjars!jquery.js"], function() {return $;});
  define("bootstrap", ["webjars!bootstrap.js"], function() {/*return $;*/});
  define("angular", ["webjars!angular.js"], function() {
    return angular; // return the global var
  });
  define("angular-cookies", ["webjars!angular-cookies.js"], function() {});

  // External Dependency that is not a RequireJS module
  define("jsRoutes", ["/jsroutes.js"], function() {
    return jsRoutes;
  });

  // Load the app. This is kept minimal so it doesn't need much updating.
  require(["angular", "angular-cookies", "jquery", "bootstrap", "app"],
    function(angular, cookies, app) {
      angular.bootstrap(document, ["app"]);
    }
  );
})(requirejs);
