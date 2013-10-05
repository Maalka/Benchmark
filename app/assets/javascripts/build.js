/*
 * A custom build profile that is passed to the optimizer via requireJsShim in build.sbt.
 * Play does this via settings it as the mainConfigFile:
 * http://requirejs.org/docs/optimization.html#mainConfigFile
 */
requirejs.config({
  packages: ["common", "main", "user", "dashboard"],
  shim: {
    "_" : {},
    "jquery": {
      exports: "$"
    },
    "angular" : {
      exports : "angular",
      deps: ["jquery"]
    },
    "angular-cookies": ["angular"],
    "bootstrap" : ["jquery"],
    "app" : ["angular"],
    "jsRoutes" : {
      deps : [],
      // it's not a RequireJS module, so we have to tell it what var is returned
      exports : "jsRoutes"
    }
  },
  paths: {
    "_" : "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.5.1/underscore-min",
    // empty: so the optimizer ignores jquery; necessary because it doesn't support fallbacks
    "jquery": "empty:",
    "bootstrap": "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min",
    "angular": "//ajax.googleapis.com/ajax/libs/angularjs/1.0.7/angular.min",
    "angular-cookies": "//ajax.googleapis.com/ajax/libs/angularjs/1.0.7/angular-cookies.min",
    // empty: so the optimizer doesn't try to find jsRoutes.js in our project
    "jsRoutes" : "empty:"
  }
});
