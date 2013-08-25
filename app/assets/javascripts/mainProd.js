(function(requirejs) {
  "use strict";

  // -- PROD RequireJS config --
  requirejs.config({
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
      "app" : ["angular"],
      "jsRoutes" : {
        deps : [],
        // it's not a RequireJS module, so we have to tell it what var is returned
        exports : "jsRoutes"
      }
    },
    paths: {
      // Map the dependencies to CDNs or WebJars directly
      "_" : "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.5.1/underscore-min",
      "jquery": "//code.jquery.com/jquery-1.10.2.min",
      "bootstrap": "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min",
      "angular": "//ajax.googleapis.com/ajax/libs/angularjs/1.0.7/angular.min",
      "angular-cookies": "//ajax.googleapis.com/ajax/libs/angularjs/1.0.7/angular-cookies.min",
      // TODO Replace with your server URL; this isn't perfect yet
      "jsRoutes": "//localhost:9005/jsroutes"
      // A WebJars URL would look like //server:port/webjars/angularjs/1.0.7/angular.min
    },
    priority: ["angular"]
  });

  requirejs.onError = function(err) {
    console.log(err);
  };

  // Make sure generic external scripts are loaded
  require(["angular", "app", "angular-cookies", "jquery", "bootstrap"], function(angular, app) {
    angular.bootstrap(document, ["app"]);
  });
})(requirejs);
