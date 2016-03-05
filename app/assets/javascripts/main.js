// `main.js` is the file that sbt-web will use as an entry point
(function (requirejs) {
  'use strict';

  // -- RequireJS config --
  requirejs.config({
    // Packages = top-level folders; loads a contained file named 'main.js"
    wrapShim: true,
    packages: ['common', 'benchmark'],
    shim: {
      'jsRoutes': {
        deps: [],
        // it's not a RequireJS module, so we have to tell it what var is returned
        exports: 'jsRoutes'
      },
      // Hopefully this all will not be necessary but can be fetched from WebJars in the future
      'angular': {
        deps: ['jquery'],
        exports: 'angular'
      },
      'angular-route': ['angular'],
      'angular-cookies': ['angular'],
      'matchmedia-ng': ['angular'],
      'highcharts-core': {
        deps: ['jquery'],
        exports: 'highcharts-core'
      },
      'highcharts': {
        deps: ['highcharts-core'],
        exports: 'highcharts'
      },
      'semantic': ['angular'],
      'semantic-daterangepicker': ['moment']
      
    },
    paths: {
      'requirejs': '../lib/requirejs/require',
      'jquery': '../lib/jquery/jquery',
      'angular': '../lib/angularjs/angular',
      'angular-route': '../lib/angularjs/angular-route',
      'angular-cookies': '../lib/angularjs/angular-cookies',
      'highcharts-core': '../lib/highstock/highstock',
      'highcharts-more': '../lib/highcharts/highcharts-more',
      'highcharts': './highcharts-theme',
      'matchmedia-ng': '../lib/matchmedia-ng/matchmedia-ng',
      'semantic': './semantic/semantic',
      'jsRoutes': '/jsroutes',
      'semantic-daterangepicker': './semantic-ui-daterangepicker/daterangepicker',
      'moment': './semantic-ui-daterangepicker/moment'
    }
  });

  requirejs.onError = function (err) {
    console.log(err);
  };

  // Load the app. This is kept minimal so it doesn't need much updating.
  require(['angular', 'angular-cookies', 'angular-route', 'jquery', 'semantic', './app'],
    function (angular) {
      angular.bootstrap(document, ['app', 'matchmedia-ng']);
    }
  );
})(requirejs);
