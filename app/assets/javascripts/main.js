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
      'angular-file-upload': {
        deps: ['angular']
      },
      'angular-route': { 
        deps: ['angular']
      },
      'angular-cookies': { 
        deps: ['angular']
      },
      'filesaver': {
        exports: 'saveAs'
      },
      'maalka-templates': {
        deps: ['angular']
      },
      'highcharts-core': {
        deps: ['jquery']
      },
      'highcharts-more': {
        deps: ['highcharts-core']
      },
      'highcharts': {
        deps: ['highcharts-more']
      },
      'semantic': {
        deps: ['jquery', 'angular']
      },
      'semantic-daterangepicker': { 
        deps: ['semantic', 'moment'],
        exports: "semantic-daterangepicker"
      }
    },
    paths: {
      'requirejs': '../lib/requirejs/require',
      'jquery': '../lib/jquery/jquery',
      'angular': '../lib/angularjs/angular',
      'angular-route': '../lib/angularjs/angular-route',
      'angular-cookies': '../lib/angularjs/angular-cookies',
      'angular-file-upload': '../lib/ng-file-upload/dist/ng-file-upload',
      'filesaver': '../lib/filesaver/FileSaver',
      'highcharts-core': '../lib/highstock/highstock',
      'highcharts-more': '../lib/highstock/highcharts-more',
      'highcharts': './highcharts-theme',
      'maalkaflags': './highcharts/maalkaFlags',
      'semantic': './semantic/semantic.min',
      'jsRoutes': '/jsroutes',
      'semantic-daterangepicker': './semantic-ui-daterangepicker/daterangepicker',
      'moment': './semantic-ui-daterangepicker/moment',
      'maalka-templates': '../templates'
    }
  });

  requirejs.onError = function (err) {
    console.log(err);
  };

  // Load the app. This is kept minimal so it doesn't need much updating.
  require(['angular', 'angular-cookies', 'angular-route', 'jquery', 'semantic', './app'],
    function (angular) {
      angular.bootstrap(document, ['app']);
    }
  );
})(requirejs);
