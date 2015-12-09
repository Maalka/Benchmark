/**
 * Common functionality.
 */
define(['angular', './services/helper', './services/playRoutes', './filters', './semantic'],
    function(angular) {
  'use strict';

  return angular.module('benchmark.common', ['common.helper', 'common.playRoutes', 'common.filters',
    'common.semantic']);
});
