/**
 * Common functionality.
 */
define(['angular', 'common'], function (angular) {
  'use strict';

  var mod = angular.module('common.PopoverService', []);
  mod.factory('errorPopoverService', [function () {
    var errorPopoverHandlers = [];

    return {
      registerErrorPopoverHandler: function(handler) {
        errorPopoverHandlers.push(handler);
      },
      errorPopoverEvent: function(string, options) {
        errorPopoverHandlers.forEach(function (handler) {
          handler(string, options);
        });
      }
    };
  }]);
  return mod;
});

