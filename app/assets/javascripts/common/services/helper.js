/** Helpers */
define(["angular"], function(angular) {
  var mod = angular.module("common.helper", []);
  mod.service("helper", function() {
    return {
      sayHi: function() {
        return "hi";
      }
    };
  });
  return mod;
});
