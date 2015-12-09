// define some builtin semantic loaders

define(['angular', 'semantic-daterangepicker'], function(angular) {
  'use strict';

  var mod = angular.module('common.semantic', []);
  /*
    mod.directive('checkbox', function ($timeout) {
      return {
        restrict: "C",
        link: function (scope, elm) {
            console.log(scope);
            $timeout(function () {
                angular.element(elm).checkbox();
            }, 0);
        }
      };
    });  
*/
    mod.directive('modal', ["$timeout", function ($timeout) {
      return {
          restrict: "E",
          scope: true,

          link: function (scope, element, attrs) {
            $timeout(function () {
              scope[attrs.model] = angular.copy(scope[attrs.model]);
              scope.approve = scope[attrs.approve];
              var modalElement = element.select();              
         
              modalElement.modal({onApprove: function(){ 
                if (scope.approve !== undefined) { 
                  scope[attrs.approve](scope[attrs.model]); 
                }
              }}).modal('attach events', attrs.attachmodel, 'show');
            }, 0);   
          }
      };
    }]);

    mod.directive('sidebar', function () {
      return {
          restrict: "E",
          replace: true,
          transclude: true,
          template: '<div class="ui sidebar" ng-transclude></div>',
          scope: {
              buttonClass : '='
          },
          link: function(scope, element){
              element.sidebar('attach events', scope.buttonClass);
          }
      };
    });

    mod.directive('dimmer', [function () {
      return {
        restrict: 'E',
        replace: true,
        transclude: true,
        scope : {
            show : "=?",
            model: '=ngModel'
        },
        template: "<div class=\"{{dimmer_class}}\" ng-click=\"click_on_dimmer()\">" +
                    "<div class=\"content\">" +
                      "<div class=\"center\" ng-transclude></div>" +
                    "</div>" +
                  "</div>",
        link : function(scope) {

            if (scope.show === true) {
                scope.dimmer_class = 'ui page active dimmer';
            }
            else {
                scope.show = false;
                scope.dimmer_class = 'ui page disable dimmer';
            }

            //
            // Click on dimmer handler
            //
            scope.click_on_dimmer = function(){
                scope.model = false;
                scope.dimmer_class = 'ui page dimmer';
            };

            //
            // Watch for the ng-model changing
            //
            scope.$watch('model', function(val){
                if (val === false || val === undefined) {
                    return;
                } else {
                    scope.dimmer_class = 'ui page active dimmer';
                }
            });
        }
      };
    }]);

    mod.directive('progress', ["$timeout", function($timeout) {
      return {
        restrict: 'A',
        scope : {
            model: '='
        },
        link : function(scope, element) {
          $timeout(function () {
                angular.element(element).progress({percent: 0});
            }, 0);
          scope.$watch('model', function(progress) {
            angular.element(element).progress('increment', progress);
          }, true);
        }
      };
    }]);

    mod.directive('dropdown', ["$timeout", function ($timeout) {
      return {
//        scope: {
//          selectedModel: "=",
//          model: "="

//        },
        restrict: "C",
        link: function (scope, elm, attr) {
          var setup = false;
          /// i really don't understand why this is a parent scope...  
          /*
          scope.$watch("selectedModel", function(selectedModel) {
            if (selectedModel === undefined) {
              return;
            }
            if (selectedModel !== undefined && selectedModel !== currentModel && setup) {
                var remove = currentModel.filter(function (v, i) { 
                  return selectedModel.indexOf(v) === -1;
                });
                remove.forEach(function(v) {
                  angular.element(elm).dropdown().dropdown('remove selected', v);
                  var i = currentModel.indexOf(v);                  
                  if (i > -1) currentModel.splice(i, 1);
                });

                selectedModel.forEach(function (v) { 
                  if (currentModel.indexOf(v) === -1) {
                    angular.element(elm).dropdown('set selected', v);

                    currentModel.push(v);
                  }
                });

            }
          });
          */
          $timeout(function () {
              angular.element(elm).dropdown().dropdown({
                 'preserveHTML': true,
                  onChange: function (value) {
                      scope[attr.ngModel] = value;
                      scope.$apply(); 

                      scope.$parent[attr.ngModel] = value;
                      scope.$parent.$apply();             
                  }

                    /*


                    console.log(value);
                    var vals = value.split(",");

                    var remove = currentModel.filter(function (v) { 
                      return vals.indexOf(v) === -1;
                    }).forEach(function (v) { 
                      var i = currentModel.indexOf(v);
                      if (i > -1) currentModel.splice(i, 1);
                    });

                    vals.forEach(function (v) { 
                      if (currentModel.indexOf(v) === -1) {
                        currentModel.push(v);
                      }
                    });

                    console.log(currentModel);
                    scope.selectedModel = currentModel.slice();
                    */
              });
              setup = true;
            }, 0);
        }
      };
    }]);

    mod.directive('sticky', ["$timeout", "$interval", function ($timeout, $interval) {
          return {
            restrict: "C",
            scope: {
              element: "="
            },
            link: function (scope, elm) {
                $timeout(function () {
                    angular.element(elm).sticky({
                      context: scope.element,
                      offset: 100,
                      bottomOffset: 50,
                    }).accordion({ "content": scope.element});
                }, 0);
                var height = 0;
                var intervalP = $interval(function () { 
                    var h = angular.element(scope.element).height();
                    if (h !== height) {
                      angular.element(elm).parent().css({"max-height": h});
                      angular.element(elm).css({"max-height": h, "overflow": "scroll"}).sticky('refresh');
                    } 
                }, 1000);
                elm.on('$destroy', function () {
                  $interval.cancel(intervalP);
                });
            }
          };
    }]);

    mod.directive('accordion', ["$timeout", function ($timeout) {
          return {
            restrict: "C",
            link: function (scope, elm, attr) {
                $timeout(function () {
                    angular.element(elm).accordion().accordion('setting', {
                        onChange: function (value) {
                            scope.$parent[attr.ngModel] = value;
                            scope.$parent.$apply();
                        }
                    });
                }, 0);
            }
          };
    }]);
    mod.directive('popup', ["$log", "$timeout", function ($log, $timeout) { 
      return {
        restrict: "E",
        scope: {
          "inline" : "=",
          "popup" : "=",
          "on": "=",
          "position": "="
        },
        link: function (scope, elm) {
          $timeout(function () {
            console.log(elm);

            angular.element(elm).popup({
              inline: scope.inline === undefined ? !(scope.popup) : scope.inline,
              popup: scope.popup === undefined ? undefined : angular.element(scope.popup),
              hoverable: true,
              debug: true,
              position : "bottom right",
              delay: {
                show: 300,
                hide: 800
              }
            });
          }, 0);
        }
      };
    }]);
    mod.directive('daterangepicker', [function() {
      return {
        restrict: "A",
        scope: {
          xeditable: "=",
          model: "=",
          startDate: "=",
          format: "="
        },
        link: function(scope, elm) { 
          var format = scope.format || "MM/DD/YYYY";
          angular.element(elm).daterangepicker({
            "autoApply": true,
            "startDate": scope.model,
            "singleDatePicker": true
          }).on('apply.daterangepicker', function(ev, picker) { 
            scope.model = picker.startDate.format(format);
            angular.element(elm).submit();
          }).on('show.daterangepicker', function () {
            angular.element(".daterangepicker").on('click', function (e) { 
              // stop the calendar click events from triggering the xeditable directive;
              e.stopPropagation();
            });
          });
        }
      };
    }]);
    mod.directive('menu', ["$log", function ($log) {
      return {
        restrict: "C",
        link: function (scope, elm) { 
            var handler = {
              activate: function(target) {
                $log.info("common.semantic: SemanticMenu - Menu Activated");
                if(!angular.element(target).hasClass('dropdown')) {
                    angular.element(target)
                      .addClass('active')
                      .closest('.ui.menu')
                      .find('.item')
                      .not(angular.element(target))
                      .removeClass('active');
                }
              }
            };
            angular.element(elm).children(".item").on('click', function () { 
              handler.activate(this);
            });
        }
      };
    }]);
  return mod;
});