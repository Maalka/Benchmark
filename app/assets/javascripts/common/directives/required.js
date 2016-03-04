/**
 * A benchmark cell directive
 * prints out the value or an error based on the response from the benchmark web server
 * that references them all.
 */
define(['angular','./main'], function(angular) {
    'use strict';

    var mod = angular.module('common.directives');
    mod.directive('required', [function() {
//        var validNodes = ['input', 'select'];
        return {
            restrict: 'C',
            controller: ["$scope", "$element", "$parse", function ($scope, $element, $parse) {
                // the name of the field in the forms hash;
                $scope.$watch (
                    function () { 
                        return $element.find("[required]").attr("name");
                    },
                    function (newValue, oldValue) {
                        if(newValue !== oldValue && newValue !== undefined) {
                            refresh($parse(newValue)($scope) || newValue);
                        }
                    }
                );
                var refresh = function(formInputName) {
                    if (formInputName === undefined) {
                        return;
                    }
                    // it is expected that the scope contains form.baselineForm.  The baseline
                    // controller will set this up

                    var watch = "forms.baselineForm." + formInputName +".$valid";
                    var isValidated = function () {
                        return $scope.forms.hasValidated;
                    };
                    var check = function() {
                        if (isValidated() === true) {
                            if (!$scope.forms.baselineForm[formInputName].$valid) {
                                $element.addClass("error");
                            }
                        }
                    };

                    check();
                    $scope.$watch("forms.hasValidated", function () { 
                        check();
                    });

                    $scope.$watch(watch, function (value) { 
                        if (isValidated() === true) {
                            if (value === true) {
                                $element.removeClass("error");
                            } if (value === false && $scope.hasValidated) { // hasValidated is set to true when the submit button is clicked;
                                $element.addClass("error");
                            }
                        }
                    });
                };
                refresh($element.find("[required]").attr("name"));
            }]
        };
    }]);
});
