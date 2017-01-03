/**
 * A directive
 * for uploading
 * CSV files
 */
define(['angular', './main'], function(angular) {
    'use strict';

    var mod = angular.module('common.directives');

    angular.module('xeditable').directive('editableSemanticDate', ['editableDirectiveFactory',
         function(editableDirectiveFactory) {
           return editableDirectiveFactory({
             directiveName: "editableSemanticDate",
             inputTpl: '<input type="text" daterangepicker>'
           });
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


  return mod;
});
