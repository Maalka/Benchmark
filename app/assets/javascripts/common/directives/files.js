/**
 * A directive
 * for uploading
 * CSV files
 */
define(['angular', './main', 'angular-file-upload'], function(angular) {
    'use strict';

    var mod = angular.module('common.directives');

    mod.directive('files', ['$log', 'errorPopoverService', 'playRoutes', 'Upload', function ($log, errorPopover, playRoutes, Upload) {
        return {
            restrict: 'E',
            scope: {},
            templateUrl: "javascripts/common/partials/files.html",
            controller: ["$scope", "$element", "$timeout", "playRoutes",
                function ($scope, $element, $timeout, playRoutes) {
                $scope.searchInput = "";


                $scope.submitFile = function() {
                    if ($scope.attachment) {
                        $scope.upload($scope.attachment);
                    }
                };
                $scope.loadingFileFiller = {};

                $scope.upload = function (file) {
                    Upload.upload({
                        url: playRoutes.controllers.CSVController.upload().url,
                        data: {attachment: file
                              }
                    }).then(function (resp) {
                        console.log('Success ' + resp.config.data.attachment.name + ' uploaded. Response: ' + resp.data);
                        $scope.attachment  = undefined;

                        if (resp.data.status === "OK") {
                            $timeout(function () { 
                                $scope.property.documents.push(resp.data.result);
                                $scope.files.unshift(resp.data.result);
                                $scope.loadingFileFiller = {};
                            }, 1000);
                        } else {
                            errorPopover.errorPopoverEvent("Error uploading attachment",
                                {
                                    messageType: "error",
                                    messageHeader: resp.data.response
                                });
                        }
                    }, function (resp) {
                        console.log('Error status: ' + resp.status);
                    }, function (evt) {
                        var progressPercentage = parseInt(100.0 * evt.loaded / evt.total);
                        $scope.loadingFileFiller = {
                            progressPercentage: progressPercentage,
                            attachmentName: evt.config.data.attachment.name
                        };
                        console.log('progress: ' + progressPercentage + '% ' + evt.config.data.attachment.name);
                    });
                 };
            }]
        };
    }]);
});