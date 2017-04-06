/**
 * A directive
 * for uploading
 * CSV files
 */
define(['angular', 'filesaver', './main', 'angular-file-upload'], function(angular, saveAs) {
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
                // function str2bytes (str) {
                //     var bytes = new Uint8Array(str.length);
                //     for (var i=0; i<str.length; i++) {
                //         bytes[i] = str.charCodeAt(i);
                //     }
                //     return bytes;
                // }

                $scope.submitFile = function() {
                    $scope.error = undefined;
                    if ($scope.attachment) {
                        $scope.upload($scope.attachment);
                    }
                };
                $scope.loadingFileFiller = {};

                $scope.upload = function (file) {
                    // https://github.com/eligrey/FileSaver.js/issues/156
                    Upload.upload({
                        responseType: "arraybuffer",
                        url: playRoutes.controllers.CSVController.upload().url,
                        cache: false,
                        headers: {
                            'Content-Type': 'application/zip; charset=utf-8'
                        },
                        transformResponse: function (data) {
                            //The data argument over here is arraybuffer but $http returns response
                            // as object, thus returning the response as an object with a property holding the
                            // binary file arraybuffer data
                            var response = {};
                            response.arrayBuffer = data;
                            return response;
                        },
                        data: {
                            attachment: file
                        }
                    }).then(function (resp) {
                        $scope.attachment  = undefined;
                        var blob = new Blob([resp.data.arrayBuffer], {type: "application/zip;charset=utf-8"});
                        saveAs(blob, "report.zip");
                        if (resp.data.status === "OK") {
                            $timeout(function () { 
                                $scope.loadingFileFiller = {};
                            }, 1000);
                        } else {
                            errorPopover.errorPopoverEvent("Error uploading attachment",
                                {
                                    messageType: "error",
                                    messageHeader: resp.data.response
                                });
                        }
                    }).catch(function () {
                        $scope.error = {
                            'messageType': "Error",
                            'messageDesscription': "There is a error with the bulk csv that you are uploading.  Please make sure that the file contains all of the fields that are required"
                        };
                    });
                 };
            }]
        };
    }]);
});