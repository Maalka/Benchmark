/**
 * A directive
 * for uploading
 * CSV files
 */
define(['angular', 'filesaver', './main', 'angular-file-upload'], function(angular) {
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
                $scope.loading = false;

                var watchForCompletedFile = function (targetFileName) { 
                    playRoutes.controllers.CSVController.getProcessedCSV(targetFileName, false).get().then(function (response){ 
                    
                        $scope.downloadLink = response.data.targetFileNamePath;
                        console.log("Exists and ready to download");
                        $scope.loading = false;
                        
                    }).catch(function (response)  {
                        if (response.status === 404) {
                            console.log("Doesn't exist");
                            $scope.loading = false; 
                        } else if (response.status === 409) {
                            $timeout(function () {
                                watchForCompletedFile(targetFileName);
                            }, 1000);
                        } else {
                            console.log(response);
                        }
                    });
                };

                $scope.upload = function (file) {
                    // https://github.com/eligrey/FileSaver.js/issues/156
                    $scope.loading = true;
                    Upload.upload({
                        url: playRoutes.controllers.CSVController.upload().url,
                        cache: false,
                        data: {
                            attachment: file
                        }
                    }).then(function (resp) {
                        watchForCompletedFile(resp.data.targetFileName);
                    }).catch(function (resp) {
                        $scope.loading = false;
                        if (resp.status === 400) {
                            var message = JSON.parse(String.fromCharCode.apply(null, new Uint8Array(resp.data.arrayBuffer)));
                            $scope.error = {
                                'messageType': "Error",
                                'messageDescription': message.response
                            };
                        } else {
                            $scope.error = {
                                'messageType': "Error",
                                'messageDescription': "There is an error with the bulk csv that you are uploading.  Please make sure that the file contains all of the fields that are required"
                            };

                        }
                    });
                 };
            }]
        };
    }]);
});