define(['angular', './main', 'angular-file-upload'], function(angular) {
    'use strict';
    var mod = angular.module('common.directives.controls');
    mod.directive('files', ['$log', 'errorPopoverService', 'playRoutes', 'Upload', function ($log, errorPopover, playRoutes, Upload) {
        return {
            restrict: 'E',
            scope: {
                'portfolioId': "=",
                'propertyId': "="
            },
            templateUrl: "javascripts/common/partials/files.html",
            controller: ["$scope", "$element", "$timeout", "playRoutes", "portfolioService", "userService", 
                function ($scope, $element, $timeout, playRoutes, portfolioService, userService) {
                $scope.searchInput = "";
                var portfolio;
                var user;

                $scope.submit = function() {
                    if ($scope.attachment) {
                        $scope.upload($scope.attachment);
                    }
                };
                $scope.loadingFileFiller = {};

                $scope.upload = function (file) {
                    Upload.upload({
                        url: playRoutes.controllers.PropertyController.putFile($scope.propertyId).url,
                        data: {attachment: file}
                    }).then(function (resp) {
                        console.log('Success ' + resp.config.data.attachment.name + 'uploaded. Response: ' + resp.data);
                        $scope.attachment  = undefined;

                        if (resp.data.status == "OK") {
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

                $scope.loading = true;
                $scope.imageModel = {};
                portfolioService.bootstrapPortfolio($scope.portfolioId).then(function (_portfolio) {
                    portfolio = _portfolio;
                    return _portfolio;
                }).then(function (_portfolio){ 
                    return portfolioService.property($scope.portfolioId, $scope.propertyId).then ( function (property){
                        return property;
                    });
                }).then(function(property){ 
                    $scope.property = property;
                    $scope.loading = false;
                });
                $scope.$watch("property", function (property) {
                    if(property !== undefined) {
                        $scope.files = property.documents.filter(function (d) { 
                            return d.mimeType === "maalka/file";
                        });
                    }
                });
                $scope.putFile = function() { 

                };
                $scope.deleteFile = function(guid) { 
                    if ($scope.property !== undefined) {
                        playRoutes.controllers.PropertyController.deleteFile($scope.propertyId, guid).delete().then ( function (response) {
                            if (response.data.status === "OK") {
                                $scope.property.documents = $scope.property.documents.filter(function (d) { 
                                    return d.guid !== guid;
                                });
                                // the watch should find this... but it doesn't.
                                $scope.files = $scope.property.documents.filter(function (d) { 
                                    return d.mimeType === "maalka/file";
                                });                                
                            } else {
                                console.log("error deleting file");
                                console.log(response);
                            }
                        });
                    }
                };
            }]
        };
    }]);
});