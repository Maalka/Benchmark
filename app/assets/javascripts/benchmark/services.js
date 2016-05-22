define(['angular', 'common'], function(angular) {
	'use strict';
	var mod = angular.module('benchmark.services', ['benchmark.common']);
	mod.service('benchmarkServices', ['playRoutes', function(playRoutes) { 
		var services = {
			'getActualEnergy': function(model) {
				return playRoutes.controllers.BaselineController.getActualEnergy().post(model).then(function (response)  {
					/// handle errors (500 etc)
					return response.data;
				});
			},
            'getPredictedEnergy': function(model) {
                return playRoutes.controllers.BaselineController.getPredictedEnergy().post(model).then(function (response)  {
                    /// handle errors (500 etc)
                    return response.data;
                });
            }

		};
		return services;
	}]);
	return mod;
});