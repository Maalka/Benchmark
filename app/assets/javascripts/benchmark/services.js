define(['angular', 'common'], function(angular) {
	'use strict';
	var mod = angular.module('benchmark.services', ['benchmark.common']);
	mod.service('benchmarkServices', ['playRoutes', function(playRoutes) { 
		var services = {
			'submit': function(model) { 
				return playRoutes.controllers.BaselineController.makeBaseline().post(model).then(function (response)  { 
					/// handle errors (500 etc)
					return response.data;
				});
			} 

		};
		return services;
	}]);
	return mod;
});