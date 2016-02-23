
define([], function() {
  'use strict';
  var CombineCtrl = function($scope) {

    $scope.round = function(value, decimals) {
        return Number(Math.round(value+'e'+decimals)+'e-'+decimals);
    };

    $scope.dotProduct = function(a,b) {
        var n = 0, lim = Math.min(a.length,b.length);
        for (var i = 0; i < lim; i++){
            n += a[i] * b[i];
        }
        return n;
     };

    $scope.getPropResponseField = function(propResponse,key){
        var returnValue;

        for (var i =0; i < propResponse.values.length; i ++) {
            if (propResponse.values[i][key] !== undefined) {
              returnValue = propResponse.values[i][key];
              break;
            }
        }
        return returnValue;
    };

   $scope.getPrimarySourceSiteRatio = function(sourceSiteRatios,propSizes,sourceEnergies,siteEnergies){

        var returnValue;

        if(sourceEnergies.indexOf(undefined) === -1 && siteEnergies.indexOf(undefined) === -1) {
            console.log("Ratio from energy input");
            returnValue = siteEnergies[0] / sourceEnergies[0];
        } else {
            console.log("Ratio from largest prop size");
            returnValue = sourceSiteRatios[propSizes.indexOf(Math.max.apply(Math, propSizes))];
        }
        return returnValue;
    };

    $scope.getMixedSourceEUI = function(sumPropSize,sourceEnergies,siteEnergies){
        var returnValue;

        if(sourceEnergies.indexOf(undefined) === -1 && siteEnergies.indexOf(undefined) === -1) {
            console.log("Mixed EUI from Energy Input");
            returnValue = sourceEnergies[0] / sumPropSize;
        }
        return returnValue;
    };

    $scope.computeBenchmarkMix = function(results){

        var mixTable;

        //grouped by row in output table
        var mixES;
        var mixTargetES;
        var mixPercentBetterES;
        var mixMedianES;

        var mixSourceEUI;
        var mixTargetSourceEUI;
        var mixPercentBetterSourceEUI;
        var mixMedianSourceEUI;

        var mixSiteEUI;
        var mixTargetSiteEUI;
        var mixPercentBetterSiteEUI;
        var mixMedianSiteEUI;

        var mixTotalSourceEnergy;
        var mixTargetSourceEnergy;
        var mixPercentBetterSourceEnergy;
        var mixMedianSourceEnergy;

        var mixTotalSiteEnergy;
        var mixTargetSiteEnergy;
        var mixPercentBetterSiteEnergy;
        var mixMedianSiteEnergy;

        var mixTotalSiteEmissions;
        var mixTotalTargetEmissions;
        var mixTotalPercentBetterEmissions;
        var mixTotalMedianEmissions;

        var propSizes = results.map(function(o){return $scope.getPropResponseField(o,'buildingSize');});
        var sumPropSize = propSizes.reduce(function(a, b) {return a + b;});
        var propWeightsBySize = propSizes.map(function(o){return o / sumPropSize;});
        var indexLargestProp = propSizes.indexOf(Math.max.apply(Math, propSizes));

        var ESs = results.map(function(o){return $scope.getPropResponseField(o,'ES');});
        var targetESs = results.map(function(o){return $scope.getPropResponseField(o,'targetES');});
        var percentBetterESs = results.map(function(o){return $scope.getPropResponseField(o,'percentBetterES');});

        var sourceEnergies = results.map(function(o){return $scope.getPropResponseField(o,'totalSourceEnergy');});
        var siteEnergies = results.map(function(o){return $scope.getPropResponseField(o,'totalSiteEnergy');});
        var siteTotalEmissions = results.map(function(o){return $scope.getPropResponseField(o,'totalSiteEmissions');});
        var medianEUIs = results.map(function(o){return $scope.getPropResponseField(o,'medianSourceEUI');});
        var percentBetterSourceEUIs = results.map(function(o){return $scope.getPropResponseField(o,'percentBetterSourceEUI');});
        var targetSourceEUIs = results.map(function(o){return $scope.getPropResponseField(o,'targetSourceEUI');});

        var propClasses = results.map(function(o){return $scope.getPropResponseField(o,'buildingClass');});

        var sourceSiteRatios = results.map(function(o){return $scope.getPropResponseField(o,'sourceSiteRatio');});
        var primarySourceSiteRatio = $scope.getPrimarySourceSiteRatio(sourceSiteRatios,propSizes,sourceEnergies,siteEnergies);

        mixSourceEUI = $scope.getMixedSourceEUI(sumPropSize,sourceEnergies,siteEnergies);

        if(results.length > 2){

            if($scope.auxModel.reportingUnits==="us"){
                mixMedianSourceEUI = 123.1;
            }else{
                mixMedianSourceEUI = 1.23;
            }
            mixPercentBetterSourceEUI = mixMedianSourceEUI*(1-$scope.auxModel.percentBetterThanMedian / 100);

        } else {

            if(propClasses.indexOf("class models.GenericBuilding") === -1) {
                mixTargetSourceEUI = $scope.dotProduct(targetSourceEUIs,propWeightsBySize);
                mixPercentBetterSourceEUI = $scope.dotProduct(percentBetterSourceEUIs,propWeightsBySize);
                mixMedianSourceEUI = $scope.dotProduct(medianEUIs,propWeightsBySize);

                mixES = ESs[indexLargestProp];
                mixTargetES = targetESs[indexLargestProp];
                mixPercentBetterES = percentBetterESs[indexLargestProp];
            } else {
                mixTargetSourceEUI = targetSourceEUIs[indexLargestProp];
                mixPercentBetterSourceEUI = percentBetterSourceEUIs[indexLargestProp];
                mixMedianSourceEUI = medianEUIs[indexLargestProp];
            }
        }

        mixMedianES = 50;

        mixSiteEUI = mixSourceEUI*primarySourceSiteRatio;
        mixTargetSiteEUI = mixTargetSourceEUI*primarySourceSiteRatio;
        mixPercentBetterSiteEUI = mixPercentBetterSourceEUI*primarySourceSiteRatio;
        mixMedianSiteEUI = mixMedianSourceEUI*primarySourceSiteRatio;

        mixTotalSourceEnergy = mixSourceEUI*sumPropSize;
        mixTargetSourceEnergy = mixTargetSourceEUI*sumPropSize;
        mixPercentBetterSourceEnergy = mixPercentBetterSourceEUI*sumPropSize;
        mixMedianSourceEnergy = mixMedianSourceEUI*sumPropSize;

        mixTotalSiteEnergy = mixSiteEUI*sumPropSize;
        mixTargetSiteEnergy = mixTargetSiteEUI*sumPropSize;
        mixPercentBetterSiteEnergy = mixPercentBetterSiteEUI*sumPropSize;
        mixMedianSiteEnergy = mixMedianSiteEUI*sumPropSize;

        var mixSiteTargetRatio=mixTotalSiteEnergy/mixTargetSiteEnergy;
        var mixSitePercentBetterRatio=mixTotalSiteEnergy/mixPercentBetterSiteEnergy;
        var mixSiteMedianRatio=mixTotalSiteEnergy/mixMedianSiteEnergy;

        mixTotalSiteEmissions = siteTotalEmissions[0];
        mixTotalTargetEmissions = mixTotalSiteEmissions/mixSiteTargetRatio;
        mixTotalPercentBetterEmissions = mixTotalSiteEmissions/mixSitePercentBetterRatio;
        mixTotalMedianEmissions = mixTotalSiteEmissions/mixSiteMedianRatio;

        mixTable = [
              {"ES":$scope.round(mixES,2)},
              {"targetES":$scope.round(mixTargetES,2)},
              {"percentBetterES":$scope.round(mixPercentBetterES,2)},
              {"medianES":$scope.round(mixMedianES,2)},

              {"sourceEUI":$scope.round(mixSourceEUI,2)},
              {"targetSourceEUI":$scope.round(mixTargetSourceEUI,2)},
              {"percentBetterSourceEUI":$scope.round(mixPercentBetterSourceEUI,2)},
              {"medianSourceEUI":$scope.round(mixMedianSourceEUI,2)},

              {"siteEUI":$scope.round(mixSiteEUI,2)},
              {"targetSiteEUI":$scope.round(mixTargetSiteEUI,2)},
              {"percentBetterSiteEUI":$scope.round(mixPercentBetterSiteEUI,2)},
              {"medianSiteEUI":$scope.round(mixMedianSiteEUI,2)},

              {"totalSourceEnergy":$scope.round(mixTotalSourceEnergy,2)},
              {"targetSourceEnergy":$scope.round(mixTargetSourceEnergy,2)},
              {"percentBetterSourceEnergy":$scope.round(mixPercentBetterSourceEnergy,2)},
              {"medianSourceEnergy":$scope.round(mixMedianSourceEnergy,2)},

              {"totalSiteEnergy":$scope.round(mixTotalSiteEnergy,2)},
              {"targetSiteEnergy":$scope.round(mixTargetSiteEnergy,2)},
              {"percentBetterSiteEnergy":$scope.round(mixPercentBetterSiteEnergy,2)},
              {"medianSiteEnergy":$scope.round(mixMedianSiteEnergy,2)},

              {"totalSiteEmissions":$scope.round(mixTotalSiteEmissions,2)},
              {"targetSiteEmissions":$scope.round(mixTotalTargetEmissions,2)},
              {"percentBetterSiteEmissions":$scope.round(mixTotalPercentBetterEmissions,2)},
              {"medianSiteEmissions":$scope.round(mixTotalMedianEmissions,2)}
              ];

        return mixTable;
    };
  };

  CombineCtrl.$inject = ['$scope'];
  return {
    CombineCtrl: CombineCtrl
  };

});
