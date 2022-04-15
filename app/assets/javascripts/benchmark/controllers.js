/*global
    maalkaIncludeHeader
*/
/**
 * Dashboard controllers.
 */
//define(["./test/sample_response_test_data"], function(sampleTestData) {
define(['angular'], function() {
  'use strict';
  var RootCtrl = function($rootScope) { 
    $rootScope.includeHeader = maalkaIncludeHeader;
  };

  RootCtrl.$inject = ['$rootScope'];

  var DashboardCtrl = function($rootScope, $scope, $window, $sce, $timeout, $q, $log, benchmarkServices) {

    $rootScope.includeHeader = maalkaIncludeHeader;
    $rootScope.pageTitle = "2030 Baseline";
    //The model that will be submitted for analysis
    $scope.auxModel = {};
    //The table of energy information input by user, default to empty
    $scope.energies = [{}, {}];
    $scope.sold = [{'renewableType': {id:"grid",name:"Sold"}}];

    $scope.renewableEnergies = [{}, {}];
    //For displaying user-input energy entries after having been saved
    $scope.propList = [];

    $scope.benchmarkResult = null;
    $scope.lacksDD = false;
    $scope.hasOnSite = false;
    $scope.hasOffSite = false;

    $scope.propOutputList = [];
    $scope.tableEUIUnits = null;
    $scope.tableEnergyUnits = null;
    $scope.forms = {'hasValidated': false};
    $scope.propTypes = [];
    $scope.mainColumnWidth = "";
    $scope.propText = "Primary Building Use";
    $scope.buildingZone = "commercial";
    $scope.auxModel.buildingList = "commercial";
    $scope.targetToggle = "percentReduction";
    $scope.isResidential = false;

    if (window.matchMedia) {

        var printQueryList = window.matchMedia('print');
        var phoneQueryList = window.matchMedia('(max-width: 767px)');      
        var tabletQueryList = window.matchMedia('(min-width: 768px) and (max-width: 1200px)');            
        var desktopQueryList = window.matchMedia('(min-width: 1200px) and (max-width: 1919px');                  
        var largeQueryList = window.matchMedia('(min-width: 1919px)');                  

        var updateMatchMedia= function (q) { 
            console.log(q);
            if (printQueryList.matches) {
                $scope.media = "print";                                    
            } else if (phoneQueryList.matches) {
                $scope.media = "phone";                        
            } else if (tabletQueryList.matches) { 
                $scope.media = "tablet";            
            } else if (desktopQueryList.matches) { 
                $scope.media = "desktop";
            } 

            if (largeQueryList.matches) {
                $scope.largeScreen = true;
            } else {
                $scope.largeScreen = false;
            }

            console.log($scope.media);
            $timeout(function () {
                $scope.$apply();
            });
        };
        updateMatchMedia();

        printQueryList.addListener(updateMatchMedia);

        $scope.$on("$destroy", function handler() {
            printQueryList.removeListener(updateMatchMedia);
        });
    }

    $scope.$watch("auxModel.buildingZone", function (v) {
        if(v === "commercial"){
            if($scope.propTypes.length !== 0) {
                $scope.auxModel.buildingList = "parking";
            }else {
                $scope.auxModel.buildingList = "commercial";
            }
        }else{
            $scope.auxModel.buildingList = "residential";
        }
    });


    $scope.$watch("auxModel.buildingType", function (v) {
        if (v === undefined || v === null) {
            return; 
        }

        if(($scope.auxModel.country) && (v)){

            $scope.isResidential = false;

            for(var i = 0; i < $scope.buildingProperties.buildingType.residential.length; i++ ) {
                if( $scope.buildingProperties.buildingType.residential[i].id === v.id){
                    if (v.id === "MultiFamily"){
                        $scope.isResidential = false;
                    } else {
                        $scope.isResidential = true;
                    }
                   if ($scope.isResidential === true && v.id !== "MultiFamily"){
                       $scope.propTypes = [];
                   }

                   // $scope.propTypes = [];
                    break;
                }
            }



            for(var j = 0; j < $scope.propTypes.length; j++ ) {
                for(var k = 0; k < $scope.buildingProperties.buildingType.residential.length; k++ ) {
                    if( $scope.propTypes[j].type === $scope.buildingProperties.buildingType.residential[k].id &&
                    $scope.propTypes[j].type !== "MultiFamily"){
                        $scope.clearProp($scope.propTypes[j]);
                        break;
                    }
                }
            }



            if($scope.auxModel.buildingZone === "commercial" || v.id === "MultiFamily"){

                if($scope.auxModel.buildingType.id === "Parking"){
                    $scope.auxModel.tempList = "parking";
                }else{
                    $scope.auxModel.tempList = "commercial";
                }
            }else{
                $scope.auxModel.tempList = "residential";
            }

            $scope.propTypes.push({
                changeTo: v,
                type: v.id,
                name: v.name,
                country:$scope.auxModel.country,
                buildingZone: $scope.auxModel.buildingZone,
                buildingList: $scope.auxModel.tempList,
                toggleTarget: $scope.auxModel.targetToggle
            });

           if($scope.auxModel.buildingZone === "commercial" || v.id === "MultiFamily"){
                $scope.auxModel.buildingZone = "commercial";
                if($scope.propTypes.length !== 0) {
                    $scope.auxModel.buildingList = "parking";
                }else{
                    $scope.auxModel.buildingList = "commercial";
                }
            }else{
                $scope.auxModel.buildingList = "residential";
            }




            $scope.propText="Add Another Use";
            // there seems to be a $digest issue where undefined isn't carried through to the dropdown directive
            $scope.auxModel.resetBuildingType = true;
            $scope.auxModel.buildingType = undefined;
        }
    });

    $scope.updatePropType = function($index) { 

        $scope.propTypes[$index] = {
            changeTo: $scope.propTypes[$index].changeTo,
            country: $scope.propTypes[$index].country,
            buildingZone: $scope.propTypes[$index].buildingZone,
            buildingList: $scope.propTypes[$index].buildingList,
            toggleTarget: $scope.propTypes[$index].toggleTarget,
            type: $scope.propTypes[$index].changeTo.id,
            name: $scope.propTypes[$index].changeTo.name
        };
    };

    $scope.$watch("auxModel.country", function () {
        $scope.benchmarkResult = null;
        $scope.clearGeography();
    });

    $scope.$watch("auxModel.targetToggle", function (v) {
        if(v === 'zeroScore'){
        $scope.auxModel.is2030=false;
    }
    });

    $scope.$watch("auxModel.newConstruction", function (v) {
        if(v === true){
            $scope.auxModel.percentBetterThanMedian = 70;
            $scope.auxModel.targetToggle = "percentReduction";
        }else{
            $scope.auxModel.percentBetterThanMedian = 20;
        }
    });

    $scope.clearGeography = function () {
        $scope.auxModel.city = "";
        $scope.auxModel.state = null;
        //$scope.auxModel.postalCode = "";
        $scope.auxModel.buildingType = undefined;
        $scope.propTypes = [];
    };


    //populate user-input energy information table to calculate site/source EUI and Energy Star metrics
    //display errors when present
    $scope.addEnergiesRow = function(){
        $scope.energies.push({});
    };

    $scope.print = function () {
        window.print();
    };

    $scope.removeRow = function(index){
        $scope.energies.splice(index, 1);
        if ($scope.energies.length ===    1) {
            $scope.addEnergiesRow();
        }
    };

    $scope.addRenewableEnergiesRow = function(){
        $scope.renewableEnergies.push({});
    };

    $scope.removeRenewableRow = function(index){
        $scope.renewableEnergies.splice(index, 1);
        if ($scope.renewableEnergies.length ===    1) {
            $scope.addRenewableEnergiesRow();
        }
    };

    $scope.removeSoldRow = function(){
        $scope.sold.pop();
        $scope.sold.push({'renewableType': {id:"grid",name:"Sold"}});
    };

    $scope.removeProp = function(prop){

        var index;
        for(var i = 0; i < $scope.propTypes.length; i++ ) {
            if($scope.propTypes[i].name === prop.model.name && $scope.propTypes[i].country === prop.model.country) {
                index = i;
                break;
            }
        }

        $scope.propTypes.splice(index, 1);

        if($scope.propTypes.length === 0){
            $scope.propText="Primary Building Use";
            if($scope.auxModel.buildingZone === "commercial"){
                 $scope.auxModel.buildingList = "commercial";
            }else{
                $scope.auxModel.buildingList = "residential";
            }
        }

    };

    $scope.clearProp = function(prop){
        var index;

        for(var i = 0; i < $scope.propTypes.length; i++ ) {
            if($scope.propTypes[i].name === prop.name && $scope.propTypes[i].country === prop.country) {
                index = i;
                break;
            }
        }

        $scope.propTypes.splice(index, 1);
        if($scope.propTypes.length === 0){
            $scope.propText="Primary Building Use";
        }
    };

    $scope.postalCodeCheck = function() {
        if($scope.auxModel.postalCode){
            if($scope.auxModel.postalCode.length > 4){
                $scope.temp = {"postalCode":$scope.auxModel.postalCode};
                $scope.computeDegreeDays();
            }else{
                $scope.clearDegreeDays();
            }
        }else{
            $scope.clearDegreeDays();
         }
    };

    $scope.clearDegreeDays = function(){
        $scope.auxModel.HDD = null;
        $scope.auxModel.CDD = null;
        $scope.lacksDD = true;
    };

    $scope.computeDegreeDays = function(){

        $scope.futures = benchmarkServices.getDDMetrics($scope.temp);
        $q.resolve($scope.futures).then(function (results) {
            $scope.auxModel.CDD = $scope.getPropResponseField(results,"CDD");
            $scope.auxModel.HDD = $scope.getPropResponseField(results,"HDD");
            if (results.errors.length > 0){
                $scope.lacksDD = true;
            }
        });
    };

    $scope.computeBenchmarkResult = function(){

        $scope.propSubmit = [];
        for (var i = 0; i < $scope.propList.length; i++){
            if($scope.propList[i].buildingType !== "Parking"){
                $scope.propSubmit.push($scope.propList[i]);
            }
        }

        $log.info($scope.propSubmit);

        $scope.futures = benchmarkServices.getZEPIMetrics($scope.propSubmit);

        $q.resolve($scope.futures).then(function (results) {
            $scope.baselineConstant = $scope.getBaselineConstant();
            $scope.scoreText = "Zero Score";
            $scope.scoreGraph = "Rating";
            $scope.FFText = $sce.trustAsHtml('Site EUI');
            $scope.scoreUnits = $scope.isResidential  ? "0-130" : "0-100";

            $scope.benchmarkResult = $scope.computeBenchmarkMix(results);
            $scope.benchmarkResult.city = $scope.auxModel.city;
            $scope.benchmarkResult.state = $scope.auxModel.state;
            $scope.benchmarkResult.postalCode = $scope.auxModel.postalCode;
            //console.log($scope.benchmarkResult);
            if ($scope.auxModel.targetToggle === "zeroScore") {
                $scope.benchmarkResult.percentBetterThanMedian = $scope.getBaselineConstant() - $scope.auxModel.percentBetterThanMedian;
            } else {
                $scope.benchmarkResult.percentBetterThanMedian = $scope.auxModel.percentBetterThanMedian;
            }

            $scope.hasOnSite = ($scope.getPropResponseField(results,"onSiteRenewableTotal") > 0.01) ;
            $scope.hasOffSite = ($scope.getPropResponseField(results,"offSitePurchasedTotal") > 0.01) ;
            $scope.hasSiteEnergy = ($scope.getPropResponseField(results,"siteEUI") > 0.01) ;




        });
    };

    $scope.getBaselineConstant = function(){
        if ($scope.isResidential === true && $scope.auxModel.is2030 === true){
            return 130;
        } else {
            return 100;
        }
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

    $scope.computeBenchmarkMix = function(results){

        $scope.propOutputList = $scope.getPropResponseField(results,"propOutputList");
        $scope.percentBetterSiteEUI = Math.ceil($scope.getPropResponseField(results,"percentBetterSiteEUI"));
        $scope.siteEUI = Math.ceil($scope.getPropResponseField(results,"siteEUI"));

        if($scope.siteEUI > $scope.percentBetterSiteEUI){
            $scope.showPercentBetterTarget = 'decrease';
            $scope.percentBetterGoal = Math.ceil($scope.getPropResponseField(results,"percentBetterActualtoGoal"));
        }else if($scope.siteEUI < $scope.percentBetterSiteEUI){
            $scope.showPercentBetterTarget = 'better';
            $scope.percentBetterGoal = Math.ceil($scope.getPropResponseField(results,"actualGoalBetter"));
        } else {
            $scope.showPercentBetterTarget = undefined;
        }

        var baselineConstant = $scope.getBaselineConstant();
        var scaleConstant = baselineConstant / 100;
        var metricsTable = [

              {"percentBetterMedian": Math.ceil($scope.getPropResponseField(results,"percentBetterMedian"))},
              {"percentBetterTarget": Math.ceil($scope.getPropResponseField(results,"percentBetterTarget"))},
              {"percentBetterActual": Math.ceil($scope.getPropResponseField(results,"percentBetterActual"))},
              {"percentBetterActualwOnSite": Math.ceil($scope.getPropResponseField(results,"percentBetterActualwOnSite"))},
              {"percentBetterActualwOffSite": Math.ceil($scope.getPropResponseField(results,"percentBetterActualwOffSite"))},
              {"percentBetterActualwOnandOffSite": Math.ceil($scope.getPropResponseField(results,"percentBetterActualwOnandOffSite"))},


              {"percentBetterActualtoGoal": Math.ceil($scope.getPropResponseField(results,"percentBetterActualtoGoal"))},
              {"actualGoalBetter": Math.ceil($scope.getPropResponseField(results,"actualGoalBetter"))},

              {"medianZEPI": baselineConstant},
              {"percentBetterZEPI": Math.ceil($scope.getPropResponseField(results,"percentBetterZEPI"))},
              {"actualZEPI": baselineConstant - Math.ceil($scope.getPropResponseField(results,"percentBetterActual") * scaleConstant)},
              {"actualZEPIwOnSite": baselineConstant - Math.ceil($scope.getPropResponseField(results,"percentBetterActualwOnSite") * scaleConstant)},
              {"actualZEPIwOffSite": baselineConstant - Math.ceil($scope.getPropResponseField(results,"percentBetterActualwOffSite") * scaleConstant)},
              {"actualZEPIwOnAndOffSite": baselineConstant - Math.ceil($scope.getPropResponseField(results,"percentBetterActualwOnandOffSite") * scaleConstant)},

              {"siteEUI": Math.ceil($scope.getPropResponseField(results,"siteEUI"))},
              {"siteEUIwOnSite": Math.ceil($scope.getPropResponseField(results,"siteEUIwOnSite"))},
              {"siteEUIwOffSite": Math.ceil($scope.getPropResponseField(results,"siteEUIwOffSite"))},
              {"siteEUIwOnAndOffSite": Math.ceil($scope.getPropResponseField(results,"siteEUIwOnAndOffSite"))},

              {"sourceEUI": Math.ceil($scope.getPropResponseField(results,"sourceEUI"))},
              {"sourceEUIwOnSite": Math.ceil($scope.getPropResponseField(results,"sourceEUIwOnSite"))},
              {"sourceEUIwOffSite": Math.ceil($scope.getPropResponseField(results,"sourceEUIwOffSite"))},
              {"sourceEUIwOnAndOffSite": Math.ceil($scope.getPropResponseField(results,"sourceEUIwOnAndOffSite"))},


              {"medianSiteEUI": Math.ceil($scope.getPropResponseField(results,"medianSiteEUI"))},
              {"medianSourceEUI": Math.ceil($scope.getPropResponseField(results,"medianSourceEUI"))},

              {"percentBetterSiteEUI": Math.ceil($scope.getPropResponseField(results,"percentBetterSiteEUI"))},
              {"percentBetterSourceEUI": Math.ceil($scope.getPropResponseField(results,"percentBetterSourceEUI"))},


              {"totalEmissions": Math.ceil($scope.getPropResponseField(results,"totalEmissions"))},
              {"percentBetterEmissions": Math.ceil($scope.getPropResponseField(results,"percentBetterEmissions"))},
              {"medianEmissions": Math.ceil($scope.getPropResponseField(results,"medianEmissions"))},

              {"onSiteRenewableTotal": $scope.getPropResponseField(results,"onSiteRenewableTotal")},
              {"offSitePurchasedTotal": $scope.getPropResponseField(results,"offSitePurchasedTotal")},
              {"siteEnergyALL": $scope.getPropResponseField(results,"siteEnergyALL")},


                //For the Maalka Platform
/*            {"actualES": $scope.getPropResponseField(results,"actualES")},
              {"medianES": $scope.getPropResponseField(results,"actualES")},
              {"targetES": $scope.getPropResponseField(results,"targetES")},*/

              {"siteEnergyList": $scope.getPropResponseField(results,"siteEnergyList")},
              {"totalSiteEnergy": Math.ceil($scope.getPropResponseField(results,"totalSiteEnergy"))},
              {"sourceEnergyList": $scope.getPropResponseField(results,"sourceEnergyList")},
              {"totalSourceEnergy": Math.ceil($scope.getPropResponseField(results,"totalSourceEnergy"))},

              {"medianSiteEnergy": Math.ceil($scope.getPropResponseField(results,"medianSiteEnergy"))},
              {"medianSourceEnergy": Math.ceil($scope.getPropResponseField(results,"medianSourceEnergy"))},

              {"percentBetterSiteEnergy": Math.ceil($scope.getPropResponseField(results,"percentBetterSiteEnergy"))},
              {"percentBetterSourceEnergy": Math.ceil($scope.getPropResponseField(results,"percentBetterSourceEnergy"))},

              {"directSiteEmissions": Math.ceil($scope.getPropResponseField(results,"directSiteEmissions"))},
              {"indirectSiteEmissions": Math.ceil($scope.getPropResponseField(results,"indirectSiteEmissions"))},

              {"parkingEnergy": $scope.getPropResponseField(results,"parkingEnergy")},
              {"parkingArea": $scope.getPropResponseField(results,"parkingArea")}


        ];

        return metricsTable;
    };


    $scope.submitErrors = function () {
        for (var i = 0; i < $scope.forms.baselineForm.$error.required.length; i++){
            $log.info($scope.forms.baselineForm.$error.required[i].$name);
        }
    };

    $scope.$watch("auxModel.reportingUnits", function (value) {
        if (value === undefined) {
            return;
        }
        // only submit if the user has already CLICK on the submit button
        if($scope.forms.hasValidated) {
            $scope.submit();
        }
    });

    $scope.degreeDaysCheck = function () {
        $scope.computeDegreeDays();
    };

    $scope.submit = function () {
        if($scope.forms.baselineForm === undefined) {
            return;
        }
        $scope.forms.hasValidated = true; /// only check the field errors if this form has attempted to validate.
        $scope.propList = [];

        if($scope.auxModel.reportingUnits==="us"){
            $scope.tableEnergyUnits="(kBtu/yr)";
            $scope.tableEUIUnits="(kBtu/ft²/yr)";
        }else {
            $scope.tableEnergyUnits="(kWh/yr)";
            $scope.tableEUIUnits="(kWh/m²/yr)";
        }

        if($scope.auxModel.CDD === undefined || $scope.auxModel.HDD === undefined ||
        $scope.auxModel.HDD === null || $scope.auxModel.HDD === null){
            $scope.lacksDD = true;
        }



        var validEnergy = function(e) {
            return (e.energyType !== undefined &&
                    e.energyName !== undefined &&
                    //e.energyName !== "Electric (renewable)" &&
                    e.energyUnits !== undefined &&
                    e.energyUse !== undefined && $scope.auxModel.newConstruction === false);
        };

        var mapEnergy = function (e) {
            return {
                'energyType': (e.energyType) ? e.energyType.id : undefined,
                'energyName': (e.energyType) ? e.energyType.name : undefined,
                'energyUnits': e.energyUnits,
                'energyUse': Number(e.energyUse),
                'energyRate': null
            };
        };
        var mapRenewableEnergy = function (e) {
            return {
                'energyType': (e.renewableType) ? e.renewableType.id : undefined,
                'energyName': (e.renewableType) ? e.renewableType.name : undefined,
                'energyUnits': e.renewableUnits,
                'energyUse': Number(e.energyUse),
                'energyRate': null
            };
        };

        var validRenewableFromRegular = function(e) {
            return (e.energyType !== undefined &&
                    e.energyName !== undefined &&
                    e.energyName === "Electric (renewable)" &&
                    e.energyUnits !== undefined &&
                    e.energyUse !== undefined && $scope.auxModel.newConstruction === false);
        };

        var getSoldEnergy = function () {

            var soldEnergyList =[];
            var soldEnergy = 0.0;
            var renewableEnergy = 0.0;

            for (var i = 0; i < $scope.sold.map(mapRenewableEnergy).filter(validEnergy).length; i++){
                soldEnergy = soldEnergy + $scope.sold.map(mapRenewableEnergy).filter(validEnergy)[i].energyUse;
            }
            for (var j = 0; j < $scope.renewableEnergies.map(mapRenewableEnergy).filter(validEnergy).length; j++){
                renewableEnergy = renewableEnergy + $scope.renewableEnergies.map(mapRenewableEnergy).filter(validEnergy)[j].energyUse;
            }

            if($scope.sold.map(mapRenewableEnergy).filter(validEnergy).length !== 0){
                if(soldEnergy < renewableEnergy){
                    soldEnergyList = $scope.sold.map(mapRenewableEnergy).filter(validEnergy);
                }else{
                    soldEnergyList = [{
                    'energyType': "grid",
                    'energyName': "Sold",
                    'energyUnits': $scope.sold[0].renewableUnits,
                    'energyUse': renewableEnergy
                    }];
                }
            }

            return soldEnergyList;
        };


        var getRenewableEnergyList = function () {

            var renewableEnergyListFromRegular = $scope.energies.map(mapEnergy).filter(validRenewableFromRegular);
            var renewableEnergyList = $scope.renewableEnergies.map(mapRenewableEnergy).filter(validEnergy);


            renewableEnergyListFromRegular.push.apply(renewableEnergyListFromRegular,getSoldEnergy());
            renewableEnergyList.push.apply(renewableEnergyList,renewableEnergyListFromRegular);

            return renewableEnergyList;
        };

        var getFullEnergyList = function () {

            var energyListFromRegular = $scope.energies.map(mapEnergy).filter(validEnergy);
            var renewableEnergyList = $scope.renewableEnergies.map(mapRenewableEnergy).filter(validEnergy);
            //var soldEnergyList = $scope.sold.map(mapRenewableEnergy).filter(validEnergy);

            renewableEnergyList.push.apply(renewableEnergyList,getSoldEnergy());
            energyListFromRegular.push.apply(energyListFromRegular,renewableEnergyList);


            return energyListFromRegular;
        };

        if($scope.forms.baselineForm.$valid){

            $scope.hasParkingHeating = null;
            $scope.openParkingArea = null;
            $scope.partiallyEnclosedParkingArea = null;
            $scope.fullyEnclosedParkingArea = null;
            $scope.parkingAreaUnits = null;
            $scope.totalParkingArea = null;

            for (var j = 0; j < $scope.propTypes.length; j++){
                if($scope.propTypes[j].valid === true){
                    if($scope.propTypes[j].propertyModel.buildingType === "Parking"){
                        $scope.hasParkingHeating = $scope.propTypes[j].propertyModel.hasParkingHeating ;
                        $scope.openParkingArea = $scope.propTypes[j].propertyModel.openParkingArea;
                        $scope.partiallyEnclosedParkingArea = $scope.propTypes[j].propertyModel.partiallyEnclosedParkingArea;
                        $scope.fullyEnclosedParkingArea = $scope.propTypes[j].propertyModel.fullyEnclosedParkingArea;
                        $scope.parkingAreaUnits = $scope.propTypes[j].propertyModel.areaUnits;
                        $scope.totalParkingArea = $scope.propTypes[j].propertyModel.openParkingArea + $scope.propTypes[j].propertyModel.partiallyEnclosedParkingArea + $scope.propTypes[j].propertyModel.fullyEnclosedParkingArea;
                    }
                }
            }
        }


        if($scope.forms.baselineForm.$valid){
            for (var i = 0; i < $scope.propTypes.length; i++){
                if($scope.propTypes[i].valid === true){

                //console.log($scope.propTypes[i]);


                    $scope.propTypes[i].propertyModel.hasParkingHeating = $scope.hasParkingHeating;
                    $scope.propTypes[i].propertyModel.openParkingArea = $scope.openParkingArea;
                    $scope.propTypes[i].propertyModel.partiallyEnclosedParkingArea = $scope.partiallyEnclosedParkingArea;
                    $scope.propTypes[i].propertyModel.fullyEnclosedParkingArea = $scope.fullyEnclosedParkingArea;
                    $scope.propTypes[i].propertyModel.parkingAreaUnits = $scope.parkingAreaUnits;
                    $scope.propTypes[i].propertyModel.totalParkingArea = $scope.totalParkingArea;

                    $scope.propTypes[i].propertyModel.baselineConstant = $scope.getBaselineConstant();
                    $scope.propTypes[i].propertyModel.country = $scope.auxModel.country;
                    $scope.propTypes[i].propertyModel.targetToggle = $scope.auxModel.targetToggle;
                    $scope.propTypes[i].propertyModel.city = $scope.auxModel.city;
                    $scope.propTypes[i].propertyModel.buildingName = ($scope.auxModel.buildingName) ? $scope.auxModel.buildingName : "Anonymous";
                    $scope.propTypes[i].propertyModel.propTypeName = ($scope.propTypes[i].name) ? $scope.propTypes[i].name : "No Name";
                    $scope.propTypes[i].propertyModel.postalCode = $scope.auxModel.postalCode;
                    $scope.propTypes[i].propertyModel.state = $scope.auxModel.state;
                    $scope.propTypes[i].propertyModel.HDD = $scope.auxModel.HDD;
                    $scope.propTypes[i].propertyModel.CDD = $scope.auxModel.CDD;
                    $scope.propTypes[i].propertyModel.reportingUnits = $scope.auxModel.reportingUnits;
                    //$scope.propTypes[i].propertyModel.netMetered = $scope.auxModel.netMetered;
                    $scope.propTypes[i].propertyModel.percentBetterThanMedian = $scope.auxModel.percentBetterThanMedian;


                    if($scope.isResidential && !$scope.auxModel.is2030 && ($scope.propTypes[i].type !== "MultiFamily")){
                        $scope.propTypes[i].propertyModel.target2030=true;
                    } else {
                        $scope.propTypes[i].propertyModel.target2030=false;
                    }

                    if($scope.renewableEnergies.map(mapRenewableEnergy).filter(validEnergy).length===0 &&
                       $scope.energies.map(mapEnergy).filter(validEnergy).length===0){
                        $scope.propTypes[i].propertyModel.energies=null;
                    } else {
                        $scope.propTypes[i].propertyModel.energies = getFullEnergyList();
                    }

                    if($scope.renewableEnergies.map(mapRenewableEnergy).filter(validEnergy).length===0 &&
                       $scope.energies.map(mapEnergy).filter(validRenewableFromRegular).length ===0){
                        $scope.propTypes[i].propertyModel.renewableEnergies=null;
                    } else {
                        $scope.propTypes[i].propertyModel.renewableEnergies = getRenewableEnergyList();
                    }


                    $scope.propList.push($scope.propTypes[i].propertyModel);

                } else {
                    $log.info('Error in ' + $scope.propTypes[i].type);
                }
            }
        }else {
            $scope.submitErrors();
        }



        if ($scope.propList.length !== 0){
                $scope.computeBenchmarkResult();
            }else{
                $scope.benchmarkResult = null;
            }

    };

        $scope.geographicProperties = {
                country:
                    [{id:"USA",name:"United States"},
                    {id:"Canada",name:"Canada"}],
                state:
                    [
                    {id:"AL",name:"Alabama",filter_id:"USA"},
                    {id:"AK",name:"Alaska",filter_id:"USA"},
                    {id:"AZ",name:"Arizona",filter_id:"USA"},
                    {id:"AR",name:"Arkansas",filter_id:"USA"},
                    {id:"CA",name:"California",filter_id:"USA"},
                    {id:"CO",name:"Colorado",filter_id:"USA"},
                    {id:"CT",name:"Connecticut",filter_id:"USA"},
                    {id:"DE",name:"Delaware",filter_id:"USA"},
                    {id:"DC",name:"District Of Columbia",filter_id:"USA"},
                    {id:"FL",name:"Florida",filter_id:"USA"},
                    {id:"GA",name:"Georgia",filter_id:"USA"},
                    {id:"HI",name:"Hawaii",filter_id:"USA"},
                    {id:"ID",name:"Idaho",filter_id:"USA"},
                    {id:"IL",name:"Illinois",filter_id:"USA"},
                    {id:"IN",name:"Indiana",filter_id:"USA"},
                    {id:"IA",name:"Iowa",filter_id:"USA"},
                    {id:"KS",name:"Kansas",filter_id:"USA"},
                    {id:"KY",name:"Kentucky",filter_id:"USA"},
                    {id:"LA",name:"Louisiana",filter_id:"USA"},
                    {id:"ME",name:"Maine",filter_id:"USA"},
                    {id:"MD",name:"Maryland",filter_id:"USA"},
                    {id:"MA",name:"Massachusetts",filter_id:"USA"},
                    {id:"MI",name:"Michigan",filter_id:"USA"},
                    {id:"MN",name:"Minnesota",filter_id:"USA"},
                    {id:"MS",name:"Mississippi",filter_id:"USA"},
                    {id:"MO",name:"Missouri",filter_id:"USA"},
                    {id:"MT",name:"Montana",filter_id:"USA"},
                    {id:"NE",name:"Nebraska",filter_id:"USA"},
                    {id:"NV",name:"Nevada",filter_id:"USA"},
                    {id:"NH",name:"New Hampshire",filter_id:"USA"},
                    {id:"NJ",name:"New Jersey",filter_id:"USA"},
                    {id:"NM",name:"New Mexico",filter_id:"USA"},
                    {id:"NY",name:"New York",filter_id:"USA"},
                    {id:"NC",name:"North Carolina",filter_id:"USA"},
                    {id:"ND",name:"North Dakota",filter_id:"USA"},
                    {id:"OH",name:"Ohio",filter_id:"USA"},
                    {id:"OK",name:"Oklahoma",filter_id:"USA"},
                    {id:"OR",name:"Oregon",filter_id:"USA"},
                    {id:"PA",name:"Pennsylvania",filter_id:"USA"},
                    {id:"RI",name:"Rhode Island",filter_id:"USA"},
                    {id:"SC",name:"South Carolina",filter_id:"USA"},
                    {id:"SD",name:"South Dakota",filter_id:"USA"},
                    {id:"TN",name:"Tennessee",filter_id:"USA"},
                    {id:"TX",name:"Texas",filter_id:"USA"},
                    {id:"UT",name:"Utah",filter_id:"USA"},
                    {id:"VT",name:"Vermont",filter_id:"USA"},
                    {id:"VA",name:"Virginia",filter_id:"USA"},
                    {id:"WA",name:"Washington",filter_id:"USA"},
                    {id:"WV",name:"West Virginia",filter_id:"USA"},
                    {id:"WI",name:"Wisconsin",filter_id:"USA"},
                    {id:"WY",name:"Wyoming",filter_id:"USA"},
                    {id:"AB",name:"Alberta",filter_id:"Canada"},
                    {id:"BC",name:"British Columbia",filter_id:"Canada"},
                    {id:"MB",name:"Manitoba",filter_id:"Canada"},
                    {id:"NB",name:"New Brunswick",filter_id:"Canada"},
                    {id:"NL",name:"Newfoundland",filter_id:"Canada"},
                    {id:"NS",name:"Nova Scotia",filter_id:"Canada"},
                    {id:"NT",name:"Northwest Territories",filter_id:"Canada"},
                    {id:"NU",name:"Nunavut",filter_id:"Canada"},
                    {id:"ON",name:"Ontario",filter_id:"Canada"},
                    {id:"PE",name:"Prince Edward Island",filter_id:"Canada"},
                    {id:"QC",name:"Quebec",filter_id:"Canada"},
                    {id:"SK",name:"Saskatchewan",filter_id:"Canada"},
                    {id:"YT",name:"Yukon",filter_id:"Canada"}]
            };

        $scope.buildingProperties = {

            buildingType: {
                commercial: [
                    {id:"FinancialOffice",name:"Bank Branch"},
                    {id:"FinancialOffice",name:"Financial Office"},
                    {id:"AdultEducation",name:"Adult Education"},
                    {id:"College",name:"College / University"},
                    {id:"K12School",name:"K-12 School"},
                    {id:"PreSchool",name:"Pre-school / DayCare"},
                    {id:"VocationalSchool",name:"Vocational School"},
                    {id:"OtherEducation",name:"Other Education"},
                    {id:"ConventionCenter",name:"Convention Center"},
                    {id:"MovieTheater",name:"Movie Theater"},
                    {id:"Museum",name:"Museum"},
                    {id:"PerformingArts",name:"Performing Arts"},
                    {id:"BowlingAlley",name:"Bowling Alley"},
                    {id:"FitnessCenter",name:"Fitness Center"},
                    {id:"IceRink",name:"Ice / Curling Rink"},
                    {id:"RollerRink",name:"Roller Rink"},
                    {id:"SwimmingPool",name:"Swimming Pool"},
                    {id:"OtherRecreation",name:"Other Recreation"},
                    {id:"Stadium",name:"Stadium"},
                    {id:"IndoorArena",name:"Indoor Arena"},
                    {id:"RaceTrack",name:"Race Track"},
                    {id:"Aquarium",name:"Aquarium"},
                    {id:"Bar",name:"Bar"},
                    //{id:"Bar",name:"Nightclub"},
                    {id:"Casino",name:"Casino"},
                    {id:"Zoo",name:"Zoo"},
                    {id:"OtherEntertainment",name:"Other Entertainment"},
                    {id:"GasStation",name:"Convenience Store with Gas Station"},
                    {id:"ConvenienceStore",name:"Convenience Store without Gas Station"},
                    {id:"FastFoodRestaurant",name:"Fast Food Restaurant"},
                    {id:"Restaurant",name:"Restaurant"},
                    {id:"Supermarket",name:"Supermarket"},
                    {id:"Retail",name:"Wholesale Club"},
                    {id:"FoodSales",name:"Food Sales"},
                    {id:"FoodService",name:"Food Service"},
                    {id:"AmbulatorySurgicalCenter",name:"Ambulatory Surgical Center"},
                    {id:"Hospital",name:"Hospital"},
                    {id:"SpecialtyHospital",name:"Specialty Hospital"},
                    {id:"MedicalOffice",name:"Medical Office"},
                    {id:"OutpatientCenter",name:"Outpatient Rehabilitation Center"},
                    {id:"PhysicalTherapyCenter",name:"Physical Therapy Center"},
                    {id:"SeniorCare",name:"Senior Care Community"},
                    {id:"UrgentCareCenter",name:"Urgent Care Center"},
                    {id:"Barracks",name:"Barracks"},
                    {id:"Hotel",name:"Hotel"},
                    {id:"MultiFamily",name:"Multifamily Housing"},
                    {id:"Prison",name:"Prison / Incarceration"},
                    {id:"ResidenceHall",name:"Residence Hall"},
                    {id:"ResidentialLodging",name:"Other Residential Lodging"},
                    {id:"MixedUse",name:"Mixed Use Property"},
                    {id:"Office",name:"Office"},
                    {id:"VeterinaryOffice",name:"Veterinary Office"},
                    {id:"Courthouse",name:"Courthouse"},
                    {id:"DrinkingWaterTreatment",name:"Drinking Water Treatment Center"},
                    {id:"FireStation",name:"Fire Station"},
                    {id:"Library",name:"Library"},
                    {id:"PostOffice",name:"Post Office"},
                    {id:"PoliceStation",name:"Police Station"},
                    {id:"MeetingHall",name:"Meeting Hall"},
                    {id:"TransportationTerminal",name:"Transportation Terminal"},
                    {id:"WastewaterCenter",name:"Wastewater Treatment Center"},
                    {id:"OtherPublicServices",name:"Other Public Services"},
                    {id:"WorshipCenter",name:"Worship Facility"},
                    {id:"AutoDealership",name:"Automobile Dealership"},
                    {id:"EnclosedMall",name:"Enclosed Mall"},
                    {id:"StripMall",name:"Strip Mall"},
                    {id:"Retail",name:"Retail Store"},
                    {id:"DataCenter",name:"Data Center"}, //Data Centers behave very different and require custom script
                    {id:"PersonalServices",name:"Personal Services (Health/Beauty, Dry Cleaning, etc.)"},
                    {id:"RepairServices",name:"Repair Services (Vehicle, Shoe Locksmith, etc.)"},
                    {id:"OtherServices",name:"Other Services"},
                    {id:"PowerStation",name:"Energy / Power Station"},
                    {id:"OtherUtility",name:"Other Utility Station"},
                    {id:"SelfStorageFacility",name:"Self Storage Facility"},
                    {id:"Warehouse",name:"Warehouse - UnRefrigerated"},
                    {id:"WarehouseRefrigerated",name:"Warehouse - Refrigerated"},
                    {id:"Warehouse",name:"Distribution Center"}
                ],
                residential: [
                    {id:"SingleFamilyDetached",name:"Single Family - Detached"},
                    {id:"SingleFamilyAttached",name:"Single Family - Attached"},
                    {id:"MobileHome",name:"Mobile Home"},
                    {id:"MultiFamily",name:"Multifamily Housing"}
                ],
                parking: [
                {id:"FinancialOffice",name:"Bank Branch"},
                {id:"FinancialOffice",name:"Financial Office"},
                {id:"AdultEducation",name:"Adult Education"},
                {id:"College",name:"College / University"},
                {id:"K12School",name:"K-12 School"},
                {id:"PreSchool",name:"Pre-school / DayCare"},
                {id:"VocationalSchool",name:"Vocational School"},
                {id:"OtherEducation",name:"Other Education"},
                {id:"ConventionCenter",name:"Convention Center"},
                {id:"MovieTheater",name:"Movie Theater"},
                {id:"Museum",name:"Museum"},
                {id:"PerformingArts",name:"Performing Arts"},
                {id:"BowlingAlley",name:"Bowling Alley"},
                {id:"FitnessCenter",name:"Fitness Center"},
                {id:"IceRink",name:"Ice / Curling Rink"},
                {id:"RollerRink",name:"Roller Rink"},
                {id:"SwimmingPool",name:"Swimming Pool"},
                {id:"OtherRecreation",name:"Other Recreation"},
                {id:"Stadium",name:"Stadium"},
                {id:"IndoorArena",name:"Indoor Arena"},
                {id:"RaceTrack",name:"Race Track"},
                {id:"Aquarium",name:"Aquarium"},
                {id:"Bar",name:"Bar"},
                //{id:"Bar",name:"Nightclub"},
                {id:"Casino",name:"Casino"},
                {id:"Zoo",name:"Zoo"},
                {id:"OtherEntertainment",name:"Other Entertainment"},
                {id:"GasStation",name:"Convenience Store with Gas Station"},
                {id:"ConvenienceStore",name:"Convenience Store without Gas Station"},
                {id:"FastFoodRestaurant",name:"Fast Food Restaurant"},
                {id:"Restaurant",name:"Restaurant"},
                {id:"Supermarket",name:"Supermarket"},
                {id:"Retail",name:"Wholesale Club"},
                {id:"FoodSales",name:"Food Sales"},
                {id:"FoodService",name:"Food Service"},
                {id:"AmbulatorySurgicalCenter",name:"Ambulatory Surgical Center"},
                {id:"Hospital",name:"Hospital"},
                {id:"SpecialtyHospital",name:"Specialty Hospital"},
                {id:"MedicalOffice",name:"Medical Office"},
                {id:"OutpatientCenter",name:"Outpatient Rehabilitation Center"},
                {id:"PhysicalTherapyCenter",name:"Physical Therapy Center"},
                {id:"SeniorCare",name:"Senior Care Community"},
                {id:"UrgentCareCenter",name:"Urgent Care Center"},
                {id:"Barracks",name:"Barracks"},
                {id:"Hotel",name:"Hotel"},
                {id:"MultiFamily",name:"Multifamily Housing"},
                {id:"Prison",name:"Prison / Incarceration"},
                {id:"ResidenceHall",name:"Residence Hall"},
                {id:"ResidentialLodging",name:"Other Residential Lodging"},
                {id:"MixedUse",name:"Mixed Use Property"},
                {id:"Office",name:"Office"},
                {id:"VeterinaryOffice",name:"Veterinary Office"},
                {id:"Courthouse",name:"Courthouse"},
                {id:"DrinkingWaterTreatment",name:"Drinking Water Treatment Center"},
                {id:"FireStation",name:"Fire Station"},
                {id:"Library",name:"Library"},
                {id:"PostOffice",name:"Post Office"},
                {id:"PoliceStation",name:"Police Station"},
                {id:"MeetingHall",name:"Meeting Hall"},
                {id:"TransportationTerminal",name:"Transportation Terminal"},
                {id:"WastewaterCenter",name:"Wastewater Treatment Center"},
                {id:"OtherPublicServices",name:"Other Public Services"},
                {id:"WorshipCenter",name:"Worship Facility"},
                {id:"AutoDealership",name:"Automobile Dealership"},
                {id:"EnclosedMall",name:"Enclosed Mall"},
                {id:"StripMall",name:"Strip Mall"},
                {id:"Retail",name:"Retail Store"},
                {id:"DataCenter",name:"Data Center"}, //Data Centers behave very different and require custom script
                {id:"PersonalServices",name:"Personal Services (Health/Beauty, Dry Cleaning, etc.)"},
                {id:"RepairServices",name:"Repair Services (Vehicle, Shoe Locksmith, etc.)"},
                {id:"OtherServices",name:"Other Services"},
                {id:"PowerStation",name:"Energy / Power Station"},
                {id:"OtherUtility",name:"Other Utility Station"},
                {id:"SelfStorageFacility",name:"Self Storage Facility"},
                {id:"Warehouse",name:"Warehouse - UnRefrigerated"},
                {id:"WarehouseRefrigerated",name:"Warehouse - Refrigerated"},
                {id:"Warehouse",name:"Distribution Center"},
                {id:"Parking",name:"Parking"}
                ]
            }
        };

        $scope.propsWithAlgorithms = [
           "DataCenter",
           "Hospital",
           "Hotel",
           "K12School",
           "MedicalOffice",
           "MultiFamily",
           "Office",
           "Parking",
           "ResidenceHall",
           "Retail",
           "SeniorCare",
           "Supermarket",
           "Warehouse",
           "WarehouseRefrigerated",
           "WastewaterCenter",
           "WorshipCenter"
       ];

        $scope.energyProperties = {

            energyType:[
                {id:"grid",name:"Electric (Grid)"},
                {id:"grid",name:"Electric (renewable)"},
                //{id:"onSiteElectricity",name:"Electric (Solar)"},
                //{id:"onSiteElectricity",name:"Electric (Wind)"},
                {id:"naturalGas",name:"Natural Gas"},
                {id:"fuelOil1",name:"Fuel Oil 1"},
                {id:"fuelOil2",name:"Fuel Oil 2"},
                {id:"fuelOil4",name:"Fuel Oil 4"},
                {id:"fuelOil6",name:"Fuel Oil 5,6"},
                {id:"propane",name:"Propane"},
                {id:"kerosene",name:"Kerosene"},
                {id:"steam",name:"District Steam"},
                {id:"hotWater",name:"District Hot Water"},
                {id:"chilledWater",name:"District Chilled Water (Absorption)"},
                {id:"chilledWater",name:"District Chilled Water (Electric)"},
                {id:"chilledWater",name:"District Chilled Water (Engine)"},
                {id:"chilledWater",name:"District Chilled Water (Other)"},
                {id:"wood",name:"Wood"},
                {id:"coke",name:"Coke"},
                {id:"coalA",name:"Coal (Anthracite)"},
                {id:"coalB",name:"Coal (Bituminous) "},
                {id:"diesel",name:"Diesel"},
                {id:"other",name:"Other"}
            ],

            renewableType:[
                {id:"grid",name:"On-Site Solar"},
                {id:"grid",name:"On-Site Wind"},
                {id:"grid",name:"On-Site Other"}
            ],

            soldType:[
                {id:"soldGrid",name:"On-Site Solar"},
                {id:"soldGrid",name:"On-Site Wind"},
                {id:"soldGrid",name:"On-Site Other"}
            ],

            renewableUnits:[
                {id:"kBtu",name:"kBtu",filter_id:"grid"},
                {id:"MBtu",name:"MBtu",filter_id:"grid"},
                {id:"kWh",name:"kWh",filter_id:"grid"},
                {id:"MWh",name:"MWh",filter_id:"grid"},
                {id:"GJ",name:"GJ",filter_id:"grid"},
            ],


            energyUnits: [
                //<!--Electricity - Grid -->
                {id:"kBtu",name:"kBtu",filter_id:"grid"},
                {id:"MBtu",name:"MBtu",filter_id:"grid"},
                {id:"kWh",name:"kWh",filter_id:"grid"},
                {id:"MWh",name:"MWh",filter_id:"grid"},
                {id:"GJ",name:"GJ",filter_id:"grid"},

                //<!--Electricity - Onsite Renewable-->
                {id:"kBtu",name:"kBtu",filter_id:"onSiteElectricity"},
                {id:"MBtu",name:"MBtu",filter_id:"onSiteElectricity"},
                {id:"kWh",name:"kWh",filter_id:"onSiteElectricity"},
                {id:"MWh",name:"MWh",filter_id:"onSiteElectricity"},
                {id:"GJ",name:"GJ",filter_id:"onSiteElectricity"},

                //<!--Natural Gas -->
                {id:"NG Mcf",name:"MCF",filter_id:"naturalGas"},
                {id:"NG kcf",name:"kcf",filter_id:"naturalGas"},
                {id:"NG ccf",name:"ccf",filter_id:"naturalGas"},
                {id:"NG cf",name:"cf",filter_id:"naturalGas"},
                {id:"NGm3",name:"Cubic Meters",filter_id:"naturalGas"},
                {id:"GJ",name:"GJ",filter_id:"naturalGas"},
                {id:"kBtu",name:"kBtu",filter_id:"naturalGas"},
                {id:"MBtu",name:"MBtu",filter_id:"naturalGas"},
                {id:"therms",name:"therms",filter_id:"naturalGas"},

                //<!--Fuel Oil No. 1 -->
                {id:"kBtu",name:"kBtu",filter_id:"fueloil1Unit"},
                {id:"MBtu",name:"MBtu ",filter_id:"fueloil1Unit"},
                {id:"GJ",name:"GJ",filter_id:"fueloil1Unit"},
                {id:"No1 igal",name:"Gallons (UK)",filter_id:"fueloil1Unit"},
                {id:"No1 gal",name:"Gallons",filter_id:"fueloil1Unit"},
                {id:"No1 L",name:"Liters",filter_id:"fueloil1Unit"},

                //<!--Fuel Oil No. 2 -->
                {id:"kBtu",name:"kBtu",filter_id:"fueloil2Unit"},
                {id:"MBtu",name:"MBtu",filter_id:"fueloil2Unit"},
                {id:"GJ",name:"GJ",filter_id:"fueloil2Unit"},
                {id:"No2 igal",name:"Gallons (UK)",filter_id:"fueloil2Unit"},
                {id:"No2 gal",name:"Gallons",filter_id:"fueloil2Unit"},
                {id:"No2 L",name:"Liters",filter_id:"fueloil2Unit"},

                //<!--Fuel Oil No. 4 -->
                {id:"kBtu",name:"kBtu",filter_id:"fueloil4Unit"},
                {id:"MBtu",name:"MBtu",filter_id:"fueloil4Unit"},
                {id:"GJ",name:"GJ",filter_id:"fueloil4Unit"},
                {id:"No4 igal",name:"Gallons (UK)",filter_id:"fueloil4Unit"},
                {id:"No4 gal",name:"Gallons",filter_id:"fueloil4Unit"},
                {id:"No4 L",name:"Liters",filter_id:"fueloil4Unit"},

                //<!--Fuel Oil No. 5,6 -->
                {id:"kBtu",name:"kBtu",filter_id:"fueloil6Unit"},
                {id:"MBtu",name:"MBtu",filter_id:"fueloil6Unit"},
                {id:"GJ",name:"GJ",filter_id:"fueloil6Unit"},
                {id:"No6 igal",name:"Gallons (UK)",filter_id:"fueloil6Unit"},
                {id:"No6 gal",name:"Gallons",filter_id:"fueloil6Unit"},
                {id:"No6 L",name:"Liters",filter_id:"fueloil6Unit"},

                //<!--Diesel-->
                {id:"kBtu",name:"kBtu",filter_id:"diesel"},
                {id:"MBtu",name:"MBtu",filter_id:"diesel"},
                {id:"GJ",name:"GJ",filter_id:"diesel"},
                {id:"Diesel igal",name:"Gallons (UK)",filter_id:"diesel"},
                {id:"Diesel gal",name:"Gallons",filter_id:"diesel"},
                {id:"Diesel L",name:"Liters",filter_id:"diesel"},

                //<!--Kerosene-->
                {id:"kBtu",name:"kBtu",filter_id:"kerosene"},
                {id:"MBtu",name:"MBtu",filter_id:"kerosene"},
                {id:"GJ",name:"GJ",filter_id:"kerosene"},
                {id:"Kerosene igal",name:"Gallons (UK)",filter_id:"kerosene"},
                {id:"Kerosene gal",name:"Gallons",filter_id:"kerosene"},
                {id:"Kerosene L",name:"Liters",filter_id:"kerosene"},

                //<!--Propane-->
                {id:"GJ",name:"GJ",filter_id:"propane"},
                {id:"kBtu",name:"kBtu",filter_id:"propane"},
                {id:"MBtu",name:"MBtu",filter_id:"propane"},
                {id:"Propane igal",name:"Gallons (UK)",filter_id:"propane"},
                {id:"Propane gal",name:"Gallons",filter_id:"propane"},
                {id:"Propane cf",name:"kcf",filter_id:"propane"},
                {id:"Propane ccf",name:"ccf",filter_id:"propane"},
                {id:"Propane kcf",name:"cf",filter_id:"propane"},
                {id:"Propane L",name:"Liters",filter_id:"propane"},

                //<!--District Steam-->
                {id:"GJ",name:"GJ",filter_id:"steam"},
                {id:"kBtu",name:"kBtu",filter_id:"steam"},
                {id:"MBtu",name:"MBtu",filter_id:"steam"},
                {id:"therms",name:"therms",filter_id:"steam"},
                {id:"Steam lb",name:"Pounds",filter_id:"steam"},
                {id:"Steam klb",name:"Thousand pounds",filter_id:"steam"},
                {id:"Steam MLb",name:"Million pounds",filter_id:"steam"},

                //<!--District Hot Water-->
                {id:"kBtu",name:"kBtu",filter_id:"hotWater"},
                {id:"MBtu",name:"MBtu",filter_id:"hotWater"},
                {id:"GJ",name:"GJ",filter_id:"hotWater"},
                {id:"therms",name:"therms",filter_id:"hotWater"},

                //<!--District Chilled Water-->
                {id:"kBtu",name:"kBtu",filter_id:"chilledWater"},
                {id:"MBtu",name:"MBtu",filter_id:"chilledWater"},
                {id:"GJ",name:"GJ",filter_id:"chilledWater"},
                {id:"CHW TonH",name:"Ton Hours",filter_id:"chilledWater"},

                //<!--Coal (Anthracite)-->
                {id:"kBtu",name:"kBtu",filter_id:"coalA"},
                {id:"MBtu",name:"MBtu",filter_id:"coalA"},
                {id:"GJ",name:"GJ",filter_id:"coalA"},
                {id:"CoalA ton",name:"Tons",filter_id:"coalA"},
                {id:"CoalA tonne",name:"Tonnes (Metric)",filter_id:"coalA"},
                {id:"CoalA lb",name:"Pounds",filter_id:"coalA"},
                {id:"CoalA klb",name:"Thousand Pounds",filter_id:"coalA"},
                {id:"CoalA MLb",name:"Million Pounds",filter_id:"coalA"},

                //<!--Coal (Bituminous)-->
                {id:"kBtu",name:"kBtu",filter_id:"coalB"},
                {id:"MBtu",name:"MBtu",filter_id:"coalB"},
                {id:"GJ",name:"GJ",filter_id:"coalB"},
                {id:"CoalBit ton",name:"Tons",filter_id:"coalB"},
                {id:"CoalBit tonne",name:"Tonnes (Metric)",filter_id:"coalB"},
                {id:"CoalBit lb",name:"Pounds",filter_id:"coalB"},
                {id:"CoalBit klb",name:"Thousand Pounds",filter_id:"coalB"},
                {id:"CoalBit MLb",name:"Million Pounds",filter_id:"coalB"},

                //<!--Coke-->
                {id:"kBtu",name:"kBtu",filter_id:"coke"},
                {id:"MBtu",name:"MBtu",filter_id:"coke"},
                {id:"GJ",name:"GJ",filter_id:"coke"},
                {id:"Coke ton",name:"Tons",filter_id:"coke"},
                {id:"Coke tonne",name:"Tonnes (Metric)",filter_id:"coke"},
                {id:"Coke lb",name:"Pounds",filter_id:"coke"},
                {id:"Coke kLb",name:"Thousand Pounds",filter_id:"coke"},
                {id:"Coke MLb",name:"Million Pounds",filter_id:"coke"},

                //<!--Wood-->
                {id:"kBtu",name:"kBtu",filter_id:"wood"},
                {id:"MBtu",name:"MBtu",filter_id:"wood"},
                {id:"GJ",name:"GJ",filter_id:"wood"},
                {id:"Wood ton",name:"Tons",filter_id:"wood"},
                {id:"Wood tonne",name:"Tonnes (Metric)",filter_id:"wood"},

                //<!--Other-->
                {id:"kBtu",name:"kBtu",filter_id:"other"},
                {id:"GJ",name:"GJ",filter_id:"other"}
                ]
        };


  };
  DashboardCtrl.$inject = ['$rootScope', '$scope', '$window','$sce','$timeout', '$q', '$log', 'benchmarkServices'];
  return {
    DashboardCtrl: DashboardCtrl,
    RootCtrl: RootCtrl    

  };
});
