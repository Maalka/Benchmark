/**
 * A building property directive
 * changes form inputs based on property type
 * and supports multiple property types
 */
define(['angular','./main'], function(angular) {
  'use strict';

  var mod = angular.module('common.directives');

    mod.directive('buildingInfo', [function() {
        return {
            restrict: 'A',
            scope: {
            model: '=model',
            baselineForm: '=baselineForm'
            },

            templateUrl: function(elem, attr){
                   return '/assets/javascripts/common/partials/'+attr.type+'.html';
            },

            controller: ["$scope", function ($scope) {

                $scope.propFieldsRequired = false;

                $scope.propertyModel = {};
                $scope.model.propertyModel = $scope.propertyModel ;
                $scope.model.propertyModel.buildingType = $scope.model.type;

                $scope.model.valid = false;

                $scope.$watch("baselineForm.$valid", function (validity) {
                    $scope.model.valid = validity;
                });

                //auto-populate default parameters by building type and country
                $scope.$watch("propertyModel.defaultValues", function () {

                    if($scope.propertyModel.defaultValues){
                        if( $scope.baselineForm.GFA.$valid && $scope.baselineForm.areaUnits.$valid) {
                            $scope.setDefaults();
                        } else {
                            if( $scope.baselineForm.GFA.$error.required ){
                                $scope.GFARequired = true;
                            }
                            $scope.propertyModel.defaultValues = false;
                        }
                    } else {
                        $scope.clearParams();
                    }

                });

                $scope.$watch("propertyModel.GFA", function () {

                    if($scope.baselineForm.GFA && $scope.baselineForm.areaUnits){
                        $scope.GFARequired = false;
                        if($scope.propertyModel.defaultValues){$scope.setDefaults();}
                    }
                });
                $scope.$watch("propertyModel.areaUnits", function () {
                    if($scope.baselineForm.GFA && $scope.baselineForm.areaUnits){
                        $scope.GFARequired = false;
                        if($scope.propertyModel.defaultValues===true){$scope.setDefaults();}
                    }
                });

                $scope.$watch("baselineForm.$valid", function () {
                    if($scope.baselineForm.$valid){
                        $scope.propFieldsRequired = false;
                    } else {
                        $scope.propFieldsRequired = true;
                    }
                });


            //for setting defaults based on building Type, Country does not matter here due to unused parameters upon analysis
                $scope.setDefaults = function() {

                        var prop = $scope.model.type;
                        var GFA = $scope.propertyModel.GFA;
                        var areaUnits = $scope.propertyModel.areaUnits;

                        if(areaUnits==="mSQ"){GFA = GFA*10.7639;}

                        if(prop==="Hospital"){
                            $scope.propertyModel.licensedBedCapacity = 0.69*GFA/1000;
                            $scope.propertyModel.numStaffedBeds = 0.46*GFA/1000;
                            $scope.propertyModel.numFTEWorkers = 2.6*GFA/1000;
                            $scope.propertyModel.numWorkersMainShift = 1.32*GFA/1000;
                            $scope.propertyModel.numMRIMachines = 1;
                            $scope.propertyModel.hasLaundryFacility = true;
                            $scope.propertyModel.percentHeated =  $scope.buildingProperties.areaHVAC[10].id;
                            $scope.propertyModel.percentCooled =  $scope.buildingProperties.areaHVAC[10].id;
                        } else
                        if(prop==="Hotel"){
                            $scope.propertyModel.numBedrooms = 1.95*GFA/1000;
                            $scope.propertyModel.numWorkersMainShift = 0.32*GFA/1000;
                            $scope.propertyModel.hasFoodPreparation = false;
                            $scope.propertyModel.numRefrUnits = 0.023*GFA/1000;
                            $scope.propertyModel.percentHeated =  $scope.buildingProperties.areaHVAC[10].id;
                            $scope.propertyModel.percentCooled =  $scope.buildingProperties.areaHVAC[10].id;
                        } else
                        if(prop==="K12School"){
                            $scope.propertyModel.gymFloorArea = 0.0;
                            $scope.propertyModel.isHighSchool = false;
                            $scope.propertyModel.numWorkersMainShift = 0.77*GFA/1000;
                            $scope.propertyModel.studentSeatingCapacity = 10.0*GFA/1000;
                            $scope.propertyModel.numWalkinRefrUnits = 0.01*GFA/1000;
                            $scope.propertyModel.isOpenWeekends = false;
                            $scope.propertyModel.hasCooking = true;
                            $scope.propertyModel.numComputers = 1.75*GFA/1000;
                            $scope.propertyModel.percentHeated =  $scope.buildingProperties.areaHVAC[10].id;
                            $scope.propertyModel.percentCooled =  $scope.buildingProperties.areaHVAC[10].id;
                        } else
                        if(prop==="MedicalOffice"){
                            $scope.propertyModel.weeklyOperatingHours = 65;
                            $scope.propertyModel.numWorkersMainShift = 2.2*GFA/1000;
                            $scope.propertyModel.percentHeated =  $scope.buildingProperties.areaHVAC[10].id;
                            $scope.propertyModel.percentCooled =  $scope.buildingProperties.areaHVAC[10].id;
                        } else
                        if(prop==="MultiFamily"){
                            $scope.propertyModel.numRezUnits = 1.2*GFA/1000;
                            $scope.propertyModel.numUnitsLowRise1to4 = 1.2*GFA/1000;
                            $scope.propertyModel.numUnitsMidRise5to9 = 0.0;
                            $scope.propertyModel.numUnitsHighRise10plus = 0.0;
                            $scope.propertyModel.numBedrooms = 1.4*GFA/1000;
                        } else
                        if(prop==="Office"){
                            $scope.propertyModel.weeklyOperatingHours = 65;
                            $scope.propertyModel.numWorkersMainShift = 2.3*GFA/1000;
                            $scope.propertyModel.numComputers = 2*GFA/1000;
                            $scope.propertyModel.percentHeated =  $scope.buildingProperties.areaHVAC[10].id;
                            $scope.propertyModel.percentCooled =  $scope.buildingProperties.areaHVAC[10].id;
                        } else
                        if(prop==="ResidenceHall"){
                            $scope.propertyModel.numBedrooms = 100;
                            $scope.propertyModel.percentHeated =  $scope.buildingProperties.areaHVAC[10].id;
                            $scope.propertyModel.percentCooled =  $scope.buildingProperties.areaHVAC[10].id;
                        } else
                        if(prop==="Retail"){
                            $scope.propertyModel.weeklyOperatingHours = 65;
                            $scope.propertyModel.numOpenClosedRefrCases = 0.0;
                            $scope.propertyModel.numCashRegisters = 0.3*GFA/1000;
                            $scope.propertyModel.numWorkersMainShift = 1.0*GFA/1000;
                            $scope.propertyModel.numComputers = 0.2*GFA/1000;
                            $scope.propertyModel.numRefrUnits = 0.0;
                            $scope.propertyModel.numWalkinRefrUnits = 0.0;
                            $scope.propertyModel.percentHeated =  $scope.buildingProperties.areaHVAC[10].id;
                            $scope.propertyModel.percentCooled =  $scope.buildingProperties.areaHVAC[10].id;
                        } else
                        if(prop==="SeniorCare"){
                            $scope.propertyModel.maxNumResidents = 2.374*GFA/1000;
                            $scope.propertyModel.avgNumResidents = 2.075*GFA/1000;
                            $scope.propertyModel.numRezUnits = 1.584*GFA/1000;
                            $scope.propertyModel.numWorkersMainShift = 0.9523*GFA/1000;
                            $scope.propertyModel.numComputers = 0.367*GFA/1000;
                            $scope.propertyModel.numRezWashingMachines = 0.05757*GFA/1000;
                            $scope.propertyModel.numCommWashingMachines = 0.04422*GFA/1000;
                            $scope.propertyModel.numElectronicLifts = 0.0704*GFA/1000;
                            $scope.propertyModel.numRefrUnits = 0.09065*GFA/1000;
                            $scope.propertyModel.percentHeated =  $scope.buildingProperties.areaHVAC[10].id;
                            $scope.propertyModel.percentCooled =  $scope.buildingProperties.areaHVAC[10].id;
                        } else
                        if(prop==="Supermarket"){
                            $scope.propertyModel.weeklyOperatingHours = 105;
                            $scope.propertyModel.numWorkersMainShift = 1.0*GFA/1000;
                            $scope.propertyModel.numCashRegisters = 0.38*GFA/1000;
                            $scope.propertyModel.numComputers = 0.51*GFA/1000;
                            $scope.propertyModel.lengthRefrFoodDisplayCases = 2.6*GFA/1000;
                            $scope.propertyModel.numRefrUnits = 0.25*GFA/1000;
                            $scope.propertyModel.hasCooking = true;
                            $scope.propertyModel.percentHeated =  $scope.buildingProperties.areaHVAC[10].id;
                            $scope.propertyModel.percentCooled =  $scope.buildingProperties.areaHVAC[10].id;
                        } else
                        if(prop==="WastewaterCenter"){
                            $scope.propertyModel.wastewaterInfluentBiologicalOxygenDemand = 200.0;
                            $scope.propertyModel.wastewaterEffluentBiologicalOxygenDemand = 8.0;
                            $scope.propertyModel.wastewaterHasTrickleFiltration = false;
                            $scope.propertyModel.wastewaterHasNutrientRemoval = false;
                        } else
                        if(prop==="WorshipCenter"){
                            $scope.propertyModel.weeklyOperatingHours = 33;
                            $scope.propertyModel.isOpenAllWeekdays = false;
                            $scope.propertyModel.seatingCapacity = 40.0*GFA/1000;
                            $scope.propertyModel.numComputers = 0.2*GFA/1000;
                            $scope.propertyModel.hasFoodPreparation = false;
                            $scope.propertyModel.numRefrUnits = 0.018*GFA/1000;
                            $scope.propertyModel.numWalkinRefrUnits = 0.0;
                            $scope.propertyModel.percentHeated =  $scope.buildingProperties.areaHVAC[10].id;
                            $scope.propertyModel.percentCooled =  $scope.buildingProperties.areaHVAC[10].id;
                        } else
                        if(prop==="Warehouse"){
                            $scope.propertyModel.weeklyOperatingHours = 60;
                            $scope.propertyModel.numWorkersMainShift = 0.59*GFA/1000;
                            $scope.propertyModel.numWalkinRefrUnits = 0.0;
                            $scope.propertyModel.percentHeated =  $scope.buildingProperties.areaHVAC[5].id;
                            $scope.propertyModel.percentCooled =  $scope.buildingProperties.areaHVAC[2].id;
                        }

                };

                $scope.buildingProperties = {
                    areaUnits: [
                            {id:"ftSQ",name:"sq.ft"},
                            {id:"mSQ",name:"sq.m"}
                    ],
                    areaHVAC: [
                            {id:0,name:"0%"},
                            {id:10,name:"10%"},
                            {id:20,name:"20%"},
                            {id:30,name:"30%"},
                            {id:40,name:"40%"},
                            {id:50,name:"50%"},
                            {id:60,name:"60%"},
                            {id:70,name:"70%"},
                            {id:80,name:"80%"},
                            {id:90,name:"90%"},
                            {id:100,name:"100%"}
                   ],
                   poolType: [
                            {id:"Recreational",name:"Recreational"},
                            {id:"ShortCourse",name:"Short Course"},
                            {id:"Olympic",name:"Olympic)"}
                           ],

                   poolInOut: [
                            {id:"Indoor",name:"Indoor"},
                            {id:"Outdoor",name:"Outdoor"}
                   ]
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
                       "WastewaterCenter",
                       "WorshipCenter"
               ];



                $scope.clearParams = function(){

                    $scope.propertyModel.isOpenAllWeekdays=false;
                    $scope.propertyModel.isWarehouseRefrigerated=false;
                    $scope.propertyModel.hasCooking=false;
                    $scope.propertyModel.isSmallBank=false;
                    $scope.propertyModel.isSecondarySchool=false;
                    $scope.propertyModel.isHighSchool=false;
                    $scope.propertyModel.hasPool=false;
                    $scope.propertyModel.hasParking=false;
                    $scope.propertyModel.indoorOutdoor=false;
                    $scope.propertyModel.isOutdoorPool=false;
                    $scope.propertyModel.isOpenWeekends=false;
                    $scope.propertyModel.hasLaundryFacility=false;
                    $scope.propertyModel.wastewaterHasTrickleFiltration=false;
                    $scope.propertyModel.wastewaterHasNutrientRemoval=false;
                    $scope.propertyModel.hasParkingHeating=false;

                    $scope.propertyModel.percentHeated=null;
                    $scope.propertyModel.percentCooled=null;
                    $scope.propertyModel.annualITEnergy=null;
                    $scope.propertyModel.seatingCapacity=null;
                    $scope.propertyModel.weeklyOperatingHours=null;
                    $scope.propertyModel.numComputers=null;
                    $scope.propertyModel.numServers=null;
                    $scope.propertyModel.hasFoodPreparation=null;
                    $scope.propertyModel.numRefrUnits=null;
                    $scope.propertyModel.numWorkersMainShift=null;
                    $scope.propertyModel.numWalkinRefrUnits=null;
                    $scope.propertyModel.numCashRegisters=null;
                    $scope.propertyModel.lengthRefrFoodDisplayCases=null;
                    $scope.propertyModel.avgNumResidents=null;
                    $scope.propertyModel.maxNumResidents=null;
                    $scope.propertyModel.numRezUnits=null;
                    $scope.propertyModel.numElectronicLifts=null;
                    $scope.propertyModel.numCommWashingMachines=null;
                    $scope.propertyModel.numRezWashingMachines=null;
                    $scope.propertyModel.numOpenClosedRefrCases=null;
                    $scope.propertyModel.numBedrooms=null;
                    $scope.propertyModel.numUnitsLowRise1to4=null;
                    $scope.propertyModel.numUnitsMidRise5to9=null;
                    $scope.propertyModel.numUnitsHighRise10plus=null;
                    $scope.propertyModel.gymFloorArea=null;
                    $scope.propertyModel.studentSeatingCapacity=null;
                    $scope.propertyModel.numStaffedBeds=null;
                    $scope.propertyModel.numMRIMachines=null;
                    $scope.propertyModel.numFTEWorkers=null;
                    $scope.propertyModel.licensedBedCapacity=null;
                    $scope.propertyModel.poolType=null;
                    $scope.propertyModel.parkingAreaUnits=null;
                    $scope.propertyModel.openParkingArea=null;
                    $scope.propertyModel.partiallyEnclosedParkingArea=null;
                    $scope.propertyModel.fullyEnclosedParkingArea=null;
                    $scope.propertyModel.wastewaterAvgInfluentInflow=null;
                    $scope.propertyModel.wastewaterLoadFactor=null;
                    $scope.propertyModel.wastewaterInfluentBiologicalOxygenDemand=null;
                    $scope.propertyModel.wastewaterEffluentBiologicalOxygenDemand=null;
                    $scope.propertyModel.wastewaterPlantDesignFlowRate=null;
                };
            }]
        };
    }]);



  return mod;
});


