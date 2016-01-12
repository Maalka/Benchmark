/**
 * Dashboard controllers.
 */
//define(["./test/sample_response_test_data"], function(sampleTestData) {
define([], function() {
  'use strict';
  var DashboardCtrl = function($rootScope, $scope, benchmarkServices) {

    $rootScope.pageTitle = "2030 Baseline";
    //The model that will be submitted for analysis
    $scope.propertyModel = {};
    //For displaying anything related to building types for which Energy Star Score can be Calculated
    $scope.hasAlgo = false;
    //The table of energy information input by user, default to empty
    $scope.energies = [];
    //For displaying user-input energy entries after having been saved
    $scope.showEnergyTable = false;

    // for testing purposes, display a sample result
    $scope.showTestResponse = true;
    $scope.benchmarkResult = null;

    //For Delayed Error Pointing After Submit Button Click (ugly if done on watch, though watch is good for removing errors)
    $scope.cityRequired = false;
    $scope.postalCodeRequired = false;
    $scope.countryRequired = false;
    $scope.stateRequired = false;

    $scope.buildingTypeRequired = false;
    $scope.GFARequired = false;
    $scope.areaUnitsRequired = false;


    $scope.energyTypeRequired = false;
    $scope.energyUnitsRequired = false;
    $scope.energyUseRequired = false;

    $scope.requireDataCenter = false;
    $scope.requireHospital = false;
    $scope.requireHospitalCanada = false;
    $scope.requireHotel = false;
    $scope.requireK12School = false;
    $scope.requireK12SchoolCanada = false;
    $scope.requireMedicalOffice = false;
    $scope.requireMedicalOfficeCanada = false;
    $scope.requireMultiFamily = false;
    $scope.requireOffice = false;
    $scope.requireOfficeCanada = false;
    $scope.requireParking = false;
    $scope.requirePool = false;
    $scope.requireResidenceHall = false;
    $scope.requireRetail = false;
    $scope.requireSeniorCare = false;
    $scope.requireSupermarket = false;
    $scope.requireSupermarketCanada = false;
    $scope.requireWarehouse = false;
    $scope.requireWastewaterCenter = false;
    $scope.requireWorshipCenter = false;

    $scope.nextProp = false;
    $scope.showPropTable = false;


    //watch on buildingType dropdown, clears parameters on change and opens additional inputs for building types for which
    // energystar scores can be calculated via Algorithm
    $scope.$watch("propertyModel.buildingType", function (v) {
        if($scope.baselineForm.buildingType.$valid){$scope.buildingTypeRequired = false;}
        if(($scope.propertyModel.country) && (v)){
            $scope.hasAlgo = ($scope.propsWithAlgorithms.indexOf(v) > -1);
            $scope.getProp();
            $scope.defaultValues = false;
            $scope.clearParams();}
    });

    //watch on GFA, remove errors if user input positive number
    $scope.$watch("propertyModel.GFA", function () {
        if($scope.baselineForm.GFA.$valid){$scope.GFARequired = false;}
    });

    //watch on areaUnits dropdown, remove errors upon user selection
    $scope.$watch("propertyModel.areaUnits", function () {
        if($scope.baselineForm.areaUnits.$valid){$scope.areaUnitsRequired = false;}
    });

    //watch on city text input, remove errors upon user text input
    $scope.$watch("propertyModel.city", function () {
        if($scope.baselineForm.city.$valid){$scope.cityRequired = false;}
    });

    //watch on postalCode, remove errors if user input positive number
    $scope.$watch("propertyModel.postalCode", function () {
        if($scope.baselineForm.postalCode.$valid){$scope.postalCodeRequired = false;}
    });

    //watch on state dropdown, remove errors upon user selection
    $scope.$watch("propertyModel.state", function () {
        if($scope.baselineForm.state.$valid){$scope.stateRequired = false;}
    });

    //watch on country dropdown, combined with buildingType dropdown to decide what building related
    //user inputs to display in order to calculate Energy Star Score related variables
    $scope.$watch("propertyModel.country", function (v) {
        if($scope.baselineForm.country.$valid){$scope.countryRequired = false;}
        if((v)&&($scope.propertyModel.buildingType)){
            $scope.hasAlgo = ($scope.propsWithAlgorithms.indexOf($scope.propertyModel.buildingType) > -1);
            $scope.getProp();
            $scope.defaultValues = false;
            $scope.clearParams();}
    });

    //watch on energyType dropdown, remove errors upon user selection
    $scope.$watch("propertyModel.energies.energyType", function (v) {
        if(v){$scope.energyTypeRequired = false;}
    });
    //watch on energyUnits dropdown, remove errors upon user selection
    $scope.$watch("propertyModel.energies.energyUnits", function (v) {
        if(v){$scope.energyUnitsRequired = false;}
    });
    //watch on energyUse input text, remove errors if user input positive number (required)
    $scope.$watch("propertyModel.energies.energyUse", function (v) {
        if(v){$scope.energyUseRequired = false;}
    });

    //auto-populate default parameters by building type and country
    $scope.$watch("defaultValues", function () {
        if($scope.baselineForm.GFA.$valid && $scope.baselineForm.areaUnits.$valid){
            $scope.setDefaults();
            } else {
            if($scope.baselineForm.GFA.$error.required){$scope.GFARequired = true;}
            if($scope.baselineForm.areaUnits.$error.required){$scope.areaUnitsRequired = true;}
            $scope.defaultValues = false;
            }
        });

    //populate user-input energy information table to calculate site/source EUI and Energy Star metrics
    //display errors when present
    $scope.addEnergiesRow = function(){

    if(!$scope.propertyModel.energies) {$scope.propertyModel.energies = {};}

    if($scope.energies===null){$scope.energies=[];}
    if(!$scope.propertyModel.energies.energyType){$scope.energyTypeRequired = true;}
    if(!$scope.propertyModel.energies.energyUnits){$scope.energyUnitsRequired = true;}
    if(!$scope.propertyModel.energies.energyUse){$scope.energyUseRequired = true;}

    if ($scope.baselineForm.energyType.$valid && $scope.propertyModel.energies.energyType &&
        $scope.baselineForm.energyUnits.$valid && $scope.propertyModel.energies.energyUnits &&
        $scope.baselineForm.energyUse.$valid && $scope.propertyModel.energies.energyUse &&
        $scope.baselineForm.energyRate.$valid) {

        $scope.energies.push({'energyType':$scope.propertyModel.energies.energyType,
                                'energyUnits': $scope.propertyModel.energies.energyUnits,
                                'energyUse':$scope.propertyModel.energies.energyUse,
                                'energyRate':$scope.propertyModel.energies.energyRate || null});

        $scope.propertyModel.energies.energyType=null;
        $scope.propertyModel.energies.energyUnits=null;
        $scope.propertyModel.energies.energyUse=null;
        $scope.propertyModel.energies.energyRate=null;
        $scope.showEnergyTable = true;

        }
    };

    $scope.removeRow = function(energyType, energyUnits, energyUse){
        var index = -1;
        var energyArr = $scope.energies;
        for( var i = 0; i < energyArr.length; i++ ) {
            if(energyArr[i].energyType === energyType && energyArr[i].energyUnits === energyUnits && energyArr[i].energyUse === energyUse) {
                index = i;
                break;
            }
        }
        if( index === -1 ) {
            window.alert( "Error" );
        }
        $scope.energies.splice(index, 1);
        if ($scope.energies.length === 0){$scope.showEnergyTable = false;}
    };


    //for setting defaults based on building Type, Country does not matter here due to unused parameters upon analysis
    $scope.setDefaults = function() {
        if($scope.baselineForm.GFA.$valid &&
            $scope.baselineForm.buildingType.$valid &&
            $scope.baselineForm.areaUnits.$valid){

            var prop = $scope.propertyModel.buildingType;
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
                $scope.propertyModel.numWalkinRefrUnits = 0.25*GFA/1000;
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
    }};

    //for displaying the relevant fields by building type and country
    $scope.getProp = function() {
            var v = $scope.propertyModel.buildingType;
            var country = $scope.propertyModel.country;
            if(v){
                $scope.reset();
                $scope.nextProp = true;
                switch(v) {
                    case "DataCenter":
                        $scope.requireDataCenter = true;
                        break;
                    case "Hospital":
                        if(country===null || country!=="Canada"){
                                $scope.requireHospital = true;
                        }
                         else if(country==="Canada"){
                                $scope.requireHospitalCanada = true;
                        }
                        break;
                    case "Hotel":
                        $scope.requireHotel = true;
                        break;
                    case "K12School":
                        if(country===null || country!=="Canada"){
                                $scope.requireK12School = true;
                        }
                         else if(country==="Canada"){
                                $scope.requireK12SchoolCanada = true;
                        }
                        break;
                    case "MedicalOffice":
                        if(country===null || country!=="Canada"){
                                $scope.requireMedicalOffice = true;
                        }
                         else if($scope.propertyModel.country==="Canada"){
                                $scope.requireMedicalOfficeCanada = true;
                        }
                        break;
                    case "MultiFamily":
                        $scope.requireMultiFamily = true;
                        break;
                    case "ResidenceHall":
                        $scope.requireResidenceHall = true;
                        break;
                    case "Retail":
                        $scope.requireRetail = true;
                        break;
                    case "SeniorCare":
                        $scope.requireSeniorCare = true;
                        break;
                    case "Supermarket":
                        if(country===null || country!=="Canada"){
                            $scope.requireSupermarket = true;
                        }
                        else if(country==="Canada"){
                            $scope.requireSupermarketCanada = true;
                        }
                        break;
                    case "Warehouse":
                        $scope.requireWarehouse = true;
                        break;
                    case "WastewaterCenter":
                        $scope.requireWastewaterCenter = true;
                        break;
                    case "Office":
                        if(country===null || country!=="Canada"){
                                $scope.requireOffice = true;
                            }
                            else if(country==="Canada"){
                                $scope.requireOfficeCanada = true;
                            }
                        break;
                    case "WorshipCenter":
                        $scope.requireWorshipCenter = true;
                        break;
                }}
                else {
                    $scope.addProp = false;
                    $scope.reset();
                }
            };

    $scope.reset = function() {
        $scope.requireDataCenter = false;
        $scope.requireHospital = false;
        $scope.requireHospitalCanada = false;
        $scope.requireHotel = false;
        $scope.requireK12School = false;
        $scope.requireK12SchoolCanada = false;
        $scope.requireMedicalOffice = false;
        $scope.requireMedicalOfficeCanada = false;
        $scope.requireMultiFamily = false;
        $scope.requireOffice = false;
        $scope.requireOfficeCanada = false;
        $scope.requireParking = false;
        $scope.requirePool = false;
        $scope.requireResidenceHall = false;
        $scope.requireRetail = false;
        $scope.requireSeniorCare = false;
        $scope.requireSupermarket = false;
        $scope.requireSupermarketCanada = false;
        $scope.requireWarehouse = false;
        $scope.requireWastewaterCenter = false;
        $scope.requireWorshipCenter = false;

        $scope.nextProp = false;

    };

    $scope.addProperty = function(){


        if(!$scope.propList) {$scope.propList = [];}


        if($scope.baselineForm.$valid){


            var tempProp = $scope.createProp();
            $scope.propList.push(tempProp);
            $scope.showPropTable = true;

            console.log($scope.propList);

            $scope.propertyModel.buildingType = null;
            $scope.propertyModel.areaUnits = null;
            $scope.defaultValues = false;
            $scope.reset();
            $scope.clearParams();

         } else {$scope.submitErrors();}
    };

    $scope.removeProp = function(propType){

        var index = -1;
        var propArr = $scope.propList;

        for( var i = 0; i < propArr.length; i++ ) {
            window.alert(propArr[i].buildingType);
            if(propArr[i].buildingType === propType) {

                index = i;
                break;
            }
        }
        if( index === -1 ) {
            window.alert( "Error" );
        }
        $scope.propList.splice(index, 1);
        if ($scope.propList.length === 0){$scope.showPropTable = false;}
    };

     $scope.createProp = function(){
     return {
            'buildingType': $scope.propertyModel.buildingType,
            'GFA':$scope.propertyModel.GFA,
            'areaUnits':$scope.propertyModel.areaUnits
            };};


    $scope.clearParams = function() {
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

            $scope.propertyModel.numBuildingsPartSingleMore=null;
            $scope.propertyModel.HDD=null;
            $scope.propertyModel.HDDbase40=null;
            $scope.propertyModel.CDD=null;
            $scope.propertyModel.percentHeated=null;
            $scope.propertyModel.percentCooled=null;
            $scope.propertyModel.annualITEnergy=null;
            $scope.propertyModel.GFA=null;
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
            "WorshipCenter"];

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
        areaUnits: [
                {id:"ftSQ",name:"Square Feet (sq. ft)"},
                {id:"mSQ",name:"Square Meters (sq. m)"}
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
                {id:100,name:"All of it: 100%"}
        ],

        poolType: [
                        {id:"Recreational",name:"Recreational"},
                        {id:"ShortCourse",name:"Short Course"},
                        {id:"Olympic",name:"Olympic)"}
                ],

        poolInOut: [
                            {id:"Indoor",name:"Indoor"},
                            {id:"Outdoor",name:"Outdoor"}
                    ],

        buildingType:[
                {id:"Office",name:"Bank Branch"},
                {id:"Office",name:"Financial Office"},
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
                {id:"Bar",name:"Nightclub"},
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
                {id:"Warehouse",name:"Warehouse / Distribution Center"}]
                };

    $scope.energyProperties = {

    energyType:[
                {id:"grid",name:"Electric (Grid)"},
                {id:"onSiteElectricity",name:"Electric (Solar)"},
                {id:"onSiteElectricity",name:"Electric (Wind)"},
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

    energyUnits: [
        //<!--Electricity - Grid -->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"grid"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"grid"},
        {id:"KWh",name:"kWh (thousand Watt-hours)",filter_id:"grid"},
        {id:"MWh",name:"MWh (million Watt-hours)",filter_id:"grid"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"grid"},

        //<!--Electricity - Onsite Renewable-->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"onSiteElectricity"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"onSiteElectricity"},
        {id:"KWh",name:"kWh (thousand Watt-hours)",filter_id:"onSiteElectricity"},
        {id:"MWh",name:"MWh (million Watt-hours)",filter_id:"onSiteElectricity"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"onSiteElectricity"},

        //<!--Natural Gas -->
        {id:"NGMcf",name:"MCF (million cubic feet)",filter_id:"naturalGas"},
        {id:"NGKcf",name:"kcf (thousand cubic feet)",filter_id:"naturalGas"},
        {id:"NGCcf",name:"ccf (hundred cubic feet)",filter_id:"naturalGas"},
        {id:"NGcf",name:"cf (cubic feet)",filter_id:"naturalGas"},
        {id:"NGm3",name:"Cubic Meters",filter_id:"naturalGas"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"naturalGas"},
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"naturalGas"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"naturalGas"},
        {id:"therms",name:"Therms",filter_id:"naturalGas"},

        //<!--Fuel Oil No. 1 -->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"fueloil1Unit"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"fueloil1Unit"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"fueloil1Unit"},
        {id:"No1UKG",name:"Gallons (UK)",filter_id:"fueloil1Unit"},
        {id:"No1USG",name:"Gallons",filter_id:"fueloil1Unit"},
        {id:"No1L",name:"Liters",filter_id:"fueloil1Unit"},

        //<!--Fuel Oil No. 2 -->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"fueloil2Unit"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"fueloil2Unit"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"fueloil2Unit"},
        {id:"No2UKG",name:"Gallons (UK)",filter_id:"fueloil2Unit"},
        {id:"No2USG",name:"Gallons",filter_id:"fueloil2Unit"},
        {id:"No2L",name:"Liters",filter_id:"fueloil2Unit"},

        //<!--Fuel Oil No. 4 -->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"fueloil4Unit"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"fueloil4Unit"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"fueloil4Unit"},
        {id:"No4UKG",name:"Gallons (UK)",filter_id:"fueloil4Unit"},
        {id:"No4USG",name:"Gallons",filter_id:"fueloil4Unit"},
        {id:"No4L",name:"Liters",filter_id:"fueloil4Unit"},

        //<!--Fuel Oil No. 5,6 -->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"fueloil6Unit"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"fueloil6Unit"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"fueloil6Unit"},
        {id:"No6UKG",name:"Gallons (UK)",filter_id:"fueloil6Unit"},
        {id:"No6USG",name:"Gallons",filter_id:"fueloil6Unit"},
        {id:"No6L",name:"Liters",filter_id:"fueloil6Unit"},

        //<!--Diesel-->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"diesel"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"diesel"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"diesel"},
        {id:"DieselUKG",name:"Gallons (UK)",filter_id:"diesel"},
        {id:"DieselUSG",name:"Gallons",filter_id:"diesel"},
        {id:"DieselL",name:"Liters",filter_id:"diesel"},

        //<!--Kerosene-->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"kerosene"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"kerosene"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"kerosene"},
        {id:"KeroseneUKG",name:"Gallons (UK)",filter_id:"kerosene"},
        {id:"KeroseneUSG",name:"Gallons",filter_id:"kerosene"},
        {id:"KeroseneL",name:"Liters",filter_id:"kerosene"},

        //<!--Propane-->
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"propane"},
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"propane"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"propane"},
        {id:"PropaneUKG",name:"Gallons (UK)",filter_id:"propane"},
        {id:"PropaneUSG",name:"Gallons",filter_id:"propane"},
        {id:"PropaneCf",name:"kcf (thousand cubic feet)",filter_id:"propane"},
        {id:"PropaneCCf",name:"ccf (hundred cubic feet)",filter_id:"propane"},
        {id:"PropaneKCf",name:"cf (cubic feet)",filter_id:"propane"},
        {id:"PropaneL",name:"Liters",filter_id:"propane"},

        //<!--District Steam-->
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"steam"},
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"steam"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"steam"},
        {id:"therms",name:"Therms",filter_id:"steam"},
        {id:"SteamLb",name:"Pounds",filter_id:"steam"},
        {id:"SteamKLb",name:"Thousand pounds",filter_id:"steam"},
        {id:"SteamMLb",name:"Million pounds",filter_id:"steam"},

        //<!--District Hot Water-->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"hotWater"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"hotWater"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"hotWater"},
        {id:"therms",name:"Therms",filter_id:"hotWater"},

        //<!--District Chilled Water-->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"chilledWater"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"chilledWater"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"chilledWater"},
        {id:"CHWTonH",name:"Ton Hours",filter_id:"chilledWater"},

        //<!--Coal (Anthracite)-->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"coalA"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"coalA"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"coalA"},
        {id:"CoalATon",name:"Tons",filter_id:"coalA"},
        {id:"CoalATonne",name:"Tonnes (Metric)",filter_id:"coalA"},
        {id:"CoalALb",name:"Pounds",filter_id:"coalA"},
        {id:"CoalAKLb",name:"Thousand Pounds",filter_id:"coalA"},
        {id:"CoalAMLb",name:"Million Pounds",filter_id:"coalA"},

        //<!--Coal (Bituminous)-->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"coalB"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"coalB"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"coalB"},
        {id:"CoalBitTon",name:"Tons",filter_id:"coalB"},
        {id:"CoalBitTonne",name:"Tonnes (Metric)",filter_id:"coalB"},
        {id:"CoalBitLb",name:"Pounds",filter_id:"coalB"},
        {id:"CoalBitKLb",name:"Thousand Pounds",filter_id:"coalB"},
        {id:"CoalBitMLb",name:"Million Pounds",filter_id:"coalB"},

        //<!--Coke-->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"coke"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"coke"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"coke"},
        {id:"CokeTon",name:"Tons",filter_id:"coke"},
        {id:"CokeTonne",name:"Tonnes (Metric)",filter_id:"coke"},
        {id:"CokeLb",name:"Pounds",filter_id:"coke"},
        {id:"CokeKLb",name:"Thousand Pounds",filter_id:"coke"},
        {id:"CokeMLb",name:"Million Pounds",filter_id:"coke"},

        //<!--Wood-->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"wood"},
        {id:"MBtu",name:"MBtu (million Btu)",filter_id:"wood"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"wood"},
        {id:"WoodTon",name:"Tons",filter_id:"wood"},
        {id:"WoodTonne",name:"Tonnes (Metric)",filter_id:"wood"},

        //<!--Other-->
        {id:"KBtu",name:"kBtu (thousand Btu)",filter_id:"other"},
        {id:"GJ",name:"GJ (billion Joules)",filter_id:"other"}
        ]
    };

    $scope.submitErrors = function () {

        if($scope.baselineForm.city.$error.required){$scope.cityRequired = true;}
        if($scope.baselineForm.postalCode.$error.required){$scope.postalCodeRequired = true;}
        if($scope.baselineForm.country.$error.required){$scope.countryRequired = true;}
        if($scope.baselineForm.state.$error.required){$scope.stateRequired = true;}
        if($scope.baselineForm.buildingType.$error.required){$scope.buildingTypeRequired = true;}
        if($scope.baselineForm.GFA.$error.required){$scope.GFARequired = true;}
        if($scope.baselineForm.areaUnits.$error.required){$scope.areaUnitsRequired = true;}
        window.alert("Please check form for errors!");
    };

    $scope.submit = function () {

        if($scope.baselineForm.$valid){
            if($scope.energies.length===0){$scope.propertyModel.energies=null;}
            else {$scope.propertyModel.energies = $scope.energies;}

            benchmarkServices.submit($scope.propertyModel).then(function (response) {
                console.log(response);
                $scope.benchmarkResult = response;
            });
        } else {$scope.submitErrors();}
    };
  };
  DashboardCtrl.$inject = ['$rootScope', '$scope', 'benchmarkServices'];
  return {
    DashboardCtrl: DashboardCtrl
  };
});
