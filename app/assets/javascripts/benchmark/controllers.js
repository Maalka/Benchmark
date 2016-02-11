/**
 * Dashboard controllers.
 */
//define(["./test/sample_response_test_data"], function(sampleTestData) {
define([], function() {
  'use strict';
  var DashboardCtrl = function($rootScope, $scope, $q, benchmarkServices) {

    $rootScope.pageTitle = "2030 Baseline";
    //The model that will be submitted for analysis
    $scope.propertyModel = {};
    //For displaying anything related to building types for which Energy Star Score can be Calculated
    $scope.hasAlgo = false;
    //The table of energy information input by user, default to empty
    $scope.energies = [];
    //For displaying user-input energy entries after having been saved
    $scope.showEnergyTable = false;
    $scope.propList = [];
    $scope.benchmarkResult = null;
    $scope.buildingTypeRequired = false;



    $scope.energyTypeRequired = false;
    $scope.energyUnitsRequired = false;
    $scope.energyUseRequired = false;

    $scope.propTypes = [];

    $scope.propText = "Primary Function of Building";


    $scope.$watch("propertyModel.buildingType", function (v) {

        if($scope.baselineForm.buildingType.$valid){$scope.buildingTypeRequired = false;}
        if(($scope.propertyModel.country) && (v)){
            $scope.hasAlgo = ($scope.propsWithAlgorithms.indexOf(v) > -1);
            $scope.propTypes.push({type:v,country:$scope.propertyModel.country});
            $scope.propText="Add Another Use";
        }
    });


    //watch on country dropdown, combined with buildingType dropdown to decide what building related
    //user inputs to display in order to calculate Energy Star Score related variables
    $scope.$watch("propertyModel.country", function (v) {
        if($scope.baselineForm.country.$valid){$scope.countryRequired = false;}
        if((v)&&($scope.propertyModel.buildingType)){
            $scope.hasAlgo = ($scope.propsWithAlgorithms.indexOf($scope.propertyModel.buildingType) > -1);
            $scope.propTypes.push({type:$scope.propertyModel.buildingType,country:v});
            $scope.propText="Add Another Use";
            }
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
            $scope.baselineForm.energyUse.$valid && $scope.propertyModel.energies.energyUse){
            // && $scope.baselineForm.energyRate.$valid) {

            $scope.energies.push({'energyType':$scope.propertyModel.energies.energyType,
                                    'energyUnits': $scope.propertyModel.energies.energyUnits,
                                    'energyUse':$scope.propertyModel.energies.energyUse,
                                    'energyRate':$scope.propertyModel.energies.energyRate || null});

            $scope.showEnergyTable = true;

        }
    };

    $scope.removeRow = function(energyType, energyUnits, energyUse){
        var index = -1;
        var energyArr = $scope.energies;
        for(var i = 0; i < energyArr.length; i++ ) {
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


    $scope.removeProp = function(prop){
        var index;
        for(var i = 0; i < $scope.propTypes.length; i++ ) {
            if($scope.propTypes[i].type === prop.type && $scope.propTypes[i].country === prop.country) {
                index = i;
                break;
            }
        }
        if( index === -1 ) {
            window.alert( "Error" );
        }
        $scope.propTypes.splice(index, 1);

        if($scope.propTypes.length === 0){
            $scope.propText="Primary Function of Building";
        }
    };

    $scope.createProp = function(){

        if($scope.energies.length===0){$scope.propertyModel.energies=null;}
            else {$scope.propertyModel.energies = $scope.energies;}

        return {
            "name": $scope.propertyModel.name,
            "country": $scope.propertyModel.country ,
            "address1": $scope.propertyModel.address1,
            "address2": $scope.propertyModel.address2,
            "city": $scope.propertyModel.city,
            "state": $scope.propertyModel.state,
            "postalCode": $scope.propertyModel.postalCode,
            "reportingUnits": $scope.propertyModel.reportingUnits,
            "areaUnits": $scope.propertyModel.areaUnits,
            "primaryType": $scope.propertyModel.primaryType,
            "numBuildingsPartSingleMore": $scope.propertyModel.numBuildingsPartSingleMore,
            "energies": $scope.propertyModel.energies ,
            "targetScore": $scope.propertyModel.targetScore,
            "percentBetterThanMedian": $scope.propertyModel.percentBetterThanMedian,
            "buildingType": $scope.propertyModel.buildingType,
            "HDD": $scope.propertyModel.HDD,
            "HDDbase40": $scope.propertyModel.HDDbase40,
            "CDD": $scope.propertyModel.CDD,
            "percentHeated": $scope.propertyModel.percentHeated,
            "percentCooled": $scope.propertyModel.percentCooled,
            "annualITEnergy": $scope.propertyModel.annualITEnergy,
            "GFA": $scope.propertyModel.GFA,
            "seatingCapacity": $scope.propertyModel.seatingCapacity,
            "isOpenAllWeekdays": $scope.propertyModel.isOpenAllWeekdays,
            "weeklyOperatingHours": $scope.propertyModel.weeklyOperatingHours,
            "numComputers": $scope.propertyModel.numComputers,
            "numServers": $scope.propertyModel.numServers,
            "hasFoodPreparation": $scope.propertyModel.hasFoodPreparation,
            "numRefrUnits": $scope.propertyModel.numRefrUnits,
            "numWorkersMainShift": $scope.propertyModel.numWorkersMainShift,
            "isWarehouseRefrigerated": $scope.propertyModel.isWarehouseRefrigerated,
            "numWalkinRefrUnits":$scope.propertyModel.numWalkinRefrUnits,
            "numCashRegisters": $scope.propertyModel.numCashRegisters,
            "lengthRefrFoodDisplayCases": $scope.propertyModel.lengthRefrFoodDisplayCases,
            "hasCooking": $scope.propertyModel.hasCooking,
            "avgNumResidents": $scope.propertyModel.avgNumResidents,
            "maxNumResidents": $scope.propertyModel.maxNumResidents,
            "numRezUnits": $scope.propertyModel.numRezUnits,
            "numElectronicLifts": $scope.propertyModel.numElectronicLifts,
            "numCommWashingMachines": $scope.propertyModel.numCommWashingMachines,
            "numRezWashingMachines": $scope.propertyModel.numRezWashingMachines,
            "numOpenClosedRefrCases": $scope.propertyModel.numOpenClosedRefrCases,
            "isSmallBank": $scope.propertyModel.isSmallBank,
            "numBedrooms": $scope.propertyModel.numBedrooms,
            "numUnitsLowRise1to4": $scope.propertyModel.numUnitsLowRise1to4,
            "numUnitsMidRise5to9": $scope.propertyModel.numUnitsMidRise5to9,
            "numUnitsHighRise10plus": $scope.propertyModel.numUnitsHighRise10plus,
            "gymFloorArea": $scope.propertyModel.gymFloorArea,
            "studentSeatingCapacity": $scope.propertyModel.studentSeatingCapacity,
            "isSecondarySchool": $scope.propertyModel.isSecondarySchool,
            "isHighSchool": $scope.propertyModel.isHighSchool,
            "isOpenWeekends": $scope.propertyModel.isOpenWeekends,
            "numStaffedBeds": $scope.propertyModel.numStaffedBeds,
            "numMRIMachines": $scope.propertyModel.numMRIMachines,
            "numFTEWorkers": $scope.propertyModel.numFTEWorkers,
            "licensedBedCapacity": $scope.propertyModel.licensedBedCapacity,
            "hasLaundryFacility": $scope.propertyModel.hasLaundryFacility,
            "hasPool": $scope.propertyModel.hasPool,
            "hasParking": $scope.propertyModel.hasParking,
            "indoorOutdoor": $scope.propertyModel.indoorOutdoor,
            "isOutdoorPool": $scope.propertyModel.isOutdoorPool,
            "poolType": $scope.propertyModel.poolType,
            "openParkingArea": $scope.propertyModel.openParkingArea,
            "partiallyEnclosedParkingArea": $scope.propertyModel.partiallyEnclosedParkingArea,
            "fullyEnclosedParkingArea": $scope.propertyModel.fullyEnclosedParkingArea,
            "hasParkingHeating": $scope.propertyModel.hasParkingHeating,
            "wastewaterAvgInfluentInflow": $scope.propertyModel.wastewaterAvgInfluentInflow,
            "wastewaterLoadFactor": $scope.propertyModel.wastewaterLoadFactor,
            "wastewaterInfluentBiologicalOxygenDemand": $scope.propertyModel.wastewaterInfluentBiologicalOxygenDemand,
            "wastewaterEffluentBiologicalOxygenDemand": $scope.propertyModel.wastewaterEffluentBiologicalOxygenDemand,
            "wastewaterPlantDesignFlowRate": $scope.propertyModel.wastewaterPlantDesignFlowRate,
            "wastewaterHasTrickleFiltration": $scope.propertyModel.wastewaterHasTrickleFiltration,
            "wastewaterHasNutrientRemoval": $scope.propertyModel.wastewaterHasNutrientRemoval
        };
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

        window.alert("Please check form for errors!");
    };

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
        var maxSourceSiteRatio = sourceSiteRatios[propSizes.indexOf(Math.max.apply(Math, propSizes))];

        if(sourceEnergies.indexOf(undefined) === -1 && siteEnergies.indexOf(undefined) === -1) {
            console.log("Ratio from energy input");
            returnValue = siteEnergies[0] / sourceEnergies[0];
        } else {
            console.log("Ratio from largest prop size");
            returnValue = maxSourceSiteRatio;
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

        var propSizes = results.map(function(o){return $scope.getPropResponseField(o,'buildingSize');});
        var sumPropSize = propSizes.reduce(function(a, b) {return a + b;});
        var propWeightsBySize = propSizes.map(function(o){return o / sumPropSize;});
        var indexLargestProp = propSizes.indexOf(Math.max.apply(Math, propSizes));

        var ESs = results.map(function(o){return $scope.getPropResponseField(o,'ES');});
        var targetESs = results.map(function(o){return $scope.getPropResponseField(o,'targetES');});
        var percentBetterESs = results.map(function(o){return $scope.getPropResponseField(o,'percentBetterES');});

        var sourceEnergies = results.map(function(o){return $scope.getPropResponseField(o,'totalSourceEnergy');});
        var siteEnergies = results.map(function(o){return $scope.getPropResponseField(o,'totalSiteEnergy');});
        var medianEUIs = results.map(function(o){return $scope.getPropResponseField(o,'medianSourceEUI');});
        var percentBetterSourceEUIs = results.map(function(o){return $scope.getPropResponseField(o,'percentBetterSourceEUI');});
        var targetSourceEUIs = results.map(function(o){return $scope.getPropResponseField(o,'targetSourceEUI');});

        var propClasses = results.map(function(o){return $scope.getPropResponseField(o,'buildingClass');});

        var sourceSiteRatios = results.map(function(o){return $scope.getPropResponseField(o,'sourceSiteRatio');});
        var primarySourceSiteRatio = $scope.getPrimarySourceSiteRatio(sourceSiteRatios,propSizes,sourceEnergies,siteEnergies);


        mixSourceEUI = $scope.getMixedSourceEUI(sumPropSize,sourceEnergies,siteEnergies);

        if(results.length > 2){

            if($scope.propertyModel.reportingUnits==="us"){
                mixMedianSourceEUI = 123.1;
            }else{
                mixMedianSourceEUI = 1.23;
            }
            mixPercentBetterSourceEUI = mixMedianSourceEUI*(1-$scope.propertyModel.percentBetterThanMedian / 100);

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
              {"medianSiteEnergy":$scope.round(mixMedianSiteEnergy,2)}
              ];

        return mixTable;

    };

    $scope.computeBenchmarkResult = function(){

        $scope.futures = $scope.propList.map(function (r) {
            return benchmarkServices.submit(r);
        });

        $q.all($scope.futures).then(function (results) {
            $scope.benchmarkResult = $scope.computeBenchmarkMix(results);
        });
    };

    $scope.submit = function () {

        $scope.propList = [];

        if($scope.energies.length===0){$scope.propertyModel.energies=null;}
                    else {$scope.propertyModel.energies = $scope.energies;}

        if($scope.baselineForm.$valid){
            for (var i = 0; i < $scope.propTypes.length; i++){
                if($scope.propTypes[i].valid === true){
                    var property = Object.assign($scope.propertyModel, $scope.propTypes[i].propertyModel);
                    $scope.propList.push(property);
                }else {console.log('Error in ' + $scope.propTypes[i].type);}
            }
        }else {console.log('Please Fix Form Errors');}

        console.log($scope.propList);

        if ($scope.propList.length !== 0){
            $scope.computeBenchmarkResult();
        }else{$scope.benchmarkResult = null;}

    };

  };
  DashboardCtrl.$inject = ['$rootScope', '$scope', '$q','benchmarkServices'];
  return {
    DashboardCtrl: DashboardCtrl
  };
});
