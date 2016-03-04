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
                forms: '=forms'
            },

            templateUrl: function(){
                   return '/assets/javascripts/common/partials/Office.html';
            },

            controller: ["$scope", function ($scope) {

                $scope.propFieldsRequired = false;
                $scope.propertyModel = {};
                $scope.model.propertyModel = $scope.propertyModel ;
                $scope.model.propertyModel.buildingType = $scope.model.type;

                $scope.model.valid = false;

                $scope.$watch("forms.baselineForm.$valid", function (validity) {
                    $scope.model.valid = validity;
                });

                $scope.round = function(value, decimals) {
                    return Number(Math.round(value+'e'+decimals)+'e-'+decimals);
                };

                //auto-populate default parameters by building type and country
                $scope.$watch("propertyModel.defaultValues", function () {

                    if($scope.propertyModel.defaultValues){
                        if( $scope.propertyModel.GFA && $scope.propertyModel.areaUnits) {
                            $scope.setDefaults();
                        } else {
                            if( $scope.propertyModel.GFA ){
                                $scope.GFARequired = true;
                            }
                            $scope.propertyModel.defaultValues = false;
                        }
                    } else {
                        $scope.clearParams();
                    }

                });

                $scope.$watch("propertyModel.GFA", function () {

                    if($scope.propertyModel.GFA && $scope.propertyModel.areaUnits){
                        $scope.GFARequired = false;
                        if($scope.propertyModel.defaultValues){$scope.setDefaults();}
                    }
                });
                $scope.$watch("propertyModel.areaUnits", function () {
                    if($scope.propertyModel.GFA && $scope.propertyModel.areaUnits){
                        $scope.GFARequired = false;
                        if($scope.propertyModel.defaultValues===true){$scope.setDefaults();}
                    }
                });

                $scope.$watch("forms.baselineForm.$valid", function () {
                    if($scope.forms.baselineForm.$valid){
                        $scope.propFieldsRequired = false;
                    } else {
                        $scope.propFieldsRequired = true;
                    }
                });
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

                var GFA = 0;
                var setPropertyModelFields = function(GFA)  {
                    $scope.propertyModelFields = {
                    Hospital: [
                        {
                            name: "licensedBedCapacity",
                            default: $scope.round(0.69*GFA/1000,2),
                            type: "number",
                            title: "Licensed Bed Capacity"
                        },
                        {
                            name: "numStaffedBeds",
                            default: $scope.round(0.46*GFA/1000,2),
                            type: "number",
                            title: "Number of Staffed Beds"
                        },
                        {
                            name: "numFTEWorkers",
                            default: $scope.round(2.6*GFA/1000,2),
                            type: "number",
                            title: "Number of Full Time Workers"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(1.32*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift"
                        },
                        {
                            name: "numMRIMachines",
                            default: 1,
                            type: "number",
                            title: "Number of MRI Machines",
                            required: ['Canada', "US"]

                        },
                        {
                            name: "hasLaundryFacility",
                            default: true,
                            type: "checkbox",
                            title: "Has Laundry Facility"
                        },
                        {
                            name: "percentHeated",
                            defualt:  $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated"
                        },
                        {
                            name: "percentCooled",
                            defualt:  $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled"
                        }
                    ],
                    Hotel: [
                        {
                           name: "numBedrooms",
                           default: $scope.round(1.95*GFA/1000,2),
                           type: "number",
                           title: "Number of Bedrooms"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(0.32*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift"
                        },
                        {
                            name: "hasFoodPreparation",
                            default: false,
                            type: "checkbox",
                            title: "Has Food Preparation"
                        },
                        {
                            name: "numRefrUnits",
                            default: $scope.round(0.023*GFA/1000,2),
                            type: "number",
                            title: "Number of Refrigerator Units"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled"
                        }
                    ],
                    K12School: [
                        {
                            name: "gymFloorArea",
                            default: 0.0,
                            type: "number",
                            title: "Gym Floor Area"
                        },
                        {
                            name: "isHighSchool",
                            default: false,
                            type: "checkbox",
                            title: "Property is a High School"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(0.77*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift"
                        },
                        {
                            name: "studentSeatingCapacity",
                            default: $scope.round(10.0*GFA/1000,2),
                            type: "number",
                            title: "Student Seeting Capacity"
                        },
                        {
                            name: "numWalkinRefrUnits",
                            default: $scope.round(0.01*GFA/1000,2),
                            type: "number",
                            title: "Number of Walk-in Refrigeration Units"
                        },
                        {
                            name: "isOpenWeekends",
                            default: false,
                            type: "checkbox",
                            title: "Property is Open Weekends"
                        },
                        {
                            name: "hasCooking",
                            default: true,
                            type: "checkbox",
                            title: "Facility Has Cooking"
                        },
                        {
                            name: "numComputers",
                            default: $scope.round(1.75*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled"
                        }
                    ],
                    MedicalOffice: [
                        {
                            name: "weeklyOperatingHours",
                            default: 65,
                            type: "number",
                            title: "Number of Weekly Operating Hours"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(0.77*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled"
                        }
                    ],
                    MultiFamily: [
                        {
                            name: "numRezUnits",
                            default: $scope.round(1.2*GFA/1000,2),
                            type: "number",
                            title: "Number of Residential Units"
                        },
                        {
                            name: "numUnitsLowRise1to4",
                            default: $scope.round(1.2*GFA/1000,2),
                            type: "number",
                            title: "Number of Low Rise (Floors 1-4) Units"
                        },
                        {
                            name: "numUnitsMidRise5to9",
                            default: 0.0,
                            type: "number",
                            title: "Number of Low Rise (Floors 5-9) Units"

                        },
                        {
                            name: "numUnitsHighRise10plus",
                            default: 0.0,
                            type: "number",
                            title: "Number of High Rise (Floors above 10) Units"
                        },
                        {
                            name: "numBedrooms",
                            default: $scope.round(1.4*GFA/1000,2),
                            type: "number",
                            title: "Number of Bedrooms"
                        }
                    ],
                    "Office": [
                        {
                            name: "isSmallBank",
                            type: "checkbox",
                            default: false,
                            title: "Property is a Small Bank"
                        },
                        {
                            name: "weeklyOperatingHours",
                            default: 65,
                            type: "number",
                            title: "Number of Weekly Operating Hours"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(2.3*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift"
                        },
                                                {
                            name: "numComputers",
                            default: $scope.round(2*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled"

                        }
                    ],
                    "ResidenceHall": [
                        {
                           name: "numBedrooms",
                           default: 100,
                           type: "number",
                           title: "Number of Bedrooms"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled"
                        }
                    ],
                    "Retail": [
                        {
                            name: "weeklyOperatingHours",
                            default: 65,
                            type: "number",
                            title: "Number of Weekly Operating Hours"
                        },
                        {
                            name: "numOpenClosedRefrCases",
                            default: 0.0,
                            type: "number",
                            title: "Number of Open/Closed Refrigeration Cases"
                        },
                        {
                            name: "numCashRegisters",
                            default: $scope.round(0.3*GFA/1000,2),
                            type: "number",
                            title: "Number of Cash Registers"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(1.0*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift"
                        },
                        {
                            name: "numComputers",
                            default: $scope.round(0.2*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers"
                        },
                        {
                            name: "numRefrUnits",
                            default: 0.0,
                            type: "number",
                            title: "Number of Refrigerator Units"
                        },
                        {
                            name: "numWalkinRefrUnits",
                            default: 0.0,
                            type: "number",
                            title: "Number of Walk-in Refrigeration Units"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled"
                        }
                    ],
                    "SeniorCare": [
                        {
                            name: "maxNumResidents",
                            default: $scope.round(2.374*GFA/1000,2),
                            type: "number",
                            title: "Maximum Number of Residents"
                        },
                        {
                            name: "avgNumResidents",
                            default: $scope.round(2.075*GFA/1000,2),
                            type: "number",
                            title: "Average Number of Residents"
                        },
                        {
                            name: "numRezUnits",
                            default: $scope.round(1.584*GFA/1000,2),
                            type: "number",
                            title: "Number of Residential Units"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(0.9523*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift"
                        },
                        {
                            name: "numComputers",
                            default: $scope.round(0.367*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers"
                        },
                        {
                            name: "numRezWashingMachines",
                            default: $scope.round(0.05757*GFA/1000,2),
                            type: "number",
                            title: "Number of Residential Washing Machines"
                        },
                        {
                            name: "numCommWashingMachines",
                            default: $scope.round(0.04422*GFA/1000,2),
                            type: "number",
                            title: "Number of Commercial Washing Machines"
                        },
                        {
                            name: "numElectronicLifts",
                            default: $scope.round(0.0704*GFA/1000,2),
                            type: "number",
                            title: "Number of Electronic Lifts"
                        },
                        {
                            name: "numRefrUnits",
                            default: $scope.round(0.09065*GFA/1000,2),
                            type: "number",
                            title: "Number of Refrigerator Units"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled"
                        }
                    ],
                    Supermarket: [
                        {
                            name: "weeklyOperatingHours",
                            default: 105,
                            type: "number",
                            title: "Number of Weekly Operating Hours"
                        }, 
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(1.0*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift"
                        },
                        {
                            name: "numCashRegisters",
                            default: $scope.round(0.38*GFA/1000,2),
                            type: "number"
                        },
                        {
                            name: "numComputers",
                            default: $scope.round(0.51*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers"
                        },
                        {
                            name: "lengthRefrFoodDisplayCases",
                            default: $scope.round(2.6*GFA/1000,2),
                            type: "number"
                        },
                        {
                            name: "numRefrUnits",
                            default: $scope.round(0.25*GFA/1000,2),
                            type: "number",
                            title: "Number of Refrigerator Units"
                        },
                        {
                            name: "hasCooking",
                            default: true,
                            type: "checkbox",
                            title: "Facility Has Cooking"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            fields: $scope.buildingProperties.areaHVAC,
                            type: "select",
                            title: "Percent Heated"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            fields: $scope.buildingProperties.areaHVAC,
                            type: "select",
                            title: "Percent Cooled"
                        }
                    ],
                    WastewaterCenter: [
                        {
                            name: "wastewaterInfluentBiologicalOxygenDemand",
                            default: 200.0,
                            type: "number",
                            title: "Influent Biological Oxygen Demand"
                        },
                        {
                            name: "wastewaterEffluentBiologicalOxygenDemand",
                            default: 8.0,
                            type: "number",
                            title: "Effluent Biological Oxygen Demand"
                        },
                        {
                            name: "wastewaterAvgInfluentInflow",
                            default: null,
                            type: "number",
                            title: "Wastewater Average Influent Inflow"
                        },
                        {
                            name: "wastewaterHasTrickleFiltration",
                            default: false,
                            type: "checkbox",
                            title: "Has Trickle Filtration"
                        },
                        {
                            name: "wastewaterHasNutrientRemoval",
                            default:false,
                            type: "checkbox",
                            title: "Has Nutrient Removal"
                        },
                        {
                            name: "wastewaterPlantDesignFlowRate",
                            default: null,
                            type: "number",
                            title: "Plant Design Flow Rate"
                        },
                        {
                            name: "wastewaterLoadFactor",
                            default: null,
                            type: "number",
                            title: "Wastewater Load Factor"
                        }


                    ],
                    "WorshipCenter": [
                        {
                            name: "weeklyOperatingHours",
                            default: 33,
                            type: "number",
                            title: "Number of Weekly Operating Hours"
                        },
                        {
                            name: "isOpenAllWeekdays",
                            default: false,
                            type: "checkbox",
                            title: "Is open All Weekdays"
                        },
                        {
                            name: "seatingCapacity",
                            default: $scope.round(40.0*GFA/1000,2),
                            type: "number",
                            title: "Seating Capacity"
                        },
                        {
                            name: "numComputers",
                            default: $scope.round(0.2*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers"
                        },
                        {
                            name: "hasFoodPreparation",
                            default: false,
                            type: "checkbox",
                            title: "Has Food Preparation"
                        },
                        {
                            name: "numRefrUnits",
                            default: $scope.round(0.018*GFA/1000,2),
                            type: "number",
                            title: "Number of Refrigerator Units"
                        },
                        {
                            name: "numWalkinRefrUnits",
                            default: 0.0,
                            type: "number",
                            title: "Number of Walk-in Refrigeration Units"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            fields: $scope.buildingProperties.areaHVAC,
                            type: "select",
                            title: "Percent Heated"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            fields: $scope.buildingProperties.areaHVAC,
                            type: "select",
                            title: "Percent Cooled"
                        }
                    ],
                    "Warehouse": [
                        {
                            name: "weeklyOperatingHours",
                            default: 60,
                            type: "number",
                            title: "Number of Weekly Operating Hours"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(0.59*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift"
                        }, 
                        {
                            name: "numWalkinRefrUnits",
                            default: 0.0,
                            type: "number",
                            title: "Number of Walk-in Refrigeration Units"
                        }, 
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            title: "Percent Heated"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            title: "Percent Cooled"
                        }
                    ]
                    };
                };
                setPropertyModelFields(GFA);

                //for setting defaults based on building Type, Country does not matter here due to unused parameters upon analysis
                $scope.setDefaults = function() {

                        var prop = $scope.model.type;
                        GFA = $scope.propertyModel.GFA;
                        var areaUnits = $scope.propertyModel.areaUnits;

                        setPropertyModelFields(GFA);
                        if( areaUnits === "mSQ" ) {
                            GFA = GFA*10.7639;
                        }

                        // set the defaults
                        $scope.propertyModelFields[prop].forEach(function (v){
                            $scope.propertyModel[v.name] = v.default;

                        });
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


