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
                   return 'javascripts/common/partials/property_fields.html';
            },

            controller: ["$scope", function ($scope) {


                $scope.buildingName =  ($scope.model.name) ? $scope.model.name : "Anonymous";
                
                $scope.benchmark = $scope.$parent;
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
                        } 
                    } else {
                        $scope.clearParams();
                    }

                });
                $scope.removeProp = function() { 
                    $scope.$parent.removeProp(this);
                };
                $scope.isRequired = function(field) {
                    if (field.required === undefined) {
                        return false;
                    } else if (field.required === $scope.model.country) {
                        return true;
                    } else if (field.required === "all") {
                        return true;
                    } else {
                        return false;
                    }
                };
                $scope.isShown = function (field) {
                    if (field.required === undefined) {
                        return true;
                    } else if (field.required === $scope.model.country) {
                        return true;
                    } else if (field.required === "all") {
                        return true;
                    } else {
                        return false;
                    }
                };
                $scope.required = function(field) {
                    return $scope.isRequired(field) ? "required" : "";
                };

                $scope.$watch("propertyModel.GFA", function () {

                    if($scope.propertyModel.GFA && $scope.propertyModel.areaUnits){
                        if($scope.propertyModel.defaultValues){$scope.setDefaults();}
                    }
                });
                $scope.$watch("propertyModel.areaUnits", function () {
                    if($scope.propertyModel.GFA && $scope.propertyModel.areaUnits){
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
                $scope.isSmall = function (GFA) {
                    if(GFA<=5000){
                       return true;
                    } else {
                       return false;
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

                var GFA = 0;
                var setPropertyModelFields = function(GFA)  {
                    $scope.propertyModelFields = {
                    Hospital    : [
                        {
                            name: "licensedBedCapacity",
                            default: $scope.round(0.69*GFA/1000,2),
                            type: "number",
                            title: "Licensed Bed Capacity",
                            required: 'Canada'
                        },
                        {
                            name: "numStaffedBeds",
                            default: $scope.round(0.46*GFA/1000,2),
                            type: "number",
                            title: "Number of Staffed Beds",
                            required: 'USA'
                        },
                        {
                            name: "numFTEWorkers",
                            default: $scope.round(2.6*GFA/1000,2),
                            type: "number",
                            title: "Number of Full Time Workers",
                            required: 'USA'
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(1.32*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift",
                            required: 'Canada'
                        },
                        {
                            name: "numMRIMachines",
                            default: 1,
                            type: "number",
                            title: "Number of MRI Machines",
                            required: "USA"
                        },
                        {
                            name: "hasLaundryFacility",
                            default: true,
                            type: "checkbox",
                            title: "Has Laundry Facility",
                            required: 'Canada'
                        },
                        {
                            name: "percentHeated",
                            default:  $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated",
                            required: 'Canada'
                        },
                        {
                            name: "percentCooled",
                            default:  $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled",
                            required: 'Canada'
                        },
                        {
                            name: "weeklyOperatingHours",
                            default: null,
                            type: "number",
                            title: "Number of Weekly Operating Hours",
                            required: 'Canada'
                        }
                    ],
                    Hotel: [

                        {
                           name: "numBedrooms",
                           default: $scope.round(1.95*GFA/1000,2),
                           type: "number",
                           title: "Number of Bedrooms",
                           required: "USA"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(0.32*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift",
                            required: "USA"
                        },
                        {
                            name: "hasFoodPreparation",
                            default: false,
                            type: "checkbox",
                            title: "Has Food Preparation",
                            required: "USA"
                        },
                        {
                            name: "numRefrUnits",
                            default: $scope.round(0.023*GFA/1000,2),
                            type: "number",
                            title: "Number of Refrigerator Units",
                            required: "USA"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated",
                            required: "USA"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled",
                            required: "USA"
                        }
                    ],
                    K12School: [

                        {
                            name: "gymFloorArea",
                            default: 0.0,
                            type: "number",
                            title: "Gym Floor Area",
                            required: "Canada"
                        },
                        {
                            name: "isHighSchool",
                            default: false,
                            type: "checkbox",
                            title: "Property is a High School",
                            required: "USA"
                        },
                        {
                            name: "isSecondarySchool",
                            default: false,
                            type: "checkbox",
                            title: "Property is a Secondary School",
                            required: "Canada"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(0.77*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift",
                            required: "Canada"
                        },
                        {
                            name: "studentSeatingCapacity",
                            default: $scope.round(10.0*GFA/1000,2),
                            type: "number",
                            title: "Student Seeting Capacity",
                            required: "Canada"
                        },
                        {
                            name: "numWalkinRefrUnits",
                            default: $scope.round(0.01*GFA/1000,2),
                            type: "number",
                            title: "Number of Walk-in Refrigeration Units",
                            required: "USA"
                        },
                        {
                            name: "isOpenWeekends",
                            default: false,
                            type: "checkbox",
                            title: "Property is Open Weekends",
                            required: "USA"
                        },
                        {
                            name: "hasCooking",
                            default: true,
                            type: "checkbox",
                            title: "Facility Has Cooking",
                            required: "USA"
                        },
                        {
                            name: "numComputers",
                            default: $scope.round(1.75*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers",
                            required: "USA"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated",
                            required: "all"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled",
                            required: "all"
                        }
                    ],
                    MedicalOffice: [

                        {
                            name: "weeklyOperatingHours",
                            default: 65,
                            type: "number",
                            title: "Number of Weekly Operating Hours",
                            required: "all"

                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(2.2*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift",
                            required: "all"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated",
                            required: "USA"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled",
                            required: "all"
                        }
                    ],
                    MultiFamily: [
                        {
                            name: "numRezUnits",
                            default: $scope.round(1.2*GFA/1000,2),
                            type: "number",
                            title: "Number of Residential Units",
                            required: "USA"
                        },
                        {
                            name: "numUnitsLowRise1to4",
                            default: $scope.round(1.2*GFA/1000,2),
                            type: "number",
                            title: "Number of Low Rise (Floors 1-4) Units",
                            required: "USA"
                        },
                        {
                            name: "numUnitsMidRise5to9",
                            default: 0.0,
                            type: "number",
                            title: "Number of Low Rise (Floors 5-9) Units",
                            required: "USA"
                        },
                        {
                            name: "numUnitsHighRise10plus",
                            default: 0.0,
                            type: "number",
                            title: "Number of High Rise (Floors above 10) Units",
                            required: "USA"
                        },
                        {
                            name: "numBedrooms",
                            default: $scope.round(1.4*GFA/1000,2),
                            type: "number",
                            title: "Number of Bedrooms",
                            required: "USA"
                        }
                    ],
                    Office: [

                        {
                            name: "weeklyOperatingHours",
                            default: 65,
                            type: "number",
                            title: "Number of Weekly Operating Hours",
                            required: "all"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(2.3*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift",
                            required: "all"
                        },
                                                {
                            name: "numComputers",
                            default: $scope.round(2*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers",
                            required: "all"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated",
                            required: "all"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled",
                            required: "all"

                        },
                        {
                             name: "numServers",
                             default: null,
                             type: "number",
                             title: "Number of Servers",
                             required: "Canada"
                        }
                    ],

                    FinancialOffice: [
                        {
                            name: "isSmallBank",
                            default: $scope.isSmall(GFA),
                            type: "checkbox",
                            title: "Small Bank",
                            required: "all"
                        },
                        {
                            name: "weeklyOperatingHours",
                            default: 65,
                            type: "number",
                            title: "Number of Weekly Operating Hours",
                            required: "all"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(2.3*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift",
                            required: "all"
                        },
                                                {
                            name: "numComputers",
                            default: $scope.round(2*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers",
                            required: "all"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated",
                            required: "all"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled",
                            required: "all"

                        },
                        {
                             name: "numServers",
                             default: null,
                             type: "number",
                             title: "Number of Servers",
                             required: "Canada"
                        }
                    ],
                    ResidenceHall: [
                        {
                           name: "numBedrooms",
                           default: 100,
                           type: "number",
                           title: "Number of Bedrooms",
                           required: "USA"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated",
                            required: "USA"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled",
                            required: "USA"
                        }
                    ],
                    Retail: [
                        {
                            name: "weeklyOperatingHours",
                            default: 65,
                            type: "number",
                            title: "Number of Weekly Operating Hours",
                            required: "USA"
                        },
                        {
                            name: "numOpenClosedRefrCases",
                            default: 0.0,
                            type: "number",
                            title: "Number of Open/Closed Refrigeration Cases",
                            required: "USA"
                        },
                        {
                            name: "numCashRegisters",
                            default: $scope.round(0.3*GFA/1000,2),
                            type: "number",
                            title: "Number of Cash Registers",
                            required: "USA"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(1.0*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift",
                            required: "USA"
                        },
                        {
                            name: "numComputers",
                            default: $scope.round(0.2*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers",
                            required: "USA"
                        },
                        {
                            name: "numWalkinRefrUnits",
                            default: 0.0,
                            type: "number",
                            title: "Number of Walk-in Refrigeration Units",
                            required: "USA"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated",
                            required: "USA"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled",
                            required: "USA"
                        }
                    ],
                    SeniorCare: [
                        {
                            name: "maxNumResidents",
                            default: $scope.round(2.374*GFA/1000,2),
                            type: "number",
                            title: "Maximum Number of Residents",
                            required: "USA"
                        },
                        {
                            name: "avgNumResidents",
                            default: $scope.round(2.075*GFA/1000,2),
                            type: "number",
                            title: "Average Number of Residents",
                            required: "USA"
                        },
                        {
                            name: "numRezUnits",
                            default: $scope.round(1.584*GFA/1000,2),
                            type: "number",
                            title: "Number of Residential Units",
                            required: "USA"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(0.9523*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift",
                            required: "USA"
                        },
                        {
                            name: "numComputers",
                            default: $scope.round(0.367*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers",
                            required: "USA"
                        },
                        {
                            name: "numRezWashingMachines",
                            default: $scope.round(0.05757*GFA/1000,2),
                            type: "number",
                            title: "Number of Residential Washing Machines",
                            required: "USA"
                        },
                        {
                            name: "numCommWashingMachines",
                            default: $scope.round(0.04422*GFA/1000,2),
                            type: "number",
                            title: "Number of Commercial Washing Machines",
                            required: "USA"
                        },
                        {
                            name: "numElectronicLifts",
                            default: $scope.round(0.0704*GFA/1000,2),
                            type: "number",
                            title: "Number of Electronic Lifts",
                            required: "USA"
                        },
                        {
                            name: "numRefrUnits",
                            default: $scope.round(0.09065*GFA/1000,2),
                            type: "number",
                            title: "Number of Refrigerator Units",
                            required: "USA"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Heated",
                            required: "USA"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            type: "select",
                            fields: $scope.buildingProperties.areaHVAC,
                            title: "Percent Cooled",
                            required: "USA"
                        }
                    ],
                    Supermarket: [
                        {
                            name: "weeklyOperatingHours",
                            default: 105,
                            type: "number",
                            title: "Number of Weekly Operating Hours",
                            required: "all"
                        }, 
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(1.0*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift",
                            required: "all"
                        },
                        {
                            name: "numCashRegisters",
                            default: $scope.round(0.38*GFA/1000,2),
                            type: "number",
                            required: "Canada"
                        },
                        {
                            name: "numComputers",
                            default: $scope.round(0.51*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers",
                            required: "Canada"
                        },
                        {
                            name: "lengthRefrFoodDisplayCases",
                            default: $scope.round(2.6*GFA/1000,2),
                            type: "number",
                            required: "Canada"
                        },
                        {
                            name: "numWalkinRefrUnits",
                            default: $scope.round(0.25*GFA/1000,2),
                            type: "number",
                            title: "Number of Walk-in Refrigerator Units",
                            required: "USA"
                        },
                        {
                            name: "hasCooking",
                            default: true,
                            type: "checkbox",
                            title: "Facility Has Cooking",
                            required: "USA"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            fields: $scope.buildingProperties.areaHVAC,
                            type: "select",
                            title: "Percent Heated",
                            required: "USA"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            fields: $scope.buildingProperties.areaHVAC,
                            type: "select",
                            title: "Percent Cooled",
                            required: "USA"
                        }
                    ],
                    WastewaterCenter: [
                        {
                            name: "wastewaterInfluentBiologicalOxygenDemand",
                            default: 200.0,
                            type: "number",
                            title: "Influent Biological Oxygen Demand (mg/l)",
                            required: "USA"
                        },
                        {
                            name: "wastewaterEffluentBiologicalOxygenDemand(mg/l)",
                            default: 8.0,
                            type: "number",
                            title: "Effluent Biological Oxygen Demand",
                            required: "USA"
                        },
                        {
                            name: "wastewaterAvgInfluentInflow",
                            default: null,
                            type: "number",
                            title: "Wastewater Average Influent Inflow (MGD)",
                            required: "USA"
                        },
                        {
                            name: "wastewaterHasTrickleFiltration",
                            default: false,
                            type: "checkbox",
                            title: "Has Trickle Filtration",
                            required: "USA"
                        },
                        {
                            name: "wastewaterHasNutrientRemoval",
                            default:false,
                            type: "checkbox",
                            title: "Has Nutrient Removal",
                            required: "USA"
                        },
                        {
                            name: "wastewaterPlantDesignFlowRate",
                            default: null,
                            type: "number",
                            title: "Plant Design Flow Rate (MGD)",
                            required: "USA"
                        },
                        {
                            name: "wastewaterLoadFactor",
                            default: null,
                            type: "number",
                            title: "Wastewater Load Factor",
                            required: "USA"
                        }


                    ],
                    WorshipCenter: [
                        {
                            name: "weeklyOperatingHours",
                            default: 33,
                            type: "number",
                            title: "Number of Weekly Operating Hours",
                            required: "USA"
                        },
                        {
                            name: "isOpenAllWeekdays",
                            default: false,
                            type: "checkbox",
                            title: "Is open All Weekdays",
                            required: "USA"
                        },
                        {
                            name: "seatingCapacity",
                            default: $scope.round(40.0*GFA/1000,2),
                            type: "number",
                            title: "Seating Capacity",
                            required: "USA"
                        },
                        {
                            name: "numComputers",
                            default: $scope.round(0.2*GFA/1000,2),
                            type: "number",
                            title: "Number of Computers",
                            required: "USA"
                        },
                        {
                            name: "hasFoodPreparation",
                            default: false,
                            type: "checkbox",
                            title: "Has Food Preparation",
                            required: "USA"
                        },
                        {
                            name: "numRefrUnits",
                            default: $scope.round(0.018*GFA/1000,2),
                            type: "number",
                            title: "Number of Refrigerator Units",
                            required: "USA"
                        },
                        {
                            name: "numWalkinRefrUnits",
                            default: 0.0,
                            type: "number",
                            title: "Number of Walk-in Refrigeration Units",
                            required: "USA"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            fields: $scope.buildingProperties.areaHVAC,
                            type: "select",
                            title: "Percent Heated",
                            required: "USA"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[10].id,
                            fields: $scope.buildingProperties.areaHVAC,
                            type: "select",
                            title: "Percent Cooled",
                            required: "USA"
                        }
                    ],
                    Warehouse: [
                        {
                            name: "weeklyOperatingHours",
                            default: 60,
                            type: "number",
                            title: "Number of Weekly Operating Hours",
                            required: "USA"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(0.59*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift",
                            required: "USA"
                        }, 
                        {
                            name: "numWalkinRefrUnits",
                            default: 0.0,
                            type: "number",
                            title: "Number of Walk-in Refrigeration Units",
                            required: "USA"
                        }, 
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[5].id,
                            fields: $scope.buildingProperties.areaHVAC,
                            type: "select",
                            title: "Percent Heated",
                            required: "USA"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[2].id,
                            fields: $scope.buildingProperties.areaHVAC,
                            type: "select",
                            title: "Percent Cooled",
                            required: "USA"
                        }
                    ],
                    RefrigeratedWarehouse: [
                         {
                            name: "isWarehouseRefrigerated",
                            default: true,
                            type: "checkbox",
                            title: "Is Refrigerated",
                            required: "all"
                        },
                        {
                            name: "weeklyOperatingHours",
                            default: 60,
                            type: "number",
                            title: "Number of Weekly Operating Hours",
                            required: "USA"
                        },
                        {
                            name: "numWorkersMainShift",
                            default: $scope.round(0.59*GFA/1000,2),
                            type: "number",
                            title: "Number of Workers on Main Shift",
                            required: "USA"
                        },
                        {
                            name: "numWalkinRefrUnits",
                            default: 0.0,
                            type: "number",
                            title: "Number of Walk-in Refrigeration Units",
                            required: "USA"
                        },
                        {
                            name: "percentHeated",
                            default: $scope.buildingProperties.areaHVAC[5].id,
                            fields: $scope.buildingProperties.areaHVAC,
                            type: "select",
                            title: "Percent Heated",
                            required: "USA"
                        },
                        {
                            name: "percentCooled",
                            default: $scope.buildingProperties.areaHVAC[2].id,
                            fields: $scope.buildingProperties.areaHVAC,
                            type: "select",
                            title: "Percent Cooled",
                            required: "USA"
                        }
                    ],
                    DataCenter: [
                        {
                            name: "annualITEnergy",
                            default: null,
                            type: "number",
                            title: "Annual IT Energy (kWh)",
                            required: "all"
                        }
                    ],
                    };
                };
                setPropertyModelFields(GFA);

                //for setting defaults based on building Type, Country does not matter here due to unused parameters upon analysis
                $scope.setDefaults = function() {

                        var prop = $scope.model.type;
                        GFA = $scope.propertyModel.GFA;
                        var areaUnits = $scope.propertyModel.areaUnits;


                        if( areaUnits === "mSQ" ) {
                            GFA = GFA*10.7639;
                        }
                        setPropertyModelFields(GFA);

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
                       "RefrigeratedWarehouse",
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


