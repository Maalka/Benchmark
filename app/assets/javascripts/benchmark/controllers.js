/**
 * Dashboard controllers.
 */
//define(["./test/sample_response_test_data"], function(sampleTestData) {
define(['angular', 'matchmedia-ng'], function(angular) {
  'use strict';
  var DashboardCtrl = function($rootScope, $scope, $window, $sce, $timeout, $q, $log, matchmedia, benchmarkServices) {

    $rootScope.pageTitle = "Weather Normalization";
    $scope.forms = {'hasValidated': false};
    $scope.matchmedia = matchmedia;
    $scope.mainColumnWidth = "";
    $scope.meter = {};
    $scope.filter = [];
    $scope.hideDays = true;



    // check the media to handel the ng-if media statements
    // it turns out that form elements do not respect "display: none"
    // and need to be removed from the dom
    var setMedia = function (){
        if (matchmedia.isPrint()) {
            $scope.media = "print";
            $scope.mainColumnWidth = "eight wide column";
        } else if (matchmedia.isPhone()) {
            $scope.media = "phone";
            $scope.mainColumnWidth = "eight wide column";
        } else if (matchmedia.isTablet()) {
            $scope.media = "tablet";
            $scope.mainColumnWidth = "eight wide column";
        } else if (matchmedia.isDesktop()) {
            $scope.media = "desktop";
            $scope.mainColumnWidth = "eight wide column";
        } else {
            $scope.mainColumnWidth = "eight wide column";
        }
    };
    matchmedia.on('only screen and (min-width: 1200px) and (max-width: 1919px)', function(match){
        $scope.largeScreen = match.matches;
    });
    matchmedia.on('only screen and (max-width: 1199px)', function(match){
        $scope.largeScreen = !match.matches;
    });

    matchmedia.onPrint(setMedia, $scope);

    setMedia();
    angular.element($window).bind("resize", function () {
        setMedia();
        $scope.$apply();
    });


    $scope.$watch("meter.frequency", function (v) {
        if(v==="day"){
            $scope.hideDays = false;
        } else {
            $scope.hideDays = true;
        }

    });


    $scope.normalizeWeather = function(){

        $scope.futures = benchmarkServices.normalize($scope.meter);

        $q.resolve($scope.futures).then(function (results) {
            console.log(results);
        });
    };



    $scope.computeBenchmarkMix = function(results){

        console.log(results);
        var metricsTable = [
            //the return after weather normalization
        ];
        return metricsTable;
    };

    $scope.submitErrors = function () {
        for (var i = 0; i < $scope.forms.baselineForm.$error.required.length; i++){
            $log.info($scope.forms.baselineForm.$error.required[i].$name);
        }
    };

    $scope.submitFile = function() {
        if ($scope.attachment) {
            $scope.upload($scope.attachment);
        }
    };

    $scope.getMeter = function() {

        if($scope.forms.baselineForm === undefined) {
            return undefined;
        }
        if($scope.forms.baselineForm.$valid){
            $scope.meter.valid = true;
        } else {
            $scope.meter.valid = false;
        }

        $scope.meter.filter = [];
        $scope.meter.ddThreshold = $scope.meter.ddThreshold ? $scope.meter.ddThreshold  : 65;
        $scope.meter.ddType = $scope.meter.ddType ? $scope.meter.ddType  : "avg";
        $scope.meter.frequency = $scope.meter.frequency ? $scope.meter.frequency  : "month";
        $scope.daysOfWeek.forEach(function (v){
            if($scope.filter[v.name]===true){
                $scope.meter.filter.push(v.name);
            }
        });

        return $scope.meter;
    };




    $scope.submit = function () {

        if($scope.forms.baselineForm === undefined) {
            return;
        }
        $scope.forms.hasValidated = true; /// only check the field errors if this form has attempted to validate.
        $scope.meter.filter = [];


        if($scope.forms.baselineForm.$valid){
            $scope.meter.ddThreshold = $scope.meter.ddThreshold ? $scope.meter.ddThreshold  : 65;
            $scope.meter.ddType = $scope.meter.ddType ? $scope.meter.ddType  : "avg";
            $scope.daysOfWeek.forEach(function (v){
                if($scope.filter[v.name]===true){
                    $scope.meter.filter.push(v.name);
                }
            });
            $scope.normalizeWeather();

        }else {
            $scope.submitErrors();
        }
    };


    $scope.daysOfWeek = [
        {
            name: "monday",
            default: false,
            type: "checkbox",
            title: "M",
        },
        {
            name: "tuesday",
            default: false,
            type: "checkbox",
            title: "Tu",
        },
        {
            name: "wednesday",
            default: false,
            type: "checkbox",
            title: "W",
        },
        {
            name: "thursday",
            default: false,
            type: "checkbox",
            title: "Th",
        },
        {
            name: "friday",
            default: false,
            type: "checkbox",
            title: "F",
        },
        {
            name: "saturday",
            default: false,
            type: "checkbox",
            title: "Sa",
        },
        {
            name: "sunday",
            default: false,
            type: "checkbox",
            title: "Su",
        }
    ];

    $scope.thresholdOptions =  [
            {id:"avg",name:"Average"},
            {id:"min",name:"Minimum"},
            {id:"max",name:"Maximum"}
    ];

    $scope.frequencyOptions =  [
            {id:"day",name:"Day"},
            {id:"month",name:"Month"}
    ];

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


    $scope.energyProperties = {

        energyType:[
            {id:"grid",name:"Electric (Grid)"},
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
            {id:"grid",name:"On-Site Other"},
            {id:"grid",name:"Off-Site Purchased"}
        ],

        renewableUnits:[
            {id:"KBtu",name:"kBtu",filter_id:"grid"},
            {id:"MBtu",name:"MBtu",filter_id:"grid"},
            {id:"kWh",name:"kWh",filter_id:"grid"},
            {id:"MWh",name:"MWh",filter_id:"grid"},
            {id:"GJ",name:"GJ",filter_id:"grid"},
        ],


        energyUnits: [
            //<!--Electricity - Grid -->
            {id:"KBtu",name:"kBtu",filter_id:"grid"},
            {id:"MBtu",name:"MBtu",filter_id:"grid"},
            {id:"kWh",name:"kWh",filter_id:"grid"},
            {id:"MWh",name:"MWh",filter_id:"grid"},
            {id:"GJ",name:"GJ",filter_id:"grid"},

            //<!--Electricity - Onsite Renewable-->
            {id:"KBtu",name:"kBtu",filter_id:"onSiteElectricity"},
            {id:"MBtu",name:"MBtu",filter_id:"onSiteElectricity"},
            {id:"kWh",name:"kWh",filter_id:"onSiteElectricity"},
            {id:"MWh",name:"MWh",filter_id:"onSiteElectricity"},
            {id:"GJ",name:"GJ",filter_id:"onSiteElectricity"},

            //<!--Natural Gas -->
            {id:"NGMcf",name:"MCF",filter_id:"naturalGas"},
            {id:"NGKcf",name:"kcf",filter_id:"naturalGas"},
            {id:"NGCcf",name:"ccf",filter_id:"naturalGas"},
            {id:"NGcf",name:"cf",filter_id:"naturalGas"},
            {id:"NGm3",name:"Cubic Meters",filter_id:"naturalGas"},
            {id:"GJ",name:"GJ",filter_id:"naturalGas"},
            {id:"KBtu",name:"kBtu",filter_id:"naturalGas"},
            {id:"MBtu",name:"MBtu",filter_id:"naturalGas"},
            {id:"therms",name:"Therms",filter_id:"naturalGas"},

            //<!--Fuel Oil No. 1 -->
            {id:"KBtu",name:"kBtu",filter_id:"fueloil1Unit"},
            {id:"MBtu",name:"MBtu ",filter_id:"fueloil1Unit"},
            {id:"GJ",name:"GJ",filter_id:"fueloil1Unit"},
            {id:"No1UKG",name:"Gallons (UK)",filter_id:"fueloil1Unit"},
            {id:"No1USG",name:"Gallons",filter_id:"fueloil1Unit"},
            {id:"No1L",name:"Liters",filter_id:"fueloil1Unit"},

            //<!--Fuel Oil No. 2 -->
            {id:"KBtu",name:"kBtu",filter_id:"fueloil2Unit"},
            {id:"MBtu",name:"MBtu",filter_id:"fueloil2Unit"},
            {id:"GJ",name:"GJ",filter_id:"fueloil2Unit"},
            {id:"No2UKG",name:"Gallons (UK)",filter_id:"fueloil2Unit"},
            {id:"No2USG",name:"Gallons",filter_id:"fueloil2Unit"},
            {id:"No2L",name:"Liters",filter_id:"fueloil2Unit"},

            //<!--Fuel Oil No. 4 -->
            {id:"KBtu",name:"kBtu",filter_id:"fueloil4Unit"},
            {id:"MBtu",name:"MBtu",filter_id:"fueloil4Unit"},
            {id:"GJ",name:"GJ",filter_id:"fueloil4Unit"},
            {id:"No4UKG",name:"Gallons (UK)",filter_id:"fueloil4Unit"},
            {id:"No4USG",name:"Gallons",filter_id:"fueloil4Unit"},
            {id:"No4L",name:"Liters",filter_id:"fueloil4Unit"},

            //<!--Fuel Oil No. 5,6 -->
            {id:"KBtu",name:"kBtu",filter_id:"fueloil6Unit"},
            {id:"MBtu",name:"MBtu",filter_id:"fueloil6Unit"},
            {id:"GJ",name:"GJ",filter_id:"fueloil6Unit"},
            {id:"No6UKG",name:"Gallons (UK)",filter_id:"fueloil6Unit"},
            {id:"No6USG",name:"Gallons",filter_id:"fueloil6Unit"},
            {id:"No6L",name:"Liters",filter_id:"fueloil6Unit"},

            //<!--Diesel-->
            {id:"KBtu",name:"kBtu",filter_id:"diesel"},
            {id:"MBtu",name:"MBtu",filter_id:"diesel"},
            {id:"GJ",name:"GJ",filter_id:"diesel"},
            {id:"DieselUKG",name:"Gallons (UK)",filter_id:"diesel"},
            {id:"DieselUSG",name:"Gallons",filter_id:"diesel"},
            {id:"DieselL",name:"Liters",filter_id:"diesel"},

            //<!--Kerosene-->
            {id:"KBtu",name:"kBtu",filter_id:"kerosene"},
            {id:"MBtu",name:"MBtu",filter_id:"kerosene"},
            {id:"GJ",name:"GJ",filter_id:"kerosene"},
            {id:"KeroseneUKG",name:"Gallons (UK)",filter_id:"kerosene"},
            {id:"KeroseneUSG",name:"Gallons",filter_id:"kerosene"},
            {id:"KeroseneL",name:"Liters",filter_id:"kerosene"},

            //<!--Propane-->
            {id:"GJ",name:"GJ",filter_id:"propane"},
            {id:"KBtu",name:"kBtu",filter_id:"propane"},
            {id:"MBtu",name:"MBtu",filter_id:"propane"},
            {id:"PropaneUKG",name:"Gallons (UK)",filter_id:"propane"},
            {id:"PropaneUSG",name:"Gallons",filter_id:"propane"},
            {id:"PropaneCf",name:"kcf",filter_id:"propane"},
            {id:"PropaneCCf",name:"ccf",filter_id:"propane"},
            {id:"PropaneKCf",name:"cf",filter_id:"propane"},
            {id:"PropaneL",name:"Liters",filter_id:"propane"},

            //<!--District Steam-->
            {id:"GJ",name:"GJ",filter_id:"steam"},
            {id:"KBtu",name:"kBtu",filter_id:"steam"},
            {id:"MBtu",name:"MBtu",filter_id:"steam"},
            {id:"therms",name:"Therms",filter_id:"steam"},
            {id:"SteamLb",name:"Pounds",filter_id:"steam"},
            {id:"SteamKLb",name:"Thousand pounds",filter_id:"steam"},
            {id:"SteamMLb",name:"Million pounds",filter_id:"steam"},

            //<!--District Hot Water-->
            {id:"KBtu",name:"kBtu",filter_id:"hotWater"},
            {id:"MBtu",name:"MBtu",filter_id:"hotWater"},
            {id:"GJ",name:"GJ",filter_id:"hotWater"},
            {id:"therms",name:"Therms",filter_id:"hotWater"},

            //<!--District Chilled Water-->
            {id:"KBtu",name:"kBtu",filter_id:"chilledWater"},
            {id:"MBtu",name:"MBtu",filter_id:"chilledWater"},
            {id:"GJ",name:"GJ",filter_id:"chilledWater"},
            {id:"CHWTonH",name:"Ton Hours",filter_id:"chilledWater"},

            //<!--Coal (Anthracite)-->
            {id:"KBtu",name:"kBtu",filter_id:"coalA"},
            {id:"MBtu",name:"MBtu",filter_id:"coalA"},
            {id:"GJ",name:"GJ",filter_id:"coalA"},
            {id:"CoalATon",name:"Tons",filter_id:"coalA"},
            {id:"CoalATonne",name:"Tonnes (Metric)",filter_id:"coalA"},
            {id:"CoalALb",name:"Pounds",filter_id:"coalA"},
            {id:"CoalAKLb",name:"Thousand Pounds",filter_id:"coalA"},
            {id:"CoalAMLb",name:"Million Pounds",filter_id:"coalA"},

            //<!--Coal (Bituminous)-->
            {id:"KBtu",name:"kBtu",filter_id:"coalB"},
            {id:"MBtu",name:"MBtu",filter_id:"coalB"},
            {id:"GJ",name:"GJ",filter_id:"coalB"},
            {id:"CoalBitTon",name:"Tons",filter_id:"coalB"},
            {id:"CoalBitTonne",name:"Tonnes (Metric)",filter_id:"coalB"},
            {id:"CoalBitLb",name:"Pounds",filter_id:"coalB"},
            {id:"CoalBitKLb",name:"Thousand Pounds",filter_id:"coalB"},
            {id:"CoalBitMLb",name:"Million Pounds",filter_id:"coalB"},

            //<!--Coke-->
            {id:"KBtu",name:"kBtu",filter_id:"coke"},
            {id:"MBtu",name:"MBtu",filter_id:"coke"},
            {id:"GJ",name:"GJ",filter_id:"coke"},
            {id:"CokeTon",name:"Tons",filter_id:"coke"},
            {id:"CokeTonne",name:"Tonnes (Metric)",filter_id:"coke"},
            {id:"CokeLb",name:"Pounds",filter_id:"coke"},
            {id:"CokeKLb",name:"Thousand Pounds",filter_id:"coke"},
            {id:"CokeMLb",name:"Million Pounds",filter_id:"coke"},

            //<!--Wood-->
            {id:"KBtu",name:"kBtu",filter_id:"wood"},
            {id:"MBtu",name:"MBtu",filter_id:"wood"},
            {id:"GJ",name:"GJ",filter_id:"wood"},
            {id:"WoodTon",name:"Tons",filter_id:"wood"},
            {id:"WoodTonne",name:"Tonnes (Metric)",filter_id:"wood"},

            //<!--Other-->
            {id:"KBtu",name:"kBtu",filter_id:"other"},
            {id:"GJ",name:"GJ",filter_id:"other"}
            ]
    };
  };

  DashboardCtrl.$inject = ['$rootScope', '$scope', '$window','$sce','$timeout', '$q', '$log', 'matchmedia', 'benchmarkServices'];
  return {
    DashboardCtrl: DashboardCtrl
  };
});
