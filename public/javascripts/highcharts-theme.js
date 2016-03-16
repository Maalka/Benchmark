'use strict';
define(['highcharts', 'highcharts-more'], function(angular, nvd3) {
    (function() {
        Highcharts.theme = {
            chart: {
                style: {
                    fontFamily: '"Open Sans", "Helvetica Neue", Arial, Helvetica, sans-serif'
                }
            },
            credits: {
                enabled: false
            }
        };

        // Apply the theme
        Highcharts.setOptions(Highcharts.theme);
        return Highcharts;
    })();
});