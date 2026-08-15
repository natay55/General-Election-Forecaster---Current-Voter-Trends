window.dashExtensions = Object.assign({}, window.dashExtensions, {
    default: {
        function0: function(feature) {
                return {
                    fillColor: feature.properties.color,
                    weight: 1,
                    color: 'black',
                    fillOpacity: 1
                };
            }

            ,
        function1: function(feature) {
                return {
                    fillColor: feature.properties.color,
                    weight: 3,
                    color: 'white',
                    fillOpacity: 1
                };
            }

            ,
        function2: function(feature, layer) {
            layer.bindTooltip(
                '<b>' + feature.properties.PCON24NM + '</b><br>' +
                'Party: ' + feature.properties.party + '<br>' +
                'Vote share: ' + (feature.properties.vote_share * 100).toFixed(1) + '%', {
                    sticky: true
                }
            );
        }

    }
});