String.prototype.supplant = function (o) {
    return this.replace(/\$(\w*)/g, (a, b) => {
            var v = o[b];
            if (v !== undefined) return v
            else throw "no value for " + b;
    });
};
function drawGraph(testData, title) {
    var best = testData[0];
    var margin = testData[1];
    var results = testData[2];
    var durations = map(res => res[1], results).reverse();
    var revisions = map(res => res[0], results).reverse();
    var baseline = best * (1+margin);
    var baselinePoints = [[revisions[0], baseline ], [revisions[revisions.length-1], baseline]]
    var options = { grid: {hoverable: true} }
    var points = {data: zip(revisions, durations),
                  points: {show: true}, lines: {show: true},
                  grid: {hoverable: true}}
    var plot = $.plot($("#"+title), [{data: baselinePoints, color: "red"}, points], options)

    function showTooltip(x, y, contents) {
        $('<div id="tooltip">' + contents + '</div>').css( {
            position: 'absolute',
            display: 'none',
            top: y + 5,
            left: x + 5,
            border: '1px solid #fdd',
            padding: '2px',
            'background-color': '#fee',
            opacity: 0.80
        }).appendTo("body").fadeIn(200);
    }

    var previousPoint = [0,0];
    $("#"+title).bind("plothover", (event, pos, item) => {
        if (item) {
            if (previousPoint[0] != item.datapoint[0] || previousPoint[1] != item.datapoint[1]) {
                previousPoint = item.datapoint;
                $("#tooltip").remove();
                var x = item.datapoint[0].toFixed(2);
                var y = item.datapoint[1].toFixed(2);
                showTooltip(Math.min(item.pageX, screen.width - 130), item.pageY,
                            "rev "+ x.replace(/\..*/, ''));
            }
        } else {
            $("#tooltip").remove();
            previousPoint = [0,0];
        }
    });
}
