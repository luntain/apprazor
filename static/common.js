String.prototype.supplant = function (o) {
    return this.replace(/\$(\w*)/g, function (a, b) {
            var v = o[b];
            if (v !== undefined) return v
            else throw "no value for " + b;
    });
};
function drawGraph(testData, title) {
    var best = testData[0];
    var margin = testData[1];
    var results = testData[2];
    var durations = map(function(res){return res[1];}, results).reverse();
    var revisions = map(function(res){return res[0];}, results).reverse();
    var baseline = best * (1+margin);
    var baselinePoints = [[revisions[0], baseline ], [revisions[revisions.length-1], baseline]]
    var options = { }
    var points = {data: zip(revisions, durations),
                  points: {show: true}, lines: {show: true}}
    $.plot($("#"+title), [{data: baselinePoints, color: "red"}, points], options)
}
