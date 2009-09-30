String.prototype.supplant = function (o) {
    return this.replace(/\$(\w*)/g, function (a, b) {
            var v = o[b];
            if (v !== undefined) return v
            else throw "no value for " + b;
    });
};
function sparseRevisionLabels(revisions) {
    var labels = {}
    var lastSeenRevision = "boo"
    $.each(revisions, function(i, rev) {
        if (rev !== lastSeenRevision) {
            labels[i] = rev;
            lastSeenRevision = rev;
        }
    });
    return labels;
}
function drawGraph(testData, title) {
    var best = testData[0];
    var margin = testData[1];
    var results = testData[2];
    var durations = map(function(res){return res[1];}, results).reverse();
    var revisions = map(function(res){return res[0];}, results).reverse();
    var g = new Bluff.Line(title, 700);
    g.theme_keynote();
    g.title = title;
    g.hide_legend = true;
    g.baseline_value = best * (1+margin)
    g.data('this test', durations);
    g.labels = sparseRevisionLabels(revisions)
    g.draw();
}
