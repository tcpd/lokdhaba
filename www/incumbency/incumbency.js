
var url = './all-ge.json'; //change json source here

function LOG (s) { if (console) { console.log(s); } }

var fixedPartyColours = [];
fixedPartyColours['BJP'] = '#ff9933';
fixedPartyColours['INC'] = '#138808';
fixedPartyColours['INC(I)'] = '#138808';
fixedPartyColours['IND'] = '#008080';
fixedPartyColours['JD'] = '#ff005c';
fixedPartyColours['AIRJP'] = '#009999';
fixedPartyColours['SP'] = '#990000';
fixedPartyColours['JD(U)'] = '#ff005c';
fixedPartyColours['BSP'] = '#99003d';
fixedPartyColours['BJNKP'] = '#333300';
fixedPartyColours['GPP'] = '#ffcc00';
fixedPartyColours['None'] = '#707070';
fixedPartyColours['JP'] = '#536896';
fixedPartyColours['JNP'] = '#536896';
fixedPartyColours['JNP(JP)'] = '#536896';
fixedPartyColours['Other'] = '#000000';
fixedPartyColours['AITC'] = '#00137f';

var partyNames = {'INC': 'Indian National Congress',
    'KJP': 'Karnataka Janata Paksha',
    'IND': 'Independent',
    'CPI': 'Communist Party of India',
    'JD(S)': 'Janata Dal (Secular)',
    'NPF': 'Naga People\'s Front',
    'SP': 'Samajwadi Party',
    'INC(I)': 'Indian National Congress (I)',
    'JNP(JP)': 'Janata Party',
    'BJP': 'Bharatiya Janata Party',
    'JNP(SC)': 'Janata Party (SC)',
    'JNP': 'Janata Party',
    'JD': 'Janata Dal',
    'JD(U)': 'Janata Dal United',
    'NCP': 'National Communist Party',
    'None': 'None',
    'Other': 'Other',
    'JP': 'Janata Party',
    'CPM': 'Communist Party of India (Marxist)',
    'CPI(ML)(L)': 'Communist Party of India (Marxist Leninist) (Liberation)'};

d3.json(url, function(data) {
    var mydata = data;

    var topParties = ['BJP', 'INC', 'AITC', 'DMK', 'SHS', 'YSRCP', 'SP', 'BSP', 'TRS', 'BJD'];
    var topParties = ['BJP', 'INC', 'AITC', 'DMK', 'SHS', 'YSRCP', 'BJD'];

    for (var i = 0; i < mydata.length; i++) {
        if (typeof(mydata[i].Last_Party) === 'undefined') {
            mydata[i].Last_Party = 'None';
        }
        mydata[i].Oth_Current = mydata[i].Party;
        mydata[i].Oth_Last = mydata[i].Last_Party;
        if (!(topParties.includes(mydata[i].Party)) && !(topParties.includes(mydata[i].Last_Party))) {
            mydata[i].Oth_Current = mydata[i].Party;
            mydata[i].Party = 'Other';
            mydata[i].Oth_Last = mydata[i].Last_Party;
            mydata[i].Last_Party = 'Other';
        }
        else if (!(topParties.includes(mydata[i].Party))) {
            mydata[i].Oth_Current = mydata[i].Party;
            mydata[i].Party = 'Other';
        }
        else if (!(topParties.includes(mydata[i].Last_Party))) {
            mydata[i].Oth_Last = mydata[i].Last_Party;
            mydata[i].Last_Party = 'Other';
        }
    }

    //get list of all parties
    var allParties = [];
    for (i = 0; i < mydata.length; i++) {
        if (!(allParties.includes(mydata[i].Party))) {
            allParties.push(mydata[i].Party);
        }
        if (!(allParties.includes(mydata[i].Last_Party))) {
            allParties.push(mydata[i].Last_Party);
        }
    }

    var numSeats = []; // # of seats won by party (all assemblies)
    mydata.forEach(function (data) {
        var party = data.Party;
        if (data.Position === 1) {
            if (numSeats[party])
                numSeats[party]++;
            else
                numSeats[party] = 1;
        }
    });

    //generate colour range for parties
    var colourRange = randomColor({
        count: allParties.length,
        luminosity: 'dark',
        format: 'rgb' // e.g. 'rgb(225,200,20)'
    });

    //dict of party and colour
    var partyColours = {};
    for (i = 0; i < allParties.length; i++) {
        var party = allParties[i];
        partyColours[party] = (fixedPartyColours[party]) ? fixedPartyColours[party] : colourRange[i];
    }

    var generateGraph = function(mydata, assemblyNo, nums, wonlost, turncoats, searchTerm) {

        LOG ('generating graph with ' + mydata.length + ' rows for assembly#' + assemblyNo + ' labels ' + nums + ' wonlost=' + wonlost + ' turncoats=' + turncoats);

        var div = d3.select("body").append("div")
            .attr("class", "tooltip")
            .style("opacity", 0);

        //get current assembly rows
        var currentAssembly;
        {
            currentAssembly = mydata.filter(function (i) {
                return i.Assembly_No === parseInt(assemblyNo);
            });

            if (wonlost == 1) {
                var currentAssembly = currentAssembly.filter(function (i) {
                    return i.Position === 1;
                });
            } else if (wonlost == 2) {
                var currentAssembly = currentAssembly.filter(function (i) {
                    return i.Position > 1;
                });
            }

            if (turncoats == 1) {
                var currentAssembly = currentAssembly.filter(function (i) {
                    return i.Turncoat === 'FALSE';
                });
            } else if (turncoats == 0) {
                var currentAssembly = currentAssembly.filter(function (i) {
                    return i.Contested > 1;
                });
            }

            LOG('filtered rows ' + currentAssembly.length);
        }

        // get parties in these rows and sort them in alphabetical order
        var parties = [];
        {
            var lookup = {};
            for (var i = 0; i < currentAssembly.length; i++) {
                var Party = currentAssembly[i].Party;
                if (!(Party in lookup)) {
                    lookup[Party] = 1;
                    parties.push(Party);
                }
            }

            // sort by the # of seats
            parties = parties.sort(function(a,b) {
                if (a == 'Other') { return 1; }
                else if (b == 'Other') { return -1; }
                else return numSeats[b] - numSeats[a]
            });
        }

        //get current assembly entries by party
        var partywise = [];
        {
            for (i = 0; i < parties.length; i++) {
                var currentParty = parties[i];
                var currentEntries = currentAssembly.filter(function(i) {
                    if (currentParty === 'IND') {
                        return (i.Party === currentParty && i.Last_Party !== 'None'); // If Ind, filter out candidates whose last_party is None
                    }
                    else {
                        return (i.Party === currentParty);
                    }
                });
                // currentEntries.sort(function(a,b) {return (a.Last_Party > b.Last_Party) ? 1 : ((b.Last_Party > a.Last_Party) ? -1 : 0);} );
                partywise.push(currentEntries);
            }
        }

        //sort other by last party and move 'none' to end
        if (parties.indexOf('Other') !== -1) {
            partywise[parties.indexOf('Other')].sort(function(a,b) {return (a.Last_Party > b.Last_Party) ? 1 : ((b.Last_Party > a.Last_Party) ? -1 : 0);} );
            partywise[parties.indexOf('Other')].forEach(function(v, i) {
                if (v.Last_Party == 'None') {
                    partywise[parties.indexOf('Other')].push(partywise[parties.indexOf('Other')][i]);
                    partywise[parties.indexOf('Other')].splice(i, 1);
                }
            });
            partywise[parties.indexOf('Other')].forEach(function(v, i) {
                if (v.Position === 1) {
                    var a = partywise[parties.indexOf('Other')].splice(i,1);   // removes the item
                    partywise[parties.indexOf('Other')].unshift(a[0]);         // adds it back to the beginning
                }
            });
        }

        partywise.forEach(function(e) {
            e.sort(function(a, b) {
                if (a.Last_Party === b.Last_Party) {
                    return b.No_Mandates - a.No_Mandates;
                }
                else if (a.Last_Party > b.Last_Party) {
                    return 1;
                }
                else if (a.Last_Party < b.Last_Party) {
                    return -1;
                }
            });
        });

        var xMax = 0;
        for (var k = 0; k < partywise.length; k++) {
            if (partywise[k].length > xMax) {
                xMax = partywise[k].length;
            }
        }

        // set legend parties, colors and colorscale
        {
            var legendParties = [];
            for (i = 0; i < partywise.length; i++) {
                if (partywise[i][0]) {
                    legendParties.push(partywise[i][0].Party)
                }
                else {
                    partywise.splice(i, 1)
                }
            }

            //get colour range for legend parties
            var legendColours = [];
            for (i = 0; i < legendParties.length; i++) {
                legendColours.push(partyColours[legendParties[i]])
            }

            //declare colour scale
            var colourScale = d3.scaleOrdinal()
                .domain(legendParties)
                .range(legendColours);
        }

        var symbolSize = 180;
        var rowMax = 5;
        var topMargin = 30;
        var squareDim = 12, // pixel dimensions of square
            // width = xMax * 17; // horizontal
            // height = partywise.length * (squareDim + 2); // horizontal
            width = (rowMax + 1) * partywise.length * (Math.sqrt(symbolSize) + 3); // horizontal
        var height = (topMargin + xMax/rowMax) * (Math.sqrt(symbolSize));
        //var height = 1000 //vertical
        var legendMargin = 300;

        var svg = d3.select('#viz')
            .append('svg')
            .attr('width', width+legendMargin)
            .attr('height', height);

        //generate shapes
        var col = -rowMax;
        var row = -1;
        for (var k=0; k<partywise.length; k++) {
            svg.selectAll('u')
                .data(partywise[k])
                .enter()
                .append('path')
                .attr('d', d3.symbol().type(function(d) { return (d.Position === 1) ? d3.symbolSquare : d3.symbolCircle;})
                .size(function(d, i) { return symbolSize*0.4; }))
                .attr("transform", function(d, i) {
                    if (i == 0) {
                        col += rowMax + 1;
                    }
                    var x = ((i % rowMax) + col) * (symbolSize / 11);
                    if (i % rowMax == 0 && i != 0) {
                        row += 1;
                    }
                    if (i == 0) {
                        row = 0;
                    }
                    var y = (row + 3) * (symbolSize/10);
                    return "translate(" + x + "," + y + ")"
                })
                .attr("currentParty", function(d, i) {
                    if (typeof(d) === 'undefined') {
                        return 0;
                    }
                    else if (d.Party === 'Other') {
                        return d.Oth_Current;
                    }
                    else {
                        return (partyNames[d.Party]);}
                })
                .attr("lastParty", function(d, i) {
                    if (typeof(d) === 'undefined') {
                        return 0;
                    }
                    if (typeof(d.Last_Party) === 'undefined') {
                        return ('None');
                    }
                    else if (d.Last_Party === 'Other') {
                        return d.Oth_Last;
                    }
                    else {
                        return partyNames[d.Last_Party];}
                })
                .attr("name", function(d, i) {
                    if (typeof(d) === 'undefined') {
                        return 0;
                    }
                    else {
                        return (d.Candidate);}
                })
                .attr("acName", function(d, i){
                    return d.Constituency_Name;
                })
                .attr("position", function(d, i){
                    return d.Position;
                })
                .style("fill", function(d, i) {
                    if (typeof(d) === 'undefined') {
                        return 0;
                    }
                    if (typeof(d.Last_Party) === 'undefined') {
                        return partyColours['None'];
                    }
                    else {
                        return partyColours[d.Last_Party];
                    }
                })
                //.style('opacity', 0.5)
                .on("mouseover", function(d, i) {
                    d3.select(this)
                        .transition()
                        .duration(200)
                        .style('opacity', 0.5);
                    div.transition()
                        .duration(200)
                        .style("opacity", .9);
                    div .html(function() {
                        var candHistory = mydata.filter(function(k) {
                            return (k.pid == d.pid && d.Assembly_No > k.Assembly_No);
                        });

                        LOG(candHistory);

                        candHistory.sort(function(a,b) {return b.Assembly_No - a.Assembly_No});
                        var tooltipText = d.Candidate + "<hr>" + d.Constituency_Name + ", " + d.Assembly_No + ", " + d.Oth_Current + ", #" + d.Position;
                        candHistory.forEach(function(k) {
                            tooltipText += "<br/>" + k.Constituency_Name + ", " + k.Assembly_No + ", " + k.Oth_Current + ", #" + k.Position
                        });
                        return tooltipText;
                    })
                        .style("left", (d3.event.pageX) + "px")
                        .style("top", (d3.event.pageY - 28) + "px");
                })
                .on("mouseout", function(d) {
                    d3.select(this)
                        .transition()
                        .duration(200)
                        .style('opacity', 1);
                    div.transition()
                        .duration(500)
                        .style("opacity", 0);
                });
        }

        //generate search shapes
        var pattern = new RegExp(searchTerm, 'i');
        col = -rowMax;
        row = -1;
        for (k=0; k<partywise.length; k++) {
            svg.selectAll('u')
                .data(partywise[k])
                .enter()
                .append('path')
                .attr('d', d3.symbol().type(function(d) {
                    if (d.Position === 1) {
                        return d3.symbolSquare;
                    }
                    else {
                        return d3.symbolCircle;
                    }
                })
                    .size(function(d, i) {
                        return symbolSize;
                    }))
                .attr("transform", function(d, i) {
                    if (i === 0) {
                        col += rowMax + 1;
                    }
                    var x = ((i % rowMax) + col) * (symbolSize / 11);
                    if (i % rowMax === 0 && i !== 0) {
                        row += 1;
                    }
                    if (i === 0) {
                        row = 0;
                    }
                    var y = (row + 3) * (symbolSize/10);
                    return "translate(" + x + "," + y + ")"
                })
                .attr("currentParty", function(d, i) {
                    if (typeof(d) === 'undefined') {
                        return 0;
                    }
                    else if (d.Party === 'Other') {
                        return d.Oth_Current;
                    }
                    else {
                        return (partyNames[d.Party]);}
                })
                .attr("lastParty", function(d, i) {
                    if (typeof(d) === 'undefined') {
                        return 0;
                    }
                    if (typeof(d.Last_Party) === 'undefined') {
                        return ('None');
                    }
                    else if (d.Last_Party === 'Other') {
                        return d.Oth_Last;
                    }
                    else {
                        return partyNames[d.Last_Party];}
                })
                .style("fill", function(d, i) {
                    if (typeof(d) === 'undefined') {
                        return 0;
                    }
                    if (typeof(d.Last_Party) === 'undefined') {
                        return partyColours['None'];
                    }
                    if (!pattern.test(d.Candidate) && !pattern.test(d.Constituency_Name)) {
                        return '#dddddd'; // ''#ffffff'
                    }
                    else {
                        return partyColours[d.Last_Party];
                    }
                })
                .on("mouseover", function(d, i) {
                    d3.select(this)
                        .transition()
                        .duration(200)
                        .style('opacity', 0.5);
                    div.transition()
                        .duration(200)
                        .style("opacity", .9);
                    div .html(function() {
                        var candHistory = mydata.filter(function(k) {
                            return (k.pid === d.pid && d.Assembly_No > k.Assembly_No);
                        });
                        candHistory.sort(function(a,b) {return b.Assembly_No - a.Assembly_No});
                        var tooltipText = d.Candidate + ''
                            + '<img class="profile-pic" src="https://yt3.ggpht.com/a/AGF-l7-nUWLbveRF3WkZDUeLBuJCt6JsZFJr3SCg3g=s288-mo-c-c0xffffffff-rj-k-no"/>'
                            + "<hr>" + d.Constituency_Name + ", " + d.Assembly_No + ", " + d.Oth_Current + ", #" + d.Position;
                        candHistory.forEach(function(k) {
                            tooltipText += "<br/>" + k.Constituency_Name + ", " + k.Assembly_No + ", " + k.Oth_Current + ", #" + k.Position
                        });
                        return tooltipText;
                    })
                        .style("left", (d3.event.pageX) + "px")
                        .style("top", (d3.event.pageY - 28) + "px");
                })
                .on("mouseout", function(d) {
                    d3.select(this)
                        .transition()
                        .duration(200)
                        .style('opacity', 1);
                    div.transition()
                        .duration(500)
                        .style("opacity", 0);
                });
        }

        //generate text
        if (nums > 0) {
            col = -rowMax;
            row = -1;
            letterArray = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'];
            for (j = 0; j < partywise.length; j++) {
                svg.selectAll(letterArray[j])
                    .data(partywise[j])
                    .enter()
                    .append('text')
                    .attr("x", function(d, i) {
                        if (i === 0) {
                            col += rowMax + 1;
                        }
                        if (nums === 2) {
                            if (d.Contested > 9) {
                                return ((i % rowMax) + col) * (symbolSize / 11) - 7;
                            }
                            else {
                                return ((i % rowMax) + col) * (symbolSize / 11) - 4;
                            }
                        }
                        if (nums === 1) {
                            if (d.No_Mandates > 9) {
                                return ((i % rowMax) + col) * (symbolSize / 11) - 7;
                            }
                            else {
                                return ((i % rowMax) + col) * (symbolSize / 11) - 3;
                            }
                        }

                    })
                    .attr("y", function(d, i) {
                        if (i % rowMax === 0 && i !== 0) {
                            row += 1;
                        }
                        if (i == 0) {
                            row = 0;
                        }
                        return (row + 3) * (symbolSize/10) + 4;
                    })
                    .text(function(d) {
                        if (nums == 2) {
                            if (d.Contested > 1) {
                                return d.Contested;
                            }
                        }
                        if (nums == 1) {
                            if (d.No_Mandates > 0) {
                                return d.No_Mandates;
                            }
                        }
                    })
                    .attr("currentParty", function(d, i) {
                        if (typeof(d) === 'undefined') {
                            return 0;
                        }
                        else if (d.Party === 'Other') {
                            return d.Oth_Current;
                        }
                        else {
                            return (partyNames[d.Party]);}
                    })
                    .attr("lastParty", function(d, i) {
                        if (typeof(d) === 'undefined') {
                            return 0;
                        }
                        if (typeof(d.Last_Party) === 'undefined') {
                            return ('None');
                        }
                        else if (d.Last_Party === 'Other') {
                            return d.Oth_Last;
                        }
                        else {
                            return partyNames[d.Last_Party];}
                    })
                    .attr("name", function(d, i) {
                        if (typeof(d) === 'undefined') {
                            return 0;
                        }
                        else {
                            return (d.Candidate);}
                    })
                    .attr("acName", function(d, i){
                        return d.Constituency_Name;
                    })
                    .attr("position", function(d, i){
                        return d.Position;
                    })
                    .style("fill", 'white')
                    .style("font-family", 'Montserrat')
                    .style("font-size", function(d) {
                        if (nums == 2) {
                            if (d.Contested > 9) {
                                return '10px';
                            }
                            else {
                                return '11px';
                            }
                        }
                        if (nums == 1) {
                            if (d.No_Mandates > 9) {
                                return '10px';
                            }
                            else {
                                return '11px';
                            }
                        }
                    })
                    .on("mouseover", function(d, i) {
                        div.transition()
                            .duration(200)
                            .style("opacity", .9);
                        div .html(function() {
                            var candHistory = mydata.filter(function(k) {
                                return (k.pid == d.pid && d.Assembly_No > k.Assembly_No);
                            });
                            LOG(candHistory);
                            candHistory.sort(function(a,b) {return b.Assembly_No - a.Assembly_No});
                            var tooltipText = d.Candidate + "<hr>" + d.Constituency_Name + ", " + d.Assembly_No + ", " + d.Oth_Current + ", #" + d.Position;
                            candHistory.forEach(function(k) {
                                tooltipText += "<br/>" + k.Constituency_Name + ", " + k.Assembly_No + ", " + k.Oth_Current + ", #" + k.Position
                            });
                            return tooltipText;
                        })
                            .style("left", (d3.event.pageX) + "px")
                            .style("top", (d3.event.pageY - 28) + "px");
                    })
                    .on("mouseout", function(d) {
                        div.transition()
                            .duration(500)
                            .style("opacity", 0);
                    });
            }
        }

        //create legend
        svg.append("g")
            .attr("class", "legendOrdinal")
            .attr("transform", "translate(" + width/(partywise.length*2.5) +"," + 5 + ")");

        var legendOrdinal = d3.legendColor()
            .orient('horizontal')
            //.shape("path", d3.symbol().type(d3.symbolTriangle).size(100)())
            .shapeWidth(25)
            .shapePadding(width/(partywise.length*1.35))
            .scale(colourScale);

        svg.select(".legendOrdinal")
            .call(legendOrdinal);

        //create symbol legend
        circle = d3.symbol().type(d3.symbolCircle).size(200)();
        square = d3.symbol().type(d3.symbolSquare).size(200)();

        var symbolScale =  d3.scaleOrdinal()
            .domain(['Winner','Loser'])
            .range([square, circle] );

        svg.append("g")
            .attr("class", "legendSymbol")
            .attr("transform", "translate("+ (width + 20) +", 100)");

        var legendPath = d3.legendSymbol()
            .scale(symbolScale)
            .orient("vertical");

        svg.select(".legendSymbol")
            .call(legendPath);
    };


    var refresh = function() {
        var assemblyNo = $('#assemblies').val();
        var nums = $('#numbers').val();
        var wonlost = $('#wonlost').val();
        var turncoats = $('#turncoats').val();
        var searchTerm = $('#search').val();
        d3.selectAll("svg").transition().duration(400).style("opacity", 0).remove();
        generateGraph(mydata, assemblyNo, nums, wonlost,turncoats,searchTerm);
    };

    // handle on click event
    $('#assemblies,#numbers,#wonlost,#turncoats,#search').on('change', refresh);
//    generateGraph(mydata, 17, 1, 1, 2, '');
    refresh();

});
