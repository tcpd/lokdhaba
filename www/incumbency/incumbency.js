var url = './rows-ge.csv'; //change json source here
var pids_url = './pids.csv'

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

function commatize(nStr) {
    nStr += '';
    x = nStr.split('.');
    x1 = x[0];
    x2 = x.length > 1 ? '.' + x[1] : '';
    var rgx = /(\d+)(\d{3})/;
    while (rgx.test(x1)) {
        x1 = x1.replace(rgx, '$1' + ',' + '$2');
    }
    return x1 + x2;
}

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

d3.csv(pids_url, function(pids_data) {

    d3.csv(url, function (data) {
        var mydata = data; // data.rows;
//    var pid_data = data.pids;

        // check data and convert strings to ints
        mydata.forEach(function (d, i) {
            d.Position = parseInt(d.Position);
            d.No_Mandates = parseInt(d.No_Mandates);
            d.Contested = parseInt(d.Contested);
            d.Assembly_No = parseInt(d.Assembly_No);
            d.No_Mandates = parseInt (d.Terms);
            d.Contested = parseInt (d.Contested);
            d.Year = parseInt(d.Year);
//            d.Votes = parseInt(d.Votes);
//            d.Margin = parseInt(d.Margin);
//            d.Age = parseInt (d.Age);
        });

        var topParties = ['BJP', 'INC', 'AITC', 'DMK', 'SHS', 'YSRCP', 'TRS', 'BJD']; // , 'SP', 'BSP',

        function isInArray(value, array) {
            return array.indexOf(value) > -1;
        }
        function isTopParty(p) {
            return isInArray(p, topParties);
        }

        for (var i = 0; i < mydata.length; i++) {
            if (!mydata[i].Last_Party) {
                mydata[i].Last_Party = mydata[i].Party;
            }

            if (typeof(mydata[i].Last_Party) === 'undefined') {
                mydata[i].Last_Party = 'None';
            }

            mydata[i].Oth_Current = mydata[i].Party;
            mydata[i].Oth_Last = mydata[i].Last_Party;
            if (!isTopParty(mydata[i].Party) && !isTopParty(mydata[i].Last_Party)) {
                mydata[i].Oth_Current = mydata[i].Party;
                mydata[i].Party = 'Other';
                mydata[i].Oth_Last = mydata[i].Last_Party;
                mydata[i].Last_Party = 'Other';
            }
            else if (!isTopParty(mydata[i].Party)) {
                mydata[i].Oth_Current = mydata[i].Party;
                mydata[i].Party = 'Other';
            }
            else if (!isTopParty(mydata[i].Last_Party)) {
                mydata[i].Oth_Last = mydata[i].Last_Party;
                mydata[i].Last_Party = 'Other';
            }
        }

        //get list of all parties
        var allParties = [];
        for (i = 0; i < mydata.length; i++) {
            if (!isInArray(mydata[i].Party, allParties)) {
                allParties.push(mydata[i].Party);
            }
            if (!isInArray(mydata[i].Last_Party, allParties)) {
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

        var generateGraph = function (mydata, assemblyNo, nums, wonlost, turncoats, searchTerm) {


            LOG('generating graph with ' + mydata.length + ' rows for assembly#' + assemblyNo + ' labels ' + nums + ' wonlost=' + wonlost + ' turncoats=' + turncoats);

            var div = d3.select("#viz").append("div")
                .attr("class", "tooltip")
                .style("opacity", 0);

            function do_mouseover(d, i) {
                div.transition().duration(200).style("opacity", 1.0);

                div.html(function () {
                    var candHistory = mydata.filter(function (k) {
                        return (k.pid === d.pid && d.Assembly_No > k.Assembly_No);
                    });
                    LOG(candHistory);
                    candHistory.sort(function (a, b) {return b.Year - a.Year});

                    // get the img link
                    var img_link = ''; //'http://164.100.47.193/mpimage/photo/4797.jpg'; // default
                    var pid = d.pid;
                    for (var x = 0; x < pids_data.length; x++) {
                        if (pids_data[x].pid === pid) {
                            img_link = pids_data[x].link;
                            break;
                        }
                    }

                    function string_for_row(k) {
                        var win_or_lose_class = k.Position === 1 ? 'won' : 'lost';
                        s = '<span class="' + win_or_lose_class + '">' + k.Constituency_Name + " (" + k.Year + ") " + k.Oth_Current + ", #" + k.Position + '</span>';
                        if (k.Poll_No > 0) {
                            s += '<span class="bypoll">BYE POLL</span>';
                        }
                        return s;
                    }

                    var tooltipText = '<img class="profile-pic" src="' + img_link + '"/> ' + '<br/>';
                    tooltipText += '<span class="cand-name">' + d.Candidate.toUpperCase() + '</span><br/>';
                    tooltipText += string_for_row(d) + '<br/>';
                    d.Constituency_Name + " (" + d.Year + ") " + d.Oth_Current + ", #" + d.Position + '<br/>';
                    tooltipText += d.MyNeta_age + ' years<br/>';
                    // tooltipText += '<i>Votes</i>: ' + commatize(d.Votes) + ' (' + d.Vote_Share_Percentage + '%) <br/>';
                    // tooltipText += '<i>Margin</i>: ' + commatize(d.Margin) + ' (' + d.Margin_Percentage + '%) <br/>';

                    tooltipText += '<hr style="color:darkgray;background-color:darkgray;margin-bottom:3px;"/>';
                    if (candHistory.length > 0) {
                        candHistory.forEach(function (k) {
                            tooltipText += string_for_row(k) + '<br/>';
                        });
                    }
                    LOG (tooltipText);
                    return tooltipText;
                })
                    .style("left", (d3.event.pageX) + "px")
                    .style("top", (d3.event.pageY - 28) + "px");
            }

            function do_mouseout(d) {
                div.transition().duration(500).style("opacity", 0);
            }

            //get current assembly rows
            var currentAssembly;
            {
                currentAssembly = mydata.filter(function (i) {
                    return i.Assembly_No === parseInt(assemblyNo);
                });

                if (wonlost === "1") {
                    currentAssembly = currentAssembly.filter(function (i) {
                        return i.Position === 1;
                    });
                } // else all candidates, do nothing

                if (turncoats === "1") {
                    currentAssembly = currentAssembly.filter(function (i) {
                        return i.Turncoat === 'FALSE';
                    });
                } else if (turncoats == 0) {
                    currentAssembly = currentAssembly.filter(function (i) {
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
                parties = parties.sort(function (a, b) {
                    if (a == 'Other') {
                        return 1;
                    }
                    else if (b == 'Other') {
                        return -1;
                    }
                    else return numSeats[b] - numSeats[a]
                });
            }

            //get current assembly entries by party
            var partywise = [];
            {
                for (i = 0; i < parties.length; i++) {
                    var currentParty = parties[i];
                    var currentEntries = currentAssembly.filter(function (i) {
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
                partywise[parties.indexOf('Other')].sort(function (a, b) {
                    return (a.Last_Party > b.Last_Party) ? 1 : ((b.Last_Party > a.Last_Party) ? -1 : 0);
                });
                partywise[parties.indexOf('Other')].forEach(function (v, i) {
                    if (v.Last_Party == 'None') {
                        partywise[parties.indexOf('Other')].push(partywise[parties.indexOf('Other')][i]);
                        partywise[parties.indexOf('Other')].splice(i, 1);
                    }
                });
                partywise[parties.indexOf('Other')].forEach(function (v, i) {
                    if (v.Position === 1) {
                        var a = partywise[parties.indexOf('Other')].splice(i, 1);   // removes the item
                        partywise[parties.indexOf('Other')].unshift(a[0]);         // adds it back to the beginning
                    }
                });
            }

            partywise.forEach(function (party_rows) {
                var party_count_for_last_party = [];
                party_rows.forEach(function(d) {
                    if (party_count_for_last_party[d.Last_Party])
                        party_count_for_last_party[d.Last_Party]++;
                    else
                        party_count_for_last_party[d.Last_Party] = 1;
                });

                party_count_for_last_party['Other'] = -1; // let this rank at the bottom

                party_rows.sort(function (a, b) {
                    // rows with last_party = higher count will come before rows with last_party = lower count
                    if (party_count_for_last_party[a.Last_Party] !== party_count_for_last_party[b.Last_Party])
                        return party_count_for_last_party[b.Last_Party] - party_count_for_last_party[a.Last_Party];

                    // rows_with_last_party is same for a and b (may or may not be the same last_party)
                    // if so, sort partywise in alpha order
                    if (a.Last_Party > b.Last_Party) { return 1; }
                    else if (a.Last_Party < b.Last_Party) { return -1;}

                    // last_party for a and b is the same
                    // put winners before losers

                    if (a.Position == 1 && b.Position > 1)
                        return -1;
                    else if (a.Position > 1 && b.Position == 1)
                        return 1;

                    // if no other difference, sort by # mandates
                    return b.No_Mandates - a.No_Mandates;
                });
                    //
                    // party_rows.sort(function (a, b) {
                    // if (a.Last_Party === b.Last_Party) {
                    //     return b.No_Mandates - a.No_Mandates;
                    // }
                    // else if (a.Last_Party > b.Last_Party) {
                    //     return 1;
                    // }
                    // else if (a.Last_Party < b.Last_Party) {
                    //     return -1;
                    // }
                // });
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
            var height = (topMargin + xMax / rowMax) * (Math.sqrt(symbolSize));
            //var height = 1000 //vertical
            var legendMargin = 300;

            var svg = d3.select('#viz')
                .append('svg')
                .attr('width', width + legendMargin)
                .attr('height', height);

            //generate shapes
            var col = -rowMax;
            var row = -1;
            for (var k = 0; k < partywise.length; k++) {
                svg.selectAll('u')
                    .data(partywise[k])
                    .enter()
                    .append('path')
                    .attr('d', d3.symbol().type(function (d) {
                        return (d.Position === 1) ? d3.symbolSquare : d3.symbolCircle;
                    })
                        .size(function (d, i) {
                            return symbolSize * 0.4;
                        }))
                    .attr("transform", function (d, i) {
                        if (i === 0) {
                            col += rowMax + 1;
                        }
                        var x = ((i % rowMax) + col) * (symbolSize / 11);
                        if (i % rowMax == 0 && i != 0) {
                            row += 1;
                        }
                        if (i == 0) {
                            row = 0;
                        }
                        var y = (row + 3) * (symbolSize / 10);
                        return "translate(" + x + "," + y + ")"
                    })
                    .attr("currentParty", function (d, i) {
                        if (typeof(d) === 'undefined') {
                            return 0;
                        }
                        else if (d.Party === 'Other') {
                            return d.Oth_Current;
                        }
                        else {
                            return (partyNames[d.Party]);
                        }
                    })
                    .attr("lastParty", function (d, i) {
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
                            return partyNames[d.Last_Party];
                        }
                    })
                    .attr("name", function (d, i) {
                        if (typeof(d) === 'undefined') {
                            return 0;
                        }
                        else {
                            return (d.Candidate);
                        }
                    })
                    .attr("acName", function (d, i) {
                        return d.Constituency_Name;
                    })
                    .attr("position", function (d, i) {
                        return d.Position;
                    })
                    .style("fill", function (d, i) {
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
                    .on("mouseover", do_mouseover)
                    .on("mouseout", do_mouseout);
            }

            //generate search shapes
            var pattern = new RegExp(searchTerm, 'i');
            col = -rowMax;
            row = -1;
            for (k = 0; k < partywise.length; k++) {
                svg.selectAll('u')
                    .data(partywise[k])
                    .enter()
                    .append('path')
                    .attr('d', d3.symbol().type(function (d) {
                        if (d.Position === 1) {
                            return d3.symbolSquare;
                        }
                        else {
                            return d3.symbolCircle;
                        }
                    })
                        .size(function (d, i) {
                            return symbolSize;
                        }))
                    .attr("transform", function (d, i) {
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
                        var y = (row + 3) * (symbolSize / 10);
                        return "translate(" + x + "," + y + ")"
                    })
                    .attr("currentParty", function (d, i) {
                        if (typeof(d) === 'undefined') {
                            return 0;
                        }
                        else if (d.Party === 'Other') {
                            return d.Oth_Current;
                        }
                        else {
                            return (partyNames[d.Party]);
                        }
                    })
                    .attr("lastParty", function (d, i) {
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
                            return partyNames[d.Last_Party];
                        }
                    })
                    .style("fill", function (d, i) {
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
                    .on("mouseover", do_mouseover)
                    .on("mouseout", do_mouseout);
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
                        .attr("x", function (d, i) {
                            if (i === 0) {
                                col += rowMax + 1;
                            }
                            if (nums == 2) {
                                if (d.Contested > 9) {
                                    return ((i % rowMax) + col) * (symbolSize / 11) - 5;
                                }
                                else {
                                    return ((i % rowMax) + col) * (symbolSize / 11) - 3;
                                }
                            }
                            if (nums == 1) {
                                if (d.No_Mandates > 9) {
                                    return ((i % rowMax) + col) * (symbolSize / 11) - 5;
                                }
                                else {
                                    return ((i % rowMax) + col) * (symbolSize / 11) - 3;
                                }
                            }

                        })
                        .attr("y", function (d, i) {
                            if (i % rowMax === 0 && i !== 0) {
                                row += 1;
                            }
                            if (i == 0) {
                                row = 0;
                            }
                            return (row + 3) * (symbolSize / 10) + 4;
                        })
                        .text(function (d) {
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
                        .attr("currentParty", function (d, i) {
                            if (typeof(d) === 'undefined') {
                                return 0;
                            }
                            else if (d.Party === 'Other') {
                                return d.Oth_Current;
                            }
                            else {
                                return (partyNames[d.Party]);
                            }
                        })
                        .attr("lastParty", function (d, i) {
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
                                return partyNames[d.Last_Party];
                            }
                        })
                        .attr("name", function (d, i) {
                            if (typeof(d) === 'undefined') {
                                return 0;
                            }
                            else {
                                return (d.Candidate);
                            }
                        })
                        .attr("acName", function (d, i) {
                            return d.Constituency_Name;
                        })
                        .attr("position", function (d, i) {
                            return d.Position;
                        })
                        .style("fill", 'white')
                        .style("font-family", 'Montserrat')
                        .style("font-size", function (d) {
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
                        .on("mouseover", do_mouseover)
                        .on("mouseout", do_mouseout);
                }
            }

            //create legend
            svg.append("g")
                .attr("class", "legendOrdinal")
                .attr("transform", "translate(" + width / (partywise.length * 2.5) + "," + 5 + ")");

            var legendOrdinal = d3.legendColor()
                .orient('horizontal')
                //.shape("path", d3.symbol().type(d3.symbolTriangle).size(100)())
                .shapeWidth(25)
                .shapePadding(width / (partywise.length * 1.35))
                .scale(colourScale);

            svg.select(".legendOrdinal")
                .call(legendOrdinal);

            //create symbol legend
            if (wonlost === '2') {
                // only show legend if we're showing all
                circle = d3.symbol().type(d3.symbolCircle).size(200)();
                square = d3.symbol().type(d3.symbolSquare).size(200)();

                var symbolScale = d3.scaleOrdinal()
                    .domain(['Winner', 'Loser'])
                    .range([square, circle]);

                svg.append("g")
                    .attr("class", "legendSymbol")
                    .attr("transform", "translate(" + (width + 20) + ", 100)");

                var legendPath = d3.legendSymbol()
                    .scale(symbolScale)
                    .orient("vertical");

                svg.select(".legendSymbol")
                    .call(legendPath);
            }
        };


        var refresh = function () {
            var assemblyNo = $('#assemblies').val();
            var nums = $('#numbers').val();
            var wonlost = $('#wonlost').val();
            var turncoats = $('#turncoats').val();
            var searchTerm = $('#search').val();
            //d3.selectAll("svg").transition().duration(400).style("opacity", 0).remove();
            d3.selectAll("svg").remove();
            generateGraph(mydata, assemblyNo, nums, wonlost, turncoats, searchTerm);
        };

        // handle on click event
        $('#assemblies,#numbers,#wonlost,#turncoats,#search').on('change', refresh);
        $('#search').on('keyup', refresh);

//    generateGraph(mydata, 17, 1, 1, 2, '');
        refresh();

    });
});
