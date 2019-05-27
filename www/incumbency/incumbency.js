
var params = (new URL(document.location)).searchParams;
var assemblyNo = params.get("a");
if (!assemblyNo)
    assemblyNo = 17;

$('.assembly-number').html(assemblyNo == 3 ? "3rd" : (assemblyNo + "th"));

var url = './ge-incumbency-' + assemblyNo + '.csv'; //change json source here

var pids_url = './pids.csv';

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
fixedPartyColours['SHS'] = '#e80839';
fixedPartyColours['DMK'] = '#08ded0';
fixedPartyColours['BJD'] = '#015275';
fixedPartyColours['YSRCP'] = '#930227';
fixedPartyColours['AITC'] = '#00137f';
fixedPartyColours['TRS'] = '#c40da5';
fixedPartyColours['Other'] = '#000';

// these are the most successful parties in LS-17
// top parties have their own column in the viz. all others are clubbed into "Other"
var topParties = ['BJP', 'INC', 'AITC', 'DMK', 'SHS', 'YSRCP', 'TRS', 'BJD']; // , 'SP', 'BSP',

function commatize(nStr) {
    if (!nStr)
        return '';
    nStr += '';
    var x = nStr.split('.');
    var x1 = x[0];
    var x2 = x.length > 1 ? '.' + x[1] : '';
    var rgx = /(\d+)(\d{3})/;
    while (rgx.test(x1)) {
        x1 = x1.replace(rgx, '$1' + ',' + '$2');
    }
    return x1 + x2;
}

// read the pids file and then the raw data file
d3.csv(pids_url, function(pids_data) {

    d3.csv(url, function (data) {

        function isInArray(value, array) {
            return array.indexOf(value) > -1;
        }
        function isTopParty(p) {
            return isInArray(p, topParties);
        }

        var allRows = data; // data.rows;

        // check data and convert strings to ints
        allRows.forEach(function (d) {
            d.Position = parseInt(d.Position);
            d.No_Mandates = parseInt(d.No_Mandates);
            d.Contested = parseInt(d.Contested);
            d.Assembly_No = parseInt(d.Assembly_No);
            d.No_Mandates = parseInt (d.Terms);
            d.Contested = parseInt (d.Contested);
            d.Year = parseInt(d.Year);
            // enable these rows if we want to show more info in the person's info box
//            d.Votes = parseInt(d.Votes);
//            d.Margin = parseInt(d.Margin);
//            d.Age = parseInt (d.Age);
        });

        //get list of all parties
        var allParties = [];
        for (var i = 0; i < allRows.length; i++) {
            if (!isInArray(allRows[i].Party, allParties)) {
                allParties.push(allRows[i].Party);
            }
            if (!isInArray(allRows[i].Last_Party, allParties)) {
                allParties.push(allRows[i].Last_Party);
            }
        }

        // # of seats won by party (all assemblies, not just the one being shown). This will be used for generating the sort order of parties.
        var numSeats = {'Other': 0};
        allRows.forEach(function (data) {
            var party = data.Party;
            if (data.Position === 1) {
                if (numSeats[party])
                    numSeats[party]++;
                else
                    numSeats[party] = 1;
            }
        });

        // if Last_Party is not set, set it to the same as Party, so the color of the box remains the same as their party
        allRows.forEach(function (row) {
            // last party is not set for someone's first election.
            if (!row.Last_Party) {
                row.Last_Party = row.Party;
            }
        });

        // If not top party, change party and last_party of a row to Other.
        // but save these fields in Oth_Current and Oth_last, so we can show the info accurately on hover
        allRows.forEach(function (row) {
            row.Oth_Current = row.Party;
            row.Oth_Last = row.Last_Party;
            if (!isTopParty(row.Party)) {
                row.Oth_Current = row.Party;
                row.Party = 'Other';
            }
            if (!isTopParty(row.Last_Party)) {
                row.Oth_Last = row.Last_Party;
                row.Last_Party = 'Other';
            }
        });

        //generate colour range for parties (after Other has been set for the non-top parties)
        {
            var colourRange = randomColor({
                count: allParties.length,
                luminosity: 'dark',
                format: 'rgb' // e.g. 'rgb(225,200,20)'
            });

            //dict of party and colour
            var partyColours = {};
            for (var i = 0; i < allParties.length; i++) {
                var party = allParties[i];
                partyColours[party] = (fixedPartyColours[party]) ? fixedPartyColours[party] : colourRange[i];
            }
        }

        var generateGraph = function (mydata, assemblyNo, labels, wonlost, turncoats, searchTerm) {

            function do_mouseover(d) {
                var tooltip = d3.select('.tooltip');
                tooltip.transition().duration(200).style("opacity", 1.0);
                tooltip.html(function () {
                        function string_for_row(row) {
                            var win_or_lose_class = row.Position === 1 ? 'won' : 'lost';
                            var result = '<span class="' + win_or_lose_class + '">' + row.Constituency_Name + " (" + row.Year + ") " + row.Oth_Current + ", #" + row.Position + '</span>';
                            if (row.Poll_No > 0) {
                                result += '<span class="bypoll">BYE POLL</span>';
                            }
                            return result;
                        }

                        // get the img link - first matching link in pids table, or empty if no match
                        var img_link = '';
                        var pid = d.pid;
                        for (var x = 0; x < pids_data.length; x++) {
                            if (pids_data[x].pid === pid) {
                                img_link = pids_data[x].link;
                                break;
                            }
                        }

                        // add the initial tooltip
                        var tooltipText = '<img class="profile-pic" src="' + img_link + '"/> ' + '<br/>';
                            tooltipText += '<span class="cand-name">' + d.Candidate.toUpperCase() + '</span><br/>';
                            tooltipText += string_for_row(d) + '<br/>';
                            if (d.MyNeta_age)
                                tooltipText += d.MyNeta_age + ' years<br/>';
                            // tooltipText += '<i>Votes</i>: ' + commatize(d.Votes) + ' (' + d.Vote_Share_Percentage + '%) <br/>';
                            // tooltipText += '<i>Margin</i>: ' + commatize(d.Margin) + ' (' + d.Margin_Percentage + '%) <br/>';

                        // then add the history. This is only the history in prev. assemblies.
                        // note this is history on all rows, not just currently filtered rows
                        // Possible improvement: show in history if same cand. has contested another seat in the same assembly also.
                        var candHistory = mydata.filter(function (k) {
                            return (k.pid === d.pid && d.Assembly_No > k.Assembly_No);
                        });
                        candHistory.sort(function (a, b) {return b.Year - a.Year});
                        tooltipText += '<hr style="color:darkgray;background-color:darkgray;margin-bottom:3px;"/>';
                        candHistory.forEach(function (k) { tooltipText += string_for_row(k) + '<br/>'; });

                        LOG (tooltipText);
                        return tooltipText;
                    })
                    .style("left", (d3.event.pageX+5) + "px") // offset the tooltip location a bit from the event's pageX/Y
                    .style("top", (d3.event.pageY - 28) + "px");
            }

            function do_mouseout() {
                var tooltip = d3.select('.tooltip');
                tooltip.transition().duration(500).style("opacity", 0);
            }

            // actual code for generateGraph begins
            LOG('generating graph with ' + mydata.length + ' rows for assembly#' + assemblyNo + ' labels ' + labels + ' wonlost=' + wonlost + ' turncoats=' + turncoats);

            // get current assembly rows
            var filteredRows;
            {
                filteredRows = mydata.filter(function (i) {
                    return i.Assembly_No === parseInt(assemblyNo);
                });

                if (wonlost === "1") {
                    filteredRows = filteredRows.filter(function (i) {
                        return i.Position === 1;
                    });
                } // else all candidates, do nothing

                if (turncoats === "1") {
                    filteredRows = filteredRows.filter(function (i) {
                        return i.Turncoat === 'FALSE';
                    });
                } else if (turncoats === "0") {
                    filteredRows = filteredRows.filter(function (i) {
                        return i.Contested > 1;
                    });
                }

                LOG('filtered rows ' + filteredRows.length);
            }

            // get parties in these rows and sort them by importance (# seats won in this dataset)
            var parties = [];
            {
                var lookup = {};
                for (var i = 0; i < filteredRows.length; i++) {
                    var Party = filteredRows[i].Party;
                    if (!(Party in lookup)) {
                        lookup[Party] = 1;
                        parties.push(Party);
                    }
                }

                // sort by the # of seats. But other's count is set to 0, so it is always shown last
                parties = parties.sort(function (a, b) {
                    return numSeats[b] - numSeats[a]
                });
            }

            //get current assembly entries by party
            var partywise = [];
            {
                for (i = 0; i < parties.length; i++) {
                    var currentParty = parties[i];
                    var currentEntries = filteredRows.filter(function (i) {
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
                    if (v.Last_Party === 'None') {
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

                // sort the boxes within a given party column
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

                    if (a.Position === 1 && b.Position > 1)
                        return -1;
                    else if (a.Position > 1 && b.Position === 1)
                        return 1;

                    // if no other difference, sort by # terms or terms_contested.
                    // IMP: Don't use # mandates here. Different rows for the same PID have different No_Mandates, but # Terms is the same
                    if (a.Terms !== b.Terms) {
                        return b.Terms - a.Terms;
                    }
                    return b.Terms_Contested - a.Terms_Contested;
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
            var width = (rowMax + 1) * partywise.length * (Math.sqrt(symbolSize) + 3); // horizontal
            var height = (topMargin + xMax / rowMax) * (Math.sqrt(symbolSize));
            //var height = 1000 //vertical
            var legendMargin = 300;

            var svg = d3.select('#viz')
                .append('svg')
                .attr('width', width + legendMargin)
                .attr('height', height);

            //generate shapes for this col
            var col = -rowMax;
            var row = -1;
            for (k = 0; k < partywise.length; k++) {
                svg.selectAll('u')
                    .data(partywise[k])
                    .enter()
                    .append('path')
                    .attr('d', d3.symbol().type(function (d) { return (d.Position === 1) ? d3.symbolSquare : d3.symbolCircle; })
                    .size(function () {return symbolSize;}))
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
                    .style("fill", function (d) {
                        var pattern = new RegExp(searchTerm, 'i');
                        // remember to test Oth_Current because the part we are looking for may be there
                        return (pattern.test(d.Candidate) || pattern.test(d.Constituency_Name) || pattern.test(d.Party) || pattern.test(d.Oth_Current)) ? partyColours[d.Last_Party] : '#dddddd';
                    })
                    //.style('opacity', 0.5)
                    .on("mouseover", do_mouseover)
                    .on("mouseout", do_mouseout);
            }

            // generate label
            if (labels !== 'NO_LABEL') {
                col = -rowMax;
                row = -1;
                var letterArray = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'];
                for (var j = 0; j < partywise.length; j++) {
                    svg.selectAll(letterArray[j])
                        .data(partywise[j])
                        .enter()
                        .append('text')
                        .attr("x", function (d, i) {
                            if (i === 0) {
                                col += rowMax + 1;
                            }
                            if (labels === 'TIMES_CONTESTED') {
                                if (d.Contested > 9) {
                                    return ((i % rowMax) + col) * (symbolSize / 11) - 5;
                                }
                                else {
                                    return ((i % rowMax) + col) * (symbolSize / 11) - 3;
                                }
                            }
                            if (labels === 'TIMES_WON') {
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
                            if (i === 0) {
                                row = 0;
                            }
                            return (row + 3) * (symbolSize / 10) + 4;
                        })
                        .text(function (d) {
                            if (labels === 'TIMES_CONTESTED') {
                                return d.Contested; // well, contested can't be == 0.
                            }
                            if (labels === 'TIMES_WON') {
                                if (d.No_Mandates > 0) {
                                    return d.No_Mandates;
                                }
                            }
                        })
                        .style("fill", 'white')
                        .style("font-family", 'Montserrat')
                        .style("font-size", function (d) {
                            if (labels === 'TIMES_CONTESTED') {
                                if (d.Contested > 9) {
                                    return '10px';
                                }
                                else {
                                    return '11px';
                                }
                            }
                            if (labels === 'TIMES_WON') {
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
            {
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

                // create symbol legend, but only if we're showing losers. no need to show it if we are only showing winners.
                if (wonlost === '2') {
                    // only show legend if we're showing all
                    var circle = d3.symbol().type(d3.symbolCircle).size(200)();
                    var square = d3.symbol().type(d3.symbolSquare).size(200)();

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
            }
        };

        var refresh = function () {
            //var assemblyNo = $('#assemblies').val();
            var labels = $('#label').val();
            var wonlost = $('#wonlost').val();
            var turncoats = $('#turncoats').val();
            var searchTerm = $('#search').val();
            //d3.selectAll("svg").transition().duration(400).style("opacity", 0).remove();
            d3.selectAll("svg").remove();
            generateGraph(allRows, assemblyNo, labels, wonlost, turncoats, searchTerm);
        };

        // handle on click event
        $('#assemblies,#label,#wonlost,#turncoats,#search').on('change', refresh);
        $('#search').on('keyup', refresh);

        refresh();

    });
});
