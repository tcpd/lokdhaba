# this script generates the json file needed by the incumbency visualization

library(data.table)
library(jsonlite)
library(readr)
filePath <- '~/tcpd_data/GE/Data/derived/mastersheet.csv'
data = fread(filePath)
#dt <- dt[dt$No_Mandates > 0]
dt <- dt[,c("Assembly_No", "Candidate", "State_Name", "Constituency_Name", "Party", "Last_Party", "pid", "Votes", "Position", "Contested", "No_Mandates", "Turncoat", "Incumbent")]

#ignore NOTAs
dt = dt[Party != 'NOTA' & Candidate != 'NOTA']

# filter dt down to only rows whose pid is present in this assembly
assembly = 17
this_assembly_pids = unique(dt[Assembly_No == assembly]$pid)
dt = dt[pid %in% this_assembly_pids]

fwrite(dt, file='all-ge.csv')
#rows_json = prettify(toJSON(dt))
#cat (rows_json, file='all-ge.csv')

# read the pics table and assign the link in the last row to that pid
pics = fread ('~/tcpd_data/GE/GE_19_working/pictures_with_pids.csv');
pid_table = pics[, .(link=.SD[nrow(.SD)]$link), by=.(pid)]
fwrite(pid_table, file='pids.csv')

pid_table_json = prettify(toJSON(pid_table))
cat (pid_table_json, file='pids.json')
