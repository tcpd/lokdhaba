# this script generates the csv files (pids.csv and rows.csv) needed by the incumbency visualization

library(data.table)
library(jsonlite)
library(readr)
filePath <- '~/tcpd_data/GE/Data/derived/mastersheet.csv'
filePath = '~/tcpd_data/GE/GE_19_working/mastersheet_with_adr_data.csv'
data = fread(filePath)
#dt <- dt[dt$No_Mandates > 0]
dt <- data[Party != 'NOTA' & Candidate != 'NOTA', c("Assembly_No", "Year", "Candidate", "State_Name", "Constituency_Name", "Party", "Last_Party", "pid", "Votes", "Sex", "Position", "Contested", "No_Mandates", "Turncoat", "Incumbent", "Vote_Share_Percentage", "Margin", "Margin_Percentage", "MyNeta_age")]

# filter dt down to only rows whose pid is present in this assembly
assembly = 17
this_assembly_pids = unique(dt[Assembly_No == assembly]$pid)
dt = dt[pid %in% this_assembly_pids]

terms_served_by_pid = dt[Position == 1, .(Terms=length(unique(Assembly_No))), by=c('pid')]
dt = merge (dt, terms_served_by_pid, by=c('pid'))

terms_contested_by_pid = dt[, .(Terms_Contested=length(unique(Assembly_No))), by=c('pid')]
dt = merge (dt, terms_contested_by_pid, by=c('pid'))

fwrite(dt, file='rows-ge.csv')

# read the pics table and assign the link in the last row to that pid
pics = fread ('~/tcpd_data/GE/GE_19_working/pictures_with_pids.csv');
pid_table = pics[, .(link=.SD[nrow(.SD)]$link), by=.(pid)]
fwrite(pid_table, file='pids.csv')

