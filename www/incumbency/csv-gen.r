# this script generates the csv files (pids.csv and rows.csv) needed by the incumbency visualization

library(data.table)
library(jsonlite)
library(readr)
filePath <- '~/tcpd_data/GE/Data/derived/mastersheet.csv'
filePath = '~/tcpd_data/GE/GE_19_working/mastersheet_with_adr_data.csv'
data = fread(filePath)
#dt <- dt[dt$No_Mandates > 0]
dt <- data[Party != 'NOTA' & Candidate != 'NOTA', c("Assembly_No", "Poll_No", "Year", "Candidate", "State_Name", "Constituency_Name", "Party", "Last_Party", "pid", "Votes", "Sex", "Position", "Contested", "No_Mandates", "Turncoat", "Incumbent", "Vote_Share_Percentage", "Margin", "Margin_Percentage", "MyNeta_age")]

# filter dt down to only rows whose pid is present in this assembly
assembly = 17
this_assembly_pids = unique(dt[Assembly_No == assembly]$pid)
dt = dt[pid %in% this_assembly_pids]

terms_served_by_pid = dt[Position == 1, .(Terms=length(unique(Assembly_No))), by=c('pid')]
dt = merge (dt, terms_served_by_pid, by=c('pid'))

terms_contested_by_pid = dt[, .(Terms_Contested=length(unique(Assembly_No))), by=c('pid')]
dt = merge (dt, terms_contested_by_pid, by=c('pid'))

fwrite(dt, file='rows-ge.csv')

# read the pics table from both LS and PRS and assign the link in the last row to that pid
pics_ls = fread ('~/tcpd_data/GE/GE_19_working/pictures_with_pids.csv');
pics_ls = pics_ls[, .(link=.SD[nrow(.SD)]$link), by=.(pid)]

pics_prs = fread ('~/tcpd_data/GE/GE_19_working/pictures_with_pids_prs.csv');
pics_prs = pics_prs[, .(link=.SD[nrow(.SD)]$link), by=.(pid)]

pics_merged = merge(pics_prs, pics_ls, by=c('pid'), all.x=TRUE, all.y=TRUE)

# the link is preferably PRS (link.x), but if its NA, use the LS link
pics_merged$link = '' 
for (i in 1:nrow(pics_merged)) {
  if (is.na(pics_merged[i]$link.x)) {
    pics_merged[i]$link = pics_merged[i]$link.y;
  } else {
    pics_merged[i]$link = pics_merged[i]$link.x;
  }
}

fwrite(pics_merged, file='pids.csv')

