# this script generates the csv files (pids.csv and rows.csv) needed by the incumbency visualization

library(data.table)
library(jsonlite)
library(readr)
filePath <- '~/github/tcpd_data/data/GE/Data/derived/mastersheet.csv'
filePath = '~/github/tcpd_data/data/GE/Data/derived/mastersheet_ADR.csv'
data = fread(filePath)
#dt <- dt[dt$No_Mandates > 0]
data <- data[Party != 'NOTA' & Candidate != 'NOTA', c("Assembly_No", "Poll_No", "Year", "Candidate", "State_Name", "Constituency_Name", "Party", "Last_Party", "pid", "Votes", "Sex", "Position", "Contested", "No_Mandates", "Turncoat", "Incumbent", "Vote_Share_Percentage", "Margin", "Margin_Percentage", "MyNeta_age")]
data = 
# filter dt down to only rows whose pid is present in this assembly
for (assembly in min(data$Assembly_No):max(data$Assembly_No)) {
  print (paste('Generating data for assembly# ', assembly))
  
  dt = data[Assembly_No <= assembly] # filter out all rows after this assembly
  
  # get pids of everyone who has a line in this assembly...
  # ... but drop INDs and non-winning-parties parties unless their Position is < 3 to avoid the long tail of insignificant cands
  
  party_seat_count = dt[Assembly_No == assembly & Position == 1, .(count = .N), by='Party']
  winning_parties = party_seat_count$Party # only winning parties will have an entry in this table
  this_assembly_pids = unique(dt[Assembly_No == assembly & (Position < 3 | (Party %in% winning_parties & Party != 'IND'))]$pid)

  print (paste("winning parties = ", winning_parties))
  
  dt = dt[pid %in% this_assembly_pids]
  # only keep those rows with a pid in this assembly
  
  terms_served_by_pid = dt[Position == 1, .(Terms=length(unique(Assembly_No))), by=c('pid')]
  dt = merge (dt, terms_served_by_pid, by=c('pid'), all.x=TRUE)
  
  # fix the #mandates and contested to be whatever it is up to the current assembly
  # otherwise rows for the same pid will have different values in these columns
  dt[, No_Mandates:=max(No_Mandates), by=c('pid')]
  dt[, Contested:=max(Contested), by=c('pid')]
  
  terms_contested_by_pid = dt[, .(Terms_Contested=length(unique(Assembly_No))), by=c('pid')]
  dt = merge (dt, terms_contested_by_pid, by=c('pid'), all.x = TRUE)
  
  fwrite(dt, file=paste0('ge-incumbency-',assembly,'.csv'))
}

# read the pics table from both LS and PRS and assign the link in the last row to that pid
pics_ls = fread ('~/github/tcpd_data/data/GE/GE_19_working/pictures_with_pids.csv');
pics_ls = pics_ls[, .(link=.SD[nrow(.SD)]$link), by=.(pid)]

pics_prs = fread ('~/github/tcpd_data/data/GE/GE_19_working/pictures_with_pids_prs.csv');
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

