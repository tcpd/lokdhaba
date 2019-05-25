library(data.table)
library(jsonlite)
library(readr)
filePath <- '~/tcpd_data/GE/Data/derived/mastersheet.csv'
data <- read_csv(filePath)
dt <- data.table(data)
#dt <- dt[dt$No_Mandates > 0]
dt <- dt[,c("Assembly_No", "Candidate", "State_Name", "Constituency_Name", "Party", "Last_Party", "pid", "Votes", "Position", "Contested", "No_Mandates", "Turncoat", "Incumbent")]

json = toJSON(dt)
json = prettify(json)
cat (json, file='all-ge.json')
