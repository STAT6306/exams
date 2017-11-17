#Go to the website: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time
# Set "Filter Year" to 2014
# Set "Filter Period" to January
# Select the fields you want to use in your analysis
#   -Some fields you might want:
#[1] "MONTH"                 "DAY_OF_MONTH"          "DAY_OF_WEEK"          
#[4] "FL_DATE"               "CARRIER"               "TAIL_NUM"             
#[7] "ORIGIN_AIRPORT_ID"     "ORIGIN_AIRPORT_SEQ_ID" "ORIGIN_CITY_MARKET_ID"
#[10] "ORIGIN"                "DEST_AIRPORT_ID"       "DEST_AIRPORT_SEQ_ID"  
#[13] "DEST_CITY_MARKET_ID"   "DEST"                  "DEST_CITY_NAME"       
#[16] "DEST_STATE_ABR"        "CRS_DEP_TIME"          "DEP_DELAY"            
#[19] "ARR_DELAY"             "CANCELLED"  
# Click "Download"
# Change the "Filter Period" to February and Click "Download"
# Change the "Filter Period" to March and Click "Download"
# ...
# Change the "Filter Period" to December and Click "Download"


# Go to the download directory and Expand/unzip the files
# Change the .csv file names (ignore the original .zip files) to:
#     01_2014.csv, 02_2014.csv, ..., 12_2014.csv
#      (note, the number doesn't have to correspond to the month, any order will do)
# Move the files to a subdirectory of your R working directory called "database"

# Run the following code to generate an R object that is a single flat file 
# contained all the flights for 2014 (make sure you install plyr)

library(plyr)

 paths=dir("database",pattern="_2014.csv",full.names=TRUE)
 print(paths)
 names(paths)=basename(paths)
# This next step took me approx. 177 seconds
 srt  = proc.time()[3]
 df=ldply(paths,read.csv)
 df$X=df$.id=NULL
 save(df,file="2014flights.Rdata")
 end  = proc.time()[3]
 cat('total time: ',end-srt)
 #From this point on, you can just load the R object to get all the flights.
load(file="./2014flights.Rdata")
format(object.size(df),units='MiB')#get the size in memory