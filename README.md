# LAUNCHESQI_linkage
LAUNCHES QI linkage: R code for the linkage of clinical records using patient identifiers

The directory contains two main R programs: 
.RprocessNCHDAdata.R (process data identifiers and create Dataset2)
.RlinkNCHDAdata.R (link internally NCHDA and save the matches and linkage type; create and save an anonymised LAUNCHES patient ID)

There are also 3 files with defined R functions:
.LAUNCHES_Dataset1_alldata_lib.R (functions used to create Dataet1, the source for Dataset2)
.LAUNCHES_Dataset2_identifierdata_lib.R (functions used to create Dataset2)
.LAUNCHES_Dataset3_linkedrecords_lib.R (functions useful to link records or evaluate the quality of matches)

The other 3 csv files were used to process postcodes and to extract country from those:
.LAUNCHES_UK_Lookup_Postcode_Country.csv
.UK_Postcode_Areas.xlsx
.ets.csv
