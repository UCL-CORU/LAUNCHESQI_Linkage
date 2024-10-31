# LAUNCHESQI_linkage
LAUNCHES QI linkage: R code for the linkage of clinical records using patient identifiers

The directory contains two main R programs: 
- RprocessNCHDAdata.R (process data identifiers and create Dataset2)
- RlinkNCHDAdata.R (link internally NCHDA and save the matches and linkage type; create and save an anonymised LAUNCHES patient ID)

There are also 3 files with defined R functions:
- LAUNCHES_Dataset1_alldata_lib.R (functions used to create Dataet1, the source for Dataset2)
- LAUNCHES_Dataset2_identifierdata_lib.R (functions used to create Dataset2)
- LAUNCHES_Dataset3_linkedrecords_lib.R (functions useful to link records or evaluate the quality of matches)

The other 3 csv files used to process postcodes were:
- LAUNCHES_UK_Lookup_Postcode_Country.csv (Not avalable, it is created from code after downloading the full list of postcodes from ODS)
- UK_Postcode_Areas.xlsx
- ets.csv

## How to cite this work

Espuny Pujol F, Pagel C, Brown KL, et al. Linkage of National Congenital Heart Disease Audit data to hospital, critical care and mortality national data sets to enable research focused on quality improvement. BMJ Open 2022;12:e057343. doi: 10.1136/bmjopen-2021-057343

