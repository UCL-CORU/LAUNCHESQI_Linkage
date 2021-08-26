#' R PROCESS NCHDA DATA. R code to process and assess the quality of NCHDA records with personal fields
#' @date 2019, July 15
#' @author Ferran Espuny Pujol, PhD
#' 
#' For the LAUNCHES project, we need to generate three datasets, based on the NCHDA data:
#' Dataset1 (original data plus a record identifier LAUNCHESrecID)
#' Dataset2 (identifier data: original identifiers, processed identifiers, and derived quality code)
#' Dataset3 (pseudo-anonymised version of Dataset1, including the derived fields and created record and linked record identifiers)
#'
#' The steps implemented in this code are:
#' We create Dataset 1, it it is not found:
#'    1 	Read the NCHDA data file with records of Interest
#'    2 	Generate a LAUNCHES ID for each record
#'    3 	Dataset 1. Store a static dataset with all records of interest and the LAUNCHES record ID
#' We create Dataset 2, if Dataset 1 is already created:
#'    4 	Identify NCHDA identifier fields for linkage
#'    5 	Process NCHDA identifier fields for linkage (the same processing needs to be applied to records in all lookup or linked files)
#'    6 	Derive a "Quality code" field for each record
#'    7 	Dataset 2. Review and store a static dataset with identifiers for linkage (original and processed) and derived fields (LAUNCHES record ID, Quality code) [no clinical field included]
#'
#' We use as input the NCHDA data extracted by Andrew Harrison on 11 June 2019.
#' Fields.  The data contains personal identifier and clinical fields
#' Dates.   Records between 1 April 2000 and 31 March 2017
#' Centres. Hospitals in England and Wales which NCHDA data is controlled by HQIP (not private centres)
#'
#' The processing functions used by this program are found in two separate files, specified by parameters inDataset1code and inDataset2code below
#' The created datasets 1 and 2 are stored as comma-separated CSV output files, with all fields quoted and their missing values being empty
#'

rm( list=ls()) #remove any previous R object
gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

require( "xlsx" ) #package used to open a xslx file [can be avoided e.g. if the file is saved in csv format]

## ANALYSIS PARAMETERS:
years_of_interest = 2000:2016 #financial years of interest

## IN-OUT PARAMETERS: directory and file names

inDIRcode      = "C://LAUNCHES/myRcode/"
inDataset1code = "LAUNCHES_Dataset1_alldata_lib.R"
inDataset2code = "LAUNCHES_Dataset2_identifierdata_lib.R"

inDIRdata  = "C://LAUNCHES/Data/"                           # input directory for the original NCHDA data
inFILEdata = "Congenital_export_launches_2019-06-11_v3.csv" # input file name for the original NCHDA data (in fact, I opened with as text file the original file in Excel, and then saved it as CSV since the original file did not open correctly in R)

inDIRpostcodes             = "C://LAUNCHES/myRcode/input_NCHDA/"       # input directory for the auxiliary files (ODS valid postcodes, UK postcode areas, and ODS postcodes for NHS trusts)
ODSpostcodesIN             = "ODSpostcodes.csv"                        # csv file with valid historical postcodes
infile_UK_PostcodeAreas    = "UK_Postcode_Areas.xlsx"                  # xlsx file with valid postcode areas in UK (and their correspondence with countries)
ODSpostcodesNHSTrustSites  = "ets.csv"                                 # csv file with the postcodes from NHS Trust Sites   
inLookupUK_PostcodeCountry = "LAUNCHES_UK_Lookup_Postcode_Country.csv" # csv file with the lookup postcode-country for UK; the file is created if not found

outDIR = "C://LAUNCHES/Output/" #Output directory for the main created LAUNCHES datasets
dataset1file  = "Dataset1_alldata.csv"         # Dataset1 file name
dataset2fileR = "Dataset2R_identifierdata.csv" # Dataset2, R version file name (dates in Date format, missing values as NA)
dataset2file  = "Dataset2_identifierdata.csv"  # Dataset2 file name

## Dataset 1 exists?
create_dataset1 = TRUE
if( file.exists( paste0(outDIR, dataset1file ) ) ) create_dataset1 = FALSE

#-- CREATION OF DATASET 1, if the file does not exist ------------------------------------------
if( create_dataset1 ){
  
  # we load the source code for the creation of Dataset 1
  if( file.exists( paste0(inDIRcode, inDataset1code) ) ){ 
    source( paste0(inDIRcode, inDataset1code) ) #we load the needed functions
  } else { 
    stop( paste0("ERROR: cannot find the source file ", paste0(inDIRcode, inDataset1code), "; please revise the source directory and file name" ) )
  } 

## 1. READING THE DATA as text (records of interest)
  if( file.exists( paste0(inDIRdata, inFILEdata) ) ){
    persdata = read.csv( paste0(inDIRdata, inFILEdata) , header=T, colClasses = "character", fill = T )
  } else {
    stop( paste0("ERROR: cannot find the input file ", paste0(inDIRdata, inFILEdata), "; please revise the input directory and file name" ) )
  }
  names( persdata )

## 2. GENERATE a LAUNCHES ID for each record
  nrecords   = nrow( persdata ) #143862 #number of records (auxiliary variable)
  randomseed = 3*7*11           #231    #setting the seed for the random number generator allows reproducibility of results
  persdata$LAUNCHESrecID <- Dataset1_generateLAUNCHESrecID( nrecords, randomseed )

## 3.	Dataset 1. Store a static dataset with records of interest and the LAUNCHES record ID ##
  # We save in a CSV file the Dataset 1 (original data + LAUNCHES record ID)
  write.csv( x = persdata, file = paste0(outDIR, dataset1file ), row.names = F )
  
  rm( persdata, nrecords, randomseed, create_dataset1 )
}

#-- CREATION OF DATASET 2 using DATASET 1 as input ------------------------------------------------

# we load the source code for the creation of Dataset 2
if( file.exists( paste0(inDIRcode, inDataset2code) ) ){ 
  source( paste0(inDIRcode, inDataset2code) ) #we load the needed functions
} else { 
  stop( paste0("ERROR: cannot find the source file ", paste0(inDIRcode, inDataset2code), "; please revise the source directory and file name" ) )
} 

# we read Dataset 1
if( file.exists( paste0(outDIR, dataset1file) ) ){
  persdata = read.csv( paste0(outDIR, dataset1file) , header=T, colClasses = "character", fill = T )
} else {
  stop( paste0("ERROR: cannot find the input file ", paste0(outDIR, dataset1file), "; please revise the input directory and file name" ) )
}

nrecords = nrow( persdata ) #143862 #number of records (auxiliary variable)

## 4.	Identify NCHDA identifier fields for linkage
 # Selection of the identifier fields
sel = c( "ParentID", "X1.03.NHS.Number", "X1.06.Date.Of.Birth", "X1.01.Hospital", "X1.02.HospitalNumber", "X1.05.Forename", "X1.04.Surname","X1.10.Patient.Post.Code", "X3.01.Procedure.Date", "X8.01.Procedure.Unique.ID", "LAUNCHESrecID" )
persdata = persdata[ , sel ]
 # The fields are renamed here so that names do not need to be changed in the rest of the code
names( persdata ) <- c( c( "parentID", "nhsnum", "dob", "orgid", "locpatid", "forename", "surname", "postcode", "procdate", "procID"), "LAUNCHESrecID" )


## 5. PROCESSING IDENTIFIER FIELDS (the same processing needs to be applied to records in all lookup or linked files)

# We keep the original data before any processing
originaldata = persdata[ , c("LAUNCHESrecID","nhsnum", "dob", "orgid", "locpatid", "forename", "surname", "postcode", "parentID", "procID") ]
names( originaldata ) <- c("LAUNCHESrecID","nhsnum0", "dob0", "orgid0", "locpatid0", "forename0", "surname0", "postcode0", "parentID0", "procID0" )
persdata = persdata[ , -which( names(persdata)=="LAUNCHESrecID") ]

# FINANCIAL YEAR OF PROCEDURE (we delete procdate once the Financial Year is computed)
# We convert procedure dates to standard R formatted dates (ignoring the time of procedure, when available)
persdata$procdate = as.Date( substring( persdata$procdate, 1, 10 ), format = "%d/%m/%Y" ) 
summary( persdata$procdate ) #No missing values
# Financial year (financial year only used for reporting quality, but will not be saved in Dataset 2)
for( FinYr in years_of_interest ){
  sel = which( as.Date( paste0( FinYr, "-04-01" ) ) <= persdata$procdate  & persdata$procdate < as.Date( paste0( FinYr+1, "-04-01" ) ) )
  persdata[ sel, "FinYr" ] = FinYr
}
rm( FinYr, sel )
table( persdata$FinYr, useNA = "always" )
hist( persdata$FinYr, breaks = seq( 1999.5, 2016.5,1.0)  ) #hand-coded values for years: revise if you change the years of interest
if( sum(is.na(persdata$FinYr)) ){
  warning( paste0("WARNING: the data contains ", sum(is.na(persdata$FinYr)) ," records outside the period of interest; we remove those records!" ) )
  persdata = persdata[ !is.na(persdata$FinYr), ]
}
persdata[ , "procdate" ] <- NULL

# R missing values are characterised as NA
aux = (persdata == "")
if( sum( aux, na.rm = T ) ) persdata[ aux ] <- NA
rm( aux )

## 6. QUALITY CHECKS

##--- 6.1.--- NHS numbers (text field)

# Processing NHS numbers
persdata$nhsnum <- Dataset2_nhsnum_process( persdata$nhsnum )

# Possible Quality values for NHS numbers:
# 0=null or missing
# 1=invalid (format or value)
# 3=valid (for linkage), including very common values not defined as invalid
# Quality of NHS numbers ...
NHSnumQ <- Dataset2_quality013( persdata$nhsnum, originaldata$nhsnum0, nrecords )  

#Report tables: I copy-paste the output into Word, and then covert to table
aux = cbind( table( NHSnumQ  ) , round( table( NHSnumQ  ) / nrecords * 100, 1 ) )  
colnames(aux) = c("Number of records","Percentage")
write.csv( aux, row.names = c("0. missing", "1. invalid", "3. valid"), quote = F ) ### TABLE: QUALITY NHS NUMBER
aux = cbind( table( NHSnumQ,  persdata$FinYr ) )                                   
write.csv( aux, row.names = c("0. missing", "1. invalid", "3. valid"), quote = F ) ### TABLE: QUALITY NHS NUMBER BY FINANCIAL YEAR


##---6.2.--- DATES OF BIRTH (date formatted field)

# Processing NHS numbers
persdata$dob <- Dataset2_dob_process( persdata$dob )

# Possible Quality values for DoB:
# 0=null or missing
# 1=invalid (format or value) 
# 3=valid (for linkage), including very common values not defined as invalid
# Quality of Dates of Birth ...
DoBQ <- Dataset2_quality013( persdata$dob, originaldata$dob0, nrecords )  

#Report tables: I copy-paste the output into Word, and then covert to table
aux = cbind( table( DoBQ  ) , round( table( DoBQ  ) / nrecords * 100, 1 ) )  
colnames(aux) = c("Number of records","Percentage")
write.csv( aux, row.names = c("0. missing", "1. invalid", "3. valid"), quote = F ) ### TABLE: QUALITY Date of Birth
aux = cbind( table( DoBQ,  persdata$FinYr ) )                                   
write.csv( aux, row.names = c("0. missing", "1. invalid", "3. valid"), quote = F ) ### TABLE: QUALITY Date of Birth BY FINANCIAL YEAR


##---6.3.--- ORGANISATION ID 

#converting to upper case (it seems best not to remove blanks)
persdata$orgid = toupper( originaldata$orgid0 )

#unifying some of the codes
persdata$orgid[ persdata$orgid == "GOS. LONDON -  GREAT ORMOND STREET HOSPITAL FOR CHILDREN" ] = "GOS. GREAT ORMOND STREET HOSPITAL FOR CHILDREN"
persdata$orgid[ persdata$orgid == "GRL. LEICESTER -  GLENFIELD HOSPITAL" ] = "GRL. GLENFIELD HOSPITAL"
persdata$orgid[ persdata$orgid == "NCR" ] = "NCR. NEW CROSS HOSPITAL"
persdata$orgid[ persdata$orgid == "STM." ] = "STM. ST MARYS HOSPITAL, PADDINGTON"

#taking as code only the given 3 first letters
persdata$orgid = substr(persdata$orgid,1,3) #I did not run this line to generate a table with (code. long name) as first column

sort( table( persdata$orgid ), decreasing = T )

# Tables of number of records per Centre (and by financial year) [centres in alphabetical order of their code]
aux = table( persdata$orgid )
aux = cbind( aux, round( aux / nrecords * 100, 1 ) )  
colnames(aux) = c("Number of records","Percentage") 
write.csv( aux, row.names = T, quote = F ) ### TABLE: Hospital Names (I added Hospital Country manually)
aux = cbind( table( persdata$orgid,  persdata$FinYr ) )  
write.csv( aux, row.names = T, quote = F ) ### TABLE: Hospital Names BY FINANCIAL YEAR

#reporting the quality of Organisation ID (= hospital)
OrgQ = rep(3,nrecords) 
##3.0.missing values
OrgQ[ is.na(persdata$orgid) ] = 0
table( OrgQ, useNA="always" ) 
#no table needed, since all records have a organisation name


##---6.4.--- LOCAL PATIENT ID
length( unique( persdata$locpatid ) ) #100051
# head( unique( persdata$locpatid[ grep( " ", persdata$locpatid ) ] ), 10 ) #9 records
# head( unique( persdata$locpatid[ grep( "a-z", persdata$locpatid ) ] ), 10 ) #0 records

#convert to upper case and remove blank spaces
persdata$locpatid = gsub( "\\s", "", toupper(persdata$locpatid) )

length( unique( persdata$locpatid ) ) #100045 

#remove trailing semicolons
sel = grep( "[;]+$", persdata$locpatid )
persdata$locpatid[ sel ] = gsub( "[;]+$", "", persdata$locpatid[ sel ] )

#remove leading zeroes
sum( grepl( "^0", persdata$locpatid ) ) #11,085 localpatient IDs start with 0
sel = grep( "^0", persdata$locpatid )
sum( gsub( "^[0]+", "", persdata$locpatid[ sel ]  ) %in% persdata$locpatid ) #266 of those are found without a leading 0
persdata$locpatid[ sel ] = gsub( "^[0]+", "", persdata$locpatid[ sel ]  )

# length( unique( persdata$parentID ) ) #99995
# length( grep( "[A-Za-z]", persdata$locpatid ) ) #81834 contain letters
# head( unique( persdata$locpatid[ grep( "[A-Za-z]", persdata$locpatid ) ] ), 10 )
# length( grep( "/", persdata$locpatid ) ) #11637
# head( unique( persdata$locpatid[ grep( "/", persdata$locpatid ) ] ), 10 )
# length( grep( "-", persdata$locpatid ) ) #863
# head( unique( persdata$locpatid[ grep( "-", persdata$locpatid ) ] ), 10 )
# head( sort( table( persdata$locpatid, useNA = "always"), decreasing = T ), 100 )  #no  values being too common apparently 
# table( nchar( persdata$locpatid) )
# 2     3     4     5     6     7     8     9    10    11    12    13    14    16 
# 2     7    15   330 32723 55718 51829  1429  1611   187     6     3     1     1 
# 2     3     4     5     6     7     8     9    10    11    12    13    14    16
# 2     5     3    75 31443 47313 61780  1431  1612   187     6     3     1     1
# unique( persdata$locpatid[ nchar( persdata$locpatid) > 11 ] )
# table( persdata$locpatid[ nchar( persdata$locpatid) < 5 ] )

#reporting the quality of Local Patient ID (= hospital number)
LocPatQ = rep(3,nrecords) 
##4.0.missing values
LocPatQ[ is.na(persdata$locpatid) ] = 0
#pending: sort out if any processing is needed
table( LocPatQ, useNA="always" ) 
#no table needed, since all records have a local patient id


##---6.5.--- FORENAME, aka FIRST NAME 

# Processing Forename
persdata$forename <- Dataset2_forename_process( persdata$forename )

# A forename is found in the surnames in the form "name2, name2"
sel = grep( "[A-Z], [A-Z]", persdata$surname, ignore.case = T )
# unique( persdata[ sel, c("forename","surname") ] ) 
if( length(sel) ) persdata$forename[ sel ] = gsub( ".*, ?" ,  "", toupper( persdata$surname[ sel ] ) ) #we fill-in the forename with the text after the comma (only if forename was missing)

##2019 July 08 ... What hapenned to the multiple forenames?
# sel = grep( ";|,", persdata$forename )
# cbind( originaldata$forename0[sel], persdata$forename[sel] )

# Possible Quality values for Forename:
# 0=null or missing
# 1=invalid (format or value)
# 3=valid (for linkage), including very common values not defined as invalid
# Quality of Forenames ...
ForenameQ <- Dataset2_quality013( persdata$forename, originaldata$forename0, nrecords )  

#Report tables: I copy-paste the output into Word, and then covert to table
aux = cbind( table( ForenameQ  ) , round( table( ForenameQ  ) / nrecords * 100, 1 ) )  
colnames(aux) = c("Number of records","Percentage")
write.csv( aux, row.names = c("0. missing", "1. invalid", "3. valid"), quote = F ) ### TABLE: QUALITY Forename
aux = cbind( table( ForenameQ,  persdata$FinYr ) )                                   
write.csv( aux, row.names = c("0. missing", "1. invalid", "3. valid"), quote = F ) ### TABLE: QUALITY Forename BY FINANCIAL YEAR

#examples of non-valid forenames
# sel = which( ForenameQ ==1 ) #411
# # unique( cbind( originaldata[ sel, c("forename0","surname0") ], persdata[ sel, c("forename","surname") ] ) )
# sort( table( toupper(originaldata$forename0[sel]) ), decreasing = T )
#examples of valid forenames that were changed
# sel = which( (ForenameQ==3) & toupper(originaldata$forename0) != persdata$forename ) #13,308 changed names
# sort( table( toupper(originaldata$forename0[sel]) ), decreasing = T )[ 1:100]


##---6.6.--- SURNAME, aka LAST NAME 

# Processing Surname
persdata$surname <- Dataset2_surname_process( persdata$surname, persdata$locpatid )

##2019 July 08 ... What hapenned to the multiple surnames?
# sel = grep( ";|,", persdata$surname )
# unique( cbind( originaldata$surname0[sel], persdata$surname[sel] ) )

# Possible Quality values for surname:
# 0=null or missing
# 1=invalid (format or value) 
# 2=very common ?
# 3=valid (for linkage), including very common values not defined as invalid
SurnameQ <- Dataset2_quality013( persdata$surname, originaldata$surname0, nrecords )  

#Report tables: I copy-paste the output into Word, and then covert to table
aux = cbind( table( SurnameQ  ) , round( table( SurnameQ  ) / nrecords * 100, 1 ) )  
colnames(aux) = c("Number of records","Percentage")
write.csv( aux, row.names = c("0. missing", "1. invalid", "3. valid"), quote = F ) ### TABLE: QUALITY Surname
aux = cbind( table( SurnameQ,  persdata$FinYr ) )                                   
write.csv( aux, row.names = c("0. missing", "1. invalid", "3. valid"), quote = F ) ### TABLE: QUALITY Surname BY FINANCIAL YEAR

# #two non-valid surnames
# sel = which( SurnameQ ==1 )
# cbind( originaldata[ sel, c("forename0","surname0") ], persdata[ sel, c("forename","surname") ] )


#---6.7.--- POSTCODES (processed together with the creation of their quality code)

aux = Dataset2_postcode_process( persdata$postcode, nrecords )
persdata$postcode = aux$postcode
postcodeQ = aux$postcodeQ
rm( aux )

#examples of non-valid postcodes
sel = which( postcodeQ ==1 )
head( sort( table( toupper(originaldata$postcode0[sel]) ), decreasing = T ), 40 )
rm( sel )

#Report tables: I copy-paste the output into Word, and then covert to table
aux = cbind( table( postcodeQ  ) , round( table( postcodeQ  ) / nrecords * 100, 1 ) )  
colnames(aux) = c("Number of records","Percentage")
write.csv( aux, row.names = c("0. missing", "1. invalid", "2. NHS trust site", "3. valid"), quote = F ) ### TABLE: QUALITY Surname
aux = cbind( table( postcodeQ,  persdata$FinYr ) )                                   
write.csv( aux, row.names = c("0. missing", "1. invalid", "2. NHS trust site", "3. valid"), quote = F ) ### TABLE: QUALITY Surname BY FINANCIAL YEAR


##--- 6. OVERALL QUALITY CODE
persdata$Qcode = paste0(NHSnumQ, DoBQ, OrgQ, LocPatQ, ForenameQ, SurnameQ, postcodeQ )

aux = cbind( table( persdata$Qcode  ) , round( table( persdata$Qcode  ) / nrecords * 100, 1 ) )  ### THIS TABLEEEEE
write.csv( aux, row.names = T, quote = F )

aux = cbind( table( persdata$Qcode,  persdata$FinYr ) )  ### THIS TABLEEEE TOOOOOO
write.csv( aux, row.names = T, quote = F )
rm( aux )


# ## --- FREQUENCY TABLES --------------- MOST FREQUENT VALUES OF IDENTIFIERS SEEM REASONABLE, i.e. valid for linkage
# #number of times each value appears 
# persdata$ntimesnhsnum   = as.numeric( ave( persdata$nhsnum, persdata$nhsnum, FUN = function(x) sum( !is.na(x) ) ) )
# persdata$ntimesdob    = as.numeric( ave( as.character(persdata$dob), persdata$dob, FUN = function(x) sum( !is.na(x) ) ) )
# persdata$ntimesforename = as.numeric( ave( persdata$forename, persdata$forename, FUN = function(x) sum( !is.na(x) ) ) )
# persdata$ntimessurname  = as.numeric( ave( persdata$surname, persdata$surname, FUN = function(x) sum( !is.na(x) ) ) )
# persdata$ntimeslocpatid = as.numeric( ave( persdata$locpatid, persdata$locpatid, FUN = function(x) sum( !is.na(x) ) ) )
# persdata$ntimespostcode = as.numeric( ave( persdata$postcode, persdata$postcode, FUN = function(x) sum( !is.na(x) ) ) )
# 
# sort( unique( persdata$nhsnum[ persdata$ntimesnhsnum >10 ] ) )
# sort( unique( persdata$dob[ persdata$ntimesdob > 30 ] ) )
# sort( unique( persdata$forename[ persdata$ntimesforename >1000 ] ) )
# sort( unique( persdata$surname[ persdata$ntimessurname >500 ] ) )
# sort( unique( persdata$locpatid[ persdata$ntimeslocpatid > 10 ] ) )
# sort( unique( persdata$postcode[ persdata$ntimespostcode > 10 ] ) )


##-- 7. STATIC DATASET 2

persdata = cbind( originaldata, persdata ) #joining the original and processed/derived data
rm( originaldata )

##--- ORDERING THE VARIABLES BY INTEREST, AND SORTING THEIR VALUES BY COMPLETENESS (NON-MISSING AT THE TOP)
# persdata = persdata[ with( persdata, order(LAUNCHESrecID, nhsnum, dob, orgid, locpatid, forename, surname, postcode, Qcode) ),  ]
persdata = persdata[ with( persdata, order(LAUNCHESrecID, nhsnum, dob, orgid, locpatid, forename, surname, postcode, Qcode) ),  ]
row.names( persdata ) <- NULL
persdata$parentID0 <- NULL
persdata$procID0 <- NULL
persdata$parentID <- NULL
persdata$procID <- NULL

names( persdata ) #the R version will contain FinYr for the internal linkage quality report (just in case)

##We save in a CSV file Dataset1 with identifiers for linkage (original and processed) and derived fields (LAUNCHES record ID, Quality code) [no clinical field included]
write.csv( x = persdata, file = paste0(outDIR, "/", dataset2fileR), row.names = F, quote = T )

## REFORMATTING TO CHARACTER AND REPLACING THE NAs WITH ""
persdata = read.csv( file = paste0(outDIR, "/", dataset2fileR), row.names = NULL, stringsAsFactors = F, colClasses = "character" )  

persdata$procdate0 <- NULL
persdata$procdate <- NULL #this was done already, but just in case
persdata$FinYr <- NULL

head( persdata,15 ) #In the processed fields, missing values are NAs and dates have Date format
if( sum(is.na(persdata) ) ) persdata[ is.na( persdata ) ] <- "" #missing values are empty quoted strings ""
head( persdata,15 ) 

write.csv( x = persdata, file = paste0(outDIR, "/", dataset2file), row.names = F, quote = T )