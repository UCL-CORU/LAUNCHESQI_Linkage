#' FEP: 2019 July 03
#' Purpose: Link records and report the linkage type (matched pairs classified as A,B,C,D) and detailed linkage quality code
#'          We also save in a separate file the created variables: LAUNCHESrecID, LAUNCHESpatID, Qcode (these will be added to Dataset1 for creating Dataset 3)
#'
#' Comments:
#'          In this version, composed names and shorter versions are considered a partial match (e.g. "Mary Jane" is matched with "Mary" or "Jane")
#' 
################################################################################################### INPUT & PRE-PROCESSING

rm( list=ls()) #remove any previous R object
gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

inDIRcode      = "C://LAUNCHES/myRcode/"
inDataset3linkcode = "LAUNCHES_Dataset3_linkrecords_lib.R"


dataIN     = "C://LAUNCHES/Output_September_2019/Dataset2R_identifierdata.csv"
dataOUT    = "C://LAUNCHES/Output_September_2019/v2LinkedRecords_LAUNCHESQI_noidentifiers_v3.csv" # LAUNCHESrecID, LAUNCHESpatID, Qcode 
dataOUT2   = "C://LAUNCHES/Output_September_2019/v2Matches_linkage_results_and_quality_v3.csv"    # LAUNCHESrecID, linkedLAUNCHESrecID, linkagetype, linkageQcode

## load CSV file with records with original and processed fields
dataset2 <- read.csv(dataIN, header = TRUE, stringsAsFactors = FALSE, colClasses = "character")

nrecords=nrow(dataset2)

persdata <- dataset2[ , c("LAUNCHESrecID", "nhsnum", "dob", "orgid", "locpatid", "forename", "surname", "postcode", "Qcode", "FinYr") ] #FinYr used only for reporting
rm( dataset2 )

##--- Dates in date format 
persdata$dob      = as.Date( persdata$dob )       # Date of birth as date

persdata$rowID = row.names( persdata )
persdata$linkID = persdata$rowID

########################################################## LINKAGE #############

persdata$linkedrecordj=rep( 0, nrecords) 
persdata$linkedrecordi=rep( 0, nrecords) 
persdata$linkagetype= rep( as.character(NA), nrecords)
nmatches <- 0

##-- 1. -- Exact match on (valid) NHS numbers: then either (exact mach on Hospital ID) OR (no disagreement in DoB) OR (exact/)

allNHSnum = setdiff( unique( persdata$nhsnum ), "" ) #possible values for NHS number (excluding missing)
allNHSnum = setdiff( unique( persdata$nhsnum ), NA ) #possible values for NHS number (excluding missing) //FEP on 2019 May 09

#iNHSnum = 34356; i=1; j=2
#for( iNHSnum in 1:35000 ){              

Linkage_A_Step <- function( nhsnum ){
  
  ## SAME NHS number (then comparing DoB)
  rows = which( persdata$nhsnum == nhsnum ) #the rows in the data where that value appears
  
  result <- data.frame( rowID = character(),
                        linkID = character(),
                        linkedrecordj = numeric(),
                        linkedrecordi = numeric(),
                        linkagetype = numeric() )
  persdata[ rows, "linkedrecordj" ] = 0
  persdata[ rows, "linkedrecordi" ] = 0
  persdata[ rows, "linkagetype" ]  = ""
  nmatches <- 0
  
  if( length( rows ) > 1 ){               #the NHS number appears more than 1 time in the data ...
    print( rows ) 

    for( i in 1:(length(rows)-1) ){       #we test all pairs of rows (i,j) with same NHS number
      for( j in (i+1):length(rows) ){  
        
        if( persdata[rows[j], "linkID" ] != persdata[rows[i], "linkID" ] ){ #the linkIDs are different, i.e. (i,j) were not linked yet
          
          ##HOSPITAL ID AGREEMENT: records are linked if the Hospital IDs match exactly
          match_hospid = (persdata$orgid[rows[i]] == persdata$orgid[rows[j]]) & (persdata$locpatid[rows[i]] == persdata$locpatid[rows[j]]) #takes the value NA if either field is missing
          
          if( !is.na(match_hospid) & match_hospid==T ){
            nmatches <- nmatches+1
            persdata[ rows[nmatches], "linkedrecordj" ] = rows[j]
            persdata[ rows[nmatches], "linkedrecordi" ] = rows[i]
            persdata[ rows[nmatches], "linkagetype" ]  = "A"
            toupdate = which( persdata$linkID == persdata$linkID[rows[j]] )
            if( sum( toupdate %in% rows ) != length(toupdate) ) print( "A weird think happened" )
            #persdata$linkID[rows[j]] = persdata$linkID[rows[i]] #we assign to row j the ID of row i (exact match on hospital ID)
            persdata[toupdate,"linkID"] = persdata[rows[i], "linkID" ] #we assign to row j the ID of row i (exact match on hospital ID)
            next #no need to compare the DOBs if the Hospital IDs match
          } 
          
          ##DOB NON-DISAGREEMENT: records are linked if the DOBs do not disagree
          dob1 = persdata$dob[rows[i]]
          dob2 = persdata$dob[rows[j]]
          
          match_dob = compare_DOB( dob1, dob2 ) #if disagreement, match_dob will be zero
          
          if( match_dob!=0 ) {
            nmatches <- nmatches+1
            persdata[ rows[nmatches], "linkedrecordj" ] = rows[j]
            persdata[ rows[nmatches], "linkedrecordi" ] = rows[i]
            persdata[ rows[nmatches], "linkagetype" ]  = "A"
            # persdata$linkID[rows[j]] = persdata$linkID[rows[i]] #we assign to row j the ID of row i (no DOB disagreement)
            toupdate = which( persdata$linkID == persdata$linkID[rows[j]] )
            if( sum( toupdate %in% rows ) != length(toupdate) ) print( "A weird think happened" )
            persdata[toupdate,"linkID"] = persdata[rows[i], "linkID" ] #we assign to row j the ID of row i
            next
          }
          
          ##names or postcodes matching? ##2019 July 12
          match_names = compare_names( persdata$forename[rows[i]], persdata$surname[rows[i]], 
                                       persdata$forename[rows[j]], persdata$surname[rows[j]] ) #2019 July 08
          match_postcodes = persdata$postcode[rows[i]] == persdata$postcode[rows[j]]
          if( is.na(match_postcodes) ) match_postcodes=0 ##FEP on 2019 May 09 
          
          if( match_names>=2 | match_postcodes ) { ##2019 July 12: we allow partial match on names
            
            nmatches <- nmatches+1
            persdata[ rows[nmatches], "linkedrecordj" ] = rows[j]
            persdata[ rows[nmatches], "linkedrecordi" ] = rows[i]
            persdata[ rows[nmatches], "linkagetype" ]  = "A"              # persdata$linkID[rows[j]] = persdata$linkID[rows[i]] #we assign to row j the ID of row i (no DOB disagreement)
            toupdate = which( persdata$linkID == persdata$linkID[rows[j]] )
            persdata[toupdate,"linkID"] = persdata[rows[i], "linkID" ] #we assign to row j the ID of row i  
          }
        }
      }
    }
    
    result <- persdata[ rows, c("rowID","linkID", "linkedrecordj", "linkedrecordi", "linkagetype") ]
    
  } else{
      result <- NULL 
  }
  
  # result <- list( rowID  = persdata[ rows, "rowID"],
  #                 linkID = persdata[ rows, "linkID"],
  #                 linkedrecord = matches[ rows, "linkedrecord"],
  #                 linkagetype  = matches[ rows, "linkagetype"] )
  
  # print( result )
  
  return( result)
}

##SAPPLY version
system.time(
  # result <- do.call( rbind, sapply( allNHSnum[1:1050], Linkage_A_Step ) )
  result <- do.call( rbind, sapply( allNHSnum, Linkage_A_Step ) )
)
# result
  
# ##PARALLEL SAPPLY (not working in the workstation)
# require( parallel )
# numCores <- detectCores() #4
# clusters <- makeCluster(numCores-1)
# clusterExport( clusters, c("persdata","compare_DOB"))
# system.time(
#   result <- parSapply( clusters, allNHSnum[1:length(allNHSnum)], Linkage_A_Step )
# )
# result <- do.call( rbind, result )
row.names( result ) <- NULL
head( result, 17 )

# stopCluster( clusters ) ##part of the parallel code
# gc()

sum( row.names( persdata ) == persdata$rowID ) == nrow( persdata )
nmatches = sum( result$linkedrecordj>0)
matches = data.frame( rowID = numeric(nmatches),
                      linkedrowID=numeric(nmatches), 
                      linkagetype= character(nmatches), stringsAsFactors = F ) #type of the linkage (i,j)
matches[ , c("rowID","linkedrowID","linkagetype") ] = result[ result$linkedrecordj>0,c("linkedrecordj","linkedrecordi","linkagetype")]
persdata$linkedrecordi <- NULL
persdata$linkedrecordj <- NULL
persdata$linkagetype <- NULL

persdata[ result$rowID, "linkID" ] <- result[ ,  "linkID" ]
rm( result )
table( matches$linkagetype ) #45349 type A links #PRAiS2: 31469 

#I give "matches" its maximum possible dimension a priori, to avoid increasing its size inside the "for" loop
matches = rbind( matches, 
                 data.frame( rowID = numeric(nrow(persdata)-nmatches),
                      linkedrowID=numeric(nrow(persdata)-nmatches), 
                      linkagetype= character(nrow(persdata)-nmatches), stringsAsFactors = F ) #type of the linkage (i,j)
)

##-- 2. -- Exact match on (valid) Hospital IDs: 
##         If 1+ Null NHSnumber: (no disagreement in DoB) OR (exact match on Names) OR (exact match on postcodes)
##         If disagree NHSnumber: Exact match on DoB

allHospIDs = unique( persdata[ , c("orgid", "locpatid") ] ) #possible values for HospitalID (excluding missing in next step)
allHospIDs = allHospIDs[ ! (is.na(allHospIDs$orgid) | is.na(allHospIDs$locpatid)), ]
row.names( allHospIDs ) <- NULL

for( iHospID in 1:nrow(allHospIDs) ){

  hospid = allHospIDs[ iHospID, ]          #we select a non-missing value for Hospital ID
  
  ## SAME Hospital ID (then comparing DoB)
  rows = which( persdata$orgid == hospid$orgid & persdata$locpatid == hospid$locpatid ) #the rows in the data where that value appears
  if( length( rows )>1 & length( unique(persdata$linkID[rows]) )>1){  #the HospitalID appears more than 1 time in the data, and some of those records have not been linked yet
    print( rows ) 
    for( i in 1:(length(rows)-1) ){       #we test all pairs of rows (i,j) with same Hospital ID
      for( j in (i+1):length(rows) ){
        
        if( persdata$linkID[rows[j]] != persdata$linkID[rows[i]] ){ #the linkIDs are different, i.e. (i,j) were not linked yet
          
          #If disagree NHSnumber: Exact match on DoB
          if( !( is.na(persdata$nhsnum[rows[i]]) | is.na(persdata$nhsnum[rows[j]]) ) &
              persdata$nhsnum[rows[i]] != persdata$nhsnum[rows[j]] ) {
            
            if( !( is.na(persdata$dob[rows[i]]) | is.na(persdata$dob[rows[j]]) ) &
              persdata$dob[rows[i]] == persdata$dob[rows[j]] ) {
              
              nmatches <- nmatches+1
              matches[ nmatches, "rowID"] = rows[j]
              matches[ nmatches, "linkedrowID" ] = rows[i]
              matches[ nmatches, "linkagetype" ] = "D"
              
              toupdate = which( persdata$linkID == persdata$linkID[rows[j]] )
              persdata[toupdate,"linkID"] = persdata[rows[i], "linkID" ] #we assign to row j the ID of row i
              next #no need to compare the DOBs if the Hospital IDs match
            }
          }
          
          #If 1+ Null NHSnumber: (no disagreement in DoB) OR (exact match on Names) OR (exact match on postcodes)
          if( is.na(persdata$nhsnum[rows[i]]) | is.na(persdata$nhsnum[rows[j]]) ) {
            
            ##DOB NON-DISAGREEMENT: records are linked if the DOBs do not disagree
            dob1 = persdata$dob[rows[i]]
            dob2 = persdata$dob[rows[j]]
            notdisagree_dob   = compare_DOB( dob1, dob2 ) != 0 #if disagreement, match_dob will be zero
            
            match_names = compare_names( persdata$forename[rows[i]], persdata$surname[rows[i]], 
                                         persdata$forename[rows[j]], persdata$surname[rows[j]] )
            match_postcodes = persdata$postcode[rows[i]] == persdata$postcode[rows[j]]
            if( is.na(match_postcodes) ) match_postcodes=0
            
            
            if( notdisagree_dob | match_names>=2 | match_postcodes ) {
              
              nmatches <- nmatches+1
              matches[ nmatches, "rowID"] = rows[j]
              matches[ nmatches, "linkedrowID" ] = rows[i]
              matches[ nmatches, "linkagetype" ] = "B"
              
              toupdate = which( persdata$linkID == persdata$linkID[rows[j]] )
              persdata[toupdate,"linkID"] = persdata[rows[i], "linkID" ] #we assign to row j the ID of row i  
            }
          }
        }
      }
    }
  }
}
rm( iHospID, hospid, match_names, match_postcodes )

# persdata0 =persdata
# matches0 = matches

##-- 3. -- Exact match on (valid) Names and Postcodes: 
##         If (1+ Null NHSnumber) and (No exact match by HospitalID): (no disagreement in DoB)
# allNamePostcode = unique( persdata[ , c("forename", "surname", "postcode") ] ) #possible values (excluding missing in next step)
# allNamePostcode = allNamePostcode[ ! (is.na(allNamePostcode$forename) | is.na(allNamePostcode$surname) | is.na(allNamePostcode$postcode)), ]
# row.names( allNamePostcode ) <- NULL #98,611
allPostcode = unique( persdata[ , "postcode" ] ) #possible values (excluding missing in next step)
allPostcode = allPostcode[ ! (is.na(allPostcode)) ]
row.names( allPostcode ) <- NULL

if( length(allPostcode) ){ 

  for( iPostcode in 1:length(allPostcode) ){              
    
    postcode = allPostcode[ iPostcode ]          #we select a non-missing value for Postcode
    
    ## SAME Postcode (then comparing Names, NHSnumber, HospID, and DOBs)
    rows = which( persdata$postcode == postcode) #the rows in the data where that value appears
    if( length( rows )>1 & length( unique(persdata$linkID[rows]) )>1 ){  #the HospitalID appears more than 1 time in the data, and some of those records have not been linked yet
      print( rows ) 
      for( i in 1:(length(rows)-1) ){       #we test all pairs of rows (i,j) with same Hospital ID
        for( j in (i+1):length(rows) ){
          
          if( persdata$linkID[rows[j]] != persdata$linkID[rows[i]] ){ #the linkIDs are different, i.e. (i,j) were not linked yet
            
            #If names match
            match_names = compare_names( persdata$forename[rows[i]], persdata$surname[rows[i]], 
                                         persdata$forename[rows[j]], persdata$surname[rows[j]] )
            
            if( match_names>=2 ) {
            
              if( (is.na(persdata$nhsnum[rows[i]]) | is.na(persdata$nhsnum[rows[j]])) ) {
                
                match_hospid = !( is.na(persdata$orgid[rows[i]]) | is.na(persdata$orgid[rows[j]]) | is.na(persdata$locpatid[rows[i]]) | is.na(persdata$locpatid[rows[j]]) ) & 
                  (persdata$orgid[rows[i]] == persdata$orgid[rows[j]]) & (persdata$locpatid[rows[i]] == persdata$locpatid[rows[j]])
                
                if( !match_hospid ){
                  ##DOB NON-DISAGREEMENT: records are linked if the DOBs do not disagree
                  dob1 = persdata$dob[rows[i]]
                  dob2 = persdata$dob[rows[j]]
                  notdisagree_dob   = compare_DOB( dob1, dob2 ) != 0 #if disagreement, match_dob will be zero
                  if( notdisagree_dob ) {
                    nmatches <- nmatches+1
                    matches[ nmatches, "rowID"] = rows[j]
                    matches[ nmatches, "linkedrowID" ] = rows[i]
                    matches[ nmatches, "linkagetype" ] = "C"

                    toupdate = which( persdata$linkID == persdata$linkID[rows[j]] )
                    persdata$linkID[toupdate] = persdata$linkID[rows[i]] #we assign to row j the ID of row i
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
rm( iPostcode, allPostcode, postcode, dob1, dob2, match_names, match_hospid, notdisagree_dob, i, j, rows )

npatients = length( unique( persdata$linkID ) )

table( matches$linkagetype )

  
##we reassign linkID so that the numbers are consecutive
persdata$linkID = match(persdata$linkID, unique(persdata$linkID))

## GENERATE a LAUNCHES ID for each patient
randomseed = 3*7*11 #setting the seed for the random number generator allows reproducibility of results

aux <- Dataset3_generateLAUNCHESpatID( npatients, randomseed )                  #we generate the needed person codes
persdata$LAUNCHESpatID = aux[ match(persdata$linkID, unique(persdata$linkID)) ] #we assign the codes (creation of variable LAUNCHESpatID) 

################## FORMATTING THE MATCHES FILE AND REPORT

#Keeping only the non-empty rows
table( matches$linkagetype[ 1:sum(matches$linkagetype !="", na.rm = T)] )
matches = matches[ (1:sum(matches$linkagetype !="")), ] #deleting the empty rows of "matches"
table( matches$linkagetype, useNA = "always" )

#Adding the record IDs: initial and linked
matches[ , "LAUNCHESrecID" ]       = persdata[ matches$rowID, "LAUNCHESrecID" ]
matches[ , "linkedLAUNCHESrecID" ] = persdata[ matches$linkedrowID, "LAUNCHESrecID" ]

records1 = persdata[ matches$linkedrowID, ]
records2 = persdata[ matches$rowID, ]

#NHS number: 3=agreement; 1=at least one NHS number is missing; 0 = disagreement
match_nhsnum = ifelse( is.na(records1$nhsnum) | is.na(records2$nhsnum), 1, ifelse( records1$nhsnum == records2$nhsnum, 3, 0 ) ) 
table( match_nhsnum, matches$linkagetype, useNA = "always" )

#Hospital patient ID (orgid,locpatid): 3=agreement; 1=at least one hospital patient ID is missing; 0 = disagreement
match_hospid = ifelse( is.na(records1$orgid) | is.na(records2$orgid) | is.na(records1$locpatid) | is.na(records2$locpatid), 1,
                       ifelse( records1$orgid == records2$orgid & records1$locpatid == records2$locpatid, 3, 0 ) ) 
table( match_hospid, matches$linkagetype, useNA = "always" )

#DoB: 3=exact agreement; 2=partial agreement; 1=at least one dob is missing; 0 = disagreement
match_dob    = sapply( 1:nmatches, function(i) compare_DOB( records1$dob[ i ], records2$dob[ i ] ) ) #THIS TAKES TIME
table( match_dob, matches$linkagetype, useNA = "always" )

#Name (forename,surname): 3=agreement;  1=at least one name is missing; 0 = disagreement
match_name = sapply( 1:nmatches, function(i) compare_names( records1$forename[i], records1$surname[i], records2$forename[i], records2$surname[i] ) )
table( match_name, matches$linkagetype, useNA = "always" )

#Postcode: 3=agreement;  1=at least one postcode is missing; 0 = disagreement
match_postcode = ifelse( is.na(records1$postcode) | is.na(records2$postcode), 1, ifelse( records1$postcode == records2$postcode, 3, 0 ) ) 
table( match_postcode, matches$linkagetype, useNA = "always" )

rm( records1, records2 )

# LINKAGE QCODE (finally)
matches[ , "linkageQcode" ] = paste0( match_nhsnum, match_hospid, match_dob, match_name, match_postcode)
table( matches$linkageQcode )
table( matches$linkageQcode, matches$linkagetype )

#REPORT TABLES
#Report tables: I copy-paste the output into Word, and then covert to table
aux = cbind( table( matches$linkagetype  ) , round( table( matches$linkagetype  ) / nmatches * 100, 1 ) )
colnames(aux) = c("Number of matches","Percentage")
write.csv( aux, row.names = c( "A [Exact NHSnum; (exact HospID) or (DoB don't disagree)]",
                               "B [1+ null NHSnum; exact HospID; (DoB don't disagree) or (exact Name) or (exact Postcode)]",
                               "C [1+ null NHSnum; no exact HospID; DoB don't disagree; exact Name; exact Postcode]",
                               "D [NHSnum different; exact HospID; exact DoB]"), quote = F )                                           ### TABLE: Linkage groups (A,B,C,D)
aux = table( matches$linkageQcode, matches$linkagetype )
aux = cbind( table( matches$linkageQcode, matches$linkagetype ), rowSums(aux), Percentage = round( table( matches$linkageQcode ) / nmatches *100, 1 ) )
write.csv( aux, quote = F ) ### TABLE: LINKAGE QUALITY CODE by linkage group



################################################################################################## OUTPUT

##SAVING THE LINKAGE IDs FOR DATA ANALYSIS
head( matches )
head( persdata )

# ORDERING THE VARIABLES BY linkID and then by rowID
persdata = persdata[ with( persdata, order(LAUNCHESrecID,linkID) ), ]
row.names( persdata ) <- NULL

# WE KEEP ONLY THE NON-IDENTIFIER DATA
persdata = persdata[ , c("LAUNCHESrecID","LAUNCHESpatID","Qcode")]

##we save in a CSV file the processed simulated data
write.csv( x = persdata, file = paste0(dataOUT), row.names = F )


##SAVING THE MATCHING TYPE AND QUALITY INFORMATION
matches = matches[ , c("LAUNCHESrecID","linkedLAUNCHESrecID","linkagetype","linkageQcode") ]
write.csv( x = matches, file = paste0(dataOUT2), row.names = F )

rm( aux, persdata, matches, allHospIDs, allNHSnum, sel, randomseed, toupdate)
rm( list=ls()[ grep( "match_", ls() ) ] )
gc()
