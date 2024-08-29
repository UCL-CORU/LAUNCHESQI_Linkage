#' LAUNCHES DATASET2 IDENTIFIERDATA LIB. R functions to create LAUNCHESQI Dataset2
#' @date 2019, July 15
#' @author Ferran Espuny Pujol, PhD
#'
#' For the LAUNCHES project, we need to create, based on the NCHDA data:
#' Dataset2 (identifier data: original, processed, and derived quality code)
#'
#' The steps implemented in this code as functions are used to:
#'    5 	Process NCHDA identifier fields for linkage (the same processing needs to be applied to records in all lookup or linked files)
#'    6 	Derive a "Quality code" field for each record
#'

#' PROCESS NHS NUMBER
#' @param nhsnum0    = input variable with the nhs numbers in text form and with missing values set to NA
#' @return nhsnumber = output variable with the processed nhs numbers in text form and with missing or invalid values in nhsnumber0 set to NA
#' 
Dataset2_nhsnum_process <- function( nhsnum0 ){

  # suppressing all blank spaces
  nhsnum = gsub( "\\s", "", nhsnum0 )
  
  # INVALID NHS NUMBER VALUES (set to missing): meaningless values and those not satisfying the 10-digit check
  
  # Records with non-missing NHS number not being in the form of 10 numeric characters
  sel = grep( "[^0-9]", nhsnum )                                    # 3 records with non-numeric characters
  sel = union( sel, which( nchar(nhsnum) != 10 & !is.na(nhsnum) ) ) # adding those with length different than 10 (none added)
  # nhsnum[ sel ] # "OUTOFAREA" "6467437780/4/9" "6481537746;"
  nhsnum[ sel ] = substr( nhsnum[ sel ], 1, 10 ) # we restrict to the first 10 digits 
  sel = sel[ grepl( "[^0-9]", nhsnum[sel] ) | ( nchar(nhsnum[sel]) != 10 & !is.na(nhsnum[sel]) ) ] #yet not 10 numeric digits
  # nhsnum[ sel ] # "outofarea"
  nhsnum[ sel ] <- NA
  # table( nchar(nhsnum ), useNA = "always" ) #now all non-missing NHS numbers have 10 numeric digits
  
  # not the same number repeated 10 times
  nhsnum[ grep( "^([0-9])\\1{9}", nhsnum ) ] <- NA #not the same number repeated 10 times
  # not of the format "n00000000n"
  nhsnum[ grep( "^([0-9])00000000\\1$", nhsnum ) ] <- NA #not the same number repeated 10 times
  # not the value "2333455667"
  nhsnum[ which( nhsnum=="2333455667" ) ] <- NA #not the value "2333455667"
  
  # NHS number check
  NHScheck = 10 * as.numeric(substr(nhsnum,1,1)) +
    9 * as.numeric(substr(nhsnum,2,2)) + 
    8 * as.numeric(substr(nhsnum,3,3)) + 
    7 * as.numeric(substr(nhsnum,4,4)) + 
    6 * as.numeric(substr(nhsnum,5,5)) + 
    5 * as.numeric(substr(nhsnum,6,6)) + 
    4 * as.numeric(substr(nhsnum,7,7)) + 
    3 * as.numeric(substr(nhsnum,8,8)) + 
    2 * as.numeric(substr(nhsnum,9,9)) 
  NHScheck = 11 - NHScheck %% 11 # check digit
  NHScheck[ NHScheck==11 ] = 0 #If the result is 11 then a check digit of 0 is used.
  
  nhsnum[ which(NHScheck==10) ] <- NA #If the result is 10, then the NHS NUMBER is invalid and not used.
  nhsnum[ which(NHScheck != as.numeric(substr(nhsnum,10,10))) ] <- NA #actual NHSnumber check using the last digit of the NHS number
  rm( NHScheck )
  
  return( nhsnum ) #we return the processed NHS number
}

#' QUALITY CODE (0, 1, or 3) FOR A VARIABLE
#' @param processedvar = input variable with the processed variable (missing or invalid values set to NA)
#' @param originalvar  = input variable with the original variable (missing values set to "" )
#' @param nrecords     = length of either of the other input variables
#' @return Qvar        = output quality code for the variable
#'
#' Possible Quality values for the Quality variable Qvar:
#' 0=null or missing
#' 1=invalid (format or value)
#' 3=valid (for linkage), including very common values not defined as invalid
#' The quality of postcodes allows also "NHS Trust Site", not defined in this function
#' 
Dataset2_quality013 <- function( processedvar, originalvar, nrecords )  {
  
  Qvar = rep(3,nrecords) #all valid by default
  
  #1.0.missing values
  Qvar[ is.na(processedvar) & originalvar=="" ] = 0
  
  #1.1.invalid values
  Qvar[ is.na(processedvar) & originalvar!="" ] = 1 
  
  return( Qvar )
}

#' PROCESS DATE OF BIRTH
#' @param dob0 = input variable with the dates of birth in text form and with missing values set to NA
#' @return dob = output variable with the processed dates of birth in date format and with missing or invalid values in dob0 set to NA
#' 
Dataset2_dob_process <- function( dob0 ){
  
  # suppressing all blank spaces
  dob = gsub( "\\s", "", dob0 )

  # suppress the termination "00:00:00"
  # table( nchar( dob ) )  
  # head( dob[ which( sapply( dob, nchar )== 18 ) ] ) #"10/08/199500:00:00" "10/08/199500:00:00" "10/08/199500:00:00" "10/08/199500:00:00" "31/08/201000:00:00" 
  dob = gsub( "00:00:00", "", dob ) #suppressed the termination "00:00:00"
  
  dob01 = dob #I save a copy before the pre-processing
  
  #general case: "%d/%m/%Y"
  dob = as.Date( dob01, format = "%d/%m/%Y" )
  
  #year with only one digit
  head( dob01[ which( nchar( dob01 )== 7 ) ], 100) #"19/05/3" "26/08/9" "19/06/8" "19/12/7" "06/11/7" "05/08/8"
  sel = grep( "/", substr( dob01, nchar(dob01)-1, nchar(dob01) ) ) #those having a backslash at the last two characters of the date "%d/%m/%y"
  dob[ sel ] = as.Date( dob01[ sel ], format = "%d/%m/%y" )
  # head( dob01[ sel ], 100 ) #day, month, year
  # head( dob[ sel ], 100 ) #day, month, year
  
  #year with only two digits
  # head( dob01[ which( sapply( dob01, nchar )== 8 ) ],100 ) #"29/01/11" "30/11/70" "30/08/98" "24/05/98" "25/08/93" "25/12/97"
  # head( dob[ which( sapply( dob01, nchar )== 8 ) ],100 ) #"29/01/11" "30/11/70" "30/08/98" "24/05/98" "25/08/93" "25/12/97"
  # # but also "1/2/1949", which was formatted correctly 
  sel = which( nchar( dob01 ) == 8 & !grepl( "/", substr( dob01, 1, 2 ) ) ) #not those with a backslash at the first two positions
  # Year without century (00-99). On input, values 00 to 68 are prefixed by 20 and 69 to 99 by 19 - that is the behaviour specified by the 2004 and 2008 POSIX standards, but they do also say 'it is expected that in a future version the default century inferred from a 2-digit year will change'.
  dob[ sel ] = as.Date( dob01[ sel ], format = "%d/%m/%y" )
  # sum( format( dob[ sel ] , "%Y" ) > 2017 ) #203 births after 2017 ... CORRECTED NEXT ...
  # tail( sort(dob), 215 )
  
  #Year without century (00-99). On input, values 00 to 68 are prefixed by 20 and 69 to 99 by 19 - that is the behaviour specified by the 2004 and 2008 POSIX standards, but they do also say 'it is expected that in a future version the default century inferred from a 2-digit year will change'.
  sel = intersect( sel, which( format( dob , "%Y" ) > 2017 ) )
  dob[ sel ] = as.Date( format( dob[ sel ], "19%y-%m-%d" ) )
  # head( dob01[ sel ], 100 ) #day, month, year
  # head( dob[ sel ], 100 ) #day, month, year 
  # sel = which( format( dob , "%Y" ) > 2017 )
  # head( originaldata$dob0[ sel ], 100 ) #day, month, year #weird original dates of birth: "25/09/3003" "11/10/2039" "30/10/2036" "03/05/2022"
  # head( dob01[ sel ], 100 ) #day, month, year
  # head( dob[ sel ], 100 ) #day, month, year 
  
  #month in letters
  # head( dob01[ which( sapply( dob01, nchar )== 9 ) ],100 ) #"21-Nov-03" "07-Sep-00" "10-Aug-05" "22/2/1956" "05-Nov-04" "05-Nov-04"
  # head( dob[ which( sapply( dob01, nchar )== 9 ) ],100 ) #"21-Nov-03" "07-Sep-00" "10-Aug-05" "22/2/1956" "05-Nov-04" "05-Nov-04"
  sel = which( nchar( dob01 ) == 9 & !grepl( "/", dob01 ) ) #not those with a backslash
  # head( dob01[ sel ], 100 ) #day, month, year
  dob[ sel ] = as.Date( dob01[sel], format = "%d-%b-%y" )
  # head( dob[ sel ], 100 ) #day, month, year 
  # sum( format( dob[ sel ] , "%Y" ) > 2017 ) #no new births after 2017 introduced
  
  #using dashes instead of backslashes
  # head( dob[ which( sapply( dob01, nchar )== 10 ) ],100 ) #"17/12/1961" "17/12/1961" "18/05/1982" "11/03/2006" "28-05-2004" "28-05-2004"
  # head( dob01[ which( sapply( dob01, nchar )== 10 ) ],100 ) #"17/12/1961" "17/12/1961" "18/05/1982" "11/03/2006" "28-05-2004" "28-05-2004"
  sel = which( sapply( dob01, nchar )== 10 & !grepl( "/", dob01 ) ) #not those with a backslash
  # head( dob01[ sel ], 100 ) #day, month, year
  dob[ sel ] = as.Date( dob01[sel], format = "%d-%m-%Y" )
  # head( dob[ sel ], 100 ) #day, month, year 
  # sum( format( dob[ sel ] , "%Y" ) > 2017 ) #no new births after 2017 introduced
  
  #revisiting if I got any extreme dob by mistake during processing the variable
  # reporting the extreme values
  if( 0 ){
    summary( dob ) #2 NAs
    head( sort( dob ),10 ) #minimum values
    unique( originaldata$dob0[ dob %in% head( sort( dob ),10 ) ] )
    tail( sort( dob ),10 ) #maximum values
    unique( originaldata$dob0[ dob %in% tail( sort( dob ),10 ) ] )
  }
  rm( dob01 )
  
  #Invalid values set to missing
  dob[ which( dob < as.Date( "1895/01/01" ) ) ]  <- NA  #dob earlier than 1895/01/01
  dob[ which( dob > as.Date( "2017/04/01" ) ) ]  <- NA #dob later than 2017/04/01
  dob[ which( dob == as.Date( "1901/01/01" ) ) ] <- NA
  dob[ which( dob == as.Date( "1899/12/31" ) ) ] <- NA

  return( dob )  
}

#' PROCESS FORENAME
#' @param forename0 = input variable with the forenames in text form and with missing values set to NA
#' @return forename = output variable with the processed forename in text format and with missing or invalid values in forename0 set to NA
#' 
Dataset2_forename_process <- function( forename0 ){

  #converting to upper-case (not removing blanks)
  forename = toupper( forename0 )
  
  # head( unique(originaldata$forename0), 43 ) # F/i, MSTR, Sgt., ...
  # head( table( originaldata$forename0  ), 43 ) #a question mark, an accent starting the name, periods, ...
  
  #replaced periods with blanks (or nothing if were already followed by a blank space)
  # length( grep( "\\.", forename ) ) #23
  # unique( forename[ grep( "\\.", forename ) ] ) #"SGT\\. |MAST\\. |MRS\\. "
  sel = grep( "\\. ", forename ) #period+space is replaced with a space
  forename[ sel ] = gsub( "\\. ", " ", forename[sel] )
  sel = grep( "\\.", forename ) #period not followed by a space is replaced by a blank space
  forename[ sel ] = gsub( "\\.", " ", forename[sel] )
  
  #suppress leading and trailing blank spaces (introduced in the last step)
  forename = trimws( forename )
  
  #remove "^MISS " & "^MISS$" #4151
  # length( grep( "^MISS ", forename ) ) #4150
  # head( unique( originaldata$forename0[ grep( "^MISS ", forename ) ] ), 10 )
  # length( grep( "^MISS", forename ) ) #4152
  # setdiff( unique( originaldata$forename0[ grep( "^MISS", forename ) ] ), unique( originaldata$forename0[ grep( "^MISS ", forename ) ] ) )
  # length( grep( "^MISS$", forename ) ) #1
  sel = grep( "^MISS |^MISS$", forename )
  forename[ sel ] = gsub( "^MISS |^MISS$", "", forename[sel] )
  
  #remove "^MSTR |^MST |^MASTER$" #4069
  # length( grep( "^MSTR |^MST |^MASTER$", forename ) ) #4070
  # length( grep( "^MSTR|^MST|^MASTER$", forename ) ) #4069
  # head( unique( originaldata$forename0[ grep( "^MSTR ", forename ) ] ), 10 )
  sel = grep( "^MSTR |^MST |^MASTER$", forename )
  forename[ sel ] = gsub( "^MSTR |^MST |^MASTER$", "", forename[sel] )
  
  #remove "^MRS |^MS |^MASTE "
  # length( grep( "^MRS|^MS|^MASTE", forename ) ) #1063
  # length( grep( "^MRS |^MS |^MASTE ", forename ) ) #1062
  sel = grep( "^MRS |^MS |^MASTE ", forename )
  # forename[ setdiff( grep( "^MRS|^MS", forename ), sel ) ] #"MSGNA" not processed
  forename[ sel ] = gsub( "^MRS |^MS |^MASTE ", "", forename[sel] )
  forename[ grep( "MSGNA", forename ) ] = "GNA" #DANGER: one data-specific hand-coded change
  
  #remove "^MR |^MAST "
  # length( grep( "^MR|^MAST", forename ) ) #1938
  # length( grep( "^MR |^MAST ", forename ) ) #1937
  sel = grep( "^MR |^MAST ", forename )
  # forename[ setdiff( grep( "^MR|^MAST", forename ), sel ) ] #"MRADUL" not processed
  forename[ sel ] = gsub( "^MR |^MAST ", "", forename[sel] )
  forename[ grep( "MRADUL", forename ) ] = "ADUL" #DANGER: one data-specific hand-coded change
  
  #remove "^DR |^SGT "
  # length( grep( "^DR|^REV|^PROF|^SGT", forename ) ) #54
  # length( grep( "^DR |^REV |^PROF |^SGT ", forename ) ) #23
  sel = grep( "^DR |^SGT ", forename )
  # forename[ setdiff( grep( "^DR|^REV|^PROF|^SGT", forename ), sel ) ] #all seem valid first names
  forename[ sel ] = gsub( "^DR |^SGT ", "", forename[sel] )
  
  #remove "SHEIKH","SULTAN" and similar
  sel = grep( "SHEIKH|SHEIK|SHYKH|SHAYK|SHAYKH|CHEIKH|SHEKH|SHAIKH|SULTAN", forename )
  # unique( persdata[ sel , c("nhsnum", "forename","surname","orgid","locpatid","ntimeslocpatid") ] )
  # unique( forename[ sel ] )
  forename[ sel ] = gsub( "SHEIKHA|SHAIKHA|SHAIKHAH|CHAICA|SULTANA", "", forename[ sel ] ) #femenine forms
  forename[ sel ] = gsub( "SHEIKH|SHEIK|SHYKH|SHAYK|SHAYKH|CHEIKH|SHEKH|SHAIKH|SULTAN", "", forename[ sel ] ) 
  #suppress leading and trailing blank spaces (introduced in the last step)
  forename = trimws( forename )
  
  #removed a semicolon at the end of a forename
  sel = grep( ";$", forename )
  forename[ sel ] = gsub( ";$" , "", forename[ sel ] ) 
  
  #"name1  prev: name2" converted to "name1; name2"
  #"PREV:"+name (two particular instances) NEW-OLD FORENAMES converted to "NAME1; NAME2"
  # unique( persdata[ grep( "PREV:", forename ), c("forename","surname") ] )  
  sel = grep( "PREV:", forename )
  forename[sel] = gsub( "([A-Z ])(\\s)*PREV:(\\s)*([A-Z]*)", "\\1; \\4", forename[sel] )
  
  #remove "[0-9]"
  unique( forename[ grep( "[0-9]", forename, ignore.case = T ) ] )
  sel = grep( "[0-9]", forename )
  forename[ sel ] = gsub( "[0-9]", "", forename[sel] )
  
  #remove " ONE| TWO| THREE| TRIPLET III$| (TRIPLET|TWIN) II$| (TRIPLET|TWIN) I$"
  # head( unique( forename[ grep( " ONE| TWO| THREE", forename, ignore.case = T ) ] ), 100 )
  sel = grep( " ONE| TWO| THREE| TRIPLET III$| (TRIPLET|TWIN) II$| (TRIPLET|TWIN) I$", forename ) #20
  forename[ sel ] = gsub( " ONE| TWO| THREE| TRIPLET III$| (TRIPLET|TWIN) II$| (TRIPLET|TWIN) I$", "", forename[sel] )
  # head( unique( forename[ grep( "ONE|TWO|THREE", forename, ignore.case = T ) ] ), 100 )
  # head( unique( forename[ grep( " I$| II$| III$", forename, ignore.case = T ) ] ), 100 )
  # head( unique( originaldata$forename0[ grep( " I$| II$| III$", forename, ignore.case = T ) ] ), 100 )
  
  #remove "BABY| INFANT|TWIN|^TRIPLETS| TRIPLET| TRIP"
  # head( unique( forename[ grep( "BABY|TWIN|TRIP|INFANT", forename, ignore.case = T ) ] ), 100 )
  # unique( forename[ grep( "BABY", forename, ignore.case = T ) ] )
  # unique( forename[ grep( "TWIN", forename, ignore.case = T ) ] )
  # unique( forename[ grep( "^TRIPLETS| TRIPLET| TRIP", forename, ignore.case = T ) ] )
  # unique( originaldata$forename0[ grep( "TRIP", forename, ignore.case = T ) ] )
  sel = grep( "BABY| INFANT|TWIN|^TRIPLETS| TRIPLET| TRIP", forename )
  forename[ sel ] = gsub( "BABY| INFANT|TWIN|^TRIPLETS| TRIPLET| TRIP", "", forename[sel] )
  
  #remove "FEMALE$|MALE$|[^A-Z]BOY$|GIRL$"
  # unique( forename[ grep( "FEMALE$|MALE$|[^A-Z]BOY$|GIRL$", forename) ] )
  sel = grep( "FEMALE$|MALE$|[^A-Z]BOY$|GIRL$", forename )
  forename[ sel ] = gsub( "FEMALE$|MALE$|[^A-Z]BOY$|GIRL$", "", forename[sel] )
  # unique( forename[ grep( "MALE|BOY|GIRL|TWIN", forename) ] )
  
  #remove "^F/I$|^M/I$|^F-I|^M-I$" #108 yet
  # unique( forename[ grep( "^F/I$|^M/I$|^F-I|^M-I$", forename ) ] )
  sel = grep( "^F/I$|^M/I$|^F-I|^M-I$", forename )
  forename[ sel ] = gsub( "^F/I$|^M/I$|^F-I|^M-I$", "", forename[sel] )
  
  #"\\(\\)|\\( \\)|\\(Z"
  # unique( forename[ grep( "\\(|\\)", forename ) ] )
  # unique( forename[ grep( "\\(\\)|\\( \\)|\\(Z", forename ) ] )
  sel = grep( "\\(\\)|\\( \\)|\\(Z", forename )
  forename[ sel ] = gsub( "\\(\\)|\\( \\)|\\(Z", "", forename[sel] )
  
  #"\"" quote
  sel = grep( "\"", forename, fixed=T )
  forename[sel] = gsub( "\"", "", forename[sel])
  # persdata[ sel, c("forename","surname") ]
  
  #"\\\" backslash ##FEP: I do not know how to process this forename "Olivia Jaros\\Ewska"; the \ could be a Z or a /
  sel = grep( "\\", forename, fixed=T )
  sel = grep( "\\\\", forename )
  forename[sel]
  persdata[ sel, c("forename","surname") ] #the surname field is not empty
  
  #"known as" names in paranthesis
  sel = grep( " \\([A-Z]*\\)", forename )
  forename[ sel ] = gsub( "([A-Z]+)\\s\\(([A-Z]+)\\)", "\\1; \\2", forename[ sel ] )
  sel = grep( " \\([A-Z]+", forename )
  forename[ sel ] = gsub( "([A-Z]+)\\s\\(([A-Z]+)", "\\1; \\2", forename[ sel ] )
  
  #hyphens or commas not followed by text
  sel = grep( " \\-$|\\,$", forename )
  forename[ sel ] = gsub( " \\-$|\\,$", "", forename[sel] )
  #remove extra blank spaces in hyphenated composed names
  # sel = grep( " \\- ", forename )
  # forename[ sel ] = gsub( " \\- ", "\\-", forename[sel] )
  # sel = grep( "\\- | \\-", forename )
  # forename[ sel ] = gsub( "\\- | \\-", "\\-", forename[sel] )
  
  #replace hyphens with a single blank space 
  sel = grep( "\\-", forename )
  forename[ sel ] = gsub( "\\-", " ", forename[sel] )
  
  #suppress leading and trailing blank spaces (introduced in the last step)
  forename = trimws( forename )
  
  #suppress extra consecutive blank spaces
  # unique( forename[ grep( "\\s\\s\\s|\\s\\s", forename ) ] ) 
  sel = grep( "\\s\\s\\s|\\s\\s", forename )
  forename[ sel ] = gsub( "\\s\\s\\s|\\s\\s", " ", forename[sel] )
  
  #fixing NAs 
  forename[ which( forename == "" ) ] <- NA
  
  #any non-letter character left starting the forename? a question mark and a grave accent "`"
  sel = grep( "^[^A-Z]", forename ) #5
  # unique( cbind( originaldata$forename0[sel], forename[sel] ) ) #a question mark and a grave accent "`"
  forename[ sel ] = gsub( "^\\`|^\\?", "", forename[sel] )
  forename[ which( forename == "" ) ] = NA
  
  #any grave accent used as apostrophe?
  sel = grep( "\\`", forename ) #29
  # unique( cbind( originaldata$forename0[sel], forename[sel] ) ) 
  forename[ sel ] = gsub( "\\`", "\\'", forename[sel] ) #we replace the grave accent with an apostrophe
  
  #no character left other than letters, semicolons, blanks and apostrophes (exceptions: a comma and a backslash)
  # head( forename[ grep( "[^A-Z; ']", forename) ] )
  #any accent on forenames? not likely
  # grep( "Á|É|Í|Ó|Ú|Ý|À|È|Ì|Ò|Ù|Â|Ã|Ä|Å", forename )
  
  # head( sort( table( forename), decreasing = T ), 200 )  #no left too-common unreal values apparently 
  # sort( table( forename), decreasing = T )[ 201:400 ]
  # tail( sort( table( forename), decreasing = T ), 200 )  #no too-common values apparently 
  
  return( forename )
}


#' PROCESS SURNAME
#' @param surname0 = input variable with the surnames in text form and with missing values set to NA
#' @param locpatid = processed local patient id, it is used to see if some surnames were filled with the local patient id
#' @return surname = output variable with the processed surname in text format and with missing or invalid values in forename0 set to NA
#' 
Dataset2_surname_process <- function( surname0, locpatid ){
  
  #converting to upper-case (not removing blanks)
  surname = toupper( surname0 )
  
  # sort( table( surname), decreasing = T )[ 1:700 ]  #most common are: Smith, Jones, Williams, Hussain, Taylor, Khan  
  # unique( persdata[ is.na( persdata$forename ), c("forename","surname") ] ) 
  # unique( persdata[ is.na( surname ), c("forename","surname") ] ) 
  
  #commas: removed, and if forename field missing and valid at surname field after the comma, then forename placed in the forename field
  # unique( persdata[ grep( ",", surname ), c("forename","surname") ] ) 
  sel = grep( ",", surname ) #2
  surname[ sel ] = gsub( ",.*" ,  "", surname[ sel ] ) #the surname is the text preceding the comma (we delete the comma and what follows)
  # unique( cbind( originaldata[ sel, c("forename0","surname0") ], surname[ sel ] ) ) 
  # sel = grep( ",", originaldata$surname0 ) #check
  # cbind( originaldata[ sel, c("forename0","surname0") ], persdata[ sel, c("forename","surname") ] )
  
  #periods: removed an replaced with a blank space if neeeded 
  # unique( persdata[ grep( "\\.", surname ), c("forename","surname") ] )  ## PREV: or (PREV.
  sel = grep( "\\.", surname ) #15
  surname[ sel ] = gsub( "\\.[^A-Z]|([A-Z])\\.([A-Z])" ,  "\\1 \\2", surname[ sel ] ) #those to replace with a blank space
  surname[ sel ] = gsub( "\\." ,  "", surname[ sel ] ) #those to be deleted
  # sel = grep( "\\.", originaldata$surname0 ) #check
  # cbind( originaldata[ sel, c("forename0","surname0") ], persdata[ sel, c("forename","surname") ] )
  
  #numbers: "0'name" replaced with "O'name"; lopatids and numbers removed 
  # unique( persdata[ grep( "[0-9]", surname ), c("forename","surname", "locpatid", "nhsnum") ] ) 
  sel = grep( "[0-9]", surname ) #5
  surname[ sel ] = gsub( "0'" ,  "O'", surname[ sel ] ) # "0'name" replaced with "O'name"
  surname[ sel[ surname[sel] == persdata$locpatid[sel] ] ] = NA #surnames equal to the local patient id were removed
  surname[ sel ] = gsub( "[0-9]", "", surname[ sel ] ) #other numbers removed
  # sel = grep( "[0-9]", originaldata$surname0 ) #check
  # cbind( originaldata[ sel, c("forename0","surname0") ], persdata[ sel, c("forename","surname") ] )
  
  #Removed " ONE| TWO| THREE| I$| II$| III$"
  # unique( persdata[ grep( " ONE| TWO| THREE", surname ), c("surname") ] ) 
  # unique( persdata[ grep( " I$| II$| III$", surname ), c("surname") ] ) 
  sel = grep( " ONE| TWO| THREE| I$| II$| III$", surname )
  surname[ sel ] = gsub( " ONE| TWO| THREE| I$| II$| III$", "", surname[ sel ] )
  # cbind( originaldata[ sel, c("forename0","surname0") ], persdata[ sel, c("forename","surname") ] )
  
  #Removed " TWIN| TW| TRIPLET| TRIP"
  # unique( persdata[ grep( " TWIN| TW| TRIPLET| TRIP", surname ), c("surname") ] ) 
  sel = grep( " TWIN| TW| TRIPLET| TRIP", surname )
  surname[ sel ] = gsub( " TWIN| TW| TRIPLET| TRIP", "", surname[ sel ] )
  # cbind( originaldata[ sel, c("forename0","surname0") ], persdata[ sel, c("forename","surname") ] )
  
  #semicolons? (I need to clear those before the next step in which multiple names are separated by a semicolon)
  sel = grep( "\\;", surname ) #6
  surname[sel] = gsub( " PREV;| ;REV:", "; ", surname[sel] ) #as in NEW-OLD surnames below
  sel = grep( "O\\;[A-Z]", surname ) #1
  surname[sel] = gsub( "\\;", "\\'", surname[sel] )

  # NEW-OLD SURNAMES converted to "NAME1; NAME2"
  
  # "name1:  prev:name2"
  sel = grep( ":  PREV:", surname ) #both the "V" and the blank space are optional
  surname[sel] = gsub( "([A-Z ])(\\s)*:  PREV:(\\s)*([A-Z]*)", "\\1; \\4", surname[sel] )
  
  # "name1  prev: name2" converted to "name1; name2"
  # unique( persdata[ grep( "PREV:", surname ), c("forename","surname") ] )  
  sel = grep( "PRE?V? ?:", surname ) #both the "V" and the blank space are optional
  surname[sel] = gsub( "([A-Z ])(\\s)*PRE?V? ?:(\\s)*([A-Z]*)", "\\1; \\4", surname[sel] )
  # length( grep( ";", surname[sel] ) ) == length(sel) #all  replaced?
  
  # "name1  :prev name2" converted to "name1; name2"
  # unique( persdata[ grep( ":PREV", surname ), c("forename","surname") ] )  
  sel = grep( ": ?PREV", surname )
  surname[sel] = gsub( "([A-Z ])(\\s)*: ?PREV(\\s)*([A-Z]*)", "\\1; \\4", surname[sel] )
  # length( grep( ";", surname[sel] ) ) == length(sel) #all replaced?
  
  # name1 (was name2)
  unique( persdata[ grep( "\\(WAS", surname ), c("forename","surname") ] ) 
  sel = grep( "\\(WAS", surname )
  surname[sel] = gsub( "([A-Z ])(\\s)*\\(WAS (\\s)*([A-Z]*)(\\s)*\\)", "\\1; \\4", surname[sel] )
  # length( grep( ";", surname[sel] ) ) == length(sel) #all replaced?
  
  # name1 (prev name2)
  # unique( persdata[ grep( "\\(PREV", surname ), c("forename","surname") ] ) ## (PREV or (PREV.
  sel = grep( "\\(PREV", surname )
  surname[sel] = gsub( "([A-Z ])(\\s)*\\(PREV (\\s)*([A-Z]*)(\\s)*\\)", "\\1; \\4", surname[sel] )
  # length( grep( ";", surname[sel] ) ) == length(sel) #all replaced?
  
  # name2 (now name1) converted to "name1; name2"
  unique( persdata[ grep( "\\(NOW", surname ), c("forename","surname") ] ) 
  sel = grep( "\\(NOW", surname )
  surname[sel] = gsub( "([A-Z]+).+\\(NOW (\\s)*([A-Z]*)(\\s)*\\)", "\\3; \\1", surname[sel] )
  # length( grep( ";", surname[sel] ) ) == length(sel) #all replaced?
  
  # "name1 (nee name2)"  
  # unique( persdata[ grep( "\\(NEE", surname ), c("forename","surname") ] ) 
  sel = grep( "\\(NEE", surname )
  surname[sel] = gsub( "([A-Z]+).+\\(NEE (\\s)*([A-Z]*)(\\s)*\\)", "\\1; \\3", surname[sel] )
  # length( grep( ";", surname[sel] ) ) == length(sel) #all replaced?

  # "name1 [nee name2]"  
  # unique( persdata[ grep( "\\(NEE", surname ), c("forename","surname") ] ) 
  sel = grep( "\\[NEE", surname )
  surname[sel] = gsub( "([A-Z]+).+\\[NEE (\\s)*([A-Z]*)(\\s)*\\]", "\\1; \\3", surname[sel] )
  # length( grep( ";", surname[sel] ) ) == length(sel) #all replaced?
  
  # "name1 (originally name2)
  # unique( persdata[ grep( "\\(ORIGINALLY", surname ), c("forename","surname") ] ) 
  sel = grep( "\\(ORIGINALLY", surname )
  surname[sel] = gsub( "([A-Z]+).+\\(ORIGINALLY (\\s)*([A-Z]*)(\\s)*\\)", "\\1; \\3", surname[sel] )
  # length( grep( ";", surname[sel] ) ) == length(sel) #all replaced?
  
  # name1 (name2)
  # unique( persdata[ grep( "\\([^WAS|PREV|NOW|NEE]", surname ), c("forename","surname") ] ) 
  sel = grep( "\\(", surname )
  surname[sel] = gsub( "([A-Z]+).+\\(([A-Z\\-]+)\\)", "\\1; \\2", surname[sel] )
  # length( grep( ";", surname[sel] ) ) == length(sel) #all replaced?
  
  # name1/name2
  # unique( persdata[ grep( "\\/", surname ), c("forename","surname") ] )
  sel = grep( "\\/", surname )
  surname[sel] = gsub( "([A-Z]+)(\\s)*\\/(\\s)*([A-Z\\-]+)", "\\1; \\4", surname[sel] )
  # length( grep( ";", surname[sel] ) ) == length(sel) #all replaced?
  
  #replace hyphens "-" and "_" with a single space 
  # tail( unique( persdata[ grep( "-", surname ), c("surname") ] )[1:700], 1000 )  #LARGE GROUP
  sel = grep( "\\-|\\_", surname )
  surname[ sel ] = gsub( "\\-|\\_", " ", surname[sel] )
  
  #suppress leading and trailing blank spaces 
  surname = trimws( surname )
  
  #suppress extra consecutive blank spaces
  # unique( surname[ grep( "\\s\\s\\s|\\s\\s", surname ) ] ) 
  sel = grep( "\\s\\s\\s|\\s\\s", surname )
  surname[ sel ] = gsub( "\\s\\s\\s|\\s\\s", " ", surname[sel] )
  
  #fixing NAs 
  surname[ which( surname == "" ) ] = NA
  
  #any non-letter character left starting the surname?
  # grep( "^[^A-Z]", surname ) #NO
  
  #any grave accent used as apostrophe?
  sel = grep( "\\`", surname ) #230
  # unique( cbind( originaldata$surname0[sel], surname[sel] ) ) #a question mark and a grave accent "`"
  surname[ sel ] = gsub( "\\`", "\\'", surname[sel] )
  
  #no character left other than letters, semicolons, blanks and apostrophes
  # head( surname[ grep( "[^A-Z; ']", surname) ] )
  
  # head( sort( table(surname) ), 100 )
  # tail( sort( table(surname) ), 50 )

  return( surname )
}

#' GENERATE A LOOKUP POSTCODE-COUNTRY
#' @param inDIRpostcodes             = input directory for the postcode data
#' @param ODSpostcodesIN             = ONS data file with all postcodes
#' @param inLookupUK_PostcodeCountry = name of the saved lookup file between postcodes and UK countries (+"Overseas" category)
#' ODS list of postcodes (source https://digital.nhs.uk/services/organisation-data-service/data-downloads/office-for-national-statistics-data)
#' 
Dataset2_Generate_LookupUK_PostcodeCountry <- function( inDIRpostcodes, ODSpostcodesIN, inLookupUK_PostcodeCountry ){ 
  
  #reading the ONS data file with all postcodes
  if( file.exists( paste0(inDIRpostcodes, ODSpostcodesIN) ) ){
    aux = read.csv( paste0(inDIRpostcodes, ODSpostcodesIN), stringsAsFactors = F ) #this takes a bit
  } else {
    stop( paste0("ERROR: cannot find the input file ", paste0paste0(inDIRpostcodes, ODSpostcodesIN), "; please revise the input directory for postcode data and file name" ) )
  }
  
  #PCD2: Unit postcode - 8 character version
  #PCDS: Unit postcode - variable length (eGif) version 
  
  #TEXT PROCESSING FOR THE LOOKUP OF POSTCODES
  for( x in names(aux) ){ 
    #Upper case
    aux[ , x ] = toupper( aux[ , x ] )
    #trimming leading and trailing blank spaces
    aux[ , x ] = trimws( aux[ , x ] )
    #further, suppressing all blank spaces
    aux[ , x ] = gsub("\\s", "", aux[ , x ] )
    #missing values set to NA (R language specific) 
    sel = which( aux[, x] %in% c("",NA) )
    if( length(sel) ) aux[ sel, x] <- NA      
  }
  
  #PCD2 processed equals PCD2 processed
  # head( aux )
  # with( aux, sum( PCD2 == PCDS ) == nrow(aux) ) 
  
  aux = aux[ , c("PCDS", "CTRY") ]
  
  #COUNTRY: ONS code version
  # E92000001 = England;
  # L93000001 = Channel Islands; #Crown Dependencies
  # M83000003 = Isle of Man      #Crown Dependencies
  # N92000002 = Northern Ireland;
  # S92000003 = Scotland;
  # W92000004 = Wales;
  # table( aux$CTRY )
  
  #COUNTRY: LAUNCHES version
  aux$country = NA
  aux$country[ aux$CTRY == "E92000001" ] = "England"
  aux$country[ aux$CTRY == "L93000001" ] = "Crown Dependencies"
  aux$country[ aux$CTRY == "M83000003" ] = "Crown Dependencies"
  aux$country[ aux$CTRY == "N92000002" ] = "Northern Ireland"
  aux$country[ aux$CTRY == "S92000003" ] = "Scotland"
  aux$country[ aux$CTRY == "W92000004" ] = "Wales"
  
  #yet the ZZ99**Z codes have no country assigned
  table( aux$country, useNA = "always" )
  sort( aux$PCDS[ is.na(aux$country) ] ) # ZZ99**Z postcodes have no country assigned ...
  
  # "ZZ99**Z" country is Overseas by default
  aux$country[ is.na(aux$country) ] = "Overseas"
  # Exceptions: 
  # "ZZ99 3CZ" England. It is used for: England; Great Britain; United Kingdom; United Kingdom (not otherwise stated).
  aux$country[ which( aux$PCDS == "ZZ993CZ" ) ] = "England"
  # "ZZ99 3GZ" was assigned Wales. It is used for: Wales.
  aux$country[ which( aux$PCDS == "ZZ993GZ" ) ] = "Wales"
  # "ZZ99 1WZ" was assigned Scotland. It is used for: Scotland.
  aux$country[ which( aux$PCDS == "ZZ991WZ" ) ] = "Scotland"
  # "ZZ99 2WZ" (9 records) was assigned Northern Ireland. It is used for: Northern Ireland; Ulster.
  aux$country[ which( aux$PCDS == "ZZ992WZ" ) ] = "Northern Ireland"
  # "ZZ99 3BZ" was assigned "Crown Dependencies". It is used for: Isle of Man.
  aux$country[ which( aux$PCDS == "ZZ993BZ" ) ] = "Crown Dependencies"
  # "ZZ99 3HZ" was assigned "Crown Dependencies". It is used for: Alderney; Brechou; Channel Islands; Sark, Little and Great; Channel Islands (not otherwise stated).
  aux$country[ which( aux$PCDS == "ZZ993HZ" ) ] = "Crown Dependencies"
  
  # table( aux$CTRY, aux$country, useNA = "always" ) # ONS-LAUNCHES correspondence looks right
  aux = aux[ , c("PCDS", "country") ] 
  names(aux) <- c("Postcode", "Country" )
  
  #I save the lookup Postcode-Country  
  write.csv( x = aux, file = paste0(inDIRpostcodes, inLookupUK_PostcodeCountry), row.names = F )
}


#' PROCESS POSTCODE 
#' @param postcode0 = input variable with the postcodes in text form and with missing values set to NA
#' @param nrecords  = length of postcode0
#' @return list(postcode,postcodeQ), where
#'           postcode  = processed postcode in text format and with missing or invalid values in forename0 set to NA
#'           postcodeQ = quality code for the postcode field (0=missing, 1=invalid (not 2), 2=NHS Trust Site, 3=valid)
#'           
Dataset2_postcode_process <- function( postcode0, nrecords ){
  
  #upper-case and removing blanks
  postcode = toupper( postcode0 )
  postcode = gsub( "\\s", "", postcode )
  
  # head( postcode, 10 )
  # table( nchar( unique(postcode ) ) )
  
  # removing periods, parentheses, hyphens, and slashes
  sel = grep( "\\.$|[0-9]\\.[0-9]", postcode )
  postcode[ sel ] = gsub( "\\.$|([0-9])\\.([0-9])", "\\1\\2", postcode[sel] )
  sel = grep( "\\.", postcode )
  postcode[ sel ] = NA ##one period left, the value is a "village.country"
  sel = grep( "\\(|\\)|\\-", postcode )
  postcode[ sel ] = gsub( "\\(|\\)|\\-", "", postcode[sel] )
  sel = grep( "\\\\|\\/", postcode )
  postcode[ sel ] = gsub( "\\\\|\\/", "", postcode[sel] )  
  
  # removed the Weird character "Á" (the reslting postcodes are all valid then)
  sel = grep( "Á", postcode ) #39 records
  # unique( originaldata$postcode0[ sel ] )
  # unique( originaldata$orgid0[ sel ] ) ##all from the same Hospital, no surprise
  test = gsub( "Á", "", postcode[ sel ])
  ##sum( test %in% LookupUK$Postcode ) == length( test ) #yep, that solves the issue
  postcode[sel] = test

  #Postcodes with only text ... setting to missig
  sel = grep( "[0-9]", postcode ) # 140389
  sel = union( sel, which( is.na( postcode ) ) ) # 143425
  sel = setdiff( 1:nrecords, sel ) # 437 !
  # sort( table( postcode[ sel ] ), decreasing = T ) 
  postcode[ sel ] <- NA
  
  # those containing "JANUARY"
  sel = grep( "JANUARY", postcode ) 
  postcode[ sel ] <- NA

  # postcodes starting with a number (I will assign the country for a few ones)
  sel = grep( "^[0-9]", postcode ) #10 records
  # unique( originaldata[ sel, c("dob0", "locpatid0", "nhsnum0", "postcode0") ] ) 
  postcode[ sel ] <- NA

  # Invalid "ZZ" codes set to missing: "ZZ999ZZ" (2,433 records), "ZZ1111Z", "ZZ000AA", "ZZ12", "ZZ", "Z99", and "ZZ99" (15 records) were considered as invalid 
  sel = which( postcode %in% c( "ZZ999ZZ", "ZZ1111Z", "ZZ000AA", "ZZ12", "Z99", "ZZ99" ) ) #2448
  postcode[ sel ] = NA
  
  # ZZ99 postcodes (denoting country) considered invalid (countries assigned later)
  sel = grep( "^ZZ99", postcode ) # 1634 records
  postcode[ sel ] = NA

  # 2 to 4 character postcodes (denoting postcode area or postcode sector) considered also invalid
  sel = which( nchar(postcode) %in% c(2,3,4) ) # 60 records
  postcode[ sel ] = NA
  
  #create the needed list of valid postcodes, if not existing
  create_lookup = TRUE
  if( file.exists( paste0(inDIRpostcodes, inLookupUK_PostcodeCountry) ) ) create_lookup = FALSE
  if( create_lookup ) Dataset2_Generate_LookupUK_PostcodeCountry( inDIRpostcodes, ODSpostcodesIN, inLookupUK_PostcodeCountry )
  
  #loading the lookup of valid postcodes (to Country)
  if( file.exists( paste0(inDIRpostcodes, inLookupUK_PostcodeCountry) ) ){
    LookupUK = read.csv( paste0(inDIRpostcodes, inLookupUK_PostcodeCountry) , header=T, colClasses = "character", fill = T ) 
  } else {
    stop( paste0("ERROR: cannot find the input file ", paste0(inDIRpostcodes, inLookupUK_PostcodeCountry), "; please revise the input directory and file name" ) )
  }
  
  #postcodes not found in the list of valid postcodes (lookup created above) are considered invalid
  # table( nchar(LookupUK$Postcode ) ) #valid postcodes have 5 to 7 characters
  # sel = which( ! postcode %in% LookupUK$Postcode ) #6083
  # sel = intersect( sel, which( nchar(postcode) %in% c(5,6,7) ) ) #914
  # sort( table( postcode[ sel ] ) )
  # sort( table( originaldata$postcode0[ sel ] ) )
  sel = which( ! ( postcode %in% LookupUK$Postcode | is.na(postcode) ) ) # 915
  postcode[ sel ] = NA
  
  #Postcodes of NHS Trust Sites
  aux = read.csv( paste0(inDIRpostcodes, ODSpostcodesNHSTrustSites), stringsAsFactors = F, header = F ) #232 Trusts and 30,288 Trust sites
  aux = unique(aux$V10[ which(nchar(aux$V1)==5) ]) #only the 14,606 postcodes of NHS Trust sites, with no blank spaces
  sel = which( postcode %in% gsub("\\s", "", aux) ) # 1165 NHS Trust sites
  postcode[ sel ] = NA ## The postcodes of NHS Trust Sites are not used for linkage

  # Possible Quality values for postcodes:
  # 0=null or missing
  # 1=invalid (format or value) 
  # 2=NHS Trust or Site
  # 3=valid (for linkage), including very common values not defined as invalid
  postcodeQ = rep(3,nrecords) #all valid by default
  #6.0.missing values
  postcodeQ[ is.na(postcode) & is.na(postcode0) ] = 0
  #6.1.invalid values     #Those that were originally filled but then non-valid; #FEP: I assigned the ZZ99 values as missing, and their quality is considered invalid for linkage
  postcodeQ[ is.na(postcode) & !is.na(postcode0) ] = 1
  #6.2.very common values #FEP: I assigned the hospital and ZZ99 values as missing, and their quality is considered invalid for linkage
  postcodeQ[ sel ] = 2
  
  return( list(postcode=postcode,
               postcodeQ=postcodeQ) )
}

