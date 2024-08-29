#' LAUNCHES DATASET1 ALLDATA LIB. R functions to create LAUNCHESQI Dataset1
#' @date 2019, July 15
#' @author Ferran Espuny Pujol, PhD
#'
#' For the LAUNCHES project, we need to create, based on the NCHDA data:
#' Dataset1 (original data plus a record identifier LAUNCHESrecID)
#'
#' The steps implemented in this code as functions are:
#'    2 	Generate a LAUNCHES ID for each record
#'


#' GENERATE a LAUNCHES ID for each record
#' @param nrecords       = number of records
#' @param randomseed     = seed for random number generator
#' @return LAUNCHESrecID = array of length nrecords containing unique 8-letter identifiers, all starting with R.
#' 
Dataset1_generateLAUNCHESrecID <- function( nrecords, randomseed ){

  set.seed( randomseed ) #setting the seed for the random number generator allows reproducibility of results
  
  #LAUNCHES record ID: #https://www.lillemets.ee/data_pseudonymization_in_r.html
  LAUNCHESrecID <- rep( NA, nrecords )
  
  while( any( duplicated(LAUNCHESrecID)) ) { # Loop until all keys are unique (brute force loop, I did not think about improving it)
    LAUNCHESrecID <- replicate( nrecords, 
                                paste0( "R",                                                        #first character is the letter "R" of Row or Record
                                        paste(sample(c(LETTERS), 7, replace = T), collapse = '')) ) # 7 following characters are upper-case letters
  }

  LAUNCHESrecID = sort( LAUNCHESrecID )
  
  return( LAUNCHESrecID)
}


