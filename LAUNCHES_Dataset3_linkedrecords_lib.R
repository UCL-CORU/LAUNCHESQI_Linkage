#' LAUNCHES DATASET3 LINKED RECORDS LIB. R functions to document the quality of matches
#' @date 2019, July 15
#' @author Ferran Espuny Pujol, PhD
#'
#'This code provides the functions used to compare or assess the quality of:
#' - two dates of birth
#' - two names (forename+surname)
#' 


#' COMPARE_DOB: Auxiliary function to compare two dates of birth
#'
#' @return  3 if exact agreement
#'          2 if partial agreement
#'          1 if at least one dob is missing
#'          0 if disagreement between both dobs
#'
compare_DOB <- function ( dob1, dob2) {
  
  #1: some missing dob
  if( anyNA( c(dob1,dob2) ) ) return ( 1 ) 
  
  #both valid ...
  
  #3: exact agreement
  if( dob1 == dob2 ) return( 3 )
  
  #2: partial agrement
  if( abs(dob1-dob2)<=5 ) return( 2 ) #5-day partial match on dob
  d1 = format( dob1, "%d")
  m1 = format( dob1, "%m")
  Y1 = format( dob1, "%Y")
  d2 = format( dob2, "%d")
  m2 = format( dob2, "%m")
  Y2 = format( dob2, "%Y")
  change2 = ( d1==d2 & m1==m2 ) #day and month partial match on dob
  change2 = change2 | (d1==d2 & Y1==Y2 ) #day and Year partial match on dob
  change2 = change2 | (m1==m2 & Y1==Y2 ) #month and Year partial match on dob
  change2 = change2 | (d1==m2 & m1==d2 ) #day and month swapped partial match on dob
  change2 = change2 | (d1==m2 & Y1==Y2 ) #day and month swapped partial match on dob
  change2 = change2 | (m1==d2 & Y1==Y2 ) #day and month swapped partial match on dob
  if( change2 ) return( 2 )
  
  #0: disagreement
  return( 0 )
}


#' COMPARE_NAMES: Auxiliary function to compare two names (forename+surname)
#'
#' @return  3 if exact agreement
#'          2 if partial agreement: when part of double forenames or surnames (e.g. "Mary" or "Jane" in "Mary Jane" ) match exctly another forename or surname
#'          1 if either value is missing #added on 2019 July 11
#'          0 if disagreement
#'
#'@example compare_names( "FERRAN; FARREN", "ESPUNY PUJOL", "FERRAN", "ESPUNY PUJOL" ) #returns 3
#'         compare_names( "FARREN", "ESPUNY PUJOL", "FERRAN", "ESPUNY PUJOL" )         #returns 0
#'         compare_names( "FERRAN; FARREN", "ESPUNY PUJOL", "FERRAN ELIAS", "ESPUNY" ) #returns 2
#'                
#' @note If you want to modify this code, take into account that this is a recursive function:
#'       First, missing values and exact agreement are sought for
#'       This step compares different possible names by calling the function with each variant of the forename/surname (in the data, multiple names are separated with semicolons)
#'       Then, partial agreement is sought 
#'                          
compare_names <- function ( forename1, surname1, forename2, surname2 ) {
  
  #1: some missing value
  if( anyNA( c(forename1,surname1,forename2,surname2) ) ) return ( 1 ) #FEP: the value 0 was returned before if missing names found
  
  #both valid ...
  
  #3: exact agreement
  if( forename1 == forename2 & surname1 == surname2 ) return( 3 )
  
  #3: exact agreement for multiple names that did not match exactly (recursive call)
  self1 = grep(";", forename1)
  self2 = grep(";", forename2)
  sels1 = grep(";", surname1)
  sels2 = grep(";", surname2)
  # we split forename 1
  if( length( self1 ) ){
    part1 = gsub( "; [A-Z]+", "", forename1 ) 
    part2 = gsub( "[A-Z]+; ", "", forename1 )
    aux1 = compare_names( part1, surname1, forename2, surname2 )
    aux2 = compare_names( part2, surname1, forename2, surname2 )
    if( aux1==3 | aux2==3 ) return( 3 )
    if( aux1==2 | aux2==2 ) return( 2 ) 
  }
  # we split forename 2
  if( length( self2 ) ){
    part1 = gsub( "; [A-Z]+", "", forename2 ) 
    part2 = gsub( "[A-Z]+; ", "", forename2 )
    aux1 = compare_names( forename1, surname1, part1, surname2 )
    aux2 = compare_names( forename1, surname1, part2, surname2 )
    if( aux1==3 | aux2==3 ) return( 3 ) 
    if( aux1==2 | aux2==2 ) return( 2 ) 
  }
  # we split surname 1
  if( length( sels1 ) ){
    part1 = gsub( "; [A-Z]+", "", surname1 ) 
    part2 = gsub( "[A-Z]+; ", "", surname1 )
    aux1 = compare_names( forename1, part1, forename2, surname2 )
    aux2 = compare_names( forename1, part2, forename2, surname2 )
    if( aux1==3 | aux2==3 ) return( 3 ) 
    if( aux1==2 | aux2==2 ) return( 2 ) 
  }
  # we split surname 2
  if( length( sels2 ) ){
    part1 = gsub( "; [A-Z]+", "", surname2 ) 
    part2 = gsub( "[A-Z]+; ", "", surname2 )
    aux1 = compare_names( forename1, surname1, forename2, part1 )
    aux2 = compare_names( forename1, surname1, forename2, part2 )
    if( aux1==3 | aux2==3 ) return( 3 ) 
    if( aux1==2 | aux2==2 ) return( 2 ) 
  }
  
  #2: partial agreement
  if( grepl(";", forename1) | grepl(";", forename2) | grepl(";", surname1) | grepl(";", surname2) ){
    return( 0 ); ## the function did not split all multiple names (containing semicolons ;)
  }
  self1 = grep(" |,|\\\\", forename1) # the characters dividing composed names are blank spaces, but a comma and backslashes are allowed once each
  self2 = grep(" |,|\\\\", forename2)
  sels1 = grep(" |,|\\\\", surname1)
  sels2 = grep(" |,|\\\\", surname2)
  partialforename = F
  partialsurname  = F
  # we split forename 1 if forename 2 is simple
  if( length( self1 ) & !length( self2 ) ){
    parts = unlist( strsplit( forename1, " |, |\\\\" ) )
    if( forename2 %in% parts ) partialforename = T
  }
  # we split forename 2 if forename 1 is simple
  if( length( self2 ) & !length( self1 ) ){
    parts = unlist( strsplit( forename2, " |, |\\\\" ) )
    if( forename1 %in% parts ) partialforename = T
  }
  # we split surname 1 if surname 2 is simple
  if( length( sels1 ) & !length( sels2 ) ){
    parts = unlist( strsplit( surname1, " |, |\\\\" ) )
    if( surname2 %in% parts ) partialsurname = T
  }
  # we split surname 2 if surname 1 is simple
  if( length( sels2 ) & !length( sels1 ) ){
    parts = unlist( strsplit( surname2, " |, |\\\\" ) )
    if( surname1 %in% parts ) partialsurname = T
  }
  if( partialforename==T   & partialsurname==T  ) return( 2 )
  if( partialforename==T   & surname1==surname2 ) return( 2 )
  if( forename1==forename2 & partialsurname==T  ) return( 2 )
  
  #0: disagreement
  return( 0 )
}

