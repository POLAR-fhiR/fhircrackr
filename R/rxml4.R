###
# load neccessary packages
###
library( xml2 )
library( stringr )

###
# convenient functions
###


###
# th conatenates the right ending with a number. e.g 1st 2nd 3rd ...
###
th <- function( n ) {

	n.th <- n < 1 | 3 < n
	n.1st <- n == 1
	n.2nd <- n == 2
	n.3rd <- n == 3
	
	n[ n.th ] <- paste0( n[ n.th ], "th" )
	n[ n.1st ] <- "1st"
	n[ n.2nd ] <- "2nd"
	n[ n.3rd ] <- "3rd"
	
	n
}
# example
# th( 0 : 4 )


###
# convenient functions
# for use in lapply to get a named list as result
###
lst <-
	function( ..., pre = NULL, post = NULL ) {
		
		v <- as.list( c( ... ) )
		
		names( v ) <- paste0( pre, v, post )
		
		v
	}
# examples
# lst( LETTERS[ 1 : 5 ] )
# lst( LETTERS[ 1 : 5 ], pre = "Patient[", post = "]" )


bind.paths <- function( path1, path2 ) paste0( sub( "/$", "", path1 ), "/", sub( "^/", "", path2 ) )
# examples
# bind.paths( "data", "patients" )
# bind.paths( "data/", "patients" )
# bind.paths( "data", "/patients" )
# bind.paths( "data/", "/patients" )



###
# some functions
###

###
# fhir.get.bundle.page.as.xml.from.server
# downloads one page from a fhir server via fhir search request (address)
###
fhir.get.bundle.page.as.xml.from.server <- function( address, max.attempts = 5 ) {
	
	for( n in 1 : max.attempts ) {
		
		cat( paste0( th( n ), " attempt to download: ", address, "\n" ) )
		
		r <- try(
			
			read_xml( address, silent = T )
		)
		
		if( class( r )[ 1 ] != "try-error" ) {
			
			xml_ns_strip( r )
			
			return( r )
		}
		
		Sys.sleep( 10 )
	}
	
	NULL
}

###
# fhir.get.bundle.as.xml.list.from.server
# downloads all pages from a fhir server via fhir search request (address)
###
fhir.get.bundle.as.xml.list.from.server <- function( address, max.attempts = 5 ) {
	
	xmls <- list( )
	
	addr <- address
	
	repeat {
		
		xml <- fhir.get.bundle.page.as.xml.from.server( addr, max.attempts )
		
		if( is.null( xml ) ) {
			
			cat( "download interrupted.\n" )
			
			break
		}
		
		xmls[[ addr ]] <- xml
		
		links <- xml_find_all( xml, "/Bundle/link" )
		
		rels.nxt  <- xml_attr( xml_find_first( links, "./relation" ), "value" ) == "next"
		
		if( ! any( ! is.na( rels.nxt ) & rels.nxt ) ) {
			
			cat( "\ndownload completed\n" )
			
			break
		}
		
		urls  <- xml_attr( xml_find_first( links, "./url" ), "value" )
		
		addr <- urls[ rels.nxt ][ 1 ]
		
		if( addr == "" ) {
			
			cat( "\ndownload completed\n" )
			
			break
		}
	}
	
	xmls
}
# example
#xmls <- fhir.get.bundle.as.xml.list.from.server( url.obs )
# )
####


###
# save all pages as xml file in a directory
###
fhir.save.bundle.as.xml.list.in.directory <- function( xmls, path ) {
	
	w <- 1 + floor( log10( length( xmls ) ) )
	
	if( ! dir.exists( path ) )
		
		dir.create( path, recursive = T )
	
	for( n in 1 : length( xmls ) )
		
		write_xml( xmls[[ n ]], bind.paths( path, paste0( str_pad( n, width = w, pad = "0" ), ".xml" ) ) )
}
# example
# fhir.save.bundle.as.xml.list.in.directory( xmls, "data/obsWithEncSubjPat2" )
###


###
# fhir.get.bundle.as.xml.list.from.directory
# reads all downloaded xml files as a list of xml into R
fhir.get.bundle.as.xml.list.from.directory <- function( directory ) {
	
	xml.files <- dir( directory, "*.xml" )
	
	lapply( lst( xml.files ), function( x ) read_xml( bind.paths( directory, x ) ) )
}
# example:
# xmls2 <- fhir.get.bundle.as.xml.list.from.directory( "data/obsWithEncSubjPat/" )
# rm( xmls2 )
###

###
# fhir.get.bundle.page.entries.as.dataframes
# transform an xml page to a dataframe
fhir.get.bundle.page.entries.as.dataframes <- function( 
	xml, 
	entries = list( 
		MEDICATION = list(
			tag     = ".//Medication",
			items   = list(
				SYSTEM  = "code/coding/system/@value",
				CODE    = "code/coding/code/@value",
				DISPLAY = "code/coding/display/@value"
			)
		)
	)
) {
	
	if( is.null( xml ) ) return( NULL )
	
	lapply( 
		lst( names( entries ) ),
		function( n.e ) {
			
			#dbg
			#n.e <- names( entries )[ 1 ]
			
			cat( "\n", n.e )

			e <- entries[[ n.e ]]
			
			entry <- e[[ 1 ]]
			items <- e[[ 2 ]]
			
			xml.tags <- xml_find_all( xml, entry )

			Reduce(
				rbind2, 
				lapply( 
					xml.tags,
					function( tg ) {
						
						#dbg
						#tg <- xml.tags[[ 1 ]]
						
						cat( "." )
						
						sapply( 
							names( items ),
							function( i.n )  {
								
								#dbg
								#i.n <- names( n$items )[[ 2 ]]
								
								i.srch <- items[[ i.n ]]
								
								addr <- sub( "/@[a-zA-Z0-9]+$", "", i.srch )
								item <- sub( "^.*/@", "", i.srch )
								val  <- xml_attr( xml_find_all( tg, addr ), item )
								
								if( is.character( val ) ) {
									
									if( length( val ) < 1 ) val <- NA
									
									else if( 1 < length( val ) ) val <- paste0( val, collapse = " # " )
								}
								
								val
							}
						)
					}
				)
			)
		}
	)
}
# example
# d <- fhir.get.bundle.page.entries.as.dataframes(
# 	xmls[[ 2 ]],
# 	entries.obs
#  )
# 
# names( as.data.frame( d$Observation ) )

fhir.get.bundle.entries.as.dataframes <- function( xmls, entries ) {
	
	d <- lapply(
		lst( names( entries ) ),
		function( n ) {
			
			cat( n, "\n" )
			
			as.data.frame(
				Reduce( 
					rbind2,
					lapply(
						xmls,
						function( x ) {
							
							fhir.get.bundle.page.entries.as.dataframes( x, entries )[[ n ]]
						}
					)
				)
			)
		}
	)
	
	cat( "\n" )
	
	d
}
# example
# all.data <- fhir.get.bundle.entries.data( xmls, entries.obs )
# 
# all.data$Observation
# all.data$Patient
# all.data$Encounter

#save( all.data, file = "data/obsWithEncSubjPat2/allData.RData" )

