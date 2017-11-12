if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_username <- Sys.getenv( "my_username" )
my_password <- Sys.getenv( "my_password" )
this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

hrs_cat <-
	get_catalog( "hrs" ,
		output_dir = file.path( getwd() ) , 
		your_username = my_username , 
		your_password = my_password )

record_categories <- ceiling( seq( nrow( hrs_cat ) ) / ceiling( nrow( hrs_cat ) / 3 ) )

hrs_cat <- unique( rbind( hrs_cat[ record_categories == this_sample_break , ] , hrs_cat[ grepl( 'rand([a-z]+)stata\\.zip' , hrs_cat$file_name ) , ] ) )

lodown( "hrs" , hrs_cat , 
		your_username = my_username , 
		your_password = my_password )

