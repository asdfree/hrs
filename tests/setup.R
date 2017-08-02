if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_username <- Sys.getenv( "my_username" )
my_password <- Sys.getenv( "my_password" )
library(lodown)

hrs_cat <-
	get_catalog( "hrs" ,
		output_dir = file.path( getwd() ) , 
		your_username = my_username , 
		your_password = my_password )

# sample 75% of the records
which_records <- sample( seq( nrow( hrs_cat ) ) , round( nrow( hrs_cat ) * 0.75 ) )

# always sample the rand a-z stata file
hrs_cat <- unique( rbind( hrs_cat[ which_records , ] , subset( hrs_cat , grepl( 'rand([a-z]+)stata\\\\.zip' , file_name ) ) ) )

lodown( "hrs" , hrs_cat , 
		your_username = my_username , 
		your_password = my_password )

