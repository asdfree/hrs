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

hrs_cat <- hrs_cat[ record_categories == this_sample_break , ]

lodown( "hrs" , hrs_cat , 
	your_username = my_username , 
	your_password = my_password )
if( any( grepl( 'rand([a-z]+)stata\\.zip' , hrs_cat$file_name ) ) ){
library(lodown)
# examine all available HRS microdata files
hrs_cat <-
	get_catalog( "hrs" ,
		output_dir = file.path( getwd() ) , 
		your_username = my_username , 
		your_password = my_password )

# RAND consolidated file only
hrs_cat <- subset( hrs_cat , grepl( 'rand([a-z]+)stata\\.zip' , file_name ) )
# download the microdata to your local computer
lodown( "hrs" , hrs_cat , 
	your_username = my_username , 
	your_password = my_password )

library(survey)

hrs_df <- 
	readRDS( list.files( hrs_cat$output_folder , full.names = TRUE ) )
	
# RAM cleanup
keep_vars <- 
	c( "raehsamp" , "raestrat" , "r3wtresp" , 
		"r3work" , "r12work" , "h12ahous" ,
		"r3mstat" , "r12mstat" , "h4ahous" )

hrs_df <- hrs_df[ keep_vars ]
	
# community residents aged 50+ in 1996
hrs_design <- 
	svydesign(
		id = ~ raehsamp ,
		strata = ~ raestrat ,
		weights = ~ r3wtresp , 
		nest = TRUE ,
		data = subset( hrs_df , r3wtresp > 0 )
	)
hrs_design <- 
	update( 
		hrs_design , 

		one = 1 ,
		
		working_in_1996 = r3work ,

		working_in_2014 = r12work ,

		marital_status_in_1996 =
			factor( r3mstat , levels = 1:8 , labels =
				c( "Married" , "Married, spouse absent" ,
				"Partnered" , "Separated" , "Divorced" ,
				"Separated/divorced" , "Widowed" ,
				"Never married" ) ) ,
				
		marital_status_in_2014 =
			factor( r12mstat , levels = 1:8 , labels =
				c( "Married" , "Married, spouse absent" ,
				"Partnered" , "Separated" , "Divorced" ,
				"Separated/divorced" , "Widowed" ,
				"Never married" ) )
	)
sum( weights( hrs_design , "sampling" ) != 0 )

svyby( ~ one , ~ marital_status_in_1996 , hrs_design , unwtd.count )
svytotal( ~ one , hrs_design )

svyby( ~ one , ~ marital_status_in_1996 , hrs_design , svytotal )
svymean( ~ h12ahous , hrs_design , na.rm = TRUE )

svyby( ~ h12ahous , ~ marital_status_in_1996 , hrs_design , svymean , na.rm = TRUE )
svymean( ~ marital_status_in_2014 , hrs_design , na.rm = TRUE )

svyby( ~ marital_status_in_2014 , ~ marital_status_in_1996 , hrs_design , svymean , na.rm = TRUE )
svytotal( ~ h12ahous , hrs_design , na.rm = TRUE )

svyby( ~ h12ahous , ~ marital_status_in_1996 , hrs_design , svytotal , na.rm = TRUE )
svytotal( ~ marital_status_in_2014 , hrs_design , na.rm = TRUE )

svyby( ~ marital_status_in_2014 , ~ marital_status_in_1996 , hrs_design , svytotal , na.rm = TRUE )
svyquantile( ~ h12ahous , hrs_design , 0.5 , na.rm = TRUE )

svyby( 
	~ h12ahous , 
	~ marital_status_in_1996 , 
	hrs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ h4ahous , 
	denominator = ~ h12ahous , 
	hrs_design ,
	na.rm = TRUE
)
sub_hrs_design <- subset( hrs_design , working_in_1996 == 1 )
svymean( ~ h12ahous , sub_hrs_design , na.rm = TRUE )
this_result <- svymean( ~ h12ahous , hrs_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ h12ahous , 
		~ marital_status_in_1996 , 
		hrs_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( hrs_design )
svyvar( ~ h12ahous , hrs_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ h12ahous , hrs_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ h12ahous , hrs_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ working_in_2014 , hrs_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( h12ahous ~ working_in_2014 , hrs_design )
svychisq( 
	~ working_in_2014 + marital_status_in_2014 , 
	hrs_design 
)
glm_result <- 
	svyglm( 
		h12ahous ~ working_in_2014 + marital_status_in_2014 , 
		hrs_design 
	)

summary( glm_result )
library(srvyr)
hrs_srvyr_design <- as_survey( hrs_design )
hrs_srvyr_design %>%
	summarize( mean = survey_mean( h12ahous , na.rm = TRUE ) )

hrs_srvyr_design %>%
	group_by( marital_status_in_1996 ) %>%
	summarize( mean = survey_mean( h12ahous , na.rm = TRUE ) )

}
