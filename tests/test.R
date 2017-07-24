if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_username <- Sys.getenv( "my_username" )
my_password <- Sys.getenv( "my_password" )
library(lodown)
# examine all available HRS microdata files
hrs_cat <-
	get_catalog( "hrs" ,
		output_dir = file.path( getwd() ) , 
		your_username = my_username , 
		your_password = my_password )

# 2015 only
hrs_cat <- subset( hrs_cat , year == 2015 )
# download the microdata to your local computer
stopifnot( nrow( hrs_cat ) > 0 )



library(survey)

hrs_df <- readRDS( file.path( getwd() , "2015 main.rds" ) )

hrs_design <- 
	svydesign( 
		~ psu , 
		strata = ~ stratum , 
		data = hrs_df , 
		weights = ~ weight , 
		nest = TRUE 
	)
hrs_design <- 
	update( 
		hrs_design , 
		q2 = q2 ,
		never_rarely_wore_bike_helmet = as.numeric( qn8 == 1 ) ,
		ever_smoked_marijuana = as.numeric( qn47 == 1 ) ,
		ever_tried_to_quit_cigarettes = as.numeric( q36 > 2 ) ,
		smoked_cigarettes_past_year = as.numeric( q36 > 1 )
	)
sum( weights( hrs_design , "sampling" ) != 0 )

svyby( ~ one , ~ ever_smoked_marijuana , hrs_design , unwtd.count )
svytotal( ~ one , hrs_design )

svyby( ~ one , ~ ever_smoked_marijuana , hrs_design , svytotal )
svymean( ~ bmipct , hrs_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , hrs_design , svymean , na.rm = TRUE )
svymean( ~ q2 , hrs_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , hrs_design , svymean , na.rm = TRUE )
svytotal( ~ bmipct , hrs_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , hrs_design , svytotal , na.rm = TRUE )
svytotal( ~ q2 , hrs_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , hrs_design , svytotal , na.rm = TRUE )
svyquantile( ~ bmipct , hrs_design , 0.5 , na.rm = TRUE )

svyby( 
	~ bmipct , 
	~ ever_smoked_marijuana , 
	hrs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ ever_tried_to_quit_cigarettes , 
	denominator = ~ smoked_cigarettes_past_year , 
	hrs_design ,
	na.rm = TRUE
)
sub_hrs_design <- subset( hrs_design , qn41 == 1 )
svymean( ~ bmipct , sub_hrs_design , na.rm = TRUE )
this_result <- svymean( ~ bmipct , hrs_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ bmipct , 
		~ ever_smoked_marijuana , 
		hrs_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( hrs_design )
svyvar( ~ bmipct , hrs_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ bmipct , hrs_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ bmipct , hrs_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ never_rarely_wore_bike_helmet , hrs_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( bmipct ~ never_rarely_wore_bike_helmet , hrs_design )
svychisq( 
	~ never_rarely_wore_bike_helmet + q2 , 
	hrs_design 
)
glm_result <- 
	svyglm( 
		bmipct ~ never_rarely_wore_bike_helmet + q2 , 
		hrs_design 
	)

summary( glm_result )
library(srvyr)
hrs_srvyr_design <- as_survey( hrs_design )
hrs_srvyr_design %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

hrs_srvyr_design %>%
	group_by( ever_smoked_marijuana ) %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

unwtd.count( ~ never_rarely_wore_bike_helmet , yrbss_design )

svytotal( ~ one , subset( yrbss_design , !is.na( never_rarely_wore_bike_helmet ) ) )
 
svymean( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE )

svyciprop( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE , method = "beta" )

