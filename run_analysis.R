## set of functions to get and clean data
run_analysis <- function(){
	# load list of features in the data
	features_list <- load_features()

	# select only features that contain mean or std in the name
	features_selected <- grepl( "mean()", features_list$Feature, fixed = TRUE ) | grepl( "std()", features_list$Feature, fixed = TRUE )

	# prepare to load only data corresponding to the selected features
	col_classes <- character()
	col_classes <- sapply( seq( 1, length( features_selected ) ), function( x ) ifelse( features_selected[x], col_classes[x]<-NA, col_classes[x]<-"NULL" ) )
	col_names <- character()
	col_names <- sapply( seq( 1, length( features_selected ) ), function( x ) ifelse( features_selected[x], col_names[x]<-as.character( features_list$Feature[x] ), col_names[x]<-"NULL" ) )

      # load data corresponding to the selected features only
	dir_name <- c( "train", "test" )
	data <- lapply( dir_name, load_file, "X_", col_classes, col_names )

	# load activity labels
	activity_labels <- load_activity_labels()

	# load activity type data set
	activities <- lapply( dir_name, load_file, "y_", NA, "Activity" )
	merged_activities <- rbind( activities[[1]], activities[[2]] )

	# use descriptive activity names (labels) to represent activities in the data set
	Activity <- activity_labels$Activity[match( merged_activities$Activity, activity_labels$ActivityId )]

	# load subjects id data set
	subjects <- lapply( dir_name, load_file, "subject_", NA, "Id" )
	Id <- rbind( subjects[[1]], subjects[[2]] )

	# merge all the data sets into one tidy data set
      merged_data <- rbind( data[[1]], data[[2]] )
	merged_data <- cbind( Activity, Id, merged_data )

	# write obtained data set into file
	write.table( merged_data, file = "total_mean_std_data.txt", row.names = FALSE )

	# create independent tidy data set with the average of each variable for each activity and each subject
	library( dplyr )
	merged_data %>% group_by( Activity, Id ) %>% summarize_each( funs( mean ) )

	# write obtained data set into file
	write.table( merged_data, file = "total_summarized_mean_std_data.txt", row.names = FALSE )
}

## load list of features in the data
load_features <- function(){
	path <- file.path( ".", "features.txt" )
	features_list <- read.table( path, sep = " ", colClasses = c( "NULL", NA ), col.names = c( "NULL", "Feature" ) )
}

## load data set
load_file <- function( dir_name, file_type, col_classes, col_names ){
	path <- file.path( ".", dir_name, paste( file_type, dir_name, ".txt", sep = "" ) )
	data <- read.table( path, colClasses = col_classes, col.names = col_names, check.names = FALSE )
}

## load activity labels
load_activity_labels <- function(){
	path <- file.path( ".", "activity_labels.txt" )
	features_list <- read.table( path, sep = " ", colClasses = c( NA, NA ), col.names = c( "ActivityId", "Activity" ) )
}