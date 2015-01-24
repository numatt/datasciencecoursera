run_analysis <- function(fileroot = getwd()){
    
    # Construct file paths to relevant files
    path.features <- paste(fileroot, "/features.txt", sep="")
    path.activity_labels <- paste(fileroot, "/activity_labels.txt", sep="")
    path.X_test <- paste(fileroot, "/test/X_test.txt", sep="")
    path.y_test <- paste(fileroot, "/test/y_test.txt", sep="")
    path.subject_test <- paste(fileroot, "/test/subject_test.txt", sep="")
    path.X_train <- paste(fileroot, "/train/X_train.txt", sep="")
    path.y_train <- paste(fileroot, "/train/y_train.txt", sep="")
    path.subject_train <- paste(fileroot, "/train/subject_train.txt", sep="")
    
    # Load Labels
    feature_labels <- read.table(path.features, stringsAsFactors = FALSE)[[2]]
    activity_labels <- read.table(path.activity_labels, stringsAsFactors = TRUE)[[2]]
    
    # Load Test Data
    table.test_data <- read.table(path.X_test, col.names = feature_labels, check.names = TRUE,
                                  colClasses = "numeric", quote = "", comment.char = "", nrows=2947)
    table.test_activities <- read.table(path.y_test, quote = "", comment.char = "")[[1]]
    table.test_subjects <- read.table(path.subject_test, quote = "", comment.char = "")[[1]]
    
    # Load Train Data
    table.train_data <- read.table(path.X_train, col.names = feature_labels, check.names = TRUE,
                                   colClasses = "numeric", quote = "", comment.char = "", nrows=7352)
    table.train_activities <- read.table(path.y_train, quote = "", comment.char = "")[[1]]
    table.train_subjects <- read.table(path.subject_train, quote = "", comment.char = "")[[1]]
    
    # Build single test table
    table.test_data["activity"] <- activity_labels[ table.test_activities ]
    table.test_data["subject"] <- table.test_subjects
    
    # Build single train table
    table.train_data["activity"] <- activity_labels[ table.train_activities ]
    table.train_data["subject"] <- table.train_subjects
    
    # Merge test and train data into single table
    table.data <- rbind(table.test_data, table.train_data)

    # Keep only activity, subject, mean, standard deviation columns
    # These match the pattern:  "-mean()" and "-std()"
    #   which were cleaned to:  ".mean.." and ".std.."
    mean.cols <- grepl("\\.mean\\.\\.", names(table.data), ignore.case = TRUE)
    std.cols <- grepl("\\.std\\.\\.", names(table.data), ignore.case = TRUE)
    activity.cols <- names(table.data) == "activity"
    subject.cols <- names(table.data) == "subject"
    table.data <- table.data[, mean.cols | std.cols | activity.cols | subject.cols]
    
    # Clean up column names
    # Capitalize "Mean" and "Std"
    names(table.data) <- gsub("\\.(m)","M",names(table.data))
    names(table.data) <- gsub("\\.(s)","S",names(table.data))
    # Remove special characters (cleaned to periods during file load)
    names(table.data) <- gsub("(\\.)","",names(table.data))
    # Expand t to "time" and f to "frequency"
    names(table.data) <- gsub("^t","time",names(table.data))
    names(table.data) <- gsub("^f","frequency",names(table.data))
    # Fix duplicate "Body"
    names(table.data) <- gsub("BodyBody","Body",names(table.data))
    # Move dimension (X,Y,Z) before Mean or Std
    names(table.data) <- gsub("(Mean|Std)([XYZ])","\\2\\1",names(table.data))
    # Add dot (.) seperators
    names(table.data) <- gsub("([A-Z])",".\\1",names(table.data))
    # Capitalize first character of each column name
    names(table.data) <- gsub("^(.)","\\U\\1",names(table.data), perl=TRUE)
    
    # Find the mean for each Subject, Activity pair
    table.agg <- aggregate(table.data[,c(1:66)],
                           list(Subject = table.data$Subject, Activity = table.data$Activity), mean)
    # Update column names to reflect the mean opareration
    names(table.agg)[3:68] <- paste(names(table.agg)[3:68],".Mean",sep="")
    
    # Order by Subject, Activity
    table.agg <- table.agg[order(table.agg$Subject,table.agg$Activity),]

    table.agg
}