generate_codebook <- function(names) {
    # Header
    string <- "# DATA DICTIONARY - Tidy UCI HAR Dataset"
    # Subject and Activity field defitions
    string <- paste(string,"**Subject**\n\tThe subject who performed the activity", sep="\n\n")
    string <- paste(string,"**Activity**\n\tThe activity performed, of:", sep="\n\n")
    string <- paste(string,"LAYING","SITTING","STANDING","WALKING","WALKING_DOWNSTAIRS","WALKING_UPSTAIRS", sep="\n\t\t")
    
    # Dynamically generate data field defintions
    for (name in names){
        # Field header
        string <- paste(string, "\n\n", sep="")
        string <- paste(string,"**",name,"**\n\t",sep="")
        string <- paste(string,"For the measurements taken of the subject performing the activity, the mean of the",sep="")
        
        # Mean or Std?
        text <- ifelse(grepl(".Std",name),"standard devations","means")
        string <- paste(string,text,sep=" ")
        
        # Dimension or magnitude
        if (grepl(".Mag.",name)){
            text <- "overall magnitude"
        }
        else {
            text <- gsub("^.+\\.([XYZ])\\..+","\\1",name)
            text <- paste(text,"dimension component",sep=" ")
        }
        string <- paste(string,"of the",text,sep=" ")
        
        # Jerk signal?
        if (grepl(".Jerk.",name)) {
            string <- paste(string,"of the jerk signals",sep=" ")
        }
        
        # Body or Gravity component?
        text <- ifelse(grepl("Body",name),"body","gravity")
        string <- paste(string, "of the",text,"component",sep=" ")
        
        #  Acclerometer or gyroscope readings?
        text <- ifelse(grepl("Acc",name),"accelerometer","gyroscope")
        string <- paste(string, "of the",text,"readings",sep=" ")
        
        # Time of frequency domain?
        # in the time domain
        text <- ifelse(grepl("Time",name),"time","frequency")
        string <- paste(string, "in the",text,"domain",sep=" ")
    }
    
    fileConn <- file("CodeBook.md")
    writeLines(string, fileConn)
    close(fileConn)
}