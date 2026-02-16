# timingfile for Flanker4 
# Created on 09/05/2023
# Modified on 10/15/2024
# Author: Parmis Khosravi 


# NOTES ---------------------------------------------------
# AT THE END IT WILL SAVE THE OUTPUTS IN THE ASSIGNED DIRECTORY (SEE LINE 549)
# CHECK THE FOLLOWING:
#' THERE SHOULD BE 10 .1D FILES FOR EACH SUBJECT
#'  s{SDAN#}_{ses#}_task-flanker3_congruent.1D
#'  s{SDAN#]_{ses#}_task-flanker3_incongruent.1D
#'  s{SDAN#}_{ses#}_task-flanker3_congruent_correct.1D
#'  s{SDAN#}_{ses#}_task-flanker3_congruent_error.1D
#'  s{SDAN#}_{ses#}_task-flanker3_congruent_omission.1D
#'  s{SDAN#}_{ses#}_task-flanker3_congruent_commission.1D
#'  s{SDAN#}_{ses#}_task-flanker3_incongruent_correct.1D
#'  s{SDAN#}_{ses#}_task-flanker3_incongruent_error.1D
#'  s{SDAN#}_{ses#}_task-flanker3_incongruent_omission.1D
#'  s{SDAN#}_{ses#}_task-flanker3_incongruent_commission.1D


# REQUIRED PACKAGES ---------------------------------------------------
# Will check if the required packages are already installed or not, 
# and if not it will install it for you.  

required_Packages_Install <- c("tidyverse", "dplyr", "doBy", "date", "psych", "purrr", "readr", "data.table", "tibble", "reshape2", "car", "semPlot", "GPArotation", "caret", "Hmisc")

for(Package in required_Packages_Install){
  if(!require(Package,character.only = TRUE)) { 
    install.packages(Package, dependencies=TRUE)
  }
  library(Package,character.only = TRUE)
}


# SET WORKING DIRECTORY ---------------------------------------------------
datadir <- (paste("/Volumes/SDAN-EDB/SDAN1/Data/MEGfMRI_InhibitoryControl/Kids/Flanker4/Data/rawdata"))
timedir <- (paste("/Volumes/SDAN-EDB/SDAN1/Data/MEGfMRI_InhibitoryControl/Kids/Flanker4/Data/timingfiles"))
outdir <- (paste("/Volumes/SDAN-EDB/SDAN1/Data/MEGfMRI_InhibitoryControl/Kids/Flanker4/Data/summarydata"))


## GetFiles --------------------------------------------------------------

output_file <- file.path(outdir, "summary_statistics.csv")
write.header <- !file.exists(output_file)

getfiles = function() {
  readSubjSession <- function() {
    subject_session_file <- "/Volumes/SDAN-EDB/SDAN1/Data/MEGfMRI_InhibitoryControl/Kids/Flanker4/Data/lists/Flanker4_subjlist_wDate.csv"  # Path to your CSV file
    subject_info <- read.csv(subject_session_file, header = TRUE, stringsAsFactors = FALSE)
    return(subject_info)
  }
  subject_data <- readSubjSession()

  read_data_file <- function(Subject, SessionDate) {
    datadir <- "/Volumes/SDAN-EDB/SDAN1/Data/MEGfMRI_InhibitoryControl/Kids/Flanker4/Data/rawdata"
  ## Read the raw data
  #read_data_file <- function(file_path) {
    subj_filename <- paste0(Subject, "_Flanker4_", SessionDate, ".csv")
    file_path <- file.path(datadir, subj_filename)
    # Deal with different file encodings
    rawdat <- tryCatch({
      df <- read.csv(file = file_path, header = TRUE, fileEncoding = "UTF-8")
    }, error = function(e) {
      tryCatch({
        df <- read.csv(file = file_path, header = TRUE, fileEncoding = "UTF-16LE")
      }, error = function(e) {
        stop("Failed to read file with both UTF-16LE and UTF-8 encoding.")
      })
    })
    return(list(rawdat = rawdat, subj_filename = subj_filename))
  }

  ## Loop through subjects
  for (i in 1:nrow(subject_data)) {
    expected_subj <- as.character(subject_data$Subject[i])
    subj<-subject_data$Subject[i]
    ses<-subject_data$SessionDate[i]
    dte <-subject_data$ses[i]
    print(paste("Reading:", subj, ses))
    ## Construct the file name
    fName <- paste0(expected_subj, "_Flanker4_", ses, ".csv")
    filePath <- file.path(datadir, fName)

    # Read the data file
    #result <- read_data_file(filePath)
    result <- read_data_file(subj, ses)
    rawdat <- result$rawdat
    subj_filename <- result$subj_filename
    scandate <- subject_data$ScanDate[i]
    sesdate <-rawdat$date[i]
    # Check if 'Subject' column exists within the data (the txt file retrieved from Eprime)
    if (!grepl(paste0("^", expected_subj, "_"), subj_filename)) {
      stop(paste("Mismatch between file name", subj_filename,
                 "and expected subject number", expected_subj))
    } else {
      print(paste("File subject number matches:", expected_subj))
    }
    
   # Now compare the dates
    print(paste("sesdate:", sesdate))
    print(paste("scandate:", scandate))
    sesdate <- as.character(sesdate)
    scandate<- as.character(scandate)
    
    if(sesdate !=scandate) {
      print(paste("Session date:", sesdate, "does not match scan date:", scandate, "for subject:", subj))
      user_decision <- readline(prompt="The dates do not match. Do you want to continue? (yes/no): ")
      
      if (tolower(user_decision) != "yes") {
        print("User chose to stop. Exiting...")
        return(NULL)
      }
      
    } else {
      print("Dates match. Continuing with the next steps...")
    }
    #### Subset Each Run; Flanker = 4 runs -------------------------------------
    rawdat$runs_iteration.thisN <- as.numeric(rawdat$runs_iteration.thisN)
    
    run1 <- subset(rawdat,rawdat$runs_iteration.thisN  == 0)
    run2 <- subset(rawdat,rawdat$runs_iteration.thisN  == 1)
    run3 <- subset(rawdat,rawdat$runs_iteration.thisN  == 2)
    run4 <- subset(rawdat,rawdat$runs_iteration.thisN  == 3)
    
    table(rawdat$runs_iteration.thisN)
    # Function to check the number of rows and proceed accordingly
    check_and_proceed <- function(run, runs_iteration.thisN) {
      if (nrow(run) == 108) {
        print(paste("Run", runs_iteration.thisN, "has 108 rows. Proceeding..."))
      } else {
        print(paste("Run", runs_iteration.thisN, "does not have 108 rows. It has", nrow(run), "rows. Skipping or stopping..."))
      }
    } 
    
    #sesdate<- gsub("[/-]", "", sesdate) 
    #scandate<- gsub("[/-]", "", scandate) 
    # Apply the function to each run
    check_and_proceed(run1, 0)
    check_and_proceed(run2, 1)
    check_and_proceed(run3, 2)
    check_and_proceed(run4, 3)
    # TYDI DATA ----------------------------------------------------------------
    # remove the columns with all blanks (NAs)
    run1 <- run1[, !apply(run1, 2, function(col) all(is.na(col) | col == "")), drop = TRUE]
    run2 <- run2[, !apply(run2, 2, function(col) all(is.na(col) | col == "")), drop = TRUE]
    run3 <- run3[, !apply(run3, 2, function(col) all(is.na(col) | col == "")), drop = TRUE]
    run4 <- run4[, !apply(run4, 2, function(col) all(is.na(col) | col == "")), drop = TRUE]
    
  ### GET DATA ----------------------------------------------------------------
  #### CALCULATE STIM TIME ---------------------------------------------------
  ##### SET RUN 1 -------------------------------------------------------------------

  ## create condition variable 
  run1$condition <-ifelse(run1$Trialtype %in% c("RC", "LC"), "Congruent", "Incongruent")
  ## create accuracy variable
  run1$ACC <- ifelse(run1$key_resp.resp_type %in% c("commission", "omission"), 0, 1)
  ## Fetch the first row value of blankblock.stopped (which is when each run of the experiment started)
  run_start_time <- run1$blankblock.stopped[1]
  
  ## Subtract that single value from every value in the flankerArrows.started column
  run1$onsetT = run1$flankerArrows.started - run_start_time
  run1$onsetT <- round(run1$onsetT, digits = 3)
  
  ## for any blanks or missing RT then add *
  run1$onsetT[is.na(run1$onsetT) | run1$onsetT == 0] <- "*"
  
  ## Make reaction time for summary data  
  ##' NOTE:
  ##' Subtract that single value from every value in the flankerArrows.started column
  ##' Because the task was set up without using global time we need to calculate the 
  ##' offset time and add it to onsetT (onset time)
  run1$key_resp.started <- as.numeric(run1$key_resp.started)
  run1$flankerArrows.started <- as.numeric(run1$flankerArrows.started)
  run1$key_resp.rt <- as.numeric(run1$key_resp.rt)
  
  run1$r1.rttime <- (run1$key_resp.started - run1$flankerArrows.started) + run1$key_resp.rt
  run1$r1.rttime <- as.numeric(run1$r1.rttime)
  
  ###### Separate By Conditions ----------------------------------------
  ####### congruent -----------------------------------------------
  ##' separate congruent conditions as follow:
  ##' overall 
  ##' correct trials 
  ##' commission trials 
  ##' omission trials
  ##' overall error trials
  
  ## overall
  r1cong <- subset(run1, run1$condition == "Congruent") # run 1 congruent 
  r1cong$onsetT[is.na(r1cong$onsetT) | r1cong$onsetT == 0] <- "*"  # for any blanks or missing RT then add *
  r1cong_times <- subset(run1, run1$condition == "Congruent")[, "onsetT", drop=FALSE] # run 1 congruent, keep only onsetT column
  
  ## correct trials 
  r1cong_corr <- subset(r1cong, r1cong$key_resp.resp_type == "correct")[, "onsetT", drop=FALSE] # run 1 congruent correct, keep only onsetT column
  if(nrow(r1cong_corr) == 0){r1cong_corr <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r1cong_corr_list <- r1cong_corr$onsetT   # Extract onsetT values and store them in lists
  
  ## commission trials 
  r1cong_com <- subset(r1cong, r1cong$key_resp.resp_type  == "commission")[, "onsetT", drop=FALSE] # run 1 congruent commission, keep only onsetT column
  if(nrow(r1cong_com) == 0){r1cong_com <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r1cong_com_list <- r1cong_com$onsetT
  
  ## omission trials
  r1cong_omi <- subset(r1cong, r1cong$key_resp.resp_type  == "omission")[, "onsetT", drop=FALSE] # run 1 congruent omission, keep only onsetT column
  if(nrow(r1cong_omi) == 0){r1cong_omi <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r1cong_omi_list <- r1cong_omi$onsetT # Extract onsetT values and store them in lists
  
  ## overall error trials
  r1cong_err <- subset(r1cong, r1cong$ACC == 0)[, "onsetT", drop=FALSE] # run 1 congruent error, keep only onsetT column
  if(nrow(r1cong_err) == 0){r1cong_err <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r1cong_err_list <- r1cong_err$onsetT # Extract onsetT values and store them in lists
  
  ####### incongruent -----------------------------------------------
  ##' separate congruent conditions as follow:
  ##' overall
  ##' correct trials
  ##' commission trials 
  ##' omission trials 
  ##' overall error trials
  
  ## overall
  r1incong <- subset(run1, run1$condition == "Incongruent") # run 1 incongruent 
  # r1incong_times <- subset(run1, run1$condition == "Incongruent")[, "onsetT", drop=FALSE] # run 1 incongruent, keep only onsetT column
  r1incong$onsetT[is.na(r1incong$onsetT) | r1incong$onsetT == 0] <- "*"  # for any blanks or missing RT then add *
  
  ## correct trials
  r1incong_corr <- subset(r1incong, r1incong$key_resp.resp_type == "correct")[, "onsetT", drop=FALSE] # run 1 incongruent correct, keep only onsetT column
  if(nrow(r1incong_corr) == 0){r1incong_corr <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r1incong_corr_list <- r1incong_corr$onsetT # Extract onsetT values and store them in lists
  
  ## commission trials 
  r1incong_com <- subset(r1incong, r1incong$key_resp.resp_type  == "commission")[, "onsetT", drop=FALSE] # run 1 incongruent commission, keep only onsetT column
  if(nrow(r1incong_com) == 0){r1incong_com <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r1incong_com_list <- r1incong_com$onsetT # Extract onsetT values and store them in lists
  
  ## omission trials 
  r1incong_omi <- subset(r1incong, r1incong$key_resp.resp_type  == "omission")[, "onsetT", drop=FALSE] # run 1 incongruent omission, keep only onsetT column
  if(nrow(r1incong_omi) == 0){r1incong_omi <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r1incong_omi_list <- r1incong_omi$onsetT # Extract onsetT values and store them in lists
  
  ## overall error trials
  r1incong_err <- subset(r1incong, r1incong$ACC == 0)[, "onsetT", drop=FALSE] # run 1 incongruent error, keep only onsetT column
  if(nrow(r1incong_err) == 0){r1incong_err <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r1incong_err_list <- r1incong_err$onsetT # Extract onsetT values and store them in lists
  
  ##### SET RUN 2 -------------------------------------------------------------------
  ## create a condition variable 
  run2$condition <-ifelse(run2$Trialtype %in% c("RC", "LC"), "Congruent", "Incongruent")
  ## create an accuracy variable
  run2$ACC <- ifelse(run2$key_resp.resp_type %in% c("commission", "omission"), 0, 1)
  ## Fetch the first row value of blankblock.stopped (which is when the experiment started)
  run_start_time <- run2$blankblock.stopped[1]
  ## Subtract that single value from every value in the flankerArrows.started column
  run2$onsetT = run2$flankerArrows.started - run_start_time
  run2$onsetT <- round(run2$onsetT, digits = 3)
  run2$onsetT[is.na(run2$onsetT) | run2$onsetT == 0] <- "*" # for any blanks or missing RT then add *
  
  
  # Make reaction time for summary data  
  ##' NOTE:
  ##' Subtract that single value from every value in the flankerArrows.started column
  ##' Because the task was set up without using global time we need to calculate the 
  ##' offset time and add it to onsetT (onset time)
  run2$key_resp.started <- as.numeric(run2$key_resp.started)
  run2$flankerArrows.started <- as.numeric(run2$flankerArrows.started)
  run2$key_resp.rt <- as.numeric(run2$key_resp.rt)
  
  run2$r2.rttime <- (run2$key_resp.started - run2$flankerArrows.started) + run2$key_resp.rt
  run2$r2.rttime <- as.numeric(run2$r2.rttime)
  
  ###### Separate By Conditions ----------------------------------------
  ####### congruent -----------------------------------------------
  ##' separate congruent conditions as follow:
  ##' overall 
  ##' correct trials 
  ##' commission trials 
  ##' omission trials
  ##' overall error trials
  
  ## overall 
  r2cong <- subset(run2, run2$condition == "Congruent") # run 2 congruent 
  r2cong$onsetT[is.na(r2cong$onsetT) | r2cong$onsetT == 0] <- "*" # for any blanks or missing RT then add *
  # r2cong_times <- subset(run2, run2$condition == "Congruent")[, "onsetT", drop=FALSE] # run 2 congruent, keep only onsetT column
  
  ## correct trials 
  r2cong_corr <- subset(r2cong, r2cong$key_resp.resp_type == "correct")[, "onsetT", drop=FALSE] # run 2 congruent correct, keep only onsetT column
  if(nrow(r2cong_corr) == 0){r2cong_corr <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r2cong_corr_list <- r2cong_corr$onsetT # Extract onsetT values and store them in lists
  
  ## commission trials 
  r2cong_com <- subset(r2cong, r2cong$key_resp.resp_type  == "commission")[, "onsetT", drop=FALSE] # run 2 congruent commission, keep only onsetT column
  if(nrow(r2cong_com) == 0){r2cong_com <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r2cong_com_list <- r2cong_com$onsetT # Extract onsetT values and store them in lists
  
  ## omission trials
  r2cong_omi <- subset(r2cong, r2cong$key_resp.resp_type  == "omission")[, "onsetT", drop=FALSE] # run 2 congruent omission, keep only onsetT column
  if(nrow(r2cong_omi) == 0){ r2cong_omi <- data.frame(onsetT = "*") } # for any blanks or missing RT then add *
  r2cong_omi_list <- r2cong_omi$onsetT # Extract onsetT values and store them in lists
  
  ## overall error trials
  r2cong_err <- subset(r2cong, r2cong$ACC == 0)[, "onsetT", drop=FALSE] # run 2 congruent error, keep only onsetT column
  if(nrow(r2cong_err) == 0){r2cong_err <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r2cong_err_list <- r2cong_err$onsetT # Extract onsetT values and store them in lists
  
  ####### incongruent -----------------------------------------------
  ##' separate congruent conditions as follow:
  ##' overall
  ##' correct trials
  ##' commission trials 
  ##' omission trials 
  ##' overall error trials
  
  ## overall
  r2incong <- subset(run2, run2$condition == "Incongruent") # run 2 incongruent 
  r2incong_times <- subset(run2, run2$condition == "Incongruent")[, "onsetT", drop=FALSE] # run 2 incongruent, keep only onsetT column
  r2incong$onsetT[is.na(r2incong$onsetT) | r2incong$onsetT == 0] <- "*" # for any blanks or missing RT then add *
  
  ## correct trials
  r2incong_corr <- subset(r2incong, r2incong$key_resp.resp_type == "correct")[, "onsetT", drop=FALSE] # run 2 incongruent correct, keep only onsetT column
  if(nrow(r2incong_corr) == 0){r2incong_corr <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r2incong_corr_list <- r2incong_corr$onsetT # Extract onsetT values and store them in lists
  
  ## commission trials 
  r2incong_com <- subset(r2incong, r2incong$key_resp.resp_type  == "commission")[, "onsetT", drop=FALSE] # run 2 incongruent commission, keep only onsetT column
  if(nrow(r2incong_com) == 0){r2incong_com <- data.frame(onsetT = "*")}  # for any blanks or missing RT then add *
  r2incong_com_list <- r2incong_com$onsetT # Extract onsetT values and store them in lists
  
  ## omission trials
  r2incong_omi <- subset(r2incong, r2incong$key_resp.resp_type  == "omission")[, "onsetT", drop=FALSE] # run 2 incongruent omission, keep only onsetT column
  if(nrow(r2incong_omi) == 0){r2incong_omi <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r2incong_omi_list <- r2incong_omi$onsetT # Extract onsetT values and store them in lists
  
  r2incong_err <- subset(r2incong, r2incong$ACC == 0)[, "onsetT", drop=FALSE] # run 2 incongruent error, keep only onsetT column
  if(nrow(r2incong_err) == 0){r2incong_err <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r2incong_err_list <- r2incong_err$onsetT # Extract onsetT values and store them in lists
  
  ##### SET RUN 3 -------------------------------------------------------------------
  # create a condition variable 
  run3$condition <-ifelse(run3$Trialtype %in% c("RC", "LC"), "Congruent", "Incongruent")
  run3$ACC <- ifelse(run3$key_resp.resp_type %in% c("commission", "omission"), 0, 1)
  # Fetch the first row value of blankblock.stopped (which is when each run started)
  run_start_time <- run3$blankblock.stopped[1]
  # Subtract that single value from every value in the flankerArrows.started column
  run3$onsetT = run3$flankerArrows.started - run_start_time
  run3$onsetT <- round(run3$onsetT, digits = 3)
  run3$onsetT[is.na(run3$onsetT) | run3$onsetT == 0] <- "*"  # for any missing RT then add *
  
  
  # Make reaction time for summary data  
  ##' NOTE:
  ##' Subtract that single value from every value in the flankerArrows.started column
  ##' Because the task was set up without using global time we need to calculate the 
  ##' offset time and add it to onsetT (onset time)
  run3$key_resp.started <- as.numeric(run3$key_resp.started)
  run3$flankerArrows.started <- as.numeric(run3$flankerArrows.started)
  run3$key_resp.rt <- as.numeric(run3$key_resp.rt)
  
  run3$r3.rttime <- (run3$key_resp.started - run3$flankerArrows.started) + run3$key_resp.rt
  run3$r3.rttime <- as.numeric(run3$r3.rttime)
  
  ###### Separate By Conditions ----------------------------------------
  ####### congruent -----------------------------------------------
  ##' separate congruent conditions as follow:
  ##' overall 
  ##' correct trials 
  ##' commission trials 
  ##' omission trials
  ##' overall error trials
  
  ## overall 
  r3cong <- subset(run3, run3$condition == "Congruent") # run 3 congruent 
  r3cong$onsetT[is.na(r3cong$onsetT) | r3cong$onsetT == 0] <- "*" # for any blanks or missing RT then add *
  # r3cong_times <- subset(run3, run3$condition == "Congruent")[, "onsetT", drop=FALSE] # run 3 congruent, keep only onsetT column
  
  ## correct trials 
  r3cong_corr <- subset(r3cong, r3cong$key_resp.resp_type == "correct")[, "onsetT", drop=FALSE] # run 3 congruent correct, keep only onsetT column
  if(nrow(r3cong_corr) == 0){r3cong_corr <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r3cong_corr_list <- r3cong_corr$onsetT # Extract onsetT values and store them in lists
  
  ## commission trials 
  r3cong_com <- subset(r3cong, r3cong$key_resp.resp_type  == "commission")[, "onsetT", drop=FALSE] # run 3 congruent commission, keep only onsetT column
  if(nrow(r3cong_com) == 0){r3cong_com <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r3cong_com_list <- r3cong_com$onsetT # Extract onsetT values and store them in lists
  
  ## omission trials
  r3cong_omi <- subset(r3cong, r3cong$key_resp.resp_type  == "omission")[, "onsetT", drop=FALSE] # run 3 congruent omission, keep only onsetT column
  if(nrow(r3cong_omi) == 0){r3cong_omi <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r3cong_omi_list <- r3cong_omi$onsetT # Extract onsetT values and store them in lists
  
  ##  overall error trials
  r3cong_err <- subset(r3cong, r3cong$ACC == 0)[, "onsetT", drop=FALSE] # run 3 congruent error, keep only onsetT column
  if(nrow(r3cong_err) == 0){r3cong_err <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r3cong_err_list <- r3cong_err$onsetT  # Extract onsetT values and store them in lists
  
  ####### incongruent -----------------------------------------------
  ##' separate congruent conditions as follow:
  ##' overall
  ##' correct trials
  ##' commission trials 
  ##' omission trials 
  ##' overall error trials
  
  ## overall
  r3incong <- subset(run3, run3$condition == "Incongruent") # run 3 incongruent 
  # r3incong_times <- subset(run3, run3$condition == "Incongruent")[, "onsetT", drop=FALSE] # run 3 incongruent, keep only onsetT column
  r3incong$onsetT[is.na(r3incong$onsetT) | r3incong$onsetT == 0] <- "*" # for any blanks or missing RT then add *
  
  ## correct trials
  r3incong_corr <- subset(r3incong, r3incong$key_resp.resp_type == "correct")[, "onsetT", drop=FALSE] # run 3 incongruent correct, keep only onsetT column
  if(nrow(r3incong_corr) == 0){r3incong_corr <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r3incong_corr_list <- r3incong_corr$onsetT # Extract onsetT values and store them in lists
  
  ## commission trials
  r3incong_com <- subset(r3incong, r3incong$key_resp.resp_type  == "commission")[, "onsetT", drop=FALSE] # run 3 incongruent commission, keep only onsetT column
  if(nrow(r3incong_com) == 0){r3incong_com <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r3incong_com_list <- r3incong_com$onsetT # Extract onsetT values and store them in lists
  
  ## omission trials 
  r3incong_omi <- subset(r3incong, r3incong$key_resp.resp_type  == "omission")[, "onsetT", drop=FALSE] # run 3 incongruent omission, keep only onsetT column
  if(nrow(r3incong_omi) == 0){r3incong_omi <- data.frame(onsetT = "*")}  # for any blanks or missing RT then add *
  r3incong_omi_list <- r3incong_omi$onsetT # Extract onsetT values and store them in lists
  
  ## overall error trials
  r3incong_err <- subset(r3incong, r3incong$ACC == 0)[, "onsetT", drop=FALSE] # run 3 incongruent error, keep only onsetT column
  if(nrow(r3incong_err) == 0){r3incong_err <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r3incong_err_list <- r3incong_err$onsetT # Extract onsetT values and store them in lists
  
  ##### SET RUN 4 -------------------------------------------------------------------
  # create a condition variable 
  run4$condition <-ifelse(run4$Trialtype %in% c("RC", "LC"), "Congruent", "Incongruent")
  # create a accuracy variable
  run4$ACC <- ifelse(run4$key_resp.resp_type %in% c("commission", "omission"), 0, 1)
  # Fetch the first row value of blankblock.stopped (which is when the experiment run started)
  run_start_time <- run4$blankblock.stopped[1]
  # Subtract that single value from every value in the flankerArrows.started column
  run4$onsetT = run4$flankerArrows.started - run_start_time 
  run4$onsetT <- round(run4$onsetT, digits = 3)
  run4$onsetT[is.na(run4$onsetT) | run4$onsetT == 0] <- "*" # for any blanks or missing RT then add *
  
  
  # Make reaction time for summary data
  ##' NOTE:
  ##' Subtract that single value from every value in the flankerArrows.started column
  ##' Because the task was set up without using global time we need to calculate the 
  ##' offset time and add it to onsetT (onset time)
  run4$key_resp.started <- as.numeric(run4$key_resp.started)
  run4$flankerArrows.started <- as.numeric(run4$flankerArrows.started)
  run4$key_resp.rt <- as.numeric(run4$key_resp.rt)
  
  run4$r4.rttime <- (run4$key_resp.started - run4$flankerArrows.started) + run4$key_resp.rt
  run4$r4.rttime <- as.numeric(run4$r4.rttime)
  
  ###### Separate By Conditions ----------------------------------------
  ####### congruent -----------------------------------------------
  ##' separate congruent conditions as follow:
  ##' overall 
  ##' correct trials 
  ##' commission trials 
  ##' omission trials
  ##' overall error trials
  
  ## overall 
  r4cong <- subset(run4, run4$condition == "Congruent") # run 4 congruent 
  r4cong$onsetT[is.na(r4cong$onsetT) | r4cong$onsetT == 0] <- "*" # for any blanks or missing RT then add *
  # r4cong_times <- subset(run4, run4$condition == "Congruent")[, "onsetT", drop=FALSE] # run 4 congruent, keep only onsetT column
  
  ## correct trials 
  r4cong_corr <- subset(r4cong, r4cong$key_resp.resp_type == "correct")[, "onsetT", drop=FALSE] # run 4 congruent correct, keep only onsetT column
  if(nrow(r4cong_corr) == 0){r4cong_corr <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r4cong_corr_list <- r4cong_corr$onsetT # Extract onsetT values and store them in lists
  
  ## commission trials 
  r4cong_com <- subset(r4cong, r4cong$key_resp.resp_type  == "commission")[, "onsetT", drop=FALSE] # run 4 congruent commission, keep only onsetT column
  if(nrow(r4cong_com) == 0){r4cong_com <- data.frame(onsetT = "*")}  # for any blanks or missing RT then add *
  r4cong_com_list <- r4cong_com$onsetT # Extract onsetT values and store them in lists
  
  ## omission trials
  r4cong_omi <- subset(r4cong, r4cong$key_resp.resp_type  == "omission")[, "onsetT", drop=FALSE] # run 4 congruent omission, keep only onsetT column
  if(nrow(r4cong_omi) == 0){r4cong_omi <- data.frame(onsetT = "*")}  # for any blanks or missing RT then add *
  r4cong_omi_list <- r4cong_omi$onsetT # Extract onsetT values and store them in lists
  
  ## overall error trials
  r4cong_err <- subset(r4cong, r4cong$ACC == 0)[, "onsetT", drop=FALSE] # run 4 congruent error, keep only onsetT column
  if(nrow(r4cong_err) == 0){r4cong_err <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r4cong_err_list <- r4cong_err$onsetT # Extract onsetT values and store them in lists
  
  ####### incongruent -----------------------------------------------
  ##' separate congruent conditions as follow:
  ##' overall
  ##' correct trials
  ##' commission trials 
  ##' omission trials 
  ##' overall error trials
  
  ## overall
  r4incong <- subset(run4, run4$condition == "Incongruent") # run 4 incongruent 
  # r4incong_times <- subset(run4, run4$condition == "Incongruent")[, "onsetT", drop=FALSE] # run 4 incongruent, keep only onsetT column
  r4incong$onsetT[is.na(r4incong$onsetT) | r4incong$onsetT == 0] <- "*" # for any blanks or missing RT then add *
  
  ## correct trials
  r4incong_corr <- subset(r4incong, r4incong$key_resp.resp_type == "correct")[, "onsetT", drop=FALSE] # run 4 incongruent correct, keep only onsetT column
  if(nrow(r4incong_corr) == 0){r4incong_corr <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r4incong_corr_list <- r4incong_corr$onsetT # Extract onsetT values and store them in lists
  
  r4incong_com <- subset(r4incong, r4incong$key_resp.resp_type  == "commission")[, "onsetT", drop=FALSE] # run 4 incongruent commission, keep only onsetT column
  if(nrow(r4incong_com) == 0){r4incong_com <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r4incong_com_list <- r4incong_com$onsetT # Extract onsetT values and store them in lists
  
  r4incong_omi <- subset(r4incong, r4incong$key_resp.resp_type  == "omission")[, "onsetT", drop=FALSE] # run 4 incongruent omission, keep only onsetT column
  if(nrow(r4incong_omi) == 0){r4incong_omi <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r4incong_omi_list <- r4incong_omi$onsetT # Extract onsetT values and store them in lists
  
  r4incong_err <- subset(r4incong, r4incong$ACC == 0)[, "onsetT", drop=FALSE] # run 4 incongruent error, keep only onsetT column
  if(nrow(r4incong_err) == 0){r4incong_err <- data.frame(onsetT = "*")} # for any blanks or missing RT then add *
  r4incong_err_list <- r4incong_err$onsetT # Extract onsetT values and store them in lists
  
  
  #### SUMMARY DATA BY RUN --------------------------------------------------------
  
  # Standardize response column (fix run4 RespType bug safely)
  standardize_resp <- function(df){
    if("RespType" %in% names(df)){
      df$key_resp.resp_type <- df$RespType
    }
    return(df)
  }
  run1 <- standardize_resp(run1)
  run2 <- standardize_resp(run2)
  run3 <- standardize_resp(run3)
  run4 <- standardize_resp(run4)
  
  # Standardize RT column name
  run1$rttime <- run1$r1.rttime
  run2$rttime <- run2$r2.rttime
  run3$rttime <- run3$r3.rttime
  run4$rttime <- run4$r4.rttime
  
  runs_list <- list(run1, run2, run3, run4)
  
  summarize_run <- function(df, run_id){
    
    r1cong <- df %>% filter(condition == "Congruent")
    r1incong <- df %>% filter(condition == "Incongruent")
    
    tibble(
      run = run_id,
      
      ACC = mean(df$ACC, na.rm=TRUE),
      CongruentACC = mean(r1cong$ACC, na.rm=TRUE),
      IncongruentACC = mean(r1incong$ACC, na.rm=TRUE),
      
      CongruentCorrectN = sum(r1cong$key_resp.resp_type=="correct"),
      CongruentCommissionN = sum(r1cong$key_resp.resp_type=="commission"),
      CongruentOmissionN = sum(r1cong$key_resp.resp_type=="omission"),
      
      IncongruentCorrectN = sum(r1incong$key_resp.resp_type=="correct"),
      IncongruentCommissionN = sum(r1incong$key_resp.resp_type=="commission"),
      IncongruentOmissionN = sum(r1incong$key_resp.resp_type=="omission"),
      
      CorrectN = sum(df$key_resp.resp_type=="correct"),
      CommissionN = sum(df$key_resp.resp_type=="commission"),
      OmissionN = sum(df$key_resp.resp_type=="omission"),
      
      OverallRT = mean(df$rttime, na.rm=TRUE),
      CorrectRT = mean(df$rttime[df$key_resp.resp_type=="correct"], na.rm=TRUE),
      CommissionRT = mean(df$rttime[df$key_resp.resp_type=="commission"], na.rm=TRUE),
      
      CongruentRT = mean(r1cong$rttime, na.rm=TRUE),
      CongruentCorrectRT = mean(r1cong$rttime[r1cong$key_resp.resp_type=="correct"], na.rm=TRUE),
      
      IncongruentRT = mean(r1incong$rttime, na.rm=TRUE),
      IncongruentCorrectRT = mean(r1incong$rttime[r1incong$key_resp.resp_type=="correct"], na.rm=TRUE)
    )
  }
  
  summary_by_run <- purrr::map2_dfr(
    runs_list,
    1:4,
    summarize_run
  )
  
  summary_by_run$subject <- subj
  summary_by_run$session <- dte
  
  summary_by_run <- summary_by_run %>%
    select(subject, session, everything())
  
  # Save Runwise Summary
  write.csv(
    summary_by_run,
    file = file.path(outdir,
                     paste0("sub-", subj, "_ses-", dte,
                            "_flanker4_runwise_summary.csv")),
    row.names = FALSE
  )
  
  print(paste("Saved run-wise summary for subject", subj))
  
  
  overall_summary <- summary_by_run %>%
    summarise(
      OverallACC = mean(ACC, na.rm=TRUE),
      CongruentACC = mean(CongruentACC, na.rm=TRUE),
      IncongruentACC = mean(IncongruentACC, na.rm=TRUE),
      OverallRT = mean(OverallRT, na.rm=TRUE)
    ) %>%
    mutate(subject = subj,
           session = dte)
  write.csv(overall_summary,
            file.path(outdir,
                      paste0("sub-",subj,"_",dte,
                             "_flanker4_summary_statistics.csv")),
            row.names=FALSE)
  # --------------------------------------------------
  # APPEND TO GROUP OVERALL FILE
  # --------------------------------------------------
  
  group_overall_file <- file.path(outdir,
                                  "flanker4_overall_ALLsubjects.csv")
  
  if(!file.exists(group_overall_file)){
    write.csv(overall_summary,
              group_overall_file,
              row.names=FALSE)
  } else {
    write.table(overall_summary,
                group_overall_file,
                sep=",",
                row.names=FALSE,
                col.names=FALSE,
                append=TRUE)
  }
  
