## ===================================================== ##
# Title:        Extract to CSV all edX events for multiple individual users ####
# Project:      edX user trajectory analysis
# 
# Copyright 2017-18 Michael Ginda & Taylor Williams
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
#     
#     http://www.apache.org/licenses/LICENSE-2.0
#     
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.
#      
#
# Authors:      Michael Ginda, Taylor Williams
# Affiliation:  Indiana University, Purdue University
#
# 
# Description:  This script extracts all of a user's activity from the edX event log files in a user 
#               selected folder.  It outputs the resulting data (all events tied to a single user's ID) 
#               as a standard a CSV file. 
#               (*NOTE: the edX provided logs are in NDJSON format, not the typical JSON format.)
#  
# File input stack: 
#            1) A folder contining one or more "*.log.gz" event log file(s)    (source: edX)
# 
# Package dependencies: jsonlite, ndjson, tcltk
#
# Changelog:
#   2017.08.11. Initial Code
#   2017.08.13. Added user output and user save as for output file
#   2017.08.15. Changed to extracting single user's activity
#   2017.08.28. Fixed require statement bug (moved required packages in individual require statements)
#   2017.08.29. Update to comments, file saving updates, added save to RData file (added to JSON and CSV) 
#   2017.09.29. Updating file for sharing
#   2017.10.19. Updated file create function to produce event trajectory logs from a list of student IDs
#   2017.10.31. Updated function to maintain only relevant fields
#   2018.02.05. Cleaned Up script and added a manual user list selection function
#   2018.02.16. Incorporated code from Taylor's fork:
#                  2017.11.09. updated to allow a list of student_id values to come from an CSV file
#                  2017.11.10. added user choice for output file type
#                              added saving processing time and details for each loop to a log file
#
## ===================================================== ##

######### Setup ########## 
## _Clean the environment ####
rm(list=ls()) 

## _start timer to track how long the script takes to execute
start <-  proc.time() #save the time (to compute elapsed time of script)

## _Load required packages #####
require("jsonlite")   #for working with JSON files (esp. read and write)
require("ndjson")     #needed to read the non-standard JSON log files (NDJSON format)
require("tcltk2")     #for OS independent GUI file and folder selection
require("dplyr")      # for building tibbles (tidy data frames) 

####Functions
#getData
##The getData is a function used to select a CSV file listing student identifiers/course structure 
#to be CSV is placed in the variable 'data' in the global environment
getData <- function() {
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{ {CSV Files} {.csv} } { {All Files} * }"))
  if (name == "")
    return(data.frame()) # Return an empty data frame if no file was selected
  data <- read.csv(name)
  assign("data", data, envir = .GlobalEnv)
  cat("The imported data are in 'data'\n")
}

#PrintMinSec
## The PrintMinSec function takes two Sys.time() values and prints their difference in MM:SS
PrintMinSecDiff <-  function(fin, ini){
  dif=as.numeric(difftime(fin, ini, units='min'))
  return(paste0(sprintf('%02d', as.integer(dif)), ":"
                ,sprintf('%02.0f', (dif-as.integer(dif))*60)))
}

#LogCapture 
## The LogCapture function allows mass extracting individual set of student
#logs based on known set of student IDs for an #   edX course. The function
#creates a unique log file saved to `path_output` for each student ID in the
#list, saved as either a CSV file (default), a JSON (fileFormat = "JSON"), or
#both (fileFormat = "both").

LogCapture <- function(student_IDs, fileList, eventLog, path_output, fileFormat = "CSV"){      
  #count number of students
  numStudents <- nrow(student_IDs)
  #count number of JSON log files 
  numLogFiles <- length(fileList) 
  #initialize time tracker
  curID_startTime <- Sys.time()
  
  #loop through the student IDs
  for(j in 1:numStudents){
    curID <- student_IDs$student_id[j]
    
    
    # Build list of all student_id values that have already completed files saved within the path
    listCompletedIDs <- list.files(full.names = FALSE, recursive = TRUE, 
                                   path = path_output,
                                   pattern = "(.json|.csv)$", include.dirs = FALSE)
    # clean list to retain only IDs
    listCompletedIDs <- sub(".*/", "", listCompletedIDs)     # remove subdirectory names
    listCompletedIDs <- sub(".json", "", listCompletedIDs)   # remove extension
    listCompletedIDs <- sub(".csv",  "", listCompletedIDs)   # remove extension
    
    
    # build the event file for the current student_id if the student's file 
    # doesn't already exist in the output folder, 
    if(!(curID %in% listCompletedIDs)){
      #save the start time of processing the current ID
      curID_startTime[j] <- Sys.time()
      
      # loop through all the event files, extract any event matching the current student_id
      for(i in 1:numLogFiles){
        curFileName <- fileList[i] 
        
        #print update message to console
        message("Processing log file ", i, " of ", numLogFiles, 
                " (for student ", j, " of ", numStudents, "; id: ", curID, ")")
        curID_startTime[j+1]  <- Sys.time()
        message(PrintMinSecDiff(fin = curID_startTime[j+1], 
                                ini = curID_startTime[j]), " since curID began",
                if(j>1) "; prevID took ", PrintMinSecDiff(fin = curID_startTime[j], 
                                                          ini = curID_startTime[j-1]))
        print(proc.time() - start)
        
        #read log data (NOTE: logs are in NDJSON format, not typical JSON format)
        ndData  <-   ndjson::stream_in(curFileName)
        
        #extract events for a single student, add to the complete eventLog for that student
        eventLog <- rbind.data.frame(eventLog, 
                                            subset(ndData,ndData$context.user_id==curID), 
                                            fill=TRUE)
      }
      
      #Identifies all columns that are maintained for the rest of the workflow
      eventLog<-subset(eventLog,
                       select=c("accept_language", "agent",
                                "augmented.country_code", "context.course_id",
                                "context.org_id", "context.user_id", "event",
                                "event_source", "event_type", "time", "username",
                                "name","session", "context.module.display_name",
                                "context.module.usage_key", "event.problem_id",
                                "event.attempts", "event.grade", "event.max_grade",
                                "event.state.seed", "event.success",
                                "event.answer.file_key", "event.attempt_number",
                                "event.created_at", "event.submission_uuid",
                                "event.submitted_at","event.feedback",
                                "event.feedback_text", "event.rubric.content_hash",
                                "event.score_type","event.scored_at",
                                "event.scorer_id"))
      ###Unused columns that could be useful for other courses
      #event.correctness	event.hint_label	event.hints.0.text	
      #event.module_id	event.problem_part_id	event.question_type	
      #event.trigger_type	event.enrollment_mode	event.social_network	
      #event.answers	event.submission
      
      #Write student log file
      # save all of this student's events to chosen filetype
      if(fileFormat == "JSON"){
        write_json(x = eventLog, 
                   path = file.path(path_output, paste0(curID, ".json")))
      }else if(fileFormat == "CSV"){
        write.csv(x = eventLog, 
                  file  = file.path(path_output, paste0(curID, ".csv")), row.names = FALSE)
      }else if(fileFormat == "both"){
        write_json(x = eventLog, 
                   path = file.path(path_output, paste0(curID, ".json")))
        write.csv(x = eventLog, 
                  file  = file.path(path_output, paste0(curID, ".csv")), row.names = FALSE)
      }else{
        message("invalid file format selected")
        return()  #exit function
      }
      
      #Clears the event log df for next student
      eventLog <- NULL
    
    }# end of building & saving eventLog
    
  }# end of single student loop
  
}# end of LogCapture function




######### Main ########## 
#Creates paths used to locate directory for research data sets and save processing outputs
## select folder contining one or more "*.log.gz" event log file(s)    (source: edX)
path_data = tclvalue(tkchooseDirectory())
## select folder for output files (a file for each user's event log)
path_output = paste0(tclvalue(tkchooseDirectory()),"/")

#read in from CSV the list of Users IDs extracted from student user database
getData() #function that reads CSV file into variable 'data'
data <- data$student_id  # retain only the id column
names(data) <- "id"   # rename column
curUserIDS <- data$id # convert dataframe of user ids to integer list (as needed for the function)
eventLog <- NULL # create empty variable

## _Build list of all event files for course####
#Store all the filenames of JSON formatted edX event logs within a user selected directory 
# (files ending with ".log.gz").
fileList <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = path_data,
                       pattern = ".log.gz$")

#call the Log Capture function for this list of users
LogCapture(curUserIDS = curUserIDS, 
           eventLog = eventLog, 
           fileList = fileList, 
           path_output = path_output,
           fileFormat = "CSV")

######### Finishing Details ########## 
#Indicate completion
message("\n**** Complete! ****\n")

## _Script processing time feedback #####
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in sec):\n")
print(proc.time() - start)

## _Clear environment variables
rm(list=ls())   
