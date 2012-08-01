## 2012. Xavier de Pedro Puente - xavier.depedro@vhir.org
## Licensed under the Creative Commons Attribution-ShareAlike 3.0 License
## cc-by-sa 3.0
##
## Derived from ideas and code from Michael Zeller, Harry Mangalam and Vinh Nguyen
## http://blog.nguyenvq.com/
##
## Acknowledgements: 
## * Aleix Ruiz de Villa, for comments and feedback.

## 0. Define params for each run, using Multi-core on a single computer
#----------------------------------
file2process <- ""
scriptname <- "eva_ueb_for_r.pl" # Adapted version to work from this R script to be parallelized
scriptparams <- " -o ./test_out"
#scriptparams <- "-i ./test_in -o ./test_out -s -k | tee /dev/tty ./logs/log_both.txt
setwd("/home/xavi/repo/peeva/")
abs_path_to_script <- getwd() # Path absolut to the script
rel_path_to_input_files <- "/test_in/" # Include both the trailing slash at the right of the folder name and the one at its left
abs_path_to_input_files <- paste(abs_path_to_script, rel_path_to_input_files, sep="")
file_list_name <- "fastq_input_list.txt"

sink('SnowFallExample.Rout', split=TRUE)
.Platform
.Machine
R.version
Sys.info()

if(!require(snowfall)){ install.packages("snowfall") }
library(snowfall)

# 1. Initialisation of snowfall.
#----------------------------------
# (if used with sfCluster, just call sfInit())
sfInit(parallel=TRUE, cpus=7) # Amb parallel=TRUE s'envien les feines als nodes fills
# Si es posa "parallel=FALSE" no ho paralelitzara 
# i mostrara els missatges d'error per pantalla de forma normal. 

# 2. Loading data (from a package in this example), or from elsewhere (adapt accordingly).
#----------------------------------

## if(!require(ExamplePackage)){ install.packages("ExamplePackage") }
## require(ExamplePackage)
## data(MyDataset)

# 3. Wrapper, which can be parallelised.
#----------------------------------
wrapper <- function(file2process.w, abs_path_to_input_files.w, scriptparams.w) {
  # Output progress in worker logfile
  cat( "Current file: ", file2process.w, "\n" )
  system(paste("perl ", abs_path_to_input_files.w, scriptparams.w, sep=""), TRUE);
  # function from the perl script come here
  setwd("/home/xavi/repo/peeva")
  
  gc() # Let's clean ouR garbage if possible
  return(NULL) # return nothing, since results are saved on disk from the perl script
}


# 4. Exporting needed data and loading required
#----------------------------------
# # packages on workers.
# sfExport("MyDataset")
# # Install dependency of sfClusterSetupRNG() if not yet installed
# if(!require(cmprsk)){ install.packages("cmprsk") }
# sfLibrary(cmprsk) 
sfExport("wrapper", "file2process", "abs_path_to_input_files", "scriptparams")

# # 5a. Start network random number generator
#----------------------------------
#  # Install dependency of sfClusterSetupRNG() if not yet installed
#  if(!require(rlecuyer)){ install.packages("rlecuyer") }
# # (as "sample" is using random numbers).
# sfClusterSetupRNG()

# 5b. Get the list of files to be processed 
#----------------------------------
# Get the list of files in "input" directory through a system call to "ls *" and save the result to a file on disk
system(paste("ls ",path_to_input_files,"*.fastq > ",file_list_name, sep=""), TRUE)
# Read the file with the list of files to be processed
file_list <- read.table(file_list_name, sep="")

# Count the number of source files
number_of_source_files = length(file_list[[1]])

# remove the directory prefix from the names
# through gsub, alternatively
file_list <- gsub(path_to_input_files,"", file_list[[1]])
file_list <- gsub(".fastq","", file_list)


# 6. Distribute calculation
#----------------------------------
# Call the wrapper function to do the Job in child processes
start <- Sys.time(); result <- sfLapply(file_list, function(file2process) wrapper(file2process, abs_path_to_input_files, scriptparams)) ; Sys.time()-start

# Result is always in list form.
mean(unlist(result))

# 7. Stop snowfall
#----------------------------------
sfStop() 

# Close the R output connection to the file
sink()
