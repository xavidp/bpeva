## 2012. Xavier de Pedro Puente - xavier.depedro@vhir.org
## Licensed under the Creative Commons Attribution-ShareAlike 3.0 License
## cc-by-sa 3.0
##
## Derived from ideas and code from Michael Zeller, Harry Mangalam and Vinh Nguyen
## http://blog.nguyenvq.com/
##
## Acknowledgements: 
## * Aleix Ruiz de Villa, for comments and feedback.

#==============================================================
# FUNCTIONS
#----------------------------------
# quality.control <- function(file2process.my2, directory_in.my2, directory_out.my2, scriptparams.my2, path_fastq.my1) {
#   # function from the perl script come here
#   
#   # Remove .fastq (substitute it with nothing) from file names
#   name = sub(".fastq","",file2process.my2,  ignore.case = FALSE, perl = FALSE, fixed = TRUE);
#   #  print_doc("$now -   Step $step_n.$step_tmp Quality Control and Preprocessing: $name ...");
#   file_in = paste(directory_in.my2, "/", file2process.my2, sep="");
#   #  $file_out = "$directory_out/$name.txt";
#   command00 = path_fastq.my1; # path to fastqc binary; adapt to your case.
#   #	$command00 = "ls"; # next command.
#   options00 = paste(file_in, " --outdir=", directory_out.my2, sep="");
#   command = paste(command00, options00, sep="");
#   system(command);
#   #  print_done();
#   
#   
#   gc() # Let's clean ouR garbage if possible
#   return(NULL) # return nothing, since results are saved on disk from the system command
# }

#==============================================================


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
sfInit(parallel=FALSE, cpus=7) # Amb parallel=TRUE s'envien les feines als nodes fills
# Si es posa "parallel=FALSE" no ho paralelitzara 
# i mostrara els missatges d'error per pantalla de forma normal. 

# Amb la següent línia s'hauria de poder debuggar: sortira una llista del recorregut de crides de funcions
# fins el punt que peta. 
# La llista esta enumerdada, així que si li poses el numero pots anar al punt del proces que vulguis
# i resseguir els calculs per a veure què ha fallat.
options(error = recover)

# 2. Loading data (from a package in this example), or from elsewhere (adapt accordingly).
#----------------------------------

## if(!require(ExamplePackage)){ install.packages("ExamplePackage") }
## require(ExamplePackage)
## data(MyDataset)

setwd("/home/xavi/repo/peeva/")

# 2a. Get the list of files to be processed 
#----------------------------------
abs_path_to_script = getwd() # Path absolut to the script
rel_path_to_input_files = "/test_in/" # Include both the trailing slash at the right of the folder name and the one at its left
abs_path_to_input_files = paste(abs_path_to_script, rel_path_to_input_files, sep="")
file_list_name = "fastq_input_list.txt"

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

## 2b. Define params for each run, using Multi-core on a single computer
#----------------------------------
params <- list(file2process = "",
               scriptname = "eva_ueb2.R", # Adapted version to work from this R script to be parallelized
               scriptparams <- " -o ./test_out",
               #scriptparams <- "-i ./test_in -o ./test_out -s -k | tee /dev/tty ./logs/log_both.txt
               file_list = file_list,
               directory_in = "test_in",
               directory_out = "test_out",
               path_fastq = "/home/ueb/fastqc/fastqc"
)

# 3. Wrapper function, which can be parallelised.
#----------------------------------
wrapper <- function(datastep.my) {
  # Output progress in worker logfile
  file2process.my1 <- params$file_list[datastep.my]
  cat( "Current file: ", file2process.my1, "\n" )
  #  system(paste("perl ", abs_path_to_input_files.my1, scriptparams.my1, sep=""), TRUE);
  # function from the perl script come here
  setwd("/home/xavi/repo/peeva")
  
  #file_list
  
#   # Parallelized Pipeline steps
#   quality.control(file2process.my2  = file2process.my1,
#                   directory_in.my2  = directory_in.my1, 
#                   directory_out.my2 = directory_out.my1,
#                   scriptparams.my1  = scriptparams, 
#                   path_fastq.my1    = path_fastq)

    # Remove .fastq (substitute it with nothing) from file names
    name = sub(".fastq","",file2process.my1,  ignore.case = FALSE, perl = FALSE, fixed = TRUE);
    #  print_doc("$now -   Step $step_n.$step_tmp Quality Control and Preprocessing: $name ...");
    file_in = paste(params$directory_in, "/", file2process.my1, ".fastq", sep="");
    #  $file_out = "$directory_out/$name.txt";
    command00 = params$path_fastq; # path to fastqc binary; adapt to your case.
    #  $command00 = "ls"; # next command.
    options00 = paste(file_in, " --outdir=", params$directory_out, sep="");
    command = paste(command00, " ", options00, sep="");
    system(command);
    #  print_done();
    
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
sfExport("params") # can functions be passed to workers from master?

# # 5a. Start network random number generator
#----------------------------------
#  # Install dependency of sfClusterSetupRNG() if not yet installed
#  if(!require(rlecuyer)){ install.packages("rlecuyer") }
# # (as "sample" is using random numbers).
# sfClusterSetupRNG()

# 6. Distribute calculation
#----------------------------------
# Call the wrapper function to do the Job in child processes
#start <- Sys.time(); result <- sfLapply(1:length(file_list), function(file2process) wrapper(datastep, abs_path_to_input_files, scriptparams)) ; Sys.time()-start
start <- Sys.time(); result <- sfLapply(1:length(file_list), wrapper) ; Sys.time()-start

# Result is always in list form.
unlist(result)

# 7. Stop snowfall
#----------------------------------
sfStop() 

# Close the R output connection to the file
sink()


