#!/home/ueb/repo/peeva/eva_ueb2.R
#
# SCRIPT: eva_ueb2.R
# Input: Directories of input and output
# Output: List of Variants
# Autor: Xavier de Pedro, Alex Sanchez (2011-2012)
#        xavier.depedro@vhir.org
###################################################

## Licensed under the Creative Commons Attribution-ShareAlike 3.0 License
## cc-by-sa 3.0
##
## Derived from ideas and code from Michael Zeller, Harry Mangalam and Vinh Nguyen
## http://blog.nguyenvq.com/
##
## Acknowledgements: 
## * Aleix Ruiz de Villa, for comments and feedback.
## * Josep Lluis Mosquera, for comments and feedback.

##########################
### FUNCTIONS
##########################

get_timestamp <- function() 
{
  # ---------------------------------------------------------------------
  paste(Sys.Date(), format(Sys.time(), " %H:%Mh - "), sep="");
  
}

now  <- function() 
{
  # ---------------------------------------------------------------------
  paste(Sys.Date(), format(Sys.time(), " %H:%Mh - "), sep="");
  
}

print_doc <- function(mess, filename.my1) 
{
  # ---------------------------------------------------------------------
  mess <- paste(now(), mess, sep="")
  cat(mess)
  if (params$log) { 
    w.output(mess, filename.my1)
  }
}

print_done <- function(filename.my1) 
{
  # ---------------------------------------------------------------------
  mess <- paste(now(), "\t\t\t\t\t\t\t[DONE]\n\n", sep="")
  cat(mess)
  if (params$log) { 
    w.output(mess, filename.my1)
  }
}


print_error <- function(mess, filename.my1) 
{
  # ---------------------------------------------------------------------
  mess <- paste(now(), "\t\t\t\t\t\t\t[ERROR]\n\n", sep="")
  cat(mess)
  if (params$log) { 
    w.output(mess, filename.my1)
  }
}

check2clean <- function(my.option)
{
  # ---------------------------------------------------------------------
  var <- my.option
  if ( !is.null(opt$k) ) # keep temporary files if requested by the user 
  { 
    # do nothing
  } else { # clean temporary files not from this but from the previous step
    system(paste("rm  ", var, sep=""));
  }
}

##########################
### FUNCTION w.output
###
### 	Write output to log files on disk
##########################

w.output <- function(mess, filename.my2)
{
  write(mess, file=paste(params$log.folder,"/log.",Sys.Date(),".", filename.my2, ".txt", sep=""), append = TRUE, sep = "");
}

##########################
### FUNCTION show_help
##########################

show_help <- function()
{
  # ---------------------------------------------------------------------
  print_doc("HELP info ======================");
  cat("The help info needs to be updated to the new argument system for calls to the", program_ueb);
  cat("\nThis program ", program_ueb," accepts 3 arguments passed to the program call:")
  cat("\n          -i: directory with source .fastq files \t\t\t *** required ***")
  cat("\n          -o: directory to save output files \t\t\t\t *** required ***")
  cat("\n          -n: -index (indexing of the reference genome)\t\t\t    [optional]")
  cat("\n          -s: summarize results with annotations in a single .csv file \t    [optional]")
  cat("\n          -f: filter results for these target genes \t\t\t    [optional]")
  cat("\n        with this syntax for one gene:")
  cat("\n          -f BRCA1 ")
  cat("\n        or")
  cat("\n          -f 'BRCA1|BRCA2|unknown' ")
  cat("\n        for more than one gene or string to filter results")
  cat("\n          -k: keep temporary files after they have been used \t\t    [optional] ")
  cat("\n          -h: show this help text \t\t\t\t\t    [optional]\n")
  cat("\n          -l: log results (optional, to be coded ;-). In the mean time, use the standard unix utilities such as:\n")
  cat("\n        Example1: Rscript ", program_ueb," -i ./dir_in -o ./dir_out -s -f 'BRCA1|BRCA2|unknown' > ./logs/log_stdout.txt 2> ./logs/log_stderr.txt")
  cat("\n        Example2: Rscript ", program_ueb," -i ./test_in -o ./test_out -s -k > ./logs/log_both.txt 2>&1")
  cat("\n        Example3: Rscript ", program_ueb," -i ./test_in -o ./test_out -s -k | tee /dev/tty ./logs/log_both.txt\n");
  cat("\n##############################################################################\n");
}

##########################
### FUNCTION fun.quality.control
##########################

fun.quality.control <- function(file2process.my2, step.my, scriptparams.my1) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste("	Step ", step.my$n, ".", step.my$tmp, ". Quality Control and Preprocessing: ", file2process.my2, "\n", sep=""), file2process.my2);
  
  # Remove .fastq (substitute it with nothing) from file names
  name = sub(".fastq","",file2process.my2,  ignore.case = FALSE, perl = FALSE, fixed = TRUE);
  #  print_doc("$now -   Step $step_n.$step_tmp Quality Control and Preprocessing: $name ...");
  file_in = paste(params$directory_in, "/", file2process.my2, ".fastq", sep="");
  #  $file_out = "$directory_out/$name.txt";
  command00 = params$path_fastq; # path to fastqc binary; adapt to your case.
  #	$command00 = "ls"; # next command.
  options00 = paste(file_in, " --outdir=", params$directory_out, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  print_done(file2process.my2);
  
   
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

##########################
### FUNCTION fun.index.reference.genome
##########################

fun.index.reference.genome <- function(file2process.my2, step.my, scriptparams.my1) {
  # update step number
  step.my$tmp <- step.my$tmp + 1

  print_doc(paste("	Step ", step.my$n, ".", step.my$tmp, ". Map against reference genome: Index the reference genome (if needed)\n", sep=""), file2process.my2);
	if ((params$opt$index) & (step.my$n == 1)) { # case to index the reference genome (time consuming, do only when really needed as requested)
		# Index the reference genome, if requested with argument -n and only for the first file if more than one sample to process
		command00 <- "bwa index"; # next command
		options00 <- paste("  -a bwtsw", params$path_genome, sep="");
	} else	{ # skip the indexing of the reference genome
		command00 <- "echo '  ...skipped...'"; # next command.
		options00 <- "";
	}
  command = paste(command00, " ", options00, sep="");
  system(command);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}
#==============================================================

##########################
### Main Program
##########################
program_ueb = "eva_ueb2.R";
sink('SnowFallExample.Rout', split=TRUE)
# .Platform
# .Machine
# R.version
# Sys.info()

# 0. Check dependencies and install missing packages
#---------------------------------------------------
# Required packages
if(!require(getopt)){ install.packages("getopt") }
library('getopt');
if(!require(snowfall)){ install.packages("snowfall") }
library(snowfall)


# Command to run on Debian machines to install some of the requriements
# ---------------------------------------------------------------------
# sudo apt-get install perl perl-suid
# sudo wget http://bioinform.usc.edu/annovar/xmpVO9ISYx/annovar.tar.gz (Versión from May 2012)
#
# For annovar to work, you need to download many annotation db to your hard disk
# with commands like:
#perl /home/ueb/annovar/annotate_variation.pl -webfrom annovar -downdb ljb_all -buildver hg19 /home/ueb/annovar/humandb/
#perl /home/ueb/annovar/annotate_variation.pl -webfrom annovar -downdb ljb_pp2 -buildver hg19 /home/ueb/annovar/humandb/
#perl /home/ueb/annovar/annotate_variation.pl -webfrom annovar -downdb ljb_mt -buildver hg19 /home/ueb/annovar/humandb/
#perl /home/ueb/annovar/annotate_variation.pl -webfrom annovar -downdb ljb_phylop -buildver hg19 /home/ueb/annovar/humandb/
#perl /home/ueb/annovar/annotate_variation.pl -downdb esp5400_all -buildver hg19 /home/ueb/annovar/humandb/
#perl /home/ueb/annovar/annotate_variation.pl -downdb ljb -buildver hg19 /home/ueb/annovar/humandb/
#perl /home/ueb/annovar/annotate_variation.pl -downdb 1000g2011may -buildver hg19 /home/ueb/annovar/humandb/
#perl /home/ueb/annovar/annotate_variation.pl -downdb avsift -buildver hg19 /home/ueb/annovar/humandb/
# ...

# ToDo: check
# http://www.bioconductor.org/help/workflows/variants/
# log file param: implement it.
# add new optional param: make colors of log files
# make number of core to use a param callable as a command line argument of the Rscript call

# 0b. Argument handling of the program call
# ---------------------------------------------------------------------
# # declare the command line flags/options we want to allow
# Possible values for the 1st column:
#   long flag name. 
# Possible values for the 2nd column:
#   short flag alias of column1 
# Possible values for the 3rd column:
#   0=no argument, 1=required argument, 2=optional argument.
# Possible values for the 4th column:
#   logical, integer, double, complex, character
# Possible values for the 5th column (optional):
#   A brief description of the purpose of the option.
my.options <- c(
  'help'     , 'h', 0, "logical"  , # "show help on usage # Optional",
  'input'    , 'i', 1, "character", # "Directory with __I__nput data files # Compulsory",
  'output'   , 'o', 1, "character", # "Directory with __O_utput data files # Compulsory",
  'index'    , 'n', 0, "logical"  , # "i__N__dex the reference genome # Optional",
  'filter'   , 'f', 1, "character", # "__F__ilter results for these target genes # Optional",
  'log'      , 'l', 1, "character", # "__L__og info about the process into a log file # Optional",
  'summarize', 's', 0, "logical"  , # "__S__summarize results in a single .csv file with annotations # Optional",
  'keep'     , 'k', 0, "logical"  , #, "__K__eep temporal dummy files after they have been used # Optional",
  'cpus'     , 'c', 1, "integer"  , #, "__C__pus to use from the multicore computer or cluster # Optional".
  'parallel' , 'p', 0, "logical"  #, "__P__arallel processing using SnowFall package # Optional"
)

opt = getopt(matrix(my.options, ncol=4, byrow=TRUE))

#help was asked for.
if ( !is.null(opt$help) ) {
  #get the script name (only works when invoked with Rscript).
  self = commandArgs()[1];
  #print a friendly message and exit with a non-zero error code
#  cat(paste("Usage: ",self," [-[vh]] [-[-mean|m] <mean>] [-[-sd|s] <sd>] [-[-count|c] <count>]\n",sep=""));
  show_help()
  q(status=1);
}

#set some reasonable defaults for the options that are needed,
#but were not specified.
if ( is.null(opt$input    ) ) { opt$input    = "test_in"     }
if ( is.null(opt$output   ) ) { opt$output   = "test_out"    }
if ( is.null(opt$index    ) ) { opt$index    = FALSE         }
if ( is.null(opt$filter   ) ) { opt$filter   = ""            }
if ( is.null(opt$log      ) ) { opt$log      = "log"         }
if ( is.null(opt$summarize) ) { opt$summarize= TRUE          }
if ( is.null(opt$keep     ) ) { opt$keep     = FALSE         }
if ( is.null(opt$cpus     ) ) { opt$cpus     = 7             }
if ( is.null(opt$parallel ) ) { opt$parallel = TRUE          }


# # other things found on the command line
# print "Other things found on the command line:\n" if $ARGV[0];
# foreach (@ARGV)
# {
#   print "$_\n";
# }

# 1. Initialisation of snowfall.
#----------------------------------
# (if used with sfCluster, just call sfInit())
sfInit(parallel=opt$parallel, cpus=opt$cpus) # Amb parallel=TRUE s'envien les feines als nodes fills
# Si es posa "parallel=FALSE" no ho paralelitzara 
# i mostrara els missatges d'error per pantalla de forma normal. 

# Amb la següent línia s'hauria de poder debuggar: sortira una llista del recorregut de crides de funcions
# fins el punt que peta. 
# La llista esta enumerdada, així que si li poses el numero pots anar al punt del proces que vulguis
# i resseguir els calculs per a veure què ha fallat.

#options(error = recover)

# 2. Loading data (from a package in this example), or from elsewhere (adapt accordingly).
#----------------------------------

## if(!require(ExamplePackage)){ install.packages("ExamplePackage") }
## require(ExamplePackage)
## data(MyDataset)

# Set the working directory
wd <- "/home/xavi/repo/peeva/"
setwd(wd)

# 2a. Get the list of files to be processed 
#----------------------------------
abs_path_to_script = getwd() # Path absolut to the script
rel_path_to_input_files = "test_in" # Include both the trailing slash at the right of the folder name and the one at its left
abs_path_to_input_files = paste(abs_path_to_script, "/", rel_path_to_input_files, "/", sep="")
file_list_name = "fastq_input_list.txt"

# Get the list of files in "input" directory through a system call to "ls *" and save the result to a file on disk
system(paste("ls ",abs_path_to_input_files,"*.fastq > ",file_list_name, sep=""), TRUE)
# Read the file with the list of files to be processed
file_list <- read.table(file_list_name, sep="")

# Count the number of source files
n_files = length(file_list[[1]])

# remove the directory prefix from the names
# through gsub, alternatively
file_list <- gsub(abs_path_to_input_files,"", file_list[[1]])
file_list <- gsub(".fastq","", file_list)

## 2b. Define params for all runs
#----------------------------------
params <- list(file2process = "",
               log = TRUE,
               scriptname = "eva_ueb2.R", # Adapted version to work from this R script to be parallelized
               scriptparams = " -o ./test_out",
               opt = opt, # command line arguments passed to the program run
               #scriptparams = "-i ./test_in -o ./test_out -s -k | tee /dev/tty ./logs/log_both.txt
               file_list = file_list,
               n_files = n_files,
               wd = wd, # the working directory
               directory_in = "test_in",
               directory_out = "test_out",
               log.folder = params$directory_out, # "logs",
               path_fastq = "/home/ueb/fastqc/fastqc",
               path_genome = "/home/xavi/Data/Data_Genomes/hg19/hg19.fa",
               path_vcfutils = "/usr/share/samtools/vcfutils.pl",
               path_convert2annovar = "/home/ueb/annovar/convert2annovar.pl",
               path_annotate_variation = "/home/ueb/annovar/annotate_variation.pl",
               path_annotate_humandb = "/home/ueb/annovar/humandb/",
               path_summarize_annovar = "/home/ueb/annovar/summarize_annovar.pl"           
)

# 3. Wrapper function, which can be parallelised.
#----------------------------------
wrapper <- function(datastep.my) {
  # Output progress in worker logfile
  file2process.my1 <- params$file_list[datastep.my]
  cat( "Current file: ", file2process.my1, "\n" )
  #  system(paste("perl ", abs_path_to_input_files.my1, scriptparams.my1, sep=""), TRUE);
  # function from the perl script come here
  setwd(params$wd)
  step <- data.frame(datastep.my, 0)
  colnames(step) <- c("n","tmp")
  # Re-set the log file, if it exists already and log is requested
  if (params$log) { 
      write(paste("			### NEW RUN (", Sys.Date()," - ", params$n_files, " files) ###\n", sep=""), file=paste(params$log.folder,"/log.", Sys.Date(),".", file2process.my1, ".txt", sep=""), append = FALSE, sep = "");
  }
#  step$n <- 0
#  step$tmp <- 0
  print_doc(paste("########## Step ", step$n, ". Processing file #", datastep.my, " (", file2process.my1, ") ... ##########\n", sep=""), file2process.my1);

  
  ### Parallelized Pipeline steps ###

  # First Step
  step <- fun.quality.control(file2process.my2  = file2process.my1,
                      step.my  = step,
                      scriptparams.my1  = scriptparams)

  
  # Next Step
  step <- fun.index.reference.genome(file2process.my2  = file2process.my1,
                      step.my  = step,
                      scriptparams.my1  = scriptparams)
  # XXX...


  # Last step of wrapper
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
sfExport("params", 
         "now",
         "print_doc",
         "print_done",
         "print_error",
         "w.output",
         "fun.quality.control",
         "fun.index.reference.genome") # functions can be passed also to workers from master

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


#signal success and exit. 
#q(status=0); # To be un-commented out at the very end of the development process,
              #so that the R process quits after execution

