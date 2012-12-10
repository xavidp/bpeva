#!/home/ueb/repo/peeva/eva_main.R
#
# SCRIPT: eva_main.R
# SCOPE:  Main program that performs Exoma Variant Analysis. 
#         Equivalent to the procesa.R from the other UEB scripts  
# Author: Xavier de Pedro (2011-2012)
#        xavier.depedro@vhir.org
###################################################

## Licensed under the Creative Commons Attribution-ShareAlike 3.0 License
## cc-by-sa 3.0
##
## Derived from ideas and code from Michael Zeller, Harry Mangalam and Vinh Nguyen
## http://blog.nguyenvq.com/
##
## Acknowledgements: 
## * Alex Sánchez Pla, for comments and feedback.
## * Aleix Ruiz de Villa, for comments and feedback.
## * Josep Lluis Mosquera, for comments and feedback.

# 0. Basic startup process (for the working direectory)
# ---------------------------------------------------------------------
program_ueb <- "eva_main.R";

# Set the working directory from either one of the two options (a and b) listed below
## a) the hardcoded way
#wd <- "/home/ueb/repo/peeva/"
#
## b) dynamically from the folder where the main script program_ueb is
wd <- getwd()
wdres <- system(paste("locate", program_ueb, "| grep", wd, sep=" "), intern=TRUE)
wdres <- gsub(program_ueb, "", wdres, ignore.case = FALSE, perl = FALSE, fixed = TRUE)
setwd(wdres)

# Import Params for this EVA analysis run and the working directory for the whole project 
source("eva_params.R")

# Import UEB Functions for the EVA analysis 
source("eva_analysis_functions.R")

##############################################################################
##############################################################################
### Main Program #############################################################
##############################################################################
##############################################################################

# 0. Check dependencies and install missing packages
#---------------------------------------------------
# Required packages
if(!require(getopt)){ install.packages("getopt") }
library('getopt', quietly = TRUE);
if(!require(snowfall)){ install.packages("snowfall") }
library(snowfall, quietly = TRUE)


# Command to run on Debian machines to install some of the requriements
# ---------------------------------------------------------------------
# sudo apt-get install perl perl-suid
# sudo apt-get install bwa samtools
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

##############################################################

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
  'bwa'      , 'w', 1, "integer"  , #, "b__W__a algorythm used. See http://bio-bwa.sourceforge.net/bwa.shtml ".
  'filter'   , 'f', 1, "character", # "__F__ilter results for these target genes # Optional",
  'log'      , 'l', 1, "integer"  , # "__L__og info about the process into a log file # Optional",
  'summarize', 's', 0, "logical"  , # "__S__summarize results in a single .csv file with annotations # Optional",
  'keep'     , 'k', 0, "logical"  , #, "__K__eep temporal dummy files after they have been used # Optional",
  'cpus'     , 'c', 1, "integer"  , #, "__C__pus to use from the multicore computer or cluster # Optional".
  'parallel' , 'p', 0, "logical"  , #, "__P__arallel processing using SnowFall package # Optional"
  'label'    , 'b', 1, "character"  #, "Run la__B__el to add to output file names, Like: "test-paralel-a", or "sample with param X as Y" # Optional"
)

opt <- getopt(matrix(my.options, ncol=4, byrow=TRUE))
#help was asked for and the script was called from the command line.
if ( !is.null(opt$help) || ((length(commandArgs()) >3 )
&& (is.null(opt$input) && is.null(opt$output)))) {

  ##get the script name (only works when invoked with Rscript).
  #self = commandArgs()[4];

  #print a friendly message and exit with a non-zero error code
#  cat(paste("Usage: ",self," [-[vh]] [-[-mean|m] <mean>] [-[-sd|s] <sd>] [-[-count|c] <count>]\n",sep=""));
  show_help(program_ueb)
  q(status=1);
}

# 0c. set some reasonable defaults for the options that are needed, but were not specified.
# ---------------------------------------------------------------------
if ( is.null(opt$input    ) ) { opt$input    = p_input  } # "dir_in_sara_207"     }
if ( is.null(opt$output   ) ) { opt$output   = p_output	} # "dir_out_sara_207"    }
if ( is.null(opt$index    ) ) { opt$index    = p_index         }
if ( is.null(opt$filter   ) ) { opt$filter   = p_filter           }
if ( is.null(opt$log) || opt$log ==1) { opt$log      = p_log        }
if ( is.null(opt$summarize) ) { opt$summarize= p_summarize          }
if ( is.null(opt$keep     ) ) { opt$keep     = p_keep         } # Enable if run through editor and you want to keep temp files
if ( is.null(opt$cpus     ) ) { opt$cpus     = p_cpus             }
if ( is.null(opt$parallel ) ) { opt$parallel = p_parallel        }
if ( is.null(opt$label    ) ) { opt$label    = p_label	} # ".sara207_4s4cpu"        } # Run Label for output filenames
if ( is.null(opt$bwa      ) ) { opt$bwa      = p_bwa          } # 1: bwa aln (short reads, low errors, allowing paired end also); 2= bwa bwasw (longer reads, single end only) # Algorythm for mapping with bwa


##############################################################

# 1. Initialisation of snowfall.
#----------------------------------
# (if used with sfCluster, just call sfInit())
sfInit(parallel=opt$parallel, cpus=opt$cpus, useRscript=TRUE) # Amb parallel=TRUE s'envien les feines als nodes fills
#useRscript : Change startup behavior (snow>0.3 needed): use shell scripts or R-script for startup (R-scripts beeing the new variant, but not working with sfCluster.

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

##############################################################

# 2a. Get the list of files to be processed 
#----------------------------------
if (path_input_absolute == 0) {
  abs_path_to_script = getwd() # Absolute path for the script
  rel_path_to_input_files = opt$input # Include both the trailing slash at the right of the folder name and the one at its left
  abs_path_to_input_files = paste(abs_path_to_script, "/", rel_path_to_input_files, "/", sep="")
} else {
  # Absolute path for the script
  abs_path_to_script = getwd() # Absolute path for the script
  abs_path_to_input_files = paste(opt$input, "/", sep="") # Include both the trailing slash at the right of the folder name and the one at its left
}

# When input files contain paired end reads (_pe), a temporal (_tmp) file name will be used first until we combine the data from both strands
if (p_bwa == 2) {
	filename_list = paste(opt$output, "/", "log.",startdate, opt$label, ".fastq_pe_tmp.txt", sep="")
	# Get the list of files in "input" directory through a system call to "ls *" and save the result to a file on disk
	system(paste("ls ",abs_path_to_input_files,"*_sequence.fastq > ", filename_list, sep=""), TRUE)
	# Add a lock file to indicate that paired end sample data is to be processed. This lock file will be removed only after all couples of files from samples have been merged into one file per sample
	system(paste("touch ", filename_list, ".lock", sep=""), TRUE)
} else {
	# When input files contained single end short reads, or long reads, the definitive file list will be created here in this step already 
	filename_list = paste(opt$output, "/", "log.",startdate, opt$label, ".fastq_input_list.txt", sep="")
	# Get the list of files in "input" directory through a system call to "ls *" and save the result to a file on disk
	system(paste("ls ",abs_path_to_input_files,"*.fastq > ", filename_list, sep=""), TRUE)
}

# Read the file with the list of files to be processed
file_list <- read.table(filename_list, sep="")

# Count the number of source files
n_files = length(file_list[[1]])

# remove the directory prefix from the names as well as the ending .fastq
# through gsub, alternatively
file_list <- gsub(abs_path_to_input_files,"", file_list[[1]])
file_list <- gsub(".fastq","", file_list)

##############################################################

# 2b. Define params list for all runs
#----------------------------------
params <- list()
params <- list(startdate = startdate,
               scriptname = program_ueb, # Adapted version to work from this R script to be parallelized
               opt = opt, # command line arguments passed to the program run
               #scriptparams = "-i ./test_in -o ./test_out -s -k | tee /dev/tty ./logs/log_both.txt
               file_list = file_list,
               abs_path_to_script = abs_path_to_script, 
               filename_list = filename_list,
               n_files = n_files,
               wd = wd, # the working directory
               directory_in = opt$input,
               directory_out = opt$output,
               log = opt$log,
               log.folder = opt$output, # "logs",
               path_fastq = path_fastq,
               path_genome = path_genome,
               path_vcfutils = path_vcfutils,
               path_convert2annovar = path_convert2annovar,
               path_annotate_variation = path_annotate_variation,
               path_annotate_humandb = path_annotate_humandb,
               path_summarize_annovar = path_summarize_annovar           
)

#routlogfile <- paste("log.", startdate, opt$label, ".SnowFall.Rout", sep="") # SnowFall R output file
routlogfile <- paste("log.", startdate, opt$label, ".run.txt", sep="") # Output file with info from SnowFall (when used), params for the run and system info
abs_routlogfile <- paste(opt$output, "/", routlogfile, sep="")
system(paste("touch ", abs_routlogfile, sep=""))

# First write the output from SnowFall (because it's not appending content but creating the file from scratch)
sink(abs_routlogfile , split=TRUE)

# After that, you can append extra information from the run to the same file
w.output.run("*** opt ***", unlist(opt),  abs_routlogfile )
w.output.run("*** params ***", unlist(params),  abs_routlogfile )
w.output.run("*** params_wseq ***", unlist(params_wseq),  abs_routlogfile )
w.output.run("*** params_w2pps ***", unlist(params_w2pps),  abs_routlogfile )
w.output.run("*** params_w2pf ***", unlist(params_w2pf),  abs_routlogfile )
w.output.run("*** .Platform ***", unlist(.Platform), abs_routlogfile )
w.output.run("*** .Machine ***", unlist(.Machine),  abs_routlogfile )
w.output.run("*** R.version ***", unlist(R.version), abs_routlogfile )
w.output.run("*** Sys.info ***", unlist(Sys.info()),  abs_routlogfile )

##############################################################

# 3. Create some files to log some data about the run
#----------------------------------

#system(paste("touch ", paste(opt$out, "/", as.matrix(params$file_list) ,sep=""), sep=""))

##############################################################

# 4. Exporting needed data and loading required
#----------------------------------
# # packages on workers.
# sfExport("MyDataset")
# # Install dependency of sfClusterSetupRNG() if not yet installed
# if(!require(cmprsk)){ install.packages("cmprsk") }
# sfLibrary(cmprsk) 
sfExport("params", 
         "params_wseq",
         "params_w2pps",
         "params_w2pf",
         "now",
         "print_doc",
         "print_done",
         "print_error",
         "print_mes",
         "w.output.samples",
         "w.lock.sample.pe",
         "w.checklock.allsamples.pe",
         "w.unlock.sample.pe",
         "check2clean",
         "fun.quality.control",
         "fun.index.reference.genome",
         "fun.map.on.reference.genome",
         "fun.map.on.reference.genome",
         "fun.sam2bam.and.sort",
	 "fun.remove.pcr.dup",
	 "fun.index.bam.file",
	 "fun.stats",
	 "fun.variant.calling",
	 "fun.variant.filtering",
	 "fun.convert2vcf4",
	 "fun.variant.annotation.geneb",
	 "fun.variant.annotation.regionb",
	 "fun.variant.annotation.filterb",
	 "fun.variant.annotation.summarize",
	 "fun.grep.variants",
	 "fun.visualize.variants",
   "fun.build.html.report") # functions can be passed also to workers from master

##############################################################

# # 5a. Start network random number generator (optional, when needed)
#----------------------------------
#  # Install dependency of sfClusterSetupRNG() if not yet installed
#  if(!require(rlecuyer)){ install.packages("rlecuyer") }
# # (as "sample" is using random numbers).
# sfClusterSetupRNG()

##############################################################

# 6. Run Sequential and unique initial processes for the whole sample set
#----------------------------------

  if (opt$index) { # index.reference.genome
    # Next Step
    start <- Sys.time(); 
    step <- data.frame(0, 0)
    colnames(step) <- c("n","tmp")
    
    step <- fun.index.reference.genome(step.my  = step)
    duration <- Sys.time()-start;
    print(duration)
  } else {
    step <- data.frame(0, 0)
    colnames(step) <- c("n","tmp")
  }


##############################################################

# 7. Run Sequential processes for the each one of the samples
#----------------------------------
# Call the wrapper function to do the Job in child processes
#start <- Sys.time(); result <- sfLapply(1:length(file_list), function(file2process) wrapper(datastep, abs_path_to_input_files, scriptparams)) ; Sys.time()-start
start2 <- Sys.time(); result <- lapply(1:length(file_list), wrapper.sequential) ; duration <- Sys.time()-start2;
#start2 <- Sys.time(); for (ii in 1:length(file_list)) wrapper.sequential(ii, params); duration <- Sys.time()-start2;
print(duration)
cat("\n")
# Result is always in list form.
unlist(result)


##############################################################

# 8. Distribute Parallelizable calculation per sample
#----------------------------------
# Call the wrapper function to do the Job in child processes
#start <- Sys.time(); result <- sfLapply(1:length(file_list), function(file2process) wrapper(datastep, abs_path_to_input_files, scriptparams)) ; Sys.time()-start

# We changed the file_list parameter for the params$file_list parameter, so that in cases of paired-emnd mode, 
# this file list will have been rewritten to the new one at this step, and therefore, we will be able to process at this time step half the number of initial fastq files
start3 <- Sys.time(); result2 <- sfLapply(1:length(params$file_list), wrapper2.parallelizable.per.sample) ; duration <- Sys.time()-start3;
cat("\nRelative duration since last step: ")
print(duration)
cat("\n")

# Result is always in list form.
unlist(result2)

##############################################################

# 9. Distribute final Parallelizable calculation 
#----------------------------------
# Call the wrapper function to do the Job in child processes
start3 <- Sys.time(); result3 <- sfLapply(1, wrapper2.parallelizable.final) ; duration <- Sys.time()-start3;
cat("\nRelative duration since last step: ")
print(duration)
cat("\n")

# Result is always in list form.
unlist(result3)

##############################################################

# 10. Stop snowfall
#----------------------------------
sfStop() 

# Close the R output connection to the file
sink()


#signal success and exit. 
#q(status=0); # To be un-commented out at the very end of the development process,
              #so that the R process quits after execution

