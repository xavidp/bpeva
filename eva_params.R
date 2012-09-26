#!/home/ueb/repo/peeva/eva_params.R
#
# SCRIPT: eva_params.R
# SCOPE: to be called from eva_main.R
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

##############################################################

# Basic startup params
# ---------------------------------------------------------------------
startdate <- paste(format(Sys.Date(), "%y%m%d"), sep="")
program_ueb <- "eva_main.R";


# 0c. set some reasonable defaults for the options that are needed, but were not specified.
# ---------------------------------------------------------------------
p_input    = "test_in"	 # "dir_in_sara_207"     }
p_output   = "test_out"	 # "dir_out_sara_207"    }
p_index    = FALSE         
p_filter   = "BRCA"            
p_log      = TRUE        
p_summarize= TRUE          
p_keep     = TRUE # Enable if run through editor and you want to keep temp files
p_cpus     = 4             
p_parallel = TRUE
p_label    = ".test_refactoring"	 # ".sara207_4s4cpu"        # Run Label for output filenames
p_bwa      = 1          # 1: bwa aln (short reads, low errors, allowing paired end also); 2= bwa bwasw (longer reads, single end only) # Algorythm for mapping with bwa

# Other early initialization of variables
# Set the working directory
wd <- "/home/xavi/repo/peeva/"
setwd(wd)


##############################################################

# 2b. Define path params for all runs
#----------------------------------
path_fastq = "/home/ueb/fastqc/fastqc"
path_genome = "/home/xavi/Data/Data_Genomes/hg19/hg19.fa"
path_vcfutils = "/usr/share/samtools/vcfutils.pl"
path_convert2annovar = "/home/ueb/annovar/convert2annovar.pl"
path_annotate_variation = "/home/ueb/annovar/annotate_variation.pl"
path_annotate_humandb = "/home/ueb/annovar/humandb/"
path_summarize_annovar = "/home/ueb/annovar/summarize_annovar.pl"           
