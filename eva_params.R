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
p_input    = "dir_in_sara_207" # "test_in"	 # "dir_in_sara_207"     }
p_output   = "dir_out_sara_207" # "test_out"	 # "dir_out_sara_207"    }
p_index    = FALSE         
p_filter   = "BRCA"            
p_log      = TRUE        
p_summarize= TRUE          
p_keep     = TRUE # Enable if run through editor and you want to keep temp files
p_cpus     = 4             
p_parallel = TRUE
p_label    = ".sara207_4s4cpu_bwa1_b"	 # "test-121002" ".sara207_4s4cpu"        # Run Label for output filenames
p_bwa      = 1          # 1: bwa aln (short reads, low errors, allowing paired end also); 2= bwa bwasw (longer reads, single end only) # Algorythm for mapping with bwa

# Other early initialization of variables
# Set the working directory
wd <- "/home/xavi/repo/peeva/"
setwd(wd)


# 2b. Define path params for all runs
#----------------------------------
path_fastq = "/home/ueb/fastqc/fastqc"
path_genome = "/home/xavi/Data/Data_Genomes/hg19/hg19.fa"
path_vcfutils = "/usr/share/samtools/vcfutils.pl"
path_convert2annovar = "/home/ueb/annovar/convert2annovar.pl"
path_annotate_variation = "/home/ueb/annovar/annotate_variation.pl"
path_annotate_humandb = "/home/ueb/annovar/humandb/"
path_summarize_annovar = "/home/ueb/annovar/summarize_annovar.pl"           


# 7b. Set flags as ON (TRUE) or OFF (FALSE) for all processes from function:
#  wrapper.sequential (wseq)
#----------------------------------
#####
runParam <- FALSE #######################
####
p_map.on.reference.genome       <- runParam
p_foo_wseq <- runParam # dummy param to attempt to prevent error message "Error in cut.default(i, breaks) : 'breaks' are not unique" https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14898

# Set all params inside a list, so that it's easier to send from main to functions
# wseq : for function wrapper.sequential
params_wseq <- list()
params_wseq <- list(
  p_foo_wseq = p_foo_wseq, # dummy param to attempt to prevent error message "Error in cut.default(i, breaks) : 'breaks' are not unique" https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14898
  p_map.on.reference.genome = p_map.on.reference.genome
)


# 8b. Set flags as ON (TRUE) or OFF (FALSE) for all processes from function:
#  wrapper2.parallelizable.per.sample (w2pps)
#----------------------------------
#####
runParam <- FALSE #######################
####
p_quality.control             <- runParam
#####
runParam <- FALSE  #######################
####
p_sam2bam.and.sort		        <- runParam
p_remove.pcr.dup		          <- runParam
p_index.bam.file		          <- runParam
p_stats			                  <- runParam
#####
runParam <- TRUE #######################
####
p_variant.calling		          <- runParam
p_variant.filtering		        <- runParam
p_convert2vcf4		            <- runParam
p_variant.annotation.geneb	  <- runParam
p_variant.annotation.regionb	<- runParam # skipped so far
p_variant.annotation.filterb	<- runParam
p_variant.annotation.summarize<- runParam
p_grep.variants		            <- runParam
p_visualize.variants		      <- runParam


# Set all params inside a list, so that it's easier to send from main to functions
# w2pps : for function wrapper2.parallelizable.per.sample
params_w2pps <- list()
params_w2pps <- list(
  p_quality.control               = p_quality.control,
  p_sam2bam.and.sort              = p_sam2bam.and.sort,
  p_remove.pcr.dup                = p_remove.pcr.dup,
  p_index.bam.file                = p_index.bam.file,
  p_stats                         = p_stats,
  p_variant.calling               = p_variant.calling,
  p_variant.filtering             = p_variant.filtering,
  p_convert2vcf4                  = p_convert2vcf4,
  p_variant.annotation.geneb      = p_variant.annotation.geneb,
  p_variant.annotation.regionb    = p_variant.annotation.regionb,
  p_variant.annotation.filterb    = p_variant.annotation.filterb,
  p_variant.annotation.summarize  = p_variant.annotation.summarize,
  p_grep.variants                 = p_grep.variants,
  p_visualize.variants            = p_visualize.variants
  )

# 9b. Set flags as ON (TRUE) or OFF (FALSE) for all processes from function:
#  wrapper2.parallelizable.final (w2pf)
#----------------------------------
#####
runParam <- FALSE #######################
####
p_build.html.report  <- runParam
p_foo_w2pf <- runParam # dummy param to attempt to prevent error message "Error in cut.default(i, breaks) : 'breaks' are not unique" https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14898



# 9b. Distribute final Parallelizable calculation 
# w2pf : for function wrapper2.parallelizable.final
params_w2pf <- list()
params_w2pf <- list(
  p_foo_w2pf = p_foo_w2pf, # dummy param to attempt to prevent error message "Error in cut.default(i, breaks) : 'breaks' are not unique" https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14898
  p_build.html.report             = p_build.html.report
)
