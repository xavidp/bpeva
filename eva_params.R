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

# 0. Basic startup params
# ---------------------------------------------------------------------
startdate <- paste(format(Sys.Date(), "%y%m%d"), sep="")

p_test     = 1 # 1/0; ### Is this a test run? ###
                # 1 = test run, so that use the predefined values for a test run; 
                # 0 = normal run
if (p_test==1) {
  p_input    = "test_in2" # "../test_in2"  # "test_in"
  p_output   = "test_out2" # "../test_out2" # "test_out"
  p_label    =  ".testrun" # "test-121002" # "test-foo"        # Run Label for output filenames
  p_keep     = TRUE # Enable if run through editor and you want to keep temp files
  p_filter   = ""            
} else {
  p_input    = "../dir_in" # "../dir_in" # "test_in"   # "dir_in"     
  p_output   = "../dir_out_293" # "../dir_out_293" # "test_out"	 # "dir_out_293"
  p_label    =  ".sg293_qa_sg3sg4" # "test-121002" # ".sg293_qa_sg3sg4"   # "test-121002" ".sara207_4s4cpu"        # Run Label for output filenames
  p_keep     = TRUE # Enable if run through editor and you want to keep temp files
  p_filter   = "BRCA"            
}
p_index    = FALSE # TRUE         
p_log      = TRUE        
p_summarize= TRUE          
p_cpus     = 4             
p_parallel = TRUE #FALSE #TRUE # Do you want to allow running some parallelized processes at all? (which ones will be specified elsewhere in the code)
p_bwa      = 2          # Algorythm for mapping with bwa - http://bio-bwa.sourceforge.net/bwa.shtml
                        # 1: bwa aln      + samse  (short reads, single ends, low errors);
                        # 2: bwa aln (x2) + sampe  (short reads, paired ends, low errors);
                        # 3: bwa bwasw             (longer reads, single end only) 


# 2b. Define path params for all runs
#----------------------------------
# p_server = Choose machine where to get the paths for
# 1 for MainHead,
# 2 for B52,
p_server <- 1 # Set the server number (see codes above)

if (p_server==1) { # MainHead server
  path_fastq = "/home/ueb/fastqc/fastqc" 
  path_genome = "/home/xavi/Data/Data_Genomes/hg19/hg19.fa" 
  path_vcfutils = "/usr/share/samtools/vcfutils.pl"
  path_convert2annovar = "/home/ueb/annovar/convert2annovar.pl"
  path_annotate_variation = "/home/ueb/annovar/annotate_variation.pl"
  path_annotate_humandb = "/home/ueb/annovar/humandb/"
  path_summarize_annovar = "/home/ueb/annovar/summarize_annovar.pl"           
  } else if (p_server==2) { # B52 server
    path_fastq = "/home/ueb/software/FastQC/fastqc"
    path_genome = "/home/ueb/Data/Data_Genomes/hg19.fa" 
    path_vcfutils = "/usr/share/samtools/vcfutils.pl"
    path_convert2annovar = "/home/ueb/software/annovar/convert2annovar.pl"
    path_annotate_variation = "/home/ueb/software/annovar/annotate_variation.pl"
    path_annotate_humandb = "/home/ueb/software/annovar/humandb/"
    path_summarize_annovar = "/home/ueb/software/annovar/summarize_annovar.pl"           
  }


# 7b. Set flags as ON (TRUE) or OFF (FALSE) for all processes from function:
#  wrapper.sequential (wseq)
#----------------------------------
#####
runParam <- FALSE #######################
p_map.on.reference.genome.sequential     <- runParam # In case we run the mapping sequentially for all samples

runParam <- FALSE # !runParam ####################### The opposite to map in sequential mode
p_map.on.reference.genome.parallel       <- runParam # In case we run the mapping in parallel for n (p_cpus) samples at a time

# Set all params inside a list, so that it's easier to send from main to functions
# wseq : for function wrapper.sequential
params_wseq <- list()
params_wseq <- list(
  p_map.on.reference.genome.sequential  = p_map.on.reference.genome.sequential, 
  p_map.on.reference.genome.parallel    = p_map.on.reference.genome.parallel
)


# 8b. Set flags as ON (TRUE) or OFF (FALSE) for all processes from function:
#  wrapper2.parallelizable.per.sample (w2pps)
#----------------------------------
# p_map.on.reference.genome.parallel  is not defined here but in the previous chunk
#####
runParam <- FALSE #######################
####
p_quality.control             <- runParam
#####
runParam <- TRUE #######################
####
p_sam2bam.and.sort		        <- runParam
p_remove.pcr.dup		          <- runParam
p_index.bam.file		          <- runParam
p_stats			                  <- runParam
p_variant.calling		          <- runParam
p_variant.filtering		        <- runParam
#####
runParam <- FALSE #######################
####
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
  p_map.on.reference.genome.parallel  = p_map.on.reference.genome.parallel,
  p_quality.control                   = p_quality.control,
  p_sam2bam.and.sort                  = p_sam2bam.and.sort,
  p_remove.pcr.dup                    = p_remove.pcr.dup,
  p_index.bam.file                    = p_index.bam.file,
  p_stats                             = p_stats,
  p_variant.calling                   = p_variant.calling,
  p_variant.filtering                 = p_variant.filtering,
  p_convert2vcf4                      = p_convert2vcf4,
  p_variant.annotation.geneb          = p_variant.annotation.geneb,
  p_variant.annotation.regionb        = p_variant.annotation.regionb,
  p_variant.annotation.filterb        = p_variant.annotation.filterb,
  p_variant.annotation.summarize      = p_variant.annotation.summarize,
  p_grep.variants                     = p_grep.variants,
  p_visualize.variants                = p_visualize.variants
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
