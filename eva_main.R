#!/home/ueb/repo/peeva/eva_main.R
# revision %$Revision-Id: $% - %$Date: $% by %$Commiter: $%
#
# SCRIPT: eva_main.R
# SCOPE:  Main program that performs Exom Variant Analysis. 
#         Equivalent to the procesa.R from the other UEB scripts  
# Author: Xavier de Pedro (2011-2013)
#         xavier.depedro@vhir.org
#         http://ueb.vhir.org/tools/EVA
###################################################

## Dual Licensed under the following copyright licenses:
## a) Creative Commons Attribution-ShareAlike 3.0 License : cc-by-sa 3.0
## b) GNU/LGPL : http://www.gnu.org/copyleft/lesser.html
##
##
##
## Acknowledgements: 
## * Michael Zeller, Harry Mangalam and Vinh Nguyen for http://blog.nguyenvq.com
## * Alex Sánchez Pla, for comments and feedback.
## * Aleix Ruiz de Villa, for comments and feedback.
## * Josep Lluis Mosquera, for comments and feedback.
## * Ferran Briansó Castilla, for comments and feedback.

# 0. Basic startup process (for the working direectory)
# ---------------------------------------------------------------------
# Start clean
rm(list=ls())
# Add new values, starting from the program name
program_ueb <- "eva_main.R";

# Set the working directory from either one of the two options (a and b) listed below
## a) the hardcoded way
#wd <- "/home/ueb/repo/peeva2/"
wd <- "/home/ueb/repo/peeva/"
setwd(wd)

#
## b) dynamically from the folder where the main script program_ueb is
#wd <- getwd()
#wdres <- system(paste("locate", program_ueb, "| grep", wd, sep=" "), intern=TRUE)
#wdres <- gsub(program_ueb, "", wdres, ignore.case = FALSE, perl = FALSE, fixed = TRUE)
#setwd(wdres)

# Import Params for this EVA analysis run and the working directory for the whole project 
source("eva_params.R")

# Import UEB Functions for the EVA analysis 
source("eva_analysis_functions.R")

# Import UEB Function Wrappers for the EVA analysis 
source("eva_analysis_wrappers.R")

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
#if(!require(reshape)){ install.packages("reshape") }
#library('reshape', quietly = TRUE);

#Bioconductor packages
if(!exists("biocLite")){
  source("http://bioconductor.org/biocLite.R")
}

# Command to run on Debian machines to install some of the requriements
# ---------------------------------------------------------------------
# sudo apt-get install bwa samtools picard bedtools 
#   bwa       needed for alignments
#   samtools  needed for processing alignments and variant calling 
#   picard    needed for gatk to perform some tasks
#   bedtools  optionally needed for some eventual operations with bam files
# sudo apt-get install sendemail # optionally needed for one case of the email sending fucntion.
# sudo apt-get install perl perl-suid # deprecated; formerly needed for the perl pipeline, but no more used since mid 2012

# Annovar:
## fetch, uncompress somewhere, and update the path in ueb_params.R accordingly
### wget http://bioinform.usc.edu/annovar/xmpVO9ISYx/annovar.tar.gz (Versión from May 2012)
#
# Proxy?:
#   If you are behind a proxy, you can download also manually these packages with urls like:
#   SNP for a version of hg
#     http://www.openbioinformatics.org/annovar/download/hg18_snp132.txt.gz
#     http://www.openbioinformatics.org/annovar/download/hg19_snp132.txt.gz
#     http://www.openbioinformatics.org/annovar/download/hg19_snp135.txt.gz
#     http://www.openbioinformatics.org/annovar/download/hg19_snp137.txt.gz
#     http://www.openbioinformatics.org/annovar/download/hg19_snp137NonFlagged.txt.gz 
#     ...
#   Cosmic63:
#     http://www.openbioinformatics.org/annovar/download/hg19_cosmic63.txt.gz 
#     http://www.openbioinformatics.org/annovar/download/hg19_cosmic63.txt.idx.gz
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

# snpEff:
## Fetch program and db for organism. See http://snpeff.sourceforge.net/manual.html
## Program: uncompress and leave as ~/snpEff
### wget http://sourceforge.net/projects/snpeff/files/snpEff_latest_core.zip/download # v3.1 as of Dec 2012, 10th.
## DB for organism (hg19 in this example): fetch, uncompress, and leave file as ~/snpEff/data/hg19/snpEffectPredictor.bin
### wget http://sourceforge.net/projects/snpeff/files/databases/v3_1/snpEff_v3_1_hg19.zip/download 

#
# R packages 
# Rsamtools, GenomicFeatures, TxDb.Hsapiens.UCSC.hg19.knownGene, TxDb.Hsapiens.UCSC.hg18.knownGene, org.Hs.eg.db

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
  'in.suffix', 'x', 2, "character", # "Suffi__x__ (ending) of the input file names without extension # Optional",
  'in.ext'   , 'e', 2, "character", # ".__E__xtension of the input filenames (.fastq, .fa, .sam, ...)# Optional",
  'output'   , 'o', 1, "character", # "Directory with __O_utput data files # Compulsory",
  'f_my_rs'  , 'm', 2, "character", # File my_rs.txt needed by SnpEff to filter by those rs SNP codes only (corresponding to the target genes).,
  'genver'   , 'g', 1, "character", #, "__G__enome version used. E.g.: hg18, hg19, even if only hg19 is supported as of Jan 2013",
  'se_db_rg' , 'G', 2, "character", #, "__S__npEffect reference __G__enome version used. GRCh37.66 as of March 2013",
  'index'    , 'n', 0, "logical"  , # "i__N__dex the reference genome # Optional",
  'bwa'      , 'w', 1, "integer"  , #, "b__W__a algorythm used. See http://bio-bwa.sourceforge.net/bwa.shtml ".
  'only.m.r' , 'y', 1, "character", # "__O__nly__ __M__apped reads used in bam files # Optional",
  'st.vf.Q'  , 'Q', 2, "integer"  , #, "__S__am__T__ools vcftools.pl __v__ariant__F__ilter Q param: Minimum base quality for a base to be considered: 10 (same for small testing sets). 10 (90% accuracy)|20 (99.9%)|30 (99.99%)|40 (99.999%)|50 (99.9999%). See Wikipedia.".
  'st.vf.d'  , 'd', 2, "integer"  , #, "__S__am__T__ools vcftools.pl __v__ariant__F__ilter d param: Minimum read depth (coverage) to call a SNP: 10-15 (1-2 for small testing sets)".
  'st.vf.a'  , 'a', 2, "integer"  , #, "__S__am__T__ools vcftools.pl __v__ariant__F__ilter a param: Minimum number of alternate bases: 2-3 (1-2 for small testing sets)".
  'st.vf.D'  , 'D', 2, "integer"  , #, "__S__am__T__ools vcftools.pl __v__ariant__F__ilter D param: Maximum read depth (coverage) to call a SNP: 10000000 (same for small testing sets)".
  'st.vf.S'  , 'S', 2, "integer"  , #, "__S__am__T__ools vcftools.pl __v__ariant__F__ilter S param: minimum SNP quality: 1000 (same for small testing sets). The smaller, the better (more precise, more quality in the SNP). High values are too permissive.".
  'filter'   , 'f', 1, "character", # "__F__ilter results for these target genes (showing all variants | ^: )# Optional",
  'filter.c' , 'F', 1, "character", # "Filter (__C__lean) results for these target genes: "g1 g2 g3 ..." # Optional",
  'tggbf'    , 'B', 0, "logical",   # "__B__ed file generation with the intervals for the target genes # Optional",
  'log'      , 'l', 1, "integer"  , # "__L__og info about the process into a log file # Optional",
  'summarize', 's', 0, "logical"  , # "__S__summarize results in a single .csv file with annotations # Optional",
  'dbsnp'    , 'N', 1, "integer"  , #, "dbS__N__P version used. E.g.: 132, 135, 137, ...".
  'keep'     , 'k', 0, "logical"  , #, "__K__eep temporal dummy files after they have been used # Optional",
  'showc'    , 'C', 0, "logical"  , #, "__S__how commands run literally # Optional",
  'cpus'     , 'c', 1, "integer"  , #, "__C__pus to use from the multicore computer or cluster # Optional".
  'parallel' , 'p', 0, "logical"  , #, "__P__arallel processing using SnowFall package # Optional"
  'label'    , 'b', 1, "character"  #, "Run la__B__el to add to output file names, Like: "test-paralel-a", or "sample with param X as Y" # Optional"
)

#Clean opt from the last run/s (spceially if running through R editors)
opt <- NULL;

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
if ( is.null(opt$input    ) ) { opt$input    = p_input   } # "dir_in_sara_207"     }
if ( is.null(opt$in.suffix) ) { opt$in.suffix= p_in.suffix} # "_sequence"    
if ( is.null(opt$in.ext   ) ) { opt$in.ext   = p_in.ext  } # ".fastq" ".fa"...   
if ( is.null(opt$output   ) ) { opt$output   = p_output	 } # "dir_out_sara_207"    }
if ( is.null(opt$f_my_rs  ) ) { opt$f_my_rs  = p_f_my_rs   } # file_my_rs.txt    }
if ( is.null(opt$genver   ) ) { opt$genver   = p_genver        }
if ( is.null(opt$se_db_rg ) ) { opt$se_db_rg = p_se_db_rg      } # GRCh37.66
if ( is.null(opt$index    ) ) { opt$index    = p_index         }
if ( is.null(opt$st.vf.Q  ) ) { opt$st.vf.Q  = p_st.vf.Q       } #
if ( is.null(opt$st.vf.d  ) ) { opt$st.vf.d  = p_st.vf.d       } #
if ( is.null(opt$st.vf.a  ) ) { opt$st.vf.a  = p_st.vf.a       } #
if ( is.null(opt$st.vf.D  ) ) { opt$st.vf.D  = p_st.vf.D       } #
if ( is.null(opt$st.vf.S  ) ) { opt$st.vf.S  = p_st.vf.S       } #
if ( is.null(opt$filter   ) ) { opt$filter   = p_filter           }
if ( is.null(opt$filter.c ) ) { opt$filter.c = p_filter.c         } # The clean version of target names: "Gene1 Gene2 Gene3 ..."
if ( is.null(opt$tggbf    ) ) { opt$tggbf    = p_tggbf            } # Logical to indicate whether the bed file with regions for the target gene needs to be
if ( is.null(opt$log) || opt$log ==1) { opt$log      = p_log        }
if ( is.null(opt$dbsnp    ) ) { opt$dbsnp    = p_dbsnp              }
if ( is.null(opt$summarize) ) { opt$summarize= p_summarize          }
if ( is.null(opt$snpeff.of) ) { opt$snpeff.of= p_snpeff.of          }
if ( is.null(opt$keep     ) ) { opt$keep     = p_keep         } # Enable if run through editor and you want to keep temp files
if ( is.null(opt$showc    ) ) { opt$showc    = p_showc        } # Enable to show the exact commands run in he command line
if ( is.null(opt$cpus     ) ) { opt$cpus     = p_cpus             }
if ( is.null(opt$parallel ) ) { opt$parallel = p_parallel        }
if ( is.null(opt$label    ) ) { opt$label    = p_label	} # "sara207_4s4cpu"        } # Run Label for output filenames
if ( is.null(opt$bwa      ) ) { opt$bwa      = p_bwa          } # 1: bwa aln (short reads, low errors, allowing paired end also); 2= bwa bwasw (longer reads, single end only) # Algorythm for mapping with bwa
if ( is.null(opt$only.m.r ) ) { opt$only.m.r = p_only.m.r     } # 0/n: no, use mapped and unmapped reads; 1/y: yes, use only mapped reads; -1/u: the opposite, use only unmapped reads


# 0d. Check requirements for the libraries dependent on params added by the user in the program run (such as genome version hg18 or hg19, etc)
# ---------------------------------------------------------------------
# Check for the TxDb.Hsapiens.UCSC.hg19.knownGene or TxDb.Hsapiens.UCSC.hg18.knownGene dynamically 
# based on the contents of param opt$genver, where hg19 or hg18 is set. 
# As of January 2013, only hg19 is supported in all functions of this pipeline (use hg18 at your own risk for local partial tests, if needed)
if( (opt$genver =="hg18" || opt$genver =="hg19") && !require(paste("TxDb.Hsapiens.UCSC.", opt$genver,".knownGene", sep=""), character.only = TRUE)) {
  biocLite(paste("TxDb.Hsapiens.UCSC.", opt$genver,".knownGene", sep="")) }
if( (opt$genver =="hg18" || opt$genver =="hg19") ) {
  library(paste("TxDb.Hsapiens.UCSC.", opt$genver,".knownGene", sep=""),
        quietly = TRUE,  character.only = TRUE)
}

if( (opt$genver =="rn4" || opt$genver =="rn5") && 
      !require(paste("TxDb.Rnorvegicus.UCSC.", opt$genver,".ensGene", sep=""), character.only = TRUE)) {
  biocLite(paste("TxDb.Rnorvegicus.UCSC.", opt$genver,".ensGene", sep="")) }
if( (opt$genver =="rn4" || opt$genver =="rn5") ) {
  library(paste("TxDb.Rnorvegicus.UCSC.", opt$genver,".ensGene", sep=""),
          quietly = TRUE,  character.only = TRUE)
}

#biocLite("BSgenome.Rnorvegicus.UCSC.rn4")
#biocLite("TxDb.Rnorvegicus.UCSC.rn4.ensGene")

##############################################################

# Set overall timer to zero
start_all <- Sys.time()

# 1. Initialisation of snowfall.
#----------------------------------
# (if used with sfCluster, just call sfInit())
sfInit(parallel=opt$parallel, cpus=opt$cpus, useRscript=TRUE) # Amb parallel=TRUE s'envien les feines als nodes fills

#useRscript : Change startup behavior (snow>0.3 needed):
# use shell scripts or R-script for startup 
# (R-scripts beeing the new variant, but not working with sfCluster).

# If we set "parallel=FALSE" there will be no parallel processes (all will be run in serial) 
# and error messages will be shown normally in the command line

# With the following line we should be able to DEBUG: 
# * The list of functions called will be shown until the point where the program stops
# * That list will be numbered, so that if you set/type the number you will be able to go to the process 
#   that you are interested in, and re-follow the process to see what went wrong

#options(error = recover)

##############################################################

# 2. Get the list of files to be processed and define params list for the whole run 

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
	filename_list = paste(opt$output, "/", "log.",startdate, ".", opt$label, ".input_fastq_pe.txt", sep="")
	# Get the list of files in "input" directory through a system call to "ls *" and save the result to a file on disk
	system(paste("ls ",abs_path_to_input_files,"*", p_in.suffix, p_in.ext, " > ", filename_list, sep=""), TRUE)
	# Add a lock file to indicate that paired end sample data is to be processed. This lock file will be removed only after all couples of files from samples have been merged into one file per sample
	system(paste("touch ", filename_list, ".lock", sep=""), TRUE)
} else {
	# When input files contained single end short reads, or long reads, the definitive file list will be created here in this step already 
	filename_list = paste(opt$output, "/", "log.",startdate, ".", opt$label, ".fastq_input_list.txt", sep="")
	# Get the list of files in "input" directory through a system call to "ls *" and save the result to a file on disk
	system(paste("ls ",abs_path_to_input_files,"*", params$opt$in.ext," > ", filename_list, sep=""), TRUE)
}

# Read the file with the list of files to be processed
file_list <- read.table(filename_list, sep="")

# Count the number of source files
n_files = length(file_list[[1]])

# remove the directory prefix from the names as well as the ending .fastq
# through gsub, alternatively
file_list <- gsub(abs_path_to_input_files,"", file_list[[1]])
file_list <- gsub(opt$in.ext,"", file_list)

##############################################################

# 2 Define params list for the whole run (for all samples)
#---------------------------------------------------------
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
               p_in.suffix = opt$in.suffix,
               p_in.ext    = opt$in.ext,
               directory_out = opt$output,
               log = opt$log,
               log.folder = opt$output, # "logs",
               p_from = p_from,
               p_to = p_to,
               p_subject  = p_subject,
               p_body = p_body,
               p_smtp = p_smtp,
               p_label = p_label,
               p_desc = p_desc,
               p_convert.file.list.pe1 = p_convert.file.list.pe1,
               p_convert.file.list.pe2 = p_convert.file.list.pe2,
               p_genedata = p_genedata,
               path_fastq = path_fastq,
               path_genome = path_genome,
               path_vcfutils = path_vcfutils,
               path_convert2annovar = path_convert2annovar,
               path_annotate_variation = path_annotate_variation,
               path_annotate_humandb = path_annotate_humandb,
               path_summarize_annovar = path_summarize_annovar,
               path_snpEff = path_snpEff,
               path_gatk = path_gatk,
               path_gatk_key = path_gatk_key,
               path_dbSNP = path_dbSNP,
               path_exon_capture_file = path_exon_capture_file
)

##############################################################

# 3. Create some files to log some data about the run
#----------------------------------

routlogfile <- paste("log.", startdate, ".", opt$label, ".run.txt", sep="") # Output file with info from SnowFall (when used), params for the run and system info
abs_routlogfile <- paste(opt$output, "/", routlogfile, sep="")
if (file.exists(abs_routlogfile)) {
  system(paste("rm ", abs_routlogfile, sep=""))
}
# If it does not exist from a previous run in the same day, create it
system(paste("touch ", abs_routlogfile, sep=""))

# First write the output from SnowFall (because it's not appending content but creating the file from scratch)
sink(abs_routlogfile, split=TRUE)

# If loging run info, create the header of the file
if (params$log) { 
  w.routlogfile.header(abs_routlogfile)
}

##############################################################

# 4. Exporting needed data and loading required
#----------------------------------
# # packages on workers.
# sfExport("MyDataset")
# # Install dependency of sfClusterSetupRNG() if not yet installed
# if(!require(cmprsk)){ install.packages("cmprsk") }
# sfLibrary(cmprsk) 
sfExport( "params", 
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
          "abs_routlogfile", 
          "routlogfile",
          "mail.send",
          "check2clean",
          "check2showcommand",
          "fun.quality.control",
          "fun.index.reference.genome",
          "fun.map.on.reference.genome",
          "fun.convert.file.list.pe",
          "fun.bowtie2sam",
          "fun.sam2bam.and.sort",
	        "fun.remove.pcr.dup",
          "fun.gatk.sortbyref",
          "fun.gatk.local.realign.step1",
          "fun.gatk.local.realign.step2",
          "fun.gatk.local.realign.step3",
          "fun.index.bam.file",
	        "fun.stats",
          "fun.addleading0.ids",
          "fun.splitAnnot",
          "fun.snpeff.count.reads",
          "fun.genedata",
          "fun.variant.calling",
	        "fun.variant.filtering",
          "fun.gatk.combine.vcfs",
	        "fun.convert2vcf4",
	        "fun.variant.annotation.geneb",
	        "fun.variant.annotation.regionb",
	        "fun.variant.annotation.filterb",
	        "fun.variant.annotation.summarize",
	        "fun.grep.variants",
	        "fun.visualize.variants",
          "fun.tgenes.generate.bed.file",
          "fun.variant.fii.pre.snpeff",
          "fun.variant.filter.pre.snpeff",
          "fun.variant.dbsnp.pre.snpeff",
          "fun.grep.pre.snpeff.report",
          "fun.variant.eff.report",
          "fun.grep.post.snpeff.report",
          "fun.build.html.report"
          ) # functions can be passed also to workers from master

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
    
    step <- fun.index.reference.genome(step.my  = step, abs_routlogfile)
    duration <- Sys.time()-start;
    cat("\n(Chunk 6a) Relative duration since last step: ")
    print(duration)
    cat("\n")
  } else {
    step <- data.frame(0, 0)
    colnames(step) <- c("n","tmp")
  }

# Generate the bed file (with sequence intervals) associated with the filters for target genes
if ( length(opt$filter.c) > 0 && opt$tggbf) { 
  # Next Step
  step$tmp <- step$tmp + 1
  start <- Sys.time(); 
  # opt$filter.c = p_filter.c -> stands for the list of target genes to process. Separated by spaces
  step <- fun.tgenes.generate.bed.file(step.my  = step, abs_routlogfile, opt$filter.c)
  duration <- Sys.time()-start;
  cat("\n(Chunk 6b) Relative duration since last step: ")
  print(duration)
  cat("\n")
}

# Generate the genedata file (with exon data) associated with the filters for target genes
if ( length(opt$filter.c) > 0 && params$p_genedata) { 
  # Next Step
  step$tmp <- step$tmp + 1
  start <- Sys.time(); 
  step <- fun.genedata(abs_routlogfile, step.my  = step)
  duration <- Sys.time()-start;
  cat("\n(Chunk 6c) Relative duration since last step: ")
  print(duration)
  cat("\n")
}
##############################################################

# 7. Run Sequential processes for the each one of the samples
#----------------------------------
# Call the wrapper function to do the Job in child processes
#start <- Sys.time(); result <- sfLapply(1:length(file_list), function(file2process) wrapper(datastep, abs_path_to_input_files, scriptparams)) ; Sys.time()-start
start2 <- Sys.time(); result <- lapply(1:length(params$file_list), wrapper.sequential) ; duration <- Sys.time()-start2;
#start2 <- Sys.time(); for (ii in 1:length(file_list)) wrapper.sequential(ii, params); duration <- Sys.time()-start2;
cat("\n(Chunk 7) Relative duration since last step: ")
print(duration)
cat("\n")
# Result is always in list form.
unlist(result)


# If paired end data, and the user has not requested to merge the 2 sai files into one merged12.sam file (in mapping genome, sequentially or in parallel)
# and the user explicitly requested to have the list of files to process converted to the merged12.sam file ones, then do it here.
# Otherwise, the list will be converted just after the merged12.sam files have been created
#if ( (params$opt$bwa == 2) && (!params_wseq$p_map.on.reference.genome.sequential.mt && !params_wseq$p_map.on.reference.genome.parallel)
#     && params_w2pps$p_convert.file.list.pe) {
  if (params$opt$bwa == 2 && params_w2pps$p_convert.file.list.pe1) {
  # Next Step
  print_mes(paste("\n ### 1st call to fun.convert.file.list.pe ###\n\n", sep=""), routlogfile);
  list.collected <- fun.convert.file.list.pe(file2process.my2  = routlogfile,
                                             step.my  = step)
  step.my           <- list.collected[[1]]
  params$file_list  <- list.collected[[2]]
  params$n_files    <- list.collected[[3]]
  file_list  <- list.collected[[2]]
  n_files    <- list.collected[[3]]
}

##############################################################

# 8. Distribute Parallelizable calculation per sample
#----------------------------------
# Call the wrapper function to do the Job in child processes
#start <- Sys.time(); result <- sfLapply(1:length(file_list), function(file2process) wrapper(datastep, abs_path_to_input_files, scriptparams)) ; Sys.time()-start

# We changed the file_list parameter for the params$file_list parameter, so that in cases of paired-emnd mode, 
# this file list will have been rewritten to the new one at this step, and therefore, we will be able to process at this time step half the number of initial fastq files
if (length(params$file_list) > 1 ) {

  ## Using sfLapply (Parallel version of function lapply.)
  start3 <- Sys.time(); result2 <- sfLapply(1:length(params$file_list), wrapper2.parallelizable.per.sample) ; duration <- Sys.time()-start3;
  
  ## using sfClusterApplyLB instead (Load balanced version of function sfLapply)
  # which should be better, as shown in figure 2, p13 as printed (p15 in pdf) in
  # http://www.imbi.uni-freiburg.de/parallel/docs/Reisensburg2009_TutParallelComputing_Knaus_Porzelius.pdf
  #start3 <- Sys.time(); result2 <- sfClusterApplyLB(1:length(params$file_list), wrapper2.parallelizable.per.sample) ; duration <- Sys.time()-start3;
  } else {
  start3 <- Sys.time(); result2 <- lapply(1:length(params$file_list), wrapper2.parallelizable.per.sample) ; duration <- Sys.time()-start3;
}
cat("\n(Chunk 8) Relative duration since last step: "); print(duration); cat("\n")

# Result is always in list form.
unlist(result2)

##############################################################

# 9. Distribute final Parallelizable calculation 
#----------------------------------
# Call the wrapper function to do the Job in child processes

# This last part could be run in parallel, whenever we know how to split it inn chunks; 
# but it has no sense to repeat several times the same process, so disabling for the time being (Jan 2013)
#start3 <- Sys.time(); result3 <- sfLapply(1:2, wrapper2.parallelizable.final) ; duration <- Sys.time()-start3;

# Running the function normally just once. 
  start3 <- Sys.time(); result3 <- wrapper2.parallelizable.final( length(params$file_list) ) ; duration <- Sys.time()-start3;

cat("\n(Chunk 9) Relative duration since last step: "); print(duration); cat("\n")

print_mes("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", routlogfile);
print_mes("\n XXX >>>>>>>>>>>>>>>>>> End of EVA UEB pipeline <<<<<<<<<<<<<<<<<<<<<<<<<<<< XXX", routlogfile);
print_mes("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n", routlogfile);


# Result is always in list form.
unlist(result3)

##############################################################

# 10. Stop snowfall
#----------------------------------
sfStop() 


# Show overatll duration of the run
duration_all <- Sys.time()-start_all;
duration_all_msg <- paste(duration_all, attr(duration_all, "units"), sep=" ");
print_mes(paste("\nDurantion of the whole run: ", duration_all_msg, paste=""), routlogfile);

# After that, you can append extra information from the run to the same file
w.output.run("\n##################################################", 
             paste("PARAMS FROM THIS RUN: ", opt$label, sep=""),
             abs_routlogfile )
w.output.run("*** params ***", unlist(params),  abs_routlogfile )
w.output.run("*** params_wseq ***", unlist(params_wseq),  abs_routlogfile )
w.output.run("*** params_w2pps ***", unlist(params_w2pps),  abs_routlogfile )
w.output.run("*** params_w2pf ***", unlist(params_w2pf),  abs_routlogfile )
w.output.run("*** .Platform ***", unlist(.Platform), abs_routlogfile )
w.output.run("*** .Machine ***", unlist(.Machine),  abs_routlogfile )
w.output.run("*** Sys.info ***", unlist(Sys.info()),  abs_routlogfile )
w.output.run("*** sessionInfo ***", unlist(sessionInfo()),  abs_routlogfile )
w.output.run("*** R.version ***", unlist(R.version), abs_routlogfile )
w.output.run("*** Installed R Packages & versions ***", unlist(installed.packages()),  abs_routlogfile )

# Suspend writing anything else to this log file for some seconds to avoid overwritting the file
Sys.sleep(5)

# Close the R output connection to the file
sink()

##############################################################

# 11. Send email to notify everything is done (it only works when run in sequential mode, it seems)
if (p_mail.send==1) {
  cat("\nSending the email confirming the run is finished... ")
  mail.send(abs_routlogfile, routlogfile)
  cat("\nEmail sent. ")  
}

#signal success and exit. 
#q(status=0); # To be un-commented out at the very end of the development process,
              #so that the R process quits after execution

