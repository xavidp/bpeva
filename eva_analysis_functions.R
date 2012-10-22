#!/home/ueb/repo/peeva/eva_analysis_functions.R
#
# SCRIPT: eva_analysis_functions.R
# SCOPE: to be called from other scripts, such as eva_main.R
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

##########################
### FUNCTIONS
##########################

now  <- function() 
{
  # ---------------------------------------------------------------------
  paste(Sys.Date(), format(Sys.time(), " %H:%Mh %Ss - "), sep="");
  
}

##########################
### FUNCTION print_doc
###
###   Print a message in screen with timestamp and eventually in log files if requested
##########################

print_doc <- function(mess, filename.my1) 
{
  # ---------------------------------------------------------------------
  mess <- paste(now(), mess, sep="")
  cat(mess)
  if (params$log) { 
    w.output(mess, filename.my1)
  }
}

##########################
### FUNCTION print_mes
###
### 	Print a message in screen "as is" and eventually in log files if requested
##########################

print_mes <- function(mess, filename.my1) # Similar to print_doc but without adding the time stamp at the beggining.
{
  # ---------------------------------------------------------------------
  cat(mess)
  if (params$log) { 
    w.output(mess, filename.my1)
  }
}

##########################
### FUNCTION print_done
###
### 	Print a simple [DONE] message on screen and eventually in log files is requested
##########################

print_done <- function(filename.my1) 
{
  # ---------------------------------------------------------------------
  mess <- paste(now(), "\t\t\t\t\t\t\t[DONE]\n\n", sep="")
  cat(mess)
  if (params$log) { 
    w.output(mess, filename.my1)
  }
}


##########################
### FUNCTION print_error
###
### 	Print error message in screen and eventually in log files if requested
##########################

print_error <- function(mess, filename.my1) 
{
  # ---------------------------------------------------------------------
  mess <- paste(now(), "\t\t\t\t\t\t\t[ERROR]\n\n", sep="")
  cat(mess)
  if (params$log) { 
    w.output(mess, filename.my1)
  }
}

##########################
### FUNCTION check2clean
###
### 	Check if temp files should be kept or can be removed on disk
##########################

check2clean <- function(my.option, filename.my1)
{
  # ---------------------------------------------------------------------
  var <- my.option
  if ( !is.null(params$opt$keep) ) # keep temporary files if requested by the user 
  { 
    # do nothing
  } else { # clean temporary files not from this but from the previous step
    system(paste("rm  ", var, sep=""));
    print_mes(paste("\t\t\t\t\tOk. Removing temporary file ", var, "\n\n", sep=""), filename.my1);
  }
}

##########################
### FUNCTION w.output
###
### 	Write output to log files on disk
##########################

w.output <- function(mess, filename.my2)
{
  write(mess, file=paste(params$log.folder,"/log.",params$startdate, params$opt$label,".", filename.my2, ".txt", sep=""), append = TRUE, sep = "");
}

##########################
### FUNCTION show_help
##########################

show_help <- function(program_ueb.my)
{
  # ---------------------------------------------------------------------
  cat("\n===============================", program_ueb.my," HELP ===============================\n");
  cat("\nUsage: Rscript", program_ueb.my, "[arguments]\n");
  cat("\nArguments  accepted:\n");
  cat("\n   -i: directory with source .fastq files \t\t\t *** required ***\n")
  cat("\n   -o: directory to save output files \t\t\t\t *** required ***\n")
  cat("\n   -n: -index (indexing of the reference genome)\t\t    [optional]\n")
  cat("\n   -w: -bwa (bwa algorythm: 1 aln+samse, 2 aln+sampe, 3 bwasw(se) ) [optional]\n")
  cat("\n         for bwa: 2, use sufixes: *_1_sequence.fasq, *_2_sequence.fastq       \n")
  cat("\n   -s: summarize results with annotations in a single .csv file     [optional]\n")
  cat("\n   -f: filter results for these target genes \t\t\t    [optional]")
  cat("\n     with this syntax for one gene:")
  cat("\n       -f BRCA1 ")
  cat("\n     or for more than one gene or string to filter results:")
  cat("\n       -f 'BRCA1|BRCA2|unknown' \n")
  cat("\n   -k: keep temporary files after they have been used \t\t    [optional]\n")
  cat("\n   -p: parallel processing \t\t\t\t\t    [optional]\n")
  cat("\n   -c: number of cpus to use (if parallel). Default: 7\t\t    [optional]\n")
  cat("\n   -h: show this help text \t\t\t\t\t    [optional]\n")
  cat("\n   -l: log process output. Default: 1 (TRUE)\t\t\t    [optional]\n")
  cat("\n Example1: Rscript", program_ueb.my,"-i ./dir_in -o ./dir_out -s -f 'BRCA1|BRCA2|unknown'")
  cat("\n Example2: Rscript", program_ueb.my,"-i ./test_in -o ./test_out -s -k > ./logs/log_both.txt 2>&1")
  cat("\n Example3: Rscript", program_ueb.my,"-i ./test_in -o ./test_out -s -k | tee /dev/tty ./logs/log_both.txt\n");
  cat("\n##############################################################################\n");
}

##########################
### FUNCTION fun.quality.control
##########################

fun.quality.control <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Quality Control and Preprocessing: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
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

fun.index.reference.genome <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Map against reference genome: Index the reference genome (if needed)\n", sep=""), file2process.my2);
  if ((params$opt$index) & (step.my$n == 1)) { # case to index the reference genome (time consuming, do only when really needed as requested)
    # Index the reference genome, if requested with argument -n and only for the first file if more than one sample to process
    command00 <- "bwa index"; # next command
    options00 <- paste("  -a bwtsw", params$path_genome, sep="");
  } else	{ # skip the indexing of the reference genome
    command00 <- "echo '  ...skipped...'"; # next command.
    options00 <- "";
  }
  command = paste(command00, " ", options00, sep="");
  # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
  start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
  print_done(file2process.my2);
  # Show the duration of this subprocess
  cat("\nRelative duration since last step: "); print(duration);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}
#==============================================================


##########################
### FUNCTION fun.map.on.reference.genome
##########################

fun.map.on.reference.genome <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Map against reference genome: do the mapping with: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  ## TODO: 
  ## Add a param to allow the user to choose the aligner with 'bwa aln' (<200bp and <3% error rate, allowing paired end reads)
  ##    or 'bwa bwasw' (for longer reads and/or with higher errors).
  # 1: bwa aln      + samse  (short reads, single ends, low errors);
  # 2: bwa aln (x2) + sampe  (short reads, paired ends, low errors);
  #        for bwa: 2, use sufixes: *_1_sequence.fasq, *_2_sequence.fastq
  # 3: bwa bwasw             (longer reads, single end only) 
  ##
  ## From: http://bio-bwa.sourceforge.net/bwa.shtml
  ## BWA is a fast light-weighted tool that aligns relatively short sequences (queries) to a sequence database (targe), such as the human reference genome. 
  ## It implements two different algorithms, both based on Burrows-Wheeler Transform (BWT). 
  ## The first algorithm is designed for short queries up to ~200bp with low error rate (<3%). 
  ##   It does gapped global alignment w.r.t. queries, supports paired-end reads, and is one of the fastest short read alignment algorithms to date while also visiting suboptimal hits. 
  ## The second algorithm, BWA-SW, is designed for long reads with more errors. It performs heuristic Smith-Waterman-like alignment to find high-scoring local hits (and thus chimera). 
  ##   On low-error short queries, BWA-SW is slower and less accurate than the first algorithm, but on long queries, it is better
  file_in = paste(params$directory_in, "/", file2process.my2, ".fastq", sep="");
  
  if (params$opt$bwa == 1) # case to use algorythm 1 from bwa: aln + samse  (short reads, single ends, low errors);
  {
    # Example - 1s part
    #bwa aln database.fasta short_read.fastq > aln_sa.sai
    file_out = paste(params$directory_out, "/", file2process.my2, ".sai", sep="");
    command00 = "bwa aln"; # next command.
    options00 = paste(params$path_genome, " ", file_in, " > ", file_out, sep="");
    command = paste(command00, " ", options00, sep="");
    # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
    start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
    # Show the duration of this subprocess
    cat("\nRelative duration since last step: "); print(duration);
    
    # Example - 2nd part
    #bwa samse database.fasta aln_sa.sai short_read.fastq > aln.sam
    file_in_sai = paste(params$directory_out, "/", file2process.my2, ".sai", sep="");
    file_out = paste(params$directory_out, "/", file2process.my2, ".sam", sep="");
    command00 = "bwa samse"; # next command.
    options00 = paste(params$path_genome, " ", file_in_sai, " ", file_in, " > ", file_out, sep="");
    command = paste(command00, " ", options00, sep="");
    # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
    start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
    # Show the duration of this subprocess
    cat("\nRelative duration since last step: "); print(duration);
  }

  if (params$opt$bwa == 2) # case to use algorythm 2 from bwa: aln (x2) + sampe  (short reads, paired ends, low errors);
  {
    # Example - 1s part
    #bwa aln database.fasta short_read1.fastq > aln_sa1.sai
    #bwa aln database.fasta short_read2.fastq > aln_sa2.sai
    file_out = paste(params$directory_out, "/", file2process.my2, ".sai", sep="");
    command00 = "bwa aln"; # next command.
    options00 = paste(params$path_genome, " ", file_in, " > ", file_out, sep="");
    command = paste(command00, " ", options00, sep="");
    # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
    start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
    # Show the duration of this subprocess
    cat("\nRelative duration since last step: "); print(duration);
    
#    file2process.my2 <- "s_4_m11_146b_1_sequence" #     file2process.my2
#    file2process.my2 <- "s_4_m11_146b_2_sequence" #     file2process.my2
#    tmp2 <- gsub("_sequence", "@", tmp)
#    length(grep("_1_sequence", file2process.my2)) == 1 # returns TRUE when matched the string
    if ( length(grep("_2_sequence", file2process.my2)) == 1 ) # returns TRUE when matched the string (2nd sample of the pair) 
    {
      # Provide temporal shorter base names for the 2 files of the paired-end set (remove "_1_sequence" and "_2_sequence")
      f2pbase <- gsub("_2_sequence", "", file2process.my2)

      # Example - 2nd part
      #bwa sampe database.fasta aln_sa.sai short_read.fastq > aln.sam
      file_in_sai1 = paste(params$directory_out, "/", f2pbase, "_1_sequence", ".sai", sep="");
      file_in_sai2 = paste(params$directory_out, "/", f2pbase, "_2_sequence", ".sai", sep="");
      file_in_fq1 = paste(params$directory_in, "/", f2pbase, "_1_sequence", ".fastq", sep="");
      file_in_fq2 = paste(params$directory_in, "/", f2pbase, "_2_sequence", ".fastq", sep="");
      file_out = paste(params$directory_out, "/", file2process.my2, ".sam", sep="");
      command00 = "bwa sampe"; # next command.
      options00 = paste(params$path_genome, " ", file_in_sai1, " ", file_in_sai2, " ", file_in_fq1, " ", file_in_fq2, " > ", file_out, sep="");
      command = paste(command00, " ", options00, sep="");
      # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
      start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
      # Show the duration of this subprocess
      cat("\nRelative duration since last step: "); print(duration);
    }

  }
  
  if (params$opt$bwa == 3) { # case to use algorythm 3 from bwa: bwasw (longer reads, single end only)
    file_out = paste(params$directory_out, "/", file2process.my2, ".sam", sep="");
    command00 = "bwa bwasw"; # next command.
    options00 = paste(params$path_genome, " ", file_in, " > ", file_out, sep="");
    command = paste(command00, " ", options00, sep="");
    # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
    start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
    # Show the duration of this subprocess
    cat("\nRelative duration since last step: "); print(duration);  
  }
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return both the step number, and the filename out that will be theinput for the next step
}



##########################
### FUNCTION fun.sam2bam.and.sort
##########################

fun.sam2bam.and.sort <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Convert sam to bam and sort it: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam", sep="");
  file_out = paste(file_in, ".sorted", sep="");
  command00 = "samtools"; # next command.
  options00 = paste(" view -bS ", file_in, " | ", command00, " sort - ", file_out, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  check2clean(file_in, file2process.my2);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.remove.pcr.dup
###
### 	Remove possible PCR duplicates
##########################

fun.remove.pcr.dup <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Remove possible PCR duplicates: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.bam", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam", sep="");
  command00 = "samtools"; # next command.
  options00 = paste("rmdup -s ", file_in, " ", file_out, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  check2clean(file_in, file2process.my2);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

##########################
### FUNCTION fun.index.bam.file
###
### 	Index the bam file
##########################

fun.index.bam.file <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Index the bam file: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam", sep="");
  #  file_out = paste(file_in, ".foo", sep="");
  command00 = "samtools"; # next command.
  options00 = paste(" index ", file_in, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  # Don't check for check2clean("$file_in") since we still need it to do some stats upon it
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

##########################
### FUNCTION fun.stats
###
### 	Get some stats using the samtools [optional]
##########################

fun.stats <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Get some stats using the samtools [optional]: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam", sep="");
  #  file_out = paste(file_in, ".foo", sep="");
  command00 = "samtools"; # next command.
  options00 = paste(" flagstat ", file_in, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  # Don't check for check2clean("$file_in") since we still need it for the variant calling
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.variant.calling
###
### 	Variant calling
##########################

fun.variant.calling <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Variant calling: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam", sep="");
  file_out = paste(file_in, ".samtools.var.raw.vcf", sep="");
  command00 = "samtools"; # next command.
  options00 = paste(" mpileup -uf ", params$path_genome, " ", file_in, " | bcftools view -vcg - >  ", file_out, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  check2clean(file_in, file2process.my2);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.variant.filtering
###
### 	Variant Filtering
##########################

fun.variant.filtering <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Variant Filtering: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam.samtools.var.raw.vcf", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam.samtools.var.filtered.vcf", sep="");
  command00 = params$path_vcfutils ; # next command.
  options00 = paste(" varFilter -Q 10 -d 15 -a 5 ", file_in, " > ", file_out, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  check2clean(file_in, file2process.my2);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.convert2vcf4
###
### 	Convert files to Annnovar vcf4 format
##########################

fun.convert2vcf4 <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Convert files to Annnovar vcf4 format: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in =  paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam.samtools.var.filtered.vcf", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".f.vcf4", sep=""); # shorten the name a bit
  command00 = params$path_convert2annovar; # next command.
  options00 = paste(" ", file_in, " -format vcf4 > ", file_out, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  # check2clean(file_in, file2process.my2); #  # Commented out so that samtools standard .vcf files (and not only the converted to .vcf4 - .vcf.annovar - format) are also always kept.
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

##########################
### INTRO to FUNCTIONs fun.variant.annotation.*
###
## Alternative annotation procedure 1: SeattleSeq Website
## See: http://snp.gs.washington.edu/SeattleSeqAnnotation134/

## Alternative annotation procedure 2: VariantAnnotation Bioconductor package for R 2.14
## See: http://www.bioconductor.org/packages/devel/bioc/html/VariantAnnotation.html

## We will be mainly annotating by (1) gene, (2) region or (3) filtering specific nucleotide changes (the 3 methos of annovar)
## See: http://www.openbioinformatics.org/annovar/
##########################

##########################
### FUNCTION fun.variant.annotation.geneb
###
### 	Variant Annotation with Annovar: Gene-based
###   annotate variants by functional consequences on genes
###     Identify whether SNPs or CNVs cause protein coding changes and the amino acids that are affected. Users can flexibly use RefSeq genes, UCSC genes, ENSEMBL genes, GENCODE genes, or many other gene definition systems.
###     http://www.openbioinformatics.org/annovar/annovar_gene.html
##########################

fun.variant.annotation.geneb <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Variant Annotation (gene-based): ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in  = paste(params$directory_out, "/", file2process.my2, ".f.vcf4", sep=""); 
  file_out = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.exonic_variant_function_gene", sep=""); 
  command00 = "perl"; # next command.
  options00 = paste(" ", params$path_annotate_variation, " -geneanno --buildver hg19 ", file_in, " ", params$path_annotate_humandb, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  # We don't do check2clean here  either since we will still use the .vcf.annovar file in the next steps
  print_done(file2process.my2);
  
  # a mà la instrucció al mainhead és:
  # perl /home/ueb/annovar/annotate_variation.pl -geneanno --buildver hg19 /home/xavi/repo/peeva/dir_out/Gutierrez_B_Sure.sequence_m50.sam.sorted.noDup.bam.samtools.var.f.vcf4 /home/ueb/annovar/humandb/
  # perl /home/ueb/annovar/annotate_variation.pl -geneanno --buildver hg19 /home/ueb/estudis/ngs/2011-08-SGutierrez-VHIO-207/111224_peeva_dir_out/Gutierrez_B_Sure.sequence.sam.sorted.noDup.bam.samtools.var.f.vcf4 /home/ueb/annovar/humandb/
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.variant.annotation.regionb
###
### 	Variant Annotation with Annovar: Region-based
###   annotate variants by functional consequences on genes
### 	  Identify variants in specific genomic regions, for example, conserved regions among 44 species, predicted transcription factor binding sites, segmental duplication regions, GWAS hits, database of genomic variants, DNAse I hypersensitivity sites, ENCODE H3K4Me1/H3K4Me3/H3K27Ac/CTCF sites, ChIP-Seq peaks, RNA-Seq peaks, or many other annotations on genomic intervals
###     Skipped so far (May 2012)
###     http://www.openbioinformatics.org/annovar/annovar_region.html
##########################

fun.variant.annotation.regionb <- function(file2process.my2, step.my) {
  # skipped so far
}


##########################
### FUNCTION fun.variant.annotation.filterb
###
### 	Variant Annotation with Annovar: Filter-based
###   filter variants based on a position list
###     Identify variants that are reported in dbSNP, or identify the subset of common SNPs (MAF>1%) in the 1000 Genome Project, or identify subset of non-synonymous SNPs with SIFT score>0.05, or many other annotations on specific mutations
###     http://www.openbioinformatics.org/annovar/annovar_filter.html
##########################

fun.variant.annotation.filterb <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Variant Annotation (filter based): ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".f.vcf4", sep=""); 
  # The output file is hardcoded by annotate_variation, and will end up with this type of suffix: .hg19_snp132_filtered
  #  file_out_dropped = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.hg19_snp132_dropped", sep=""); 
  #  file_out_passed = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.hg19_snp132_filtered", sep=""); 
  command00 = "perl"; # next command.
  options00 = paste(" ", params$path_annotate_variation, " -filter --buildver hg19 -dbtype snp132 ", file_in, " ", params$path_annotate_humandb, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  # We don't do check2clean here  either since we will still use the .vcf.annovar file in the next steps (summarizing annotations and filtering)
  print_done(file2process.my2);
  
  # a mà la instrucció al mainhead és:
  # perl /home/ueb/annovar/annotate_variation.pl -filter --buildver hg19 -dbtype snp132 /home/xavi/repo/peeva/dir_out/vhir_sample_a_sure_1e6.sam.sorted.noDup.bam.samtools.var.f.vcf4 /home/ueb/annovar/humandb/
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.variant.annotation.summarize
###
### 	Variant Annotation with Annovar: summarize_annovar.pl
###     Given a list of variants from whole-exome or whole-genome sequencing, it will generate an Excel-compatible file with gene annotation, amino acid change annotation, SIFT scores, PolyPhen scores, LRT scores, MutationTaster scores, PhyloP conservation scores, GERP++ conservation scores, dbSNP identifiers, 1000 Genomes Project allele frequencies, NHLBI-ESP 5400 exome project allele frequencies and other information.
###     http://www.openbioinformatics.org/annovar/annovar_accessary.html#excel
##########################

fun.variant.annotation.summarize <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Variant Annotation (summarize annotations in .csv): ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  # TODO: 	if ($options{s}) # case to summarize annotation results in a single .csv file ...
  file_in = paste(params$directory_out, "/", file2process.my2, ".f.vcf4", sep=""); 
  file_out = paste(file_in, ".sum", sep=""); # summarize_annovar.pl adds the extension .exome_summary.csv, and many other partial .csv files (hardcoded in annovar). 
  command00 = "perl"; # next command.
  options00 = paste(" ", params$path_summarize_annovar, " --buildver hg19 --verdbsnp 132 ", file_in, " ", params$path_annotate_humandb, " --outfile ", file_out, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  # # We don't do check2clean here  either since we will still use the .vcf.annovar file in the next steps (filtering)
  print_done(file2process.my2);
  
  # a mà la instrucció al mainhead és:
  # perl /home/ueb/annovar/summarize_annovar.pl --buildver hg19  --verdbsnp 132 /home/xavi/repo/peeva/dir_out/vhir_sample_a_sure_1e6.sam.sorted.noDup.bam.samtools.var.f.vcf4 /home/ueb/annovar/humandb/ --outfile sample_a_sure_sum
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.grep.variants
###
### 	Select variants for the target genes based on grep calls

### XXX to be revised
##########################

fun.grep.variants <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Select variants for the target genes based on grep calls: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  if (!is.null(params$opt$filter) && (params$opt$filter != "")) {
    file_in  = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.hg19_snp132_filtered", sep=""); 
    file_out = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.hsf.results.", params$startdate, params$opt$label, ".txt", sep="");
    command00 = "grep"; # next command.
    options00 = paste(" '", params$opt$filter,"' ", file_in, " > ", file_out, sep="");
    # Remember that the values in the previous opt$filter variable needs to be like: 'BRCA1\|BRCA2' 
    # in order to end up performing a command like:
    # grep 'BRCA1\|BRCA2' dir_out/Gutierrez_A_*.exonic* 
    if (params$opt$summarize) # case to have a file with summarized annotations to search also for specific target genes 
    {
      file_in = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.exome_summary.csv", sep=""); # summarize_annovar.pl adds the extension .exome_summary.csv (hardcoded in annovar).
      file_out = paste(file_in, ".grep.results.csv", sep="");  
      command01 = "head -1";
      command02 = command00;
      options01 = paste(" ", file_in, " > ", file_out, sep="");
      options02 = paste(" '", params$opt$filter,"' ", file_in, " >> ", file_out, sep="");
    }
  } else { # skip the searching for specific target genes 
    command00 = "echo '  ...skipped...'"; # next command.
    options00 = "";
  }
  command = paste(command00, " ", options00, sep="");
  system(command);
  if (!is.null(params$opt$filter) && (params$opt$filter != "") && !is.null(params$opt$summarize) ) # case to have a file with summarized annotations to search also for specific target genes 
  {
    command = paste(command01, " ", options01, sep="");
    system(command);
    command = paste(command02, " ", options02, sep="");
    system(command);
  }
  
  #check2clean(file_in, file2process.my2);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

##########################
### FUNCTION fun.build.html.report
###
###   Build html report
##########################

fun.build.html.report <- function(file2process.my2, step.my) {
  
  # Adding a first column with sample file name, to merge all results from all samples in one same spreadsheet and/or html report
  # Using awk or similar.
  # TODO XXX
  
  # Adding some html link with url like this one below for the column related to dbSnp in our results like rsNNN in csv files.
  #http://www.ncbi.nlm.nih.gov/snp/?term=rsNNN
  # TODO XXX
  
}

##########################
### FUNCTION fun.visualize.variants
###
### 	Visualization of Variants
##########################

fun.visualize.variants <- function(file2process.my2, step.my) {
  # ignored so far
  # To use IGV (LGPL'd) in batch mode, as indicated in their website.
  # http://www.broadinstitute.org/igv/
  
  # Running IGV with a batch file
  # http://www.broadinstitute.org/igv/batch
  # As of version 1.5, a user can load a text file to execute a series of sequential tasks by using File>Run Batch Script. The user loads a TXT file that contains a list of commands, one per line, that will be run by IGV.   Arguments are delimited by spaces (NOTE: not tabs).  Lines beginning with # or // are are skipped. See Controlling IGV through a Port for accepted commands.
  
  # Controlling IGV through a Port
  # http://www.broadinstitute.org/igv/PortCommands
  # IGV can optionally listen for http requests over a port. This option is turned off by default but can be enabled from the Advanced tab of the Preferences window. 
  # Note:  IGV will write a response back to the port socket upon completion of each command.  It is good practice to read this response before sending the next command.   Failure to do so can overflow the socket buffer and cause IGV to freeze.   See the example below for the recommended pattern.
  
  # TODO XXX
}



##########################
### FUNCTION wrapper.sequential
###
###   7a. Wrapper functions, One (here) is run always sequentially. The other one (further below), parallelized.
##########################
#----------------------------------
wrapper.sequential <- function(datastep.my) {
  
  # -----------------------------
  # Define which processes to run (in later stage, this will be in an external R file sourced here)
  # names of control process are like functions but without the "fun." prefix.
  # -----------------------------
  map.on.reference.genome.sequential               <- params_wseq$p_map.on.reference.genome.sequential
  # -----------------------------
  
  
  # Get the file name to process now
  file2process.my1 <- params$file_list[datastep.my]
  
  # Re-set working directory while in child worker, just in case
  setwd(params$wd)
  step <- data.frame(datastep.my, 0)
  colnames(step) <- c("n","tmp")

  # Re-set the log file, if it exists already and log is requested. Create it.
  if (params$log) { 
    write(paste("\n", sep=""), file=paste(params$log.folder,"/log.", params$startdate, params$opt$label, ".", file2process.my1, ".txt", sep=""), append = FALSE, sep = "");
  }
  
  # Re-set the log file, if it exists already and log is requested
  if (params$log && map.on.reference.genome.sequential) { 
    write(paste("\n", sep=""), file=paste(params$log.folder,"/log.", params$startdate, params$opt$label, ".", file2process.my1, ".txt", sep=""), append = TRUE, sep = "");
    print_mes("\n################################################################################\n", file2process.my1);
    print_mes(paste("			NEW RUN - A. SEQUENTIAL. ", params$n_files, " files; Current: *** ", file2process.my1, " ***\n", sep=""), file2process.my1);
    print_mes("################################################################################\n\n", file2process.my1);
    
    }
  
  print_doc(paste("### Start processing file #", datastep.my, " (", file2process.my1, ") ... ###\n", sep=""), file2process.my1);
  
  
  #--- Sequential Pipeline steps into wrapper.sequential function ###----------------------------------------
  
  if (map.on.reference.genome.sequential) { 
    # Next Step
    step <- fun.map.on.reference.genome(file2process.my2  = file2process.my1,
                                        step.my  = step)
  }
  
  
}

##############################################################

##########################
### FUNCTION wrapper2.parallelizable.per.sample
###
###   3b. Wrapper functions, to be run per input sample file, Can be parallelized.
##########################
wrapper2.parallelizable.per.sample <- function(datastep.my2) {
  # -----------------------------
  # Define which processes to run (in later stage, this will be in an external R file sourced here)
  # names of control process are like functions but without the "fun." prefix.
  # -----------------------------
  
  map.on.reference.genome.parallel  <- params_w2pps$p_map.on.reference.genome.parallel
  quality.control   	              <- params_w2pps$p_quality.control
  sam2bam.and.sort	 	              <- params_w2pps$p_sam2bam.and.sort
  remove.pcr.dup		                <- params_w2pps$p_remove.pcr.dup
  index.bam.file		                <- params_w2pps$p_index.bam.file
  stats			                        <- params_w2pps$p_stats
  variant.calling		                <- params_w2pps$p_variant.calling
  variant.filtering		              <- params_w2pps$p_variant.filtering
  convert2vcf4		                  <- params_w2pps$p_convert2vcf4
  variant.annotation.geneb	        <- params_w2pps$p_variant.annotation.geneb
  variant.annotation.regionb	      <- params_w2pps$p_variant.annotation.regionb
  variant.annotation.filterb	      <- params_w2pps$p_variant.annotation.filterb
  variant.annotation.summarize      <- params_w2pps$p_variant.annotation.summarize
  grep.variants		                  <- params_w2pps$p_grep.variants
  visualize.variants		            <- params_w2pps$p_visualize.variants
  
  # -----------------------------
  
  # Get the file name to process now
  file2process.my1 <- params$file_list[datastep.my2]
  
  # Re-set working directory while in child worker, just in case
  setwd(params$wd)
  step <- data.frame(datastep.my2, 0)
  colnames(step) <- c("n","tmp")

  # Continue with the log file when/where needed
  if (params$log && map.on.reference.genome.parallel) { 
    write(paste("\n", sep=""), file=paste(params$log.folder,"/log.", params$startdate, params$opt$label, ".", file2process.my1, ".txt", sep=""), append = TRUE, sep = "");
    print_mes("\n################################################################################\n", file2process.my1);
    print_mes(paste("	NEW RUN - A. PARALLELIZED. ", params$n_files, " files; Current: *** ", file2process.my1, " ***\n", sep=""), file2process.my1);
    print_mes("################################################################################\n\n", file2process.my1);
  }
  

  
  #--- Parallel Pipeline steps into wrapper2.parallelizable.per.sample function ###----------------------------------------
  
  if (map.on.reference.genome.parallel) { 
    # Report about the next step
    print_doc(paste("### Start processing file #", datastep.my2, " (", file2process.my1, ") ... ###\n", sep=""), file2process.my1);
    # Next Step
    step <- fun.map.on.reference.genome(file2process.my2  = file2process.my1,
                                        step.my  = step)
  }
  
  
  # Re-set the log file, if it exists already and log is requested
  if (params$log) { 
    #      write(paste("			### NEW RUN (", Sys.Date()," - ", params$n_files, " files) ###\n", sep=""), file=paste(params$log.folder,"/log.", params$startdate, ".", file2process.my1, ".txt", sep=""), append = FALSE, sep = "");
    print_mes("\n################################################################################\n", file2process.my1);
    print_mes(paste(" NEW RUN - B. PARALLELIZABLE. ", params$n_files, " files; Current: *** ", file2process.my1, " ***\n", sep=""), file2process.my1);
    print_mes("################################################################################\n\n", file2process.my1);
  }
  #  step$n <- 0
  #  step$tmp <- 0
  print_doc(paste("### Start processing file #", datastep.my2, " (", file2process.my1, ") ... ###\n", sep=""), file2process.my1);
  
  
  #--- Parallelized Pipeline steps inside wrapper.parallelizable function ###----------------------------------------
  
  if (quality.control) { 
    # First Step
    step <- fun.quality.control(file2process.my2  = file2process.my1,
                                step.my  = step)
  }
  
  if (sam2bam.and.sort) {
    # Next Step
    step <- fun.sam2bam.and.sort(file2process.my2  = file2process.my1,
                                 step.my  = step)
  }
  
  if (remove.pcr.dup) {
    # Next Step
    step <- fun.remove.pcr.dup(file2process.my2  = file2process.my1,
                               step.my  = step)
  }
  
  if (index.bam.file) {
    # Next Step
    step <- fun.index.bam.file(file2process.my2  = file2process.my1,
                               step.my  = step)
  }
  
  if (stats) {
    # Next Step
    step <- fun.stats(file2process.my2  = file2process.my1,
                      step.my  = step)
  }
  
  if (variant.calling) {
    # Next Step
    step <- fun.variant.calling(file2process.my2  = file2process.my1,
                                step.my  = step)
  }
  
  if (variant.filtering) {
    # Next Step
    step <- fun.variant.filtering(file2process.my2  = file2process.my1,
                                  step.my  = step)
  }
  
  if (convert2vcf4) {
    # Next Step
    step <- fun.convert2vcf4(file2process.my2  = file2process.my1,
                             step.my  = step)
  }
  
  if (variant.annotation.geneb) {
    # Next Step
    step <- fun.variant.annotation.geneb(file2process.my2  = file2process.my1,
                                         step.my  = step)
  }
  
  ## fun.variant.annotation.regionb skipped so far
  #  if (variant.annotation.regionb) {
  #    # Next Step
  #    step <- fun.variant.annotation.regionb(file2process.my2  = file2process.my1,
  #                      step.my  = step)
  #  }
  
  if (variant.annotation.filterb) {
    # Next Step
    step <- fun.variant.annotation.filterb(file2process.my2  = file2process.my1,
                                           step.my  = step)
  }
  
  if (variant.annotation.summarize) {
    # Next Step
    step <- fun.variant.annotation.summarize(file2process.my2  = file2process.my1,
                                             step.my  = step)
  }
  
  if (grep.variants) {
    # Next Step
    step <- fun.grep.variants(file2process.my2  = file2process.my1,
                              step.my  = step)
  }
  
  if (visualize.variants) {
    # Next Step
    step <- fun.visualize.variants(file2process.my2  = file2process.my1,
                                   step.my  = step)
  }
  
  step$tmp <- step$tmp+1;
  print_doc(paste("	End of processing this file: ", file2process.my1, "\n", sep=""), file2process.my1);
  print_mes("\n--------------------------------------------------------------------------------\n\n", file2process.my1);
  
  # XXX...
  
  
  # Last step of wrapper
  #  gc() # Let's clean ouR garbage if possible
  return(NULL) # return nothing, since results are saved on disk from the perl script
} # end of wrapper function

##############################################################


##########################
### FUNCTION wrapper2.parallelizable.final
###
###   3c. Wrapper functions, parallelizable.
##########################
wrapper2.parallelizable.final <- function(datastep.my2) {
  # -----------------------------
  # Define which processes to run (in later stage, this will be in an external R file sourced here)
  # names of control process are like functions but without the "fun." prefix.
  # -----------------------------
  
  build.html.report  	<- params_w2pf$p_build.html.report
  
  
  # -----------------------------
  
  # Get the file name to process now
  file2process.my1 <- params$file_list[datastep.my2]
  
  # Re-set working directory while in child worker, just in case
  setwd(params$wd)
  step <- data.frame(datastep.my2, 0)
  colnames(step) <- c("n","tmp")
  
  # Re-set the log file, if it exists already and log is requested
  if (params$log) { 
    #      write(paste("			### NEW RUN (", Sys.Date()," - ", params$n_files, " files) ###\n", sep=""), file=paste(params$log.folder,"/log.", params$startdate, ".", file2process.my1, ".txt", sep=""), append = FALSE, sep = "");
    print_mes("\n################################################################################\n", file2process.my1);
    print_mes(paste("	NEW RUN - C. PARALLELIZABLE also. ", params$n_files, " files.", sep=""), file2process.my1);
    print_mes("################################################################################\n\n", file2process.my1);
  }
  #  step$n <- 0
  #  step$tmp <- 0
  print_doc(paste("### Start processing file #", datastep.my2, " (", file2process.my1, ") ... ###\n", sep=""), file2process.my1);
  
  
  #--- Parallelized Pipeline steps inside wrapper.parallelizable.final function ###----------------------------------------
  
  
  if (build.html.report) {
    # Next Step
    step <- fun.build.html.report(file2process.my2  = file2process.my1,
                                  step.my  = step)
  }
  
  step$tmp <- step$tmp+1;
  print_doc(paste("	End of EVA UEB pipeline", "\n", sep=""), file2process.my1);
  print_mes("\n--------------------------------------------------------------------------------\n\n", file2process.my1);
  
  # XXX...
  
  
  # Last step of wrapper
  #  gc() # Let's clean ouR garbage if possible
  return(NULL) # return nothing, since results are saved on disk from the perl script
} # end of wrapper function
