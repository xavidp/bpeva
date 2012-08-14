#!/home/ueb/repo/peeva/eva_ueb2.R
#
# SCRIPT: eva_ueb2.R
# Input: Directories of input and output
# Output: List of Variants
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
### 	Print a message in screen with timestamp and eventually in log files if requested
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
  system(command);
  print_done(file2process.my2);
  
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
  
  file_in = paste(params$directory_in, "/", file2process.my2, ".fastq", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam", sep="");
  command00 = "bwa bwasw"; # next command.
  options00 = paste(params$path_genome, " ", file_in, " > ", file_out, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
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


##############################################################################
##############################################################################
### Main Program #############################################################
##############################################################################
##############################################################################
startdate <- paste(format(Sys.Date(), "%y%m%d"), sep="")
program_ueb <- "eva_ueb2.R";

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

##############################################################

# 0c. set some reasonable defaults for the options that are needed, but were not specified.
# ---------------------------------------------------------------------
if ( is.null(opt$input    ) ) { opt$input    = "test_in"	} # "dir_in_sara_207"     }
if ( is.null(opt$output   ) ) { opt$output   = "test_out"	} # "dir_out_sara_207"    }
if ( is.null(opt$index    ) ) { opt$index    = FALSE         }
if ( is.null(opt$filter   ) ) { opt$filter   = "BRCA"            }
if ( is.null(opt$log) || opt$log ==1) { opt$log      = TRUE        }
if ( is.null(opt$summarize) ) { opt$summarize= TRUE          }
if ( is.null(opt$keep     ) ) { opt$keep     = TRUE         } # Enable if run through editor and you want to keep temp files
if ( is.null(opt$cpus     ) ) { opt$cpus     = 4             }
if ( is.null(opt$parallel ) ) { opt$parallel = TRUE        }
if ( is.null(opt$label    ) ) { opt$label    = ".test_s_p"	} # ".sara207_4s4cpu"        } # Run Label for output filenames

# Other early initialization of variables
# Set the working directory
wd <- "/home/xavi/repo/peeva/"
setwd(wd)


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
abs_path_to_script = getwd() # Path absolut to the script
rel_path_to_input_files = opt$input # Include both the trailing slash at the right of the folder name and the one at its left
abs_path_to_input_files = paste(abs_path_to_script, "/", rel_path_to_input_files, "/", sep="")
file_list_name = paste(opt$output, "/", "log.",startdate, opt$label, ".fastq_input_list.txt", sep="")

# Get the list of files in "input" directory through a system call to "ls *" and save the result to a file on disk
system(paste("ls ",abs_path_to_input_files,"*.fastq > ", file_list_name, sep=""), TRUE)
# Read the file with the list of files to be processed
file_list <- read.table(file_list_name, sep="")

# Count the number of source files
n_files = length(file_list[[1]])

# remove the directory prefix from the names
# through gsub, alternatively
file_list <- gsub(abs_path_to_input_files,"", file_list[[1]])
file_list <- gsub(".fastq","", file_list)

##############################################################

# 2b. Define params for all runs
#----------------------------------
params <- list()
params <- list(startdate = startdate,
               scriptname = "eva_ueb2.R", # Adapted version to work from this R script to be parallelized
               opt = opt, # command line arguments passed to the program run
               #scriptparams = "-i ./test_in -o ./test_out -s -k | tee /dev/tty ./logs/log_both.txt
               file_list = file_list,
               n_files = n_files,
               wd = wd, # the working directory
               directory_in = opt$input,
               directory_out = opt$output,
               log = opt$log,
               log.folder = opt$output, # "logs",
               path_fastq = "/home/ueb/fastqc/fastqc",
               path_genome = "/home/xavi/Data/Data_Genomes/hg19/hg19.fa",
               path_vcfutils = "/usr/share/samtools/vcfutils.pl",
               path_convert2annovar = "/home/ueb/annovar/convert2annovar.pl",
               path_annotate_variation = "/home/ueb/annovar/annotate_variation.pl",
               path_annotate_humandb = "/home/ueb/annovar/humandb/",
               path_summarize_annovar = "/home/ueb/annovar/summarize_annovar.pl"           
)

routlogfile <- paste("log.", startdate, opt$label, ".SnowFall.Rout", sep="") # SnowFall R output file
abs_routlogfile <- paste(opt$output, "/", routlogfile, sep="")
system(paste("touch ", abs_routlogfile, sep=""))
#w.output(unlist(.Platform), routlogfile )
#w.output(unlist(.Machine),  routlogfile )
#w.output(unlist(R.version), routlogfile )
#w.output(unlist(Sys.info()),  routlogfile )
sink(abs_routlogfile , split=TRUE)
# .Platform
# .Machine
# R.version
# Sys.info()

##############################################################

# 3a. Wrapper functions, One is run always sequentially. The other one, parallelized.
#----------------------------------
wrapper.sequential <- function(datastep.my) {

  # -----------------------------
  # Define which processes to run (in later stage, this will be in an external R file sourced here)
  # names of control process are like functions but without the "fun." prefix.
  # -----------------------------
    map.on.reference.genome 	<- TRUE # FALSE
  # -----------------------------
  

  # Get the file name to process now
  file2process.my1 <- params$file_list[datastep.my]

  # Re-set working directory while in child worker, just in case
  setwd(params$wd)
  step <- data.frame(datastep.my, 0)
  colnames(step) <- c("n","tmp")

  # Re-set the log file, if it exists already and log is requested
  if (params$log) { 
      write(paste("\n", sep=""), file=paste(params$log.folder,"/log.", params$startdate, params$opt$label, ".", file2process.my1, ".txt", sep=""), append = FALSE, sep = "");
      print_mes("\n################################################################################\n", file2process.my1);
      print_mes(paste("			NEW RUN - Part A. Allways in SEQUENTIAL Mode (", Sys.Date()," - ", params$n_files, " files). Current file: *** ", file2process.my1, " ***\n", sep=""), file2process.my1);
      print_mes("################################################################################\n\n", file2process.my1);
  }
#  step$n <- 0
#  step$tmp <- 0
  print_doc(paste("### Start processing file #", datastep.my, " (", file2process.my1, ") ... ###\n", sep=""), file2process.my1);

  
  #--- Sequential Pipeline steps into wrapper.sequential function ###----------------------------------------

  if (map.on.reference.genome) { 
    # Next Step
    step <- fun.map.on.reference.genome(file2process.my2  = file2process.my1,
                      step.my  = step)
  }


}

##############################################################

# 3b. Wrapper functions, to be run per input sample file, Can be parallelized.
#----------------------------------
wrapper2.parallelizable.per.sample <- function(datastep.my2) {
  # -----------------------------
  # Define which processes to run (in later stage, this will be in an external R file sourced here)
  # names of control process are like functions but without the "fun." prefix.
  # -----------------------------

  #####
  runParam <- FALSE # TRUE # FALSE
  ####

    quality.control 		<- runParam

  #####
  runParam <- TRUE
  ####

    sam2bam.and.sort		<- runParam
    remove.pcr.dup		<- runParam
    index.bam.file		<- runParam
    stats			<- runParam
  #####
  runParam <- FALSE
  ####
    variant.calling		<- runParam
    variant.filtering		<- runParam
    convert2vcf4		<- runParam
    variant.annotation.geneb	<- runParam
    variant.annotation.regionb	<- runParam # skipped so far
    variant.annotation.filterb	<- runParam
    variant.annotation.summarize<- runParam
    grep.variants		<- runParam


    visualize.variants		<- runParam
#    XXX		<- runParam


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
      print_mes(paste("			NEW RUN - Part B. Can be run in PARALLELIZED Mode (", Sys.Date()," - ", params$n_files, " files). Current file: *** ", file2process.my1, " ***\n", sep=""), file2process.my1);
      print_mes("################################################################################\n\n", file2process.my1);
  }
#  step$n <- 0
#  step$tmp <- 0
  print_doc(paste("### Start processing file #", datastep.my2, " (", file2process.my1, ") ... ###\n", sep=""), file2process.my1);

  
  #--- Parallelized Pipeline steps insidewrapper.parallelizable function ###----------------------------------------

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

# 3c. Wrapper functions, parallelizable.
#----------------------------------
wrapper2.parallelizable.final <- function(datastep.my2) {
  # -----------------------------
  # Define which processes to run (in later stage, this will be in an external R file sourced here)
  # names of control process are like functions but without the "fun." prefix.
  # -----------------------------
  
  #####
  runParam <- FALSE # TRUE # FALSE
  ####
  
  build.html.report		<- runParam
  #    XXX		<- runParam
  
  
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
    print_mes(paste("			NEW RUN - Part C. Can be run in PARALLELIZED Mode also (", Sys.Date()," - ", params$n_files, " files).", sep=""), file2process.my1);
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

##############################################################

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
         "print_mes",
         "w.output",
         "check2clean",
         "fun.quality.control",
         "fun.index.reference.genome",
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
    step <- fun.index.reference.genome(file2process.my2  = file2process.my1,
                      step.my  = step)
    duration <- Sys.time()-start;
    print(duration)
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
start3 <- Sys.time(); result2 <- sfLapply(1:length(file_list), wrapper2.parallelizable.per.sample) ; duration <- Sys.time()-start3;
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

