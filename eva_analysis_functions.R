#!/home/ueb/repo/peeva/eva_analysis_functions.R
#
# SCRIPT: eva_analysis_functions.R
# SCOPE: to be called from other scripts, such as eva_main.R
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
    w.output.samples(mess, filename.my1)
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
    w.output.samples(mess, filename.my1)
  }
}

##########################
### FUNCTION print_mes_fullpath
###
###   Print a message in screen "as is" and eventually in a files with a full path provided to access it
##########################

print_mes_fullpath <- function(mess, filename.my1) # Similar to print_doc but without adding the time stamp at the beggining.
{
  # ---------------------------------------------------------------------
  cat(mess)
  if (params$log) { 
    w.output.samples.fullpath(mess, filename.my1)
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
    w.output.samples(mess, filename.my1)
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
    w.output.samples(mess, filename.my1)
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
    print_mes(paste("\t\t\t\t\tOk. Removing temporary file ", var, "\n\n", sep=""), filename.my1);
    system(paste("rm  ", var, sep=""));
  }
}

##########################
### FUNCTION w.output.samples
###
### 	Write output to log files on disk
##########################

w.output.samples <- function(mess, filename.my2)
{
  write(mess, file=paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", filename.my2, ".txt", sep=""), append = TRUE, sep = "");
}

##########################
### FUNCTION w.output.samples.fullpath
###
###   Write output to files on disk with a full path provided to acces them
##########################

w.output.samples.fullpath <- function(mess, filename.my2)
{
  write(mess, file=filename.my2, append = TRUE, sep = "");
}

##########################
### FUNCTION w.routlogfile.header
###
###   Write header of foutputlog file 
##########################

w.routlogfile.header <- function(filename.my1)
{
  # Re-set the log file, if it exists already and log is requested
  write(paste("\n", sep=""), file=filename.my1, append = TRUE, sep = "");
  print_mes_fullpath("\n################################################################################\n", filename.my1);
  print_mes_fullpath(paste("    	NEW RUN - ", params$n_files, " files; ", filename.my1, "\n", sep=""), filename.my1);
  print_mes_fullpath("################################################################################\n\n", filename.my1);  
}

##########################
### FUNCTION w.output.run
###
###   Write output with info from SnowFall (when used), params for the run and system info
##########################

w.output.run <- function(label, mess, filename.my2)
{
  write(paste("\n", label, "\n", sep=""), file=filename.my2, append = TRUE, sep = "");
  write.table(as.data.frame(mess), file=filename.my2, append = TRUE, row.names = TRUE, col.names = FALSE, sep = " = ");
  write("\n--------------------------------------------------------------", file=filename.my2, append = TRUE, sep = "");
}


##########################
### FUNCTION w.lock.sample.pe
###
###   Create a lock file in the file system when using paired end data to prevent the pipeline to continue when there is still
###   some sample pending to have the read merged from both strands (_1_sequence.fastq & _2_sequence.fastq) into one single file (_12.sam)
##########################

w.lock.sample.pe <- function(filename.my2)
{
	system(paste("touch ", params$opt$output, "/", "log.",params$startdate, ".", params$opt$label, ".", filename.my2, ".lock", sep=""), TRUE)
}

##########################
### FUNCTION w.checklock.allsamples.pe
###
###   Check if the lock file is present in the file system when using paired end data (it will indicate that this sample is still being processed and limiting
###   the continuation of the pipeline; this sample have not yet had the reads merged from both strands (_1_sequence.fastq & _2_sequence.fastq) into one single file (_12.sam)
##########################

w.checklock.allsamples.pe <- function(file_list.my2)
{
  any(file.exists(paste(params$opt$output, "/", "log.",params$startdate, ".", params$opt$label, ".",
                    as.matrix(file_list.my2)[1:length(file_list.my2)], ".lock", sep="")))  
}

##########################
### FUNCTION w.unlock.sample.pe
###
###   Remove the lock file in the file system when using paired end data to indicate that this sample is processed already and not limiting
###   the continuation of the pipeline; this sample have had the reads merged from both strands (_1_sequence.fastq & _2_sequence.fastq) into one single file (_12.sam)
##########################

w.unlock.sample.pe <- function(filename.my2)
{
	print_mes(paste(now(), "Removing lock: ", params$opt$output, "/", "log.",params$startdate, ".", params$opt$label, ".", filename.my2, ".lock \n", sep=""), filename.my2)
  system(paste("rm ", params$opt$output, "/", "log.",params$startdate, ".", params$opt$label, ".", filename.my2, ".lock", sep=""), TRUE)
}


##########################
### FUNCTION mail.send
###
###   Send an email (eventually with attachments) when requested at the end of the whole run. 
###   
###   attachmentPath <- "subfolder/log.txt" # needs full path if not in working directory
###   attachmentName <- "log.txt" # same as attachmentPath if not in working directory
##########################
mail.send <- function(attachmentPath, attachmentName)
{
  if(!require(sendmailR)){ install.packages("sendmailR") }
  require(sendmailR)
  
  # Get param values as defined in eva_params.R
  from <- params$p_from
  to <- params$p_to
  subject <- params$p_subject
  body <- params$p_body                     
  mailControl=list(smtpServer=params$p_smtp)
  
  # #####send plain email
  # sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)
  
  #####send same email with attachment
  #needs full path if not in working directory
  #attachmentPath <- "subfolder/log.txt"
  #attachmentPath <- abs_routlogfile
  
  #same as attachmentPath if using working directory
  #attachmentName <- "log.txt"
  #attachmentName <- routlogfile
  
  #key part for attachments, put the body and the mime_part in a list for msg
  attachmentObject <- mime_part(x=attachmentPath,name=attachmentName)
  bodyWithAttachment <- list(body,attachmentObject)

  ## If more than one attachment, use this syntax
  #attachmentObject <- mime_part(x="subfolder/log.txt",name="log.txt")
  #attachmentObject2 <- mime_part(x="subfolder/log2.txt",name="log2.txt")
  #bodyWithAttachment <- list(body,attachmentObject,attachmentObject2)
  
  # Send the email. This procedure works only with non-parallel runs. 
  # When parallel run, no email received :-()
#  sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)

  # Testing another way to send the email through a direct system call.
  # To check whether this also works in parallel-run mode.
  params$p_subject <- paste("EVA Pipeline run finished: ", params$p_label, sep="")
  params$p_body <- paste(params$p_subject, " _ See some log information attached", sep="")                   
  system(paste("sendEmail -f ", params$p_from, " -t ", params$p_to, " -u ", params$p_subject,
               " -m ", params$p_body, " -s ", params$p_smtp, " -a ", attachmentPath,
               " >> ", attachmentPath, sep=""))
#  return() # return nothing
}

##########################


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
  cat("\n         for bwa: 2, use suffixes: *_1_sequence.fastq, *_2_sequence.fastq     \n")
  cat("\n   -x: suffix (ending) in input filenames: _sequence, ...                     \n")
  cat("\n   -e: .extension (with dot) of input filenames: .fastq, .sam, .bam, ...      \n")
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
  
#  # Remove .fastq (substitute it with nothing) from file names
#  name = sub(".fastq","",file2process.my2,  ignore.case = FALSE, perl = FALSE, fixed = TRUE);
#^unneded since this is done in eva_main.R for the whole file_list at once at the beginning
  
  #  print_doc("$now -   Step $step_n.$step_tmp Quality Control and Preprocessing: $name ...");
  file_in = paste(params$directory_in, "/", file2process.my2, params$opt$in.ext, sep="");
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

fun.index.reference.genome <- function(step.my, filename.my1) {
  # update step number
  step.my$tmp <- step.my$tmp + 1

  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Map against reference genome: Index the reference genome (if needed) ###\n", sep=""), "Index the reference genome");
  if ((params$opt$index) & (step.my$n == 0)) { # case to index the reference genome (time consuming, do only when really needed as requested)
    # Index the reference genome, if requested with argument -n and only for the first file if more than one sample to process
    command00 <- "bwa index"; # next command
    options00 <- paste("  -a bwtsw ", params$path_genome, " >> ", filename.my1, sep="");
  } else	{ # skip the indexing of the reference genome
    command00 <- "echo '  ...skipped...'"; # next command.
    options00 <- "";
  }
  command = paste(command00, " ", options00, sep="");
  # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
  start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
  print_done("Index reference genome");
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
  #        for bwa: 2, use sufixes: *_1_sequence.fastq, *_2_sequence.fastq
  # 3: bwa bwasw             (longer reads, single end only) 
  ##
  ## From: http://bio-bwa.sourceforge.net/bwa.shtml
  ## BWA is a fast light-weighted tool that aligns relatively short sequences (queries) to a sequence database (targe), such as the human reference genome. 
  ## It implements two different algorithms, both based on Burrows-Wheeler Transform (BWT). 
  ## The first algorithm is designed for short queries up to ~200bp with low error rate (<3%). 
  ##   It does gapped global alignment w.r.t. queries, supports paired-end reads, and is one of the fastest short read alignment algorithms to date while also visiting suboptimal hits. 
  ## The second algorithm, BWA-SW, is designed for long reads with more errors. It performs heuristic Smith-Waterman-like alignment to find high-scoring local hits (and thus chimera). 
  ##   On low-error short queries, BWA-SW is slower and less accurate than the first algorithm, but on long queries, it is better

  ## Jan 2013: GATK requires that you specify a read group tag when aligning your sequences with BWA. 
  ##    Just add the following argument within the bwa sampe step
  ##      -r "@RG\tID:sample\tLB:sample\tPL:ILLUMINA\tSM:sample"
  ##    RG: defines a ReadGroup, 
  ##    ID: specifies the name of the ReadGroup, 
  ##    LB: defines the name of the library sequenced
  ##    SM: defines the individual sample, 
  ##    PL: defines the platform you used.
  ##      GATK currently supports 454, LS454, Illumina, Solid, ABI_Solid, and CG (all case-insensitive).
  ##  You can leave all of them as sample (or anything else) in case you really got a single sample you wish to analyze. 
  ##  You might wonder what that could be good for: These tags are used when merging BAM files from different samples to distinguish between the different samples.
  ##
  ## Alternatively, you could add the read group by means of 'picard' software programs:
  ## http://seqanswers.com/forums/showthread.php?t=23332
  
  file_in = paste(params$directory_in, "/", file2process.my2, params$opt$in.ext, sep="");
  
  if (params$opt$bwa == 1) # case to use algorythm 1 from bwa: aln + samse  (short reads, single ends, low errors);
  {
    # Example - 1s part
    #bwa aln database.fasta short_read.fastq > aln_sa.sai
    file_out = paste(params$directory_out, "/", file2process.my2, ".sai", sep="");
    command00 = "bwa aln"; # next command.
    # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
    file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
    options00 = paste(params$path_genome, " ", file_in, " > ", file_out, " 2>> ", file_stderr, sep="");
    command = paste(command00, " ", options00, sep="");
    # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
    start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
    # Show the duration of this subprocess
    cat("\n 1st part (samse) - Relative duration since last step: "); print(duration);
    
    # Example - 2nd part
    #bwa samse database.fasta aln_sa.sai short_read.fastq > aln.sam
    file_in_sai = paste(params$directory_out, "/", file2process.my2, ".sai", sep="");
    file_out = paste(params$directory_out, "/", file2process.my2, ".sam", sep="");
    command00 = "bwa samse"; # next command.
    # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
    file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
    options00 = paste(params$path_genome, " ", file_in_sai, " ", file_in, 
                      " -r \"@RG\tID:", file2process.my2, "\tLB:", file2process.my2, "\tPL:ILLUMINA\tSM:", 
                      file2process.my2, "\"", " > ", file_out, " 2>> ", file_stderr, sep="");
    command = paste(command00, " ", options00, sep="");
    # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
    start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
    # Show the duration of this subprocess
    cat("\n 2nd part (samse) - Relative duration since last step: "); print(duration); cat("\n")
  }

  if (params$opt$bwa == 2) # case to use algorythm 2 from bwa: aln (x2) + sampe  (short reads, paired ends, low errors);
  {
    # Write the lock file to indicate that this sample is being processed from paired end reads dual file-set.
    w.lock.sample.pe(file2process.my2)

    # Example - 1s part
    #bwa aln database.fasta short_read1.fastq > aln_sa1.sai
    #bwa aln database.fasta short_read2.fastq > aln_sa2.sai
    file_out = paste(params$directory_out, "/", file2process.my2, ".sai", sep="");
    command00 = "bwa aln"; # next command.
    # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
    file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
    options00 = paste(params$path_genome, " ", file_in, " > ", file_out,  " 2>> ", file_stderr, sep="");
    command = paste(command00, " ", options00, sep="");
    # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
    start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
    # Show the duration of this subprocess
    cat("\n 1st part (sampe) - Relative duration since last step: "); print(duration); cat("\n")
    
#    file2process.my2 <- "s_4_m11_146b_1_sequence" #     file2process.my2
#    file2process.my2 <- "s_4_m11_146b_2_sequence" #     file2process.my2
#    file2process.my2 <- "sample_a_1_sequence" #     file2process.my2
#    file2process.my2 <- "sample_a_2_sequence" #     file2process.my2
#    tmp2 <- gsub("_sequence", "@", tmp)
#    length(grep("_1_sequence", file2process.my2)) == 1 # returns TRUE when matched the string
    
#file2process.my2 <- "sample_a_2_sequence"

    # Previous file of the 2 file set: first mate of the pair
    #   (if we are analyzing here the sample_foo_2_sequence, 
    #   then get the name corresponding to sample_foo_1_sequence)
    file2process.mate1 <- params$file_list[match(file2process.my2, params$file_list) -1]
    # get the file name of the lock file for the mate 1 of the pair 
    # (it should have completed its sam file before attempting to merge it with the 2mate of the pair)
    mate1.unfinished <- file.exists(paste(params$opt$output, "/", "log.",params$startdate, ".", params$opt$label, ".",
                      file2process.mate1, ".lock", sep=""))  
    
    
    # Condition returns TRUE when matched the string and there is no lock file for the first sample of the paired-end set
    if ( length(grep(paste("_2", params$opt$in.suffix, sep=""), file2process.my2)) == 1 ) # returns TRUE when matched the string (2nd sample of the pair)
    {
      if (mate1.unfinished) {
        # Report the user that sam is not finished for mate1 of the sample pair
        print_doc(paste(" ### Waiting for sam file for the mate1 of the pair to finish ###\n", sep=""), file2process.my2);
        # If A lock file exists; wait a while and check again until no lock file from samples exist
        while (file.exists(paste(params$opt$output, "/", "log.", params$startdate, ".", params$opt$label, ".",
                                 file2process.mate1, ".lock", sep=""))) {
            cat(".")
            Sys.sleep(60) # Wait 60 seconds while for the creation of the sam file for the mat1 sample file to finish, and check again
        }       
      } 

      # Provide temporal shorter base names for the 2 files of the paired-end set (remove "_1_sequence" and "_2_sequence")
      f2pbase <- gsub(paste("_2", params$opt$in.suffix, sep=""), "", file2process.my2)

      # Example - 2nd part
      #bwa sampe database.fasta aln_sa.sai short_read.fastq > aln.sam
      file_in_sai1 = paste(params$directory_out, "/", f2pbase, paste("_1", params$opt$in.suffix, sep=""), ".sai", sep="");
      file_in_sai2 = paste(params$directory_out, "/", f2pbase, paste("_2", params$opt$in.suffix, sep=""), ".sai", sep="");
      file_in_fq1 = paste(params$directory_in,   "/", f2pbase, paste("_1", params$opt$in.suffix, sep=""), params$opt$in.ext, sep="");
      file_in_fq2 = paste(params$directory_in,   "/", f2pbase, paste("_2", params$opt$in.suffix, sep=""), params$opt$in.ext, sep="");
      file_out = paste(params$directory_out,     "/", f2pbase, "_merged12.sam", sep="");
      command00 = "bwa sampe"; # next command.
      # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
      file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
      #      options00 = paste(params$path_genome, " ", file_in_sai1, " ", file_in_sai2, " ", file_in_fq1, " ", file_in_fq2, " > ", file_out, sep="");
      options00 = paste(params$path_genome, " ", file_in_sai1, " ", file_in_sai2, " ", file_in_fq1, " ", file_in_fq2,
                        # The following -r param in bwa is only in recent versions of bwa, not in 0.5.5.x which is the latest one supported in ubuntu lucid 10.04 repositories as of 2013 January at least.
                        # So when running in servers with Ubuntu Lucid or similar, keep this following line related to the "-r" param commented out. 
                        # This param is needed mainly for later usage with GATK. Otherwise, it seems safely removable.
                        #                        " -r \"@RG\tID:", file2process.my2, "\tLB:", file2process.my2, "\tPL:ILLUMINA\tSM:", 
                        file2process.my2, "\"", " > ", file_out,       " 2>> ", file_stderr, sep="");
      command = paste(command00, " ", options00, sep="");
      # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
      start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
      # Show the duration of this subprocess
      cat("\n 2nd part (sampe) - Relative duration since last step: "); print(duration); cat("\n")

      # direct calls to this step in a terminal (indicated here for debugging purposes):
      # ueb@ueb:/path1$ bwa sampe /home/ueb/Data/Data_Genomes/hg19.fa Sample_1820_1_sequence.sai  Sample_1820_2_sequence.sai  ../dir_in/Sample_1820_1_sequence.fastq ../dir_in/Sample_1820_2_sequence.fastq  -r "@RG\tID:Sample_1820\tLB:Sample_1820\tPL:ILLUMINA\tSM:Sample_1820" > Sample_18020_merged12.sam
      # ueb@ueb:/path2# bwa sampe /home/ueb/Data/Data_Genomes/rn4/rn4.fa Sample_1797_1_sequence.sai  Sample_1797_2_sequence.sai  /path2/Sample_1797_1.fastq /path2/Sample_1797_2.fastq  > Sample_1797_merged12.byhand.sam

##      cat("\nWe will now stop the pipeline. You need to tweak the eva_params.R file to stop any attemp to rerun the previous steps and continue from here");
##      stop()
      # geterrmessage()
    }
    
    # Remove the lock file from the paired-end process for this sample file.
    w.unlock.sample.pe(file2process.my2)
    
  }
  
  if (params$opt$bwa == 3) { # case to use algorythm 3 from bwa: bwasw (longer reads, single end only)
    file_out = paste(params$directory_out, "/", file2process.my2, ".sam", sep="");
    command00 = "bwa bwasw"; # next command.
    # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
    file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
    #    options00 = paste(params$path_genome, " ", file_in, " > ", file_out, sep="");
    options00 = paste(params$path_genome, " ", file_in,
                      " -r \"@RG\tID:", file2process.my2, "\tLB:", file2process.my2, "\tPL:Roche454\tSM:", 
                      file2process.my2, "\"", " > ", file_out,  " 2>> ", file_stderr, sep="");
    command = paste(command00, " ", options00, sep="");
    # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
    start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
    # Show the duration of this subprocess
    cat("\n (Case bwasw) - Relative duration since last step: "); print(duration);  cat("\n")
  }
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return both the step number, and the filename out that will be theinput for the next step
}


##########################
### FUNCTION func.convert.file.list.pe
###
### Convert the list of files to process from *1_sequence and *2_sequence into to *_merged12 
### Hald the number of files for the loops, sfapply's, etc.
##########################

fun.convert.file.list.pe <- function(file2process.my2, step.my) {

#   # Manual debuging - ini
#   file2process.my2 <- "s_4_m11_145b_1_sequence.fastq"
#   #file2process.my2 <- "s_3_m11_145b_merged12.sam"
#   step.my <- data.frame(1, 0)
#   colnames(step.my) <- c("n","tmp")
#   step.my$tmp <- 0
#   # Manual debuging - end
  
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Convert the file list regarding paired-end merged .sam files: ###\n", sep=""), file2process.my2);
  
  # When input files contain paired end reads (_pe), a temporal (_tmp) file name will be used first until we combine the data from both strands
  params$filename_list <- paste(params$opt$output, "/", "log.",params$startdate, ".", params$opt$label, ".merged12_input_list.txt", sep="")
  # Get the list of files in "input" directory through a system call to "ls *" and save the result to a file on disk
  system(paste("ls ", params$opt$output,"/", "*merged12.sam > ", params$filename_list, sep=""), TRUE)
  
  # Read the file with the list of files to be processed
  params$file_list <- read.table(params$filename_list, sep="")
  
  # Count the number of source files
  params$n_files = length(params$file_list[[1]])
  
  # remove the directory prefix from the names as well as the ending .fastq
  # through gsub, alternatively
  params$file_list <- gsub(paste(params$opt$output, "/", sep=""), "", params$file_list[[1]])
  params$file_list <- gsub(".sam","", params$file_list)
  
  ## Replace the file name to process from now onwards: divide by 2 and apply the floor function to it
  ## To revise XXXX TODO
  #datastep.my2 <- floor(step.my$n/2)
  #  # New file to process in the next step
  #  file2process.my1 <- params$file_list[datastep.my2]
    output2return <- list(step.my, params$file_list, params$n_files)
  ## To be deleted in the near future probably (Dec 13, 2012)
  
  print_done(file2process.my2);
  gc() # Let's clean ouR garbage if possible
#  return(step.my) # return the step number
  return(output2return) # return the step number
  
}



##########################
### FUNCTION fun.bowtie2sam
###
### So far hardcoded to use the modified version of bowtie2sam.pl 
###   (from its samtools counterpart) in order to include all reads and not just the best read).
### See http://seqanswers.com/forums/showthread.php?p=46232#post46232
###   & http://seqanswers.com/forums/attachment.php?attachmentid=865&d=1310484858
##########################

fun.bowtie2sam <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Convert bowtie to sam: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".bowtie", sep="");
  file_out = paste(file_in, ".sam", sep="");
  command00 = "perl allbowtie2sam.pl "; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", file_in, " > ", file_out,   " 2>> ", file_stderr, sep="");
  
  # direct command line call for testing other things:
  # perl allbowtie2sam.pl Sample_1820.bowtie > Sample_1820.bowtie.sam
  
  command = paste(command00, " ", options00, sep="");
  system(command);
  check2clean(file_in, file2process.my2);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.sam2bam.and.sort
##########################

fun.sam2bam.and.sort <- function(file2process.my2, step.my) {
  #   #Manual debugging - ini
  #   file2process.my2 <-"s_1_m11_143b_merged12.sam"
  #   step.my <- data.frame(10, 0)
  #   colnames(step.my) <- c("n","tmp")
  #   step.my$tmp <- 0
  #   #Manual debugging - end
  
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Convert sam to bam and sort it: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam", sep="");
  # Sometimes we might be running the whole pipeline with sam files as starting point.
  # Therefore, source files could be in directory_in instead of directory_out. The next if clause should take care of these.
  if (!file.exists(file_in)){
    file_in = paste(params$directory_in, "/", file2process.my2, ".sam", sep="");
  }
  # We need to explicitly indicate that the file out is placed inside the directory out, 
  # so that the pipeline works also when sam files are in directory in, etc. 
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted", sep="");
  command00 = "samtools"; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" view -bS -t ", params$path_genome, ".fai ", file_in, " | ", command00, " sort - ", file_out,   " 2>> ", file_stderr, sep="");

  # direct command line call for testing other things:
  # samtools  view -bS file_in | samtools sort - file_in.sorted
  
# XXX ToDo : In theory, the -u option is better (faster) for piped processes, since it does not compress/uncrompress data.... but untested yet.
#  options00 = paste(" view -buS ", file_in, " | ", command00, " sort - ", file_out,   " 2>> ", file_stderr, sep="");
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
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste("rmdup -s ", file_in, " ", file_out,  " 2>> ", file_stderr, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  check2clean(file_in, file2process.my2);
  print_done(file2process.my2);

  # direct command line call for testing other things:
  # samtools  rmdup -s file_in.sam.sorted.bam file_in.sam.sorted.noDup.bam
  
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

##########################
### FUNCTION fun.gatk.sortbyref
###
### SortByRef.pl from Broad Institute 
### Sorts lines of the input file INFILE according to the reference contig order specified by the
### reference dictionary REF_DICT (.fai file). The sort is stable. 
### If -k option is not specified,  it is assumed that the contig name is the first field in each line.
### See: 
### http://gatkforums.broadinstitute.org/discussion/1328/script-for-sorting-an-input-file-based-on-a-reference-sortbyref-pl
###   Usage:
###     sortByRef.pl [--k POS] INPUT REF_DICT
###   
###   INPUT      input file to sort. If '-' is specified, then reads from STDIN.
###   REF_DICT   .fai file, or ANY file that has contigs, in the desired soting order, as its first column.
###   --k POS :  contig name is in the field POS (1-based) of input lines.
##########################

fun.gatk.sortbyref <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Sorts lines of the input file according to the reference (GATK): ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = params$path_dbSNP
  file_out = paste(params$path_dbSNP, ".sortbyref", sep="");
  command00 = "perl "; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(params$path_gatk_sortbyref, " ", file_in, " ", params$path_genome,
#                    " > ", file_out,
#                    " >> ", file_stderr,
                     " 2>> ", file_stderr
                    , sep="");
  
  command = paste(command00, " ", options00, sep="");
  system(command);
  check2clean(file_in, file2process.my2);
  print_done(file2process.my2);

  # direct command line call for testing other things:
  # perl SortByRef.pl file_in Homo_sapiens_assembly19.fasta.fai
  # perl SortByRef.pl /home/ueb/Data/Data_Genomes/hg19_Broad_Reference_Genome/dbsnp_135_human_9606_v4.0_00-All.vcf /home/ueb/Data/Data_Genomes/hg19_Broad_Reference_Genome/Homo_sapiens_assembly19.fasta.fai
  #   xavi@mainhead:/mnt/magatzem02/tmp/run_sara_293a/dir_out_293a3b$ perl /home/ueb/Data/gatk-data-2.3/SortByRef.pl /home/ueb/Data/Data_Genomes/hg19_Broad_Reference_Genome/dbsnp_135_human_9606_v4.0_00-All.vcf /home/ueb/Data/Data_Genomes/hg19_Broad_Reference_Genome/Homo_sapiens_assembly19.fasta.fai
  #   Can not open temporary file 1080: Too many open files at /home/ueb/Data/gatk-data-2.3/SortByRef.pl line 95, <$INPUT> line 50737751.
  #   xavi@mainhead:/mnt/magatzem02/tmp/run_sara_293a/dir_out_293a3b$ 
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.gatk.local.realign.step1
###
###   Local Realignment with GATK - Step 1: Generating interval file for sample_sorted.bam
##########################

fun.gatk.local.realign.step1 <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Local Realignment with GATK - Step 1: Generating interval file: ", file2process.my2, " ###\n", sep=""), file2process.my2);

  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam.intervals", sep="");
  command00 = "java -jar "; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(params$path_gatk, " -T RealignerTargetCreator -R ", params$path_genome, 
                    " -I ", file_in, " --known:dbsnp,vcf ", params$path_dbSNP,
#                    " -L ", params$path_exon_capture_file ,
                    " -o ", file_out, 
                    " -et NO_ET -K ", params$path_gatk_key,
#                    " >> ", file_stderr, " 2>> ", file_stderr,
                    sep="");
    # Former option -B:dbsnp,vcf has been converted into -known:dbsnp,vcf. See http://seqanswers.com/forums/showthread.php?t=14013  

  # As a refence, see also this pipeline from Alberta Children's Hospital Research Institue
  # http://achri-bioinformatics.appspot.com/Alignment_Post-processing
  # --------------------------------
  # java -Xmx4G -jar /home/gordonp/ngs_utils/gatk/GenomeAnalysisTK.jar 
  #   -I merged-SampleName.rmdup.bam 
  #   -R /export/geno_tmp/achri/dbs/hg19.fa 
  #   -T RealignerTargetCreator 
  #   -o merged-SampleName.rmdup.gatk_realigner.intervals 
  # [decides where to realign]
  #
  # java -Xmx4G -jar ~/ngs_utils/gatk/GenomeAnalysisTK.jar 
  #   -I merged-SampleName.rmdup.bam 
  #   -R /export/geno_tmp/achri/dbs/hg19.fa 
  #   -T IndelRealigner 
  #   -targetIntervals merged-SampleName.rmdup.gatk_realigner.intervals 
  #   -o merged-SampleName.rmdup.gatk_realigner.bam 
  # [does the actual realignment with output to a new BAM file]
     
  command = paste(command00, " ", options00, sep="");
  system(command);
  check2clean(file_in, file2process.my2);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

##########################
### FUNCTION fun.gatk.local.realign.step2
###
###   Local Realignment with GATK - Step 2:
##########################

fun.gatk.local.realign.step2 <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Local Realignment with GATK - Step 1: Generating interval file: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.intervals", sep="");
  command00 = "java -jar "; # next command.
  # options00 = ...
  
  command = paste(command00, " ", options00, sep="");
  system(command);
  check2clean(file_in, file2process.my2);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

##########################
### FUNCTION fun.gatk.local.realign.step3
###
###   Local Realignment with GATK - Step 3:
##########################

fun.gatk.local.realign.step3 <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Local Realignment with GATK - Step 1: Generating interval file: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.intervals", sep="");
  command00 = "java -jar "; # next command.
  # options00 = ...
  
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
  
  # direct command line call for testing other things:
  # samtools  index file_in.sam.sorted.noDup.bam
  
  
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
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" flagstat ", file_in,  " >> ", file_stderr,  " 2>> ", file_stderr, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  # Don't check for check2clean("$file_in") since we still need it for the variant calling
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.snpeff.count.reads
###
###   Count Reads through snpEff software [optional]
##########################

fun.snpeff.count.reads <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Count Reads using snpEff [optional]: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam", sep="");
  file_out = paste(file_in, ".cr.txt", sep="");
  command00 = "java -Xmx4g -jar "; # next command.
#  options00 = paste(params$path_snpEff, "/snpEff.jar  -c ", params$path_snpEff, "/snpEff.config countReads ", params$opt$genver," ", file_in, " > ", file_out, sep="");
  options00 = paste(params$path_snpEff, "/snpEff.jar  countReads ", params$opt$genver," ", file_in, " > ", file_out, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  # Don't check for check2clean("$file_in") since we still need it for the variant calling
  print_done(file2process.my2);
  
  # direct command line call for testing other things:
  # java -Xmx4g -jar /home/ueb/snpEff/snpEff.jar countReads hg19 file_in.sam.sorted.noDup.bam > file_in.sam.sorted.noDup.bam.cr.txt

  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

##########################
### FUNCTION fun.exon.coverage
###
###   Gene Exons Coverage
##########################

fun.exon.coverage <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Gene Exons Coverage: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  # From http://www.bioconductor.org/help/workflows/variants/
  #   > ## get entrez ids from gene symbols
  #     > library(org.Hs.eg.db)
  #   > genesym <- c("TRPV1", "TRPV2", "TRPV3")
  #   > geneid <- select(org.Hs.eg.db, keys=genesym, keytype="SYMBOL",
  #                      +                  cols="ENTREZID")
  #   > geneid
  #   SYMBOL ENTREZID
  #   1  TRPV1     7442
  #   2  TRPV2    51393
  #   3  TRPV3   162514

  library(org.Hs.eg.db)
  genesym <- c("BRCA1", "BRCA2", "MSH2")
  geneid <- select(org.Hs.eg.db, keys=genesym, keytype="SYMBOL",
                    cols="ENTREZID")
  geneid
  
  library(paste("TxDb.Hsapiens.UCSC.", params$opt$genver, ".knownGene", sep="") )
  txdb <- paste("TxDb.Hsapiens.UCSC.", params$opt$genver, ".knownGene", sep="") #shorthand (for convenience)
  txdb
  exons(txdb)[1:10]
  head(exons(txdb))
  tail(exons(txdb))
  
#   file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam", sep="");
#   file_out = paste(file_in, ".samtools.var.raw.vcf", sep="");
#   command00 = "samtools"; # next command.
#   options00 = paste(" mpileup -uf ", params$path_genome, " ", file_in, " | bcftools view -vcg - >  ", file_out, sep="");
#   command = paste(command00, " ", options00, sep="");
#   system(command);
#   check2clean(file_in, file2process.my2);
#   print_done(file2process.my2);
  
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
  
  # From samtools documentation for mpileup
  # http://samtools.sourceforge.net/mpileup.shtml
  # ---------------------------
  # Apply -C50 to reduce the effect of reads with excessive mismatches. This aims to fix overestimated mapping quality and appears to be preferred for BWA-short.
  #
  
  # From samtools man pages in version 0.1.18 (r982:313) from ubuntu precise packages
  # ----------------------------------------------
  #
  #   mpileup   samtools  mpileup  [-EBug]  [-C  capQcoef] [-r reg] [-f in.fa] [-l list] [-M capMapQ] [-Q minBaseQ] [-q minMapQ] in.bam [in2.bam
  #                                                                                                                                      [...]]
  #   
  #   Generate BCF or pileup for one or multiple BAM files. Alignment records are grouped by sample identifiers in @RG  header  lines.
  #   If sample identifiers are absent, each input file is regarded as one sample.
  #   
  #   In  the pileup format (without -uor-g), each line represents a genomic position, consisting of chromosome name, coordinate, ref‐
  #   erence base, read bases, read qualities and alignment mapping qualities. Information on match, mismatch, indel, strand,  mapping
  #   quality  and  start  and  end of a read are all encoded at the read base column. At this column, a dot stands for a match to the
  #   reference base on the forward strand, a comma for a match on the reverse strand, a '>' or '<' for a reference skip, `ACGTN'  for
  #   a  mismatch  on the forward strand and `acgtn' for a mismatch on the reverse strand. A pattern `\+[0-9]+[ACGTNacgtn]+' indicates
  #   there is an insertion between this reference position and the next reference position. The length of the insertion is  given  by
  #   the integer in the pattern, followed by the inserted sequence. Similarly, a pattern `-[0-9]+[ACGTNacgtn]+' represents a deletion
  #   from the reference. The deleted bases will be presented as `*' in the following lines. Also at the read base  column,  a  symbol
  #   `^'  marks  the start of a read. The ASCII of the character following `^' minus 33 gives the mapping quality. A symbol `$' marks
  #   the end of a read segment.
  #   
  #   Input Options:
  #   
  #   -6        Assume the quality is in the Illumina 1.3+ encoding.  -A Do not skip anomalous read pairs in variant calling.
  #   
  #   -B        Disable probabilistic realignment for the computation of base alignment quality (BAQ). BAQ is the Phred-scaled  proba‐
  #   bility  of  a  read base being misaligned. Applying this option greatly helps to reduce false SNPs caused by misalign‐
  #   ments.
  #   
  #   -b FILE   List of input BAM files, one file per line [null]
  #   
  #   -C INT    Coefficient for downgrading mapping quality for reads containing excessive mismatches. Given  a  read  with  a  phred-
  #   scaled  probability  q  of  being  generated  from  the  mapped  position, the new mapping quality is about sqrt((INT-
  #   q)/INT)*INT. A zero value disables this functionality; if enabled, the recommended value for BWA is 50. [0]
  #   
  #   -d INT    At a position, read maximally INT reads per input BAM. [250]
  #   
  #   -E        Extended BAQ computation. This option helps sensitivity especially for MNPs, but may hurt specificity a little bit.
  #   
  #   -f FILE   The faidx-indexed reference file in the FASTA format. The file can be optionally compressed by razip.  [null]
  #   
  #   -l FILE   BED or position list file containing a list of regions or sites where pileup or BCF should be generated [null]
  #   
  #   -q INT    Minimum mapping quality for an alignment to be used [0]
  #   
  #   -Q INT    Minimum base quality for a base to be considered [13]
  #   
  #   -r STR    Only generate pileup in region STR [all sites]
  #   
  #   Output Options:
  #   
  #   
  #   -D        Output per-sample read depth
  #   
  #   -g        Compute genotype likelihoods and output them in the binary call format (BCF).
  #   
  #   -S        Output per-sample Phred-scaled strand bias P-value
  #   
  #   -u        Similar to -g except that the output is uncompressed BCF, which is preferred for piping.
  #   
  #   
  #   Options for Genotype Likelihood Computation (for -g or -u):
  #   
  #   
  #   -e INT    Phred-scaled gap extension sequencing error probability. Reducing INT leads to longer indels. [20]
  #   
  #   -h INT    Coefficient for modeling homopolymer errors. Given an l-long homopolymer run, the sequencing error of an indel of size
  #   s is modeled as INT*s/l.  [100]
  #   
  #   -I        Do not perform INDEL calling
  #   
  #   -L INT    Skip INDEL calling if the average per-sample depth is above INT.  [250]
  #   
  #   -o INT    Phred-scaled gap open sequencing error probability. Reducing INT leads to more indel calls. [40]
  #   
  #   -P STR    Comma  dilimited  list of platforms (determined by @RG-PL) from which indel candidates are obtained. It is recommended
  #   to collect indel candidates from sequencing technologies that have low indel error rate such as ILLUMINA. [all]
  #   
  #   

  #   See also calmd option in samtools:
  # --------------------------------------
  #     calmd     samtools calmd [-EeubSr] [-C capQcoef] <aln.bam> <ref.fasta>
  #     
  #     Generate  the  MD  tag.  If the MD tag is already present, this command will give a warning if the MD tag generated is different
  #   from the existing tag. Output SAM by default.
  #   
  #   OPTIONS:
  #     
  #     -A      When used jointly with -r this option overwrites the original base quality.
  #   
  #   -e      Convert a the read base to = if it is identical to the aligned reference base. Indel caller does not support the = bases
  #   at the moment.
  #   
  #   -u      Output uncompressed BAM
  #   
  #   -b      Output compressed BAM
  #   
  #   -S      The input is SAM with header lines
  #   
  #   -C INT  Coefficient to cap mapping quality of poorly mapped reads. See the pileup command for details. [0]
  #   
  #   -r      Compute the BQ tag (without -A) or cap base quality by BAQ (with -A).
  #   
  #   -E      Extended BAQ calculation. This option trades specificity for sensitivity, though the effect is minor.
  
  # See also calmd option in BCFTOOLS:
  # --------------------------------------
  #   BCFTOOLS COMMANDS AND OPTIONS
  #   
  #   view  bcftools view [-AbFGNQSucgv] [-D seqDict] [-l listLoci] [-s listSample] [-i gapSNPratio] [-t mutRate] [-p varThres] [-P prior] [-1 nGroup1] [-d minFrac] [-U nPerm] [-X permThres] [-T trioType] in.bcf [region]
  #   Convert between BCF and VCF, call variant candidates and estimate allele frequencies.
  #   
  #   Input/Output Options: 
  #     -A
  #   Retain all possible alternate alleles at variant sites. By default, the view command discards unlikely alleles.
  #   -b	 Output in the BCF format. The default is VCF.
  #   -D FILE	 Sequence dictionary (list of chromosome names) for VCF->BCF conversion [null]
  #   -F	 Indicate PL is generated by r921 or before (ordering is different).
  #   -G	 Suppress all individual genotype information.
  #   -l FILE	 List of sites at which information are outputted [all sites]
  #   -N	 Skip sites where the REF field is not A/C/G/T
  #   -Q	 Output the QCALL likelihood format
  #   -s FILE	 List of samples to use. The first column in the input gives the sample names and the second gives the ploidy, which can only be 1 or 2. When the 2nd column is absent, the sample ploidy is assumed to be 2. In the output, the ordering of samples will be identical to the one in FILE. [null]
  #   -S	 The input is VCF instead of BCF.
  #   -u	 Uncompressed BCF output (force -b).
  #   Consensus/Variant Calling Options: 
  #     -c
  #   Call variants using Bayesian inference. This option automatically invokes option -e.
  #   -d FLOAT	 When -v is in use, skip loci where the fraction of samples covered by reads is below FLOAT. [0]
  #   -e	 Perform max-likelihood inference only, including estimating the site allele frequency, testing Hardy-Weinberg equlibrium and testing associations with LRT.
  #   -g	 Call per-sample genotypes at variant sites (force -c)
  #   -i FLOAT	 Ratio of INDEL-to-SNP mutation rate [0.15]
  #   -p FLOAT	 A site is considered to be a variant if P(ref|D)<FLOAT [0.5]
  #   -P STR	 Prior or initial allele frequency spectrum. If STR can be full, cond2, flat or the file consisting of error output from a previous variant calling run.
  #   -t FLOAT	 Scaled muttion rate for variant calling [0.001]
  #   -T STR	 Enable pair/trio calling. For trio calling, option -s is usually needed to be applied to configure the trio members and their ordering. In the file supplied to the option -s, the first sample must be the child, the second the father and the third the mother. The valid values of STR are ‘pair’, ‘trioauto’, ‘trioxd’ and ‘trioxs’, where ‘pair’ calls differences between two input samples, and ‘trioxd’ (‘trioxs’) specifies that the input is from the X chromosome non-PAR regions and the child is a female (male). [null]
  #   -v	 Output variant sites only (force -c)
  #   Contrast Calling and Association Test Options: 
  #     -1 INT
  #   Number of group-1 samples. This option is used for dividing the samples into two groups for contrast SNP calling or association test. When this option is in use, the following VCF INFO will be outputted: PC2, PCHI2 and QCHI2. [0]
  #   -U INT	 Number of permutations for association test (effective only with -1) [0]
  #   -X FLOAT	 Only perform permutations for P(chi^2)<FLOAT (effective only with -U) [0.01]
  
  # changed on Jan 8th, 2013, to test the potential reduction of false positives (adding -C50 and -B , see documentation above)
  #options00 = paste(" mpileup -uf ", params$path_genome, " ", file_in, " | bcftools view -vcg - >  ", file_out, sep="");

  #  options00 = paste(" mpileup -uf -C50 -EDS -q50 -d10000  ", params$path_genome, " ", file_in, " | bcftools view -Avcg - >  ", file_out, sep="");
  # Since all this together doesn't work, let's try one by one.
#  options00 = paste(" mpileup -uf      -EDS", params$path_genome, " ", file_in, " | bcftools view -Avcg - >  ", file_out, sep="");
#  options00 = paste(" mpileup -uf           -q50 ", params$path_genome, " ", file_in, " | bcftools view -Avcg - >  ", file_out, sep="");
#  options00 = paste(" mpileup -uf                -d10000  ", params$path_genome, " ", file_in, " | bcftools view -Avcg - >  ", file_out, sep="");
#  options00 = paste(" mpileup -uf                         ", params$path_genome, " ", file_in, " | bcftools view -vcg - >  ", file_out, sep="");

  # Those params needed to be called after the file_in!!!
  options00 = paste(" mpileup -uf ", params$path_genome, " ", file_in, " -C50 -EDS -q50 -d10000 | bcftools view -Avcg - >  ", file_out, sep="");
  
  # Jan 9th, 2013: 
  ## added params in samtools mpileup: 
  ##  -C50: for short reads (recommended)
  ##  -E: extended BAQ calculation
  ##  -D: Output per-sample read depth
  ##  -S: write down strand-bias p-value: "Output per-sample Phred-scaled strand bias P-value"
  ##  -q: minimum mapping quality of the reads to be considered in the variant calling process. "Minimum mapping quality for an alignment to be used [0]"
  ##  -d: At a position, read maximally this amount of reads per input BAM
  ## added params in bcftools view: 
  ##  -A: Retain all possible alternate alleles at variant sites. By default, the view command discards unlikely alleles.
  
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
#  options00 = paste(" varFilter -Q 10 -d 15 -a 5 ", file_in, " > ", file_out, sep="");
  options00 = paste(" varFilter -Q1 -d15 -D10000000 -a2 -S1 ", file_in, " > ", file_out, sep="");
  command = paste(command00, " ", options00, sep="");
  system(command);
  check2clean(file_in, file2process.my2);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.gatk.combine.vcfs
###
###   Combine variants from various VCF through GATK's CombineVariants (ToDo)
###   See http://www.broadinstitute.org/gatk/gatkdocs/org_broadinstitute_sting_gatk_walkers_variantutils_CombineVariants.html
##########################

fun.gatk.combine.vcfs <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Combine variants from various VCF: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
# code to come here XXX
  # Taken from here: http://crocea.mednet.ucla.edu/research/variation/next-gen-sequence-how-to
  # Combine variants from various VCF through GATK's CombineVariants
  # combine two single-sample VCF into one
  # java  -Xmx2g -jar /home/crocea/script/gatk/dist/GenomeAnalysisTK.jar -R  /Network/Data/vervet/db/individual_sequence/524_superContigsMinSize2000.fasta -T CombineVariants  -o tmp/CombineSAMtools555_556.vcf -V:foo tmp/SelectSAMtoolsContig0_555_15_1987079_GA_vs_524.vcf -V:foo1 tmp/SelectSAMtoolsContig0_556_16_1985088_GA_vs_524.vcf

  #but see also full documentation here: http://www.broadinstitute.org/gatk/gatkdocs/org_broadinstitute_sting_gatk_walkers_variantutils_CombineVariants.html
  command = paste(command00, " ", options00, sep="");
  system(command);
  # check2clean(file_in, file2process.my2); #  # Commented out so that samtools standard .vcf files (and not only the converted to .vcf4 - .vcf.annovar - format) are also always kept.
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
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", file_in, " -format vcf4 -includeinfo --allallele --withzyg > ", file_out, " 2>> ", file_stderr, sep="");
  # OPTIONS from convert2annovar command
  #   --format <string>           input format (default: pileup)
  #   --outfile <file>            output file name (default: STDOUT)
  #   --snpqual <float>           quality score threshold in pileup file (default: 20)
  #   --snppvalue <float>         SNP P-value threshold in GFF3-SOLiD file (default: 1)
  #   --coverage <int>            read coverage threshold in pileup file (default: 0)
  #   --maxcoverage <int>         maximum coverage threshold (default: none)
  #   --includeinfo               include supporting information in output
  #   --chr <string>              specify the chromosome (for CASAVA format)
  #   --chrmt <string>            chr identifier for mitochondria (default: M)
  #   --altcov <int>              alternative allele coverage threshold (for pileup format)
  #   --fraction <float>          minimum allelic fraction to claim a mutation (for pileup/vcf4_indel format)
  #   --species <string>          if human, convert chr23/24/25 to X/Y/M (for gff3-solid format)
  #   --filter <string>           output variants with this filter (case insensitive, for vcf4 format)
  #   --confraction <float>       minimum consensus indel / all indel fraction (for vcf4 format)
  #   --allallele                 print all alleles when multiple calls are present (for vcf4 format)
  #   --withzyg                   print zygosity when -includeinfo is used (for vcf4 format)
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
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_annotate_variation, " -geneanno --buildver ", params$opt$genver, " ", file_in, " ", params$path_annotate_humandb, " 2>> ", file_stderr, sep="");
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
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_annotate_variation, " -filter --buildver ", params$opt$genver,
                    " -dbtype snp", params$opt$dbsnp," ", file_in, " ", params$path_annotate_humandb, " 2>> ", file_stderr, sep="");
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
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_summarize_annovar, " --buildver ", params$opt$genver,
                    " --verdbsnp ", params$opt$dbsnp," ", file_in, " ", params$path_annotate_humandb, " --outfile ", file_out, " 2>> ", file_stderr, sep="");
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
    file_in  = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.", params$opt$genver ,"_snp", params$opts$dbsnp, "_filtered", sep=""); 
    file_out = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.results.", params$startdate, ".", params$opt$label, ".txt", sep="");
    command00 = "grep"; # next command.
    options00 = paste(" '", params$opt$filter,"' ", file_in, " > ", file_out, sep="");
    # Remember that the values in the previous opt$filter variable needs to be like: 'BRCA1\|BRCA2' 
    # in order to end up performing a command like:
    # grep 'BRCA1\|BRCA2' dir_out/Gutierrez_A_*.exonic* 
    if (params$opt$summarize) # case to have a file with summarized annotations to search also for specific target genes 
    {
      #######################
      # 1st part- sample.*.exome_summary.csv
      #-------------------------------
      file_in_g_csv  = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.genome_summary.csv", sep=""); # summarize_annovar.pl adds the extension .genome_summary.csv (hardcoded in annovar).
      file_out_g_csv = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.genome_summary.fg.csv", sep=""); 
      command01 = "head -1";
      command02 = command00;
      options01 = paste(" ", file_in_g_csv, " > ", file_out_g_csv, sep="");
      options02 = paste(" '", params$opt$filter,"' ", file_in_g_csv, " >> ", file_out_g_csv, sep="");

      #######################
      # 2nd - sample.*.exome_summary.csv
      #-------------------------------
      file_in_e_csv  = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.exome_summary.csv", sep=""); # summarize_annovar.pl adds the extension .exome_summary.csv (hardcoded in annovar).
      file_out_e_csv = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.exome_summary.fg.csv", sep=""); 
      command03 = "head -1";
      command04 = command00;
      options03 = paste(" ", file_in_e_csv2, " > ", file_out_e_csv, sep="");
      options04 = paste(" '", params$opt$filter,"' ", file_in_e_csv, " >> ", file_out_e_csv, sep="");
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
    command = paste(command03, " ", options03, sep="");
    system(command);
    command = paste(command04, " ", options04, sep="");
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
### FUNCTION fun.variant.dbsnp.pre.snpeff
###
###     We annotate using dbSnp before using SnpEff in order to have 'known' and 'unknown' statistics in SnpEff's summary page.
###     Those stats are based on the presence of an ID field. If the ID is non-empty, then it is assumed to be a 'known variant'. 
###     From: http://snpeff.sourceforge.net/examples.html
##########################

fun.variant.dbsnp.pre.snpeff <- function(file2process.my2, step.my) {
  

#   SnpSift (related software)
#  From http://snpeff.sourceforge.net/SnpSift.html
#   SnpSift is a collection of tools to manipulate VCF (variant call format) files.

#   What can you do:
#     
#   Filter: You can filter using arbitrary expressions, for instance "(QUAL > 30) | (exists INDEL) | ( countHet() > 2 )". The actual expressions can be quite complex, so it allows for a lot of flexibility.
#   Annotate: You can add 'ID' from another database (e.g. variants from dbSnp)
#   CaseControl: You can compare how many variants are in 'case' and in 'control' groups. Also calculates p-values (Fisher exact test).
#   Intervals: Filter variants that intersect with intervals.
#   Intervals (intidx): Filter variants that intersect with intervals. Index the VCF file using memory mapped I/O to speed up the search. This is intended for huge VCF files and a small number of intervals to retrieve.
#   Join: Join by generic genomic regions (intersecting or closest).
#   RmRefGen: Remove reference genotype (i.e. replace '0/0' genotypes by '.')
#   TsTv: Calculate transiton to transversion ratio.
#   Extract fields: Extract fields from a VCF file to a TXT (tab separated) format.
#   Variant type: Adds SNP/MNP/INS/DEL to info field. It also adds "HOM/HET" if there is only one sample.
#   GWAS Catalog: Annotate using GWAS Catalog.
#   dbNSFP: Annotate using dbNSFP: The dbNSFP is an integrated database of functional predictions from multiple algorithms (SIFT, Polyphen2, LRT and MutationTaster, PhyloP and GERP++, etc.) 

#   # Download and uncompress dbSnp database.
#   wget -O dbSnp.vcf.gz ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606/VCF/00-All.vcf.gz
#   gunzip dbSnp.vcf.gz
#   
#   # Annotate ID field using dbSnp
#   java -jar SnpSif.jar annotate -v dbSnp.vcf.gz file.vcf > file.dbSnp.vcf
#   
#   2-) Annotate using SnpEff:
#   
#   # Do this only if you don't already have the database installed.
#   java -jar snpEff.jar download -v GRCh37.66
# 
# # Annotate the file
# java -Xmx4g -jar snpEff.jar eff -v GRCh37.66 file.dbSnp.vcf > file.eff.vcf
# 
# 3-) Filter out variants that have a non-empty ID field. These variants are the ones that are NOT in dbSnp, since we annotated the ID field using rs-numbers from dbSnp in step 1.
# 
# java -jar SnpSift.jar filter -f file.eff.vcf "! exists ID" > file.eff.not_in_dbSnp.vcf
# 
# Note: The expression using to filter the file is "! exists ID". This means that the ID field does not exists (i.e. the value is empty) which is represented as a dot (".") in a VCF file. 

  
}

##########################
### FUNCTION fun.variant.eff.report
###
###   Report the effects from the variants detected, with snpEff, which is a variant annotation and effect prediction tool.
###   It annotates and predicts the effects of variants on genes (such as amino acid changes).
###   See http://snpeff.sourceforge.net/manual.html
###
###   Typical usage :
###
###   Input: The inputs are predicted variants (SNPs, insertions, deletions and MNPs). 
###     The input file is usually obtained as a result of a sequencing experiment, and it is usually in variant call format (VCF).
###   Output: SnpEff analyzes the input variants. It annotates the variants and calculates the effects they produce on known genes (e.g. amino acid changes).
###     A list of effects and annotations that SnpEff can calculate can be found here.
##########################

fun.variant.eff.report <- function(file2process.my2, step.my) {

#   #Manual debugging - ini
#   file2process.my2 <-"s_1_m11_143b_merged12.sam"
#   step.my <- data.frame(10, 0)
#   colnames(step.my) <- c("n","tmp")
#   step.my$tmp <- 0
#   #Manual debugging - end
  
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Variant Annotation & Effect prediction with snpEff: ", file2process.my2, " ###\n", sep=""), file2process.my2);

  file_in  = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.noDup.bam.samtools.var.filtered.vcf", sep=""); 
  file_out = paste(params$directory_out, "/", file2process.my2, ".f.snpEff.", params$opt$snpeff.of, sep=""); 
  file_out_base = paste(params$directory_out, "/", file2process.my2, ".f.snpEff", sep="");     
  command00 = "java -jar"; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_snpEff, "/snpEff.jar -c ", params$path_snpEff, "/snpEff.config ", params$opt$genver," ", file_in, 
                    " -a 0 -i vcf -o ", params$opt$snpeff.of," -chr chr -stats ", file_out_base,"_summary.html > ", file_out,
                    " 2>> ", file_stderr, sep="");

  # Documentation from snpEff: "Calculate variant effects: snpEff [eff]" http://snpeff.sourceforge.net/manual.html 
  #
  #   Input file: Default is STDIN
  #   
  #   Options:
  #   -a , -around            : Show N codons and amino acids around change (only in coding regions). Default is 0 codons.
  #   -i format               : Input format [ vcf, txt, pileup, bed ]. Default: VCF.
  #   -o format               : Ouput format [ txt, vcf, gatk, bed, bedAnn ]. Default: VCF.
  #   -interval               : Use a custom interval file (you may use this option many times)
  #   -chr string             : Prepend 'string' to chromosome name (e.g. 'chr1' instead of '1'). Only on TXT output.
  #   -s,  -stats             : Name of stats file (summary). Default is 'snpEff_summary.html'
  #   -t                      : Use multiple threads (implies '-noStats'). Default 'off'
  
  
  #   Sequence change filter options:
  #     -del                    : Analyze deletions only
  #   -ins                    : Analyze insertions only
  #   -hom                    : Analyze homozygous variants only
  #   -het                    : Analyze heterozygous variants only
  #   -minQ X, -minQuality X  : Filter out variants with quality lower than X
  #   -maxQ X, -maxQuality X  : Filter out variants with quality higher than X
  #   -minC X, -minCoverage X : Filter out variants with coverage lower than X
  #   -maxC X, -maxCoverage X : Filter out variants with coverage higher than X
  #   -nmp                    : Only MNPs (multiple nucleotide polymorphisms)
  #   -snp                    : Only SNPs (single nucleotide polymorphisms)
     
  command = paste(command00, " ", options00, sep="");
  system(command);
  # Show errors (if any)
  obj.file_stderr <- read.delim(file_stderr, header = FALSE, sep=":")
  obj.file_stderr <- unlist(obj.file_stderr, use.names = FALSE)[obj.file_stderr$V1=="Error"]
  # If we found any error, print it.
  if (length(obj.file_stderr) > 2) {
    print_doc(obj.file_stderr[1:2],  file2process.my2);
    # If this shows NA, then there was only 1 error message
    print_doc(obj.file_stderr[3:4],  file2process.my2);  
  } # Display the error message
  # # We don't do check2clean here  since the output are results
  print_done(file2process.my2);
  
#   # a mà la instrucció al mainhead és:
# Estudi 293
# java  -Xmx4g -jar /home/ueb/snpEff/snpEff.jar  -c /home/ueb/snpEff/snpEff.config hg19 /mnt/magatzem02/tmp/run_sara_293a/dir_out_293a/s_4_m11_146b_merged12.sam.sorted.noDup.bam.samtools.var.filtered.vcf -a 0 -i vcf -o vcf -chr chr -stats /mnt/magatzem02/tmp/run_sara_293a/dir_out_293a/s_4_m11_146b_merged12.f.snpEff_summary.html > /mnt/magatzem02/tmp/run_sara_293a/dir_out_293a/s_4_m11_146b_merged12.f.snpEff.vcf
# java  -Xmx4g -jar /home/ueb/snpEff/snpEff.jar  -c /home/ueb/snpEff/snpEff.config hg19 /mnt/magatzem02/tmp/run_sara_293a/dir_out_293a/s_3_m11_145b_merged12.sam.sorted.noDup.bam.samtools.var.filtered.vcf -a 0 -i vcf -o vcf -chr chr -stats /mnt/magatzem02/tmp/run_sara_293a/dir_out_293a/s_3_m11_145b_merged12.f.snpEff_summary.html > /mnt/magatzem02/tmp/run_sara_293a/dir_out_293a/s_3_m11_145b_merged12.f.snpEff.vcf
#Estudi 207
# java  -Xmx4g -jar /home/ueb/snpEff/snpEff.jar  -c /home/ueb/snpEff/snpEff.config hg19 /mnt/magatzem02/tmp/run_sara_207v05/G_A_I.sam.sorted.noDup.bam.samtools.var.filtered.vcf -a 0 -i vcf -o vcf -chr chr -stats /mnt/magatzem02/tmp/run_sara_207v05/G_A_I.f.snpEff_summary.html > /mnt/magatzem02/tmp/run_sara_207v05/G_A_I.f.snpEff.vcf
# java  -Xmx4g -jar /home/ueb/snpEff/snpEff.jar  -c /home/ueb/snpEff/snpEff.config hg19 /mnt/magatzem02/tmp/run_sara_207v05/G_A_S.sam.sorted.noDup.bam.samtools.var.filtered.vcf -a 0 -i vcf -o vcf -chr chr -stats /mnt/magatzem02/tmp/run_sara_207v05/G_A_S.f.snpEff_summary.html > /mnt/magatzem02/tmp/run_sara_207v05/G_A_S.f.snpEff.vcf
# java  -Xmx4g -jar /home/ueb/snpEff/snpEff.jar  -c /home/ueb/snpEff/snpEff.config hg19 /mnt/magatzem02/tmp/run_sara_207v05/G_B_I.sam.sorted.noDup.bam.samtools.var.filtered.vcf -a 0 -i vcf -o vcf -chr chr -stats /mnt/magatzem02/tmp/run_sara_207v05/G_B_I.f.snpEff_summary.html > /mnt/magatzem02/tmp/run_sara_207v05/G_B_I.f.snpEff.vcf
# java  -Xmx4g -jar /home/ueb/snpEff/snpEff.jar  -c /home/ueb/snpEff/snpEff.config hg19 /mnt/magatzem02/tmp/run_sara_207v05/G_B_S.sam.sorted.noDup.bam.samtools.var.filtered.vcf -a 0 -i vcf -o vcf -chr chr -stats /mnt/magatzem02/tmp/run_sara_207v05/G_B_S.f.snpEff_summary.html > /mnt/magatzem02/tmp/run_sara_207v05/G_B_S.f.snpEff.vcf
  
  #   ####### test in
#   #   tmp_file_in = "/mnt/magatzem02/tmp/run_sara_293a/dir_out_293a/s_4_m11_146b_merged12.f.vcf4"; 
#      tmp_file_in = "/mnt/magatzem02/tmp/run_sara_293a/dir_out_293a/s_4_m11_146b_merged12.sam.sorted.noDup.bam.samtools.var.filtered.vcf"; 
#     tmp_file_out = "/mnt/magatzem02/tmp/run_sara_293a/dir_out_293a/00snpeff_146b_merged12.vcf"; 
#     tmp_command00 = "java -Xmx4g -jar"; # next command. -Xmx4g stands for indicating the computer to use at least 4Gb of RAM because the Human Genome Database is large
# #   #tmp_file_stderr = "/mnt/magatzem02/tmp/run_sara_293a/dir_out_293a/log.121207.sg293a.s_4_m11_146b_merged12.txt";
#    tmp_file_stderr = "/mnt/magatzem02/tmp/run_sara_293a/dir_out_293a/00snpeff_log.txt";
#    tmp_options00 = paste("/home/ueb/snpEff/snpEff.jar -c /home/ueb/snpEff/snpEff.config hg19 ", tmp_file_in, 
#                      " -a 0 -i vcf -o vcf -chr chr -stats /mnt/magatzem02/tmp/run_sara_293a/dir_out_293a/snpEff_summary.html > ", tmp_file_out, " 2>> ", tmp_file_stderr, sep="");
#    tmp_command = paste(tmp_command00, " ", tmp_options00, sep="");
#    system(tmp_command);
####### test out

  # Other stuff snpEff related
  # v3.1 (2012-11): SnpEff 'countReads' count number of reads and bases (form a BAM file) on each gene, transcript, exon, intron, etc. 
  # See https://ueb.vhir.org/PEEVA+4+Execuci%C3%B3+detalls&no_bl=y#Other_calculations_for_the_report
  
#   file.in.report2 <- paste(params$directory_out, "/", "s_7_m11_149b_merged12.f.snpEff.txt", sep=""); 
#   r2_data <- read.delim(file.in.report2, skip="2", header=TRUE, comment.char="")
#   table(r2_data$Exon_Rank, useNA="ifany")
#   table(r2_data$Exon_Rank, addNA(r2_data$Quality))
#   stem(r2_data$Exon_Rank)
#   exon.max <- max(r2_data$Exon_Rank, na.rm=TRUE)
#   hist(r2_data$Exon_Rank, breaks=exon.max, main="Exons holding variants", xlab="Exon Number", )
#   
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
  
  # TODO XXX
}


##########################
### FUNCTION fun.grep.post.snpeff.variants
###
###   Select variants (based on grep calls) for the target genes
###   in the vcf file after the snpEff report is called 

### XXX to be revised
##########################

fun.grep.post.snpeff.variants <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Select variants for the target genes based on grep calls after the snpEff report: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  if (!is.null(params$opt$filter) && (params$opt$filter != "")) {
    #######################
    # 1st part - sample.*.snpEff.[params$opt$snpeff.of] (= .vcf, .txt, ...)
    #-------------------------------
    file_in  = paste(params$directory_out, "/", file2process.my2, ".f.snpEff.", params$opt$snpeff.of, sep=""); 
    file_out = paste(params$directory_out, "/", file2process.my2, ".f.snpEff.fg.", params$opt$snpeff.of, sep=""); 
    command00 = "grep -P"; # next command.
    options00 = paste(" '", params$opt$filter,"' ", file_in, " > ", file_out, sep="");
    # Remember that the values in the previous opt$filter variable needs to be like: 'BRCA1\|BRCA2' 
    # in order to end up performing a command like:
    # grep 'BRCA1\|BRCA2' dir_out/Gutierrez_A_*.exonic* 
    command01 = "grep ^\\#";
    command02 = command00;
    options01 = paste(" ", file_in, " > ", file_out, sep="");
    options02 = paste(" '", params$opt$filter,"' ", file_in, " >> ", file_out, sep="");
    
    command = paste(command01, " ", options01, sep="");
    system(command);
    
    command = paste(command02, " ", options02, sep="");
    system(command);

    #######################
    # 2nd part- sample.*.snpEff_summary.genes.txt
    #-------------------------------
    file_in  = paste(params$directory_out, "/", file2process.my2, ".f.snpEff_summary.genes.txt", sep=""); 
    file_out = paste(params$directory_out, "/", file2process.my2, ".f.snpEff_summary.genes.fg.txt", sep=""); 
    command00 = "grep"; # next command.
    options00 = paste(" '", params$opt$filter,"' ", file_in, " > ", file_out, sep="");
    # Remember that the values in the previous opt$filter variable needs to be like: 'BRCA1\|BRCA2' 
    # in order to end up performing a command like:
    # grep 'BRCA1\|BRCA2' dir_out/Gutierrez_A_*.exonic* 
    command01 = "grep ^\\#";
    command02 = command00;
    options01 = paste(" ", file_in, " > ", file_out, sep="");
    options02 = paste(" '", params$opt$filter,"' ", file_in, " >> ", file_out, sep="");
    
    command = paste(command01, " ", options01, sep="");
    system(command);
    
    command = paste(command02, " ", options02, sep="");
    system(command);
    
  } else { # skip the searching for specific target genes 
    
    command00 = "echo '  ...skipped...'"; # next command.
    options00 = "";
    command = paste(command00, " ", options00, sep="");
    system(command);
  }
  
  #check2clean(file_in, file2process.my2);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

