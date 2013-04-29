#!/home/ueb/repo/peeva/eva_analysis_functions.R
# revision $Revision-Id: $ - $Date: $ by $Commiter: $
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
  } else if (file.exists(var)) { # clean temporary files not from this but from the previous step
    print_mes(paste("\t\t\t\t\tOk. Removing temporary file ", var, "\n\n", sep=""), filename.my1);
    system(paste("rm  ", var, sep=""));
  } else {
    print_mes(paste("\t\t\t\t\t[Warning] The file to be removed is missing: ", var, "\n\n", sep=""), filename.my1);
  }
}

##########################
### FUNCTION check2showcommand
###
###   Check if the user wants to show the exact commands run
##########################

check2showcommand <- function(option.my, command.my, file.my)
{
  # show exact commands run if requested
  if ( option.my ) { 
    mess.my <- paste("\n[COMMAND RUN]----->\n", command.my, "\n<-----\n", sep="");
    print_mes(mess=mess.my, filename.my1=file.my)
  } 
}

##########################
### FUNCTION w.output.samples
###
### 	Write output to log files on disk
##########################

w.output.samples <- function(mess, filename.my2)
{
  # Make the abs path the file to write to, with the sample to process in it
  file2write2sample <- paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", filename.my2, ".txt", sep="")
  # Make the abs path the file to write to, without the filename.my2 added which is a duplicate, it seems, for processes run only once for all samples such as converting file list, indexing the genome, etc.
  file2write2other <- paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".run.txt", sep="")
  if (file.exists(file2write2sample)) {
    write(mess, file=file2write2sample, append = TRUE, sep = "");
  } else if (file.exists(file2write2other)) {
    write(mess, file=file2write2other, append = TRUE, sep = "");
  } else {
    print(paste("This file doesn't exist: ", file2write2sample, " (nor without '", filename.my2 , "') ", sep="") )
  }
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
  if (file.exists(paste(params$filename_list, ".lock", sep=""))){
    print_mes(paste(now(), "Removing lock: ", params$opt$output, "/", "log.",params$startdate, ".", params$opt$label, ".", filename.my2, ".lock \n", sep=""), filename.my2)
    file2remove <- paste(params$opt$output, "/", "log.",params$startdate, ".", params$opt$label, ".", filename.my2, ".lock", sep="")
    # I suppress warnings here since the file can be removed by one node, and the other nodes (when run in parallel)
    # that the file to delete does not exist (any more).
    suppressWarnings(system(paste("rm ", file2remove, sep=""), TRUE))
  }
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
  #params$p_subject <- paste("EVA Pipeline run finished: ", params$p_label, sep="")
  #params$p_body <- paste(params$p_subject, " _ See some log information attached", sep="")                   
  command <- paste("sendEmail -f ", params$p_from, " -t ", params$p_to, " -u \"", params$p_subject,
               "\" -m \"", params$p_body, "\" -s ", params$p_smtp, " -a \"", attachmentPath,
               "\" >> \"", attachmentPath, "\" ", sep="");
  check2showcommand(params$opt$showc, command, routlogfile);
  system(command);
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
  check2showcommand(params$opt$showc, command, file2process.my2);
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

  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Map against reference genome: Index the reference genome (if needed) ###\n", sep=""), "run");
  if ((params$opt$index) & (step.my$n == 0)) { # case to index the reference genome (time consuming, do only when really needed as requested)
    # Index the reference genome, if requested with argument -n and only for the first file if more than one sample to process
    command00 <- "bwa index"; # next command
    options00 <- paste(" -p ", params$p_label, " -a bwtsw ", params$path_genome, " >> ", filename.my1, sep="");
  } else	{ # skip the indexing of the reference genome
    command00 <- "echo '  ...skipped...'"; # next command.
    options00 <- "";
  }
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, filename.my1);
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
  ## Manual debugging
  # step.my <- data.frame(1, 0)
  # colnames(step.my) <- c("n","tmp")
  # file2process.my2 <- params$file_list[step.my$n]

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
  ##
  ## BWA is a fast light-weighted tool that aligns relatively short sequences (queries) to a sequence database (targe), such as the human reference genome. 
  ## It implements two different algorithms, both based on Burrows-Wheeler Transform (BWT). 
  ## The first algorithm is designed for short queries up to ~200bp with low error rate (<3%). 
  ##   It does gapped global alignment w.r.t. queries, supports paired-end reads, and is one of the fastest short read alignment algorithms to date while also visiting suboptimal hits. 
  ## The second algorithm, BWA-SW, is designed for long reads with more errors. It performs heuristic Smith-Waterman-like alignment to find high-scoring local hits (and thus chimera). 
  ##   On low-error short queries, BWA-SW is slower and less accurate than the first algorithm, but on long queries, it is better
  ##
  ## Parallelization in BWA
  ## Both alignment processes (for short reads with bwa aln, and for long reads with bwa bwasw) accept working in multi-thread (parallel) mode.
  ## As of Feb'2013 we are not yet using the multithread mode for single sample, but we are (usually) running the alignment (in serial, mono-thread each) of several samples in parallel 
  ##
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
    options00 = paste(" -t ", params$opt$cpus, " ", params$path_genome, " ", file_in, " > ", file_out, " 2>> ", file_stderr, sep="");
    command = paste(command00, " ", options00, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
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
    check2showcommand(params$opt$showc, command, file2process.my2);
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
    #bwa aln -t 2 database.fasta short_read1.fastq > aln_sa1.sai
    #bwa aln -t 2 database.fasta short_read2.fastq > aln_sa2.sai
    file_out = paste(params$directory_out, "/", file2process.my2, ".sai", sep="");
    command00 = "bwa aln"; # next command.
    # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
    file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
    options00 = paste(" -t ", params$opt$cpus, " ", params$path_genome, " ", file_in, " > ", file_out,  " 2>> ", file_stderr, sep="");
    command = paste(command00, " ", options00, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
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
      options00 = paste(params$path_genome, " ", file_in_sai1, " ", file_in_sai2, " ", file_in_fq1, " ", file_in_fq2, " ",
                        # The following -r param in bwa is only in recent versions of bwa, not in 0.5.5.x which is the latest one supported in ubuntu lucid 10.04 repositories as of 2013 January at least.
                        # So when running in servers with Ubuntu Lucid or similar, keep this following line related to the "-r" param commented out. 
                        # This param is needed mainly for later usage with GATK. Otherwise, it seems safely removable.
                        #                        " -r \"@RG\tID:", file2process.my2, "\tLB:", file2process.my2, "\tPL:ILLUMINA\tSM:", file2process.my2, "\"",
                        " > ", file_out,       " 2>> ", file_stderr, sep="");
      command = paste(command00, " ", options00, sep="");
      check2showcommand(params$opt$showc, command, file2process.my2);
      # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
      start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
      # Show the duration of this subprocess
      cat("\n 2nd part (sampe) - Relative duration since last step: "); print(duration); cat("\n")

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
    options00 = paste("-t ", params$opt$cpus, " ", params$path_genome, " ", file_in,
                      " -r \"@RG\tID:", file2process.my2, "\tLB:", file2process.my2, "\tPL:Roche454\tSM:", 
                      file2process.my2, "\"", " > ", file_out,  " 2>> ", file_stderr, sep="");
    command = paste(command00, " ", options00, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
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
#   file2process.my2 <- "sample_a_1_sequence.fastq"
#   #file2process.my2 <- "sample_a_merged12.sam"
#   step.my <- data.frame(1, 0)
#   colnames(step.my) <- c("n","tmp")
#   step.my$tmp <- 0
#   # Manual debuging - end
  
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Convert the file list regarding paired-end merged .sam files: ###\n", sep=""), file2process.my2);
  
  # When input files contain paired end reads (_pe), a temporal (_tmp) file name will be used first until we combine the data from both strands
  params$filename_list <- paste(params$opt$output, "/", "log.",params$startdate, ".", params$opt$label, ".input_merged12_pe.txt", sep="")
  # Name of the sam file to start with
  pattern_sam <- "merged12.sam"
  # Check if there is at least one merged12.sam file
  #list.files(path=params$opt$input, pattern=pattern_sam)
  if (length(list.files(path=params$opt$input, pattern=pattern_sam, full.names=F, recursive=F, ignore.case=F)) > 0) {
      files_sam = paste(params$opt$input, "/", "*merged12.sam", sep="");
      # Get the list of files in "input" directory through a system call to "ls *" and save the result to a file on disk
      system(paste("ls ", files_sam, " > ", params$filename_list, sep=""), TRUE)
      
  } else if (length(list.files(path=params$opt$output, pattern=pattern_sam, full.names=F, recursive=F, ignore.case=F)) > 0) {
      files_sam <- paste(params$opt$output,"/", "*merged12.sam", sep="")
      # Get the list of files in "input" directory through a system call to "ls *" and save the result to a file on disk
      system(paste("ls ", files_sam, " > ", params$filename_list, sep=""), TRUE)
  } else {
      print_doc(paste(" [Error] ", step.my$n, ".", step.my$tmp, ". No *merged12.sam files found neither in input nor ouptput dirs ###\n", sep=""), file2process.my2);
  }
  
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
### FUNCTION fun.tgenes.generate.bed.file
###
###     Generate a bed file with the intersecting intervals (genomic regions) for the target genes
###     Needed to filter vcf files with SnpSift.jar for target genes before reunning custom snpEff Report 

##########################
# x stands for the list of target genes to process. Separated by spaces

fun.tgenes.generate.bed.file <- function(step.my, file2process.my2, x) {
  
  # Manual debugging
  # file2process.my2 <- abs_routlogfile
  # x <- "BRCA1 BRCA2 CHEK2 PALB2 BRIP1 TP53 PTEN STK11 CDH1 ATM BARD1 APC MLH1 MRE11A MSH2 MSH6 MUTYH NBN PMS1 PMS2 RAD50 RAD51D RAD51C XRCC2 UIMC1 FAM175A ERCC4 RAD51 RAD51B XRCC3 FANCA FANCB FANCC FANCD2 FANCE FANCF FANCG FANCI FANCL FANCM SLX4 CASP8 FGFR2 TOX3 MAP3K1 MRPS30 SLC4A7 NEK10 COX11 ESR1 CDKN2A CDKN2B ANKRD16 FBXO18 ZNF365 ZMIZ1 BABAM1 LSP1 ANKLE1 TOPBP1 BCCIP TP53BP1"
  # step.my <- data.frame(0, 0)
  # colnames(step.my) <- c("n","tmp")
  
  # update step number
  #step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "a. Generate a bed file with the genomic regions for the target genes (...my_genes.bed) ###\n", sep=""), "run");
  
  # Get a list of the genetic intervals of those genes (in BED format): my_genes.bed
  # Using Bioconductor R package biomaRt
  
  # Install library if necessary
  if(!require(biomaRt)){ biocLite("biomaRt") }
  # Load libraries 
  library(biomaRt, quietly = TRUE)
  
  # Load the mart for ensembl
  mart <- useMart(biomart = "ensembl", dataset = "hsapiens_gene_ensembl")
  
  ## Manual debugging
  #listMarts()
  # g = getGene( id = "BRCA1", type = "hgnc_symbol", mart = mart)
  # show(g)
  #x <- "BRCA1 BRCA2 CHEK2 PALB2 BRIP1 TP53 PTEN STK11 CDH1 ATM BARD1 APC MLH1 MRE11A MSH2 MSH6 MUTYH NBN PMS1 PMS2 RAD50 RAD51D RAD51C XRCC2 UIMC1 FAM175A ERCC4 RAD51 RAD51B XRCC3 FANCA FANCB FANCC FANCD2 FANCE FANCF FANCG FANCI FANCL FANCM SLX4 CASP8 FGFR2 TOX3 MAP3K1 MRPS30 SLC4A7 NEK10 COX11 ESR1 CDKN2A CDKN2B ANKRD16 FBXO18 ZNF365 ZMIZ1 BABAM1 LSP1 ANKLE1 TOPBP1 BCCIP TP53BP1"
  
  tgenes <- unlist(strsplit(x, split=" ", fixed=TRUE))
  #length(tgenes)
  #head(tgenes)
  tgenes.o <- tgenes[order(tgenes)]
  #head(tgenes2)
  
  tg = getGene( id = tgenes, type = "hgnc_symbol", mart = mart)
  #dim(tg)
  #head(tg)
  # tg$hgnc_symbol
  # tg$ensembl_gene_id
  
  # Remove the LRG (Locus Reference Genome) names, since they show up as duplicated records to a differnt chromosome called LR_xxx, besides the other record for the normal chromosome. 
  #   "LRG sequences provide a stable genomic DNA framework for reporting mutations with a permanent ID and core content that never changes."
  #   See http://www.lrg-sequence.org/
  tg.lrg.idx <- grep("LRG", tg$chromosome_name, fixed=T)
  # Remove those records only if someting is found
  if (length(tg.lrg.idx) > 0) {
    # Keep only the list of target genes without lrg 
    tg.nolrg <- tg[-tg.lrg.idx,]
  } else {
    tg.nolrg <- tg
  }

  # Sort results by gene name
  tg.nolrg.o <- tg.nolrg[order(tg.nolrg$hgnc_symbol),]
  #head(tg.nolrg.o)
  #head(tg.nolrg.o$hgnc_symbol)
  #str(tg2)
  #?match
  
  # Get the indexes of genes from the ordered list of target genes which are not found in the results 
  tg.nolrg.o.new.idx <- which(!tgenes.o %in% tg.nolrg.o$hgnc_symbol)
  # If there are missmatches between target gene names, display a warning and display them
  if ( length(tg.nolrg.o.new.idx) > 0) {
    print("Warning: some missmatch found between target gene names and their corresponding matches through biomaRt")
    # Display those gene names
    tgenes.o[tg.nolrg.o.new.idx]
  }
  #dim(tg.nolrg.o)
  #tg.nolrg.o
  #row.names(tg.nolrg.o)
  #dim(tg.nolrg.o)[1]
  score <- rep(0, dim(tg.nolrg.o)[1])
  #class(foo)
  tg.nolrg.o <- cbind(tg.nolrg.o, score)
  #str(tg.nolrg.o)
  
  # Substep 1. Generate the bed file
  # --------------------------------
  # Get just the columns of interest to generate a valid BED file. 
  # See http://genome.ucsc.edu/FAQ/FAQformat.html#format1
  my_bed <- tg.nolrg.o[, c("chromosome_name", "start_position", "end_position", "hgnc_symbol", "score", "strand")]
  
  # I sort by Chromosome first, and by Start position later. 
  # In addition, I remove warnings explicitly because I'm coercing chr names to integers, adn Chr X is not an integer.
  my_bed <- suppressWarnings(my_bed[order(as.integer(my_bed$chromosome_name), as.integer(my_bed$start_position)), ])
  
  # Add chr prefix to the chromosome names for consistency with the other parts of the pipeline
  my_bed$chromosome_name <- paste("chr", my_bed$chromosome_name, sep="")
  
  # Rename colname "chromosome_name" with "#chromosome_name"
  colnames(my_bed) <- c("#chromosome_name", "start_position", "end_position", "hgnc_symbol", "score", "strand")
  # ATTENTION: 
  # I'm writing here a number sign (#) prepending "chromosome_name" in order to have titles added to the file, 
  # (see below the function "write.table" with argument "col.names=T") but not breaking SnpSift.jar intidx command
  # (which doesn't like to have column names in the first row but seems to accept comments starting with "#")
  
  # Write the results to the file my_bed.txt
  # The params to define file_my_bed here are not preppended with params$ (startdate instead of params$startdate, etc)
  # because this function is called from the main program, not parallelized, so not in the nodes but the master computer/cpu.
  file_my_bed = file=paste(params$log.folder,"/", startdate, ".", opt$label,".my_genes.bed", sep="")
  
  # I remove the warning messages from this call (below) because of the coercion type of message (harmless, afaik, but annoying and polluting output in log files)
  # Write all: data with column names in one go
  suppressWarnings(write.table(my_bed, file=file_my_bed, append=F, quote=F, sep="\t", row.names=F, col.names=T))
  
  # Substep 2. Generate a file with both gene names: hgnc_symbol and ensembl_gene_id, for later optional re-use at count reads
  # ------------------------------------------------------------------------------------------------------------------------------
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "b. Generate a file with both GeneSymbols and Ensembl Gene Id (...my_genes.names.txt) ###\n", sep=""), "run");
  
  # Get just the columns of interest to generate a valid BED file. 
  # See http://genome.ucsc.edu/FAQ/FAQformat.html#format1
  my_genenames <- tg.nolrg.o[, c("chromosome_name", "start_position", "end_position", "hgnc_symbol", "score", "strand", "ensembl_gene_id")]
  
  # I sort by Chromosome first, and by Start position later. 
  # In addition, I remove warnings explicitly because I'm coercing chr names to integers, adn Chr X is not an integer.
  my_genenames <- suppressWarnings(my_genenames[order(as.integer(my_genenames$chromosome_name), as.integer(my_genenames$start_position)), ])
  
  # Add chr prefix to the chromosome names for consistency with the other parts of the pipeline
  my_genenames$chromosome_name <- paste("chr", my_genenames$chromosome_name, sep="")
  
  # Write the results to the file my_genes.names.txt
  # The params to define file_my_genes.names here are not preppended with params$ (startdate instead of params$startdate, etc)
  # because this function is called from the main program, not parallelized, so not in the nodes but the master computer/cpu.
  file_my_genenames = file=paste(params$log.folder,"/", startdate, ".", opt$label,".my_genes.names.txt", sep="")
  
  # I remove the warning messages from this call (below) because of the coercion type of message (harmless, afaik, but annoying and polluting output in log files)
  # Write all: data with column names in one go
  suppressWarnings(write.table(my_genenames, file=file_my_genenames, append=F, quote=F, sep="\t", row.names=F, col.names=T))
  
  # End substep 2
  # ------------------
  
  # # We don't do check2clean here  since the output are results
  print_done("run");
  
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
  
}


##########################
### FUNCTION fun.genedata
###
###   Gene Exons Coverage and other gene data
###
### April 2013: Unfinsihed implementation based on R packages.
###   So far it only creates as reference file with all data for the target genes in the run
### Alternatively, this could be performed with bedtools directly. 
### See http://bedtools.readthedocs.org/en/latest/content/advanced-usage.html#computing-the-coverage-of-bam-alignments-on-exons
##########################

fun.genedata <- function(file2process.my2, step.my) {
  # file2process.my2 <- abs_routlogfile
  # step.my$tmp <- 1
  # update step number
  #step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Gene Genedata for the target genes (.my_genes.data.csv): ", file2process.my2, " ###\n", sep=""), "run");
  
  if(!require(Rsamtools)){ biocLite("Rsamtools") }
  library(Rsamtools, quietly = TRUE)
  
  if(!require(GenomicFeatures)){ biocLite("GenomicFeatures") }
  library(GenomicFeatures, quietly = TRUE)
  
  # Check for the TxDb.Hsapiens.UCSC.hg19.knownGene or TxDb.Hsapiens.UCSC.hg18.knownGene dynamically 
  # based on the contents of param opt$genver, where hg19 or hg18 is set. 
  # See below after the opt$genver is defined
  
  if(!require(org.Hs.eg.db)){ biocLite("org.Hs.eg.db") }
  library(org.Hs.eg.db, quietly = TRUE)
  
  # if(!require(annotate)){ biocLite("annotate") }
  # library(annotate, quietly = TRUE)
  
  # From http://www.bioconductor.org/help/workflows/variants/
  #   > ## get entrez ids from gene symbols
  #     > library(org.Hs.eg.db)
  #   > genesym <- c("TRPV1", "TRPV2", "TRPV3")
  #   > geneid <- select(org.Hs.eg.db, keys=genesym, keytype="SYMBOL", cols="ENTREZID")
  #   > geneid
  #   SYMBOL ENTREZID
  #   1  TRPV1     7442
  #   2  TRPV2    51393
  #   3  TRPV3   162514
  #
  #   > genesym <- c("BRCA1", "BRCA2", "KIAA1462")
  #   > geneid <- select(org.Hs.eg.db, keys=genesym, keytype="SYMBOL", cols="ENTREZID")
  #   > geneid
  #   SYMBOL ENTREZID
  #   1    BRCA1      672
  #   2    BRCA2      675
  #   3 KIAA1462    57608
  
  # Get the ENTREZIDs for the target genes (as gene symbols). Their Gene symbols are in a string in params$opt$filter.c 
  genesym <- unlist(strsplit(params$opt$filter.c, split=" ", fixed=TRUE))
  geneid <- select(org.Hs.eg.db, keys=genesym, keytype="SYMBOL", cols="ENTREZID")
  # geneid$SYMBOL
  # geneid$ENTREZID
  # length(geneid$ENTREZID)
  # length(unique(geneid$ENTREZID))
  
  # Load the other required packaage
  txdbpackage <- paste("TxDb.Hsapiens.UCSC.", params$opt$genver, ".knownGene", sep="")
  if(!require(txdbpackage, character.only=TRUE)) { biocLite(txdbpackage) }
  library(txdbpackage, character.only=TRUE, quietly = TRUE)
  
  # Since the dynamic way of using txdb package doesn't work for me (cols(txdb) below complains with 
  #   'Error in x$conn : $ operator is invalid for atomic vectors)'
  #    #txdb <- substitute(pkg, list(pkg = as.character(txdbpackage)))
  #    #txdb <- substitute(pkg, list(pkg = as.name(txdbpackage)))
  #    #class(txdb) <- "TranscriptDb"
  #    #attr(txdb,"package") <- "GenomicFeatures"
  #    #get(txdbpackage)
  #    #str(txdb)
  #    #cols(txdb)
  # I implement it with a if clause for each type of params$opt$genver
  if (params$opt$genver == "hg19" ) {
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
  } else if (params$opt$genver == "hg18" ) {
    txdb <- TxDb.Hsapiens.UCSC.hg18.knownGene
  }
  
  # Testing
  #exons(txdb)[1:10]
  #head(exons(txdb))
  #tail(exons(txdb))
  
  #str(txdb)
  #   Reference class 'TranscriptDb' [package "GenomicFeatures"] with 5 fields
  #   $ conn        :Formal class 'SQLiteConnection' [package "RSQLite"] with 1 slots
  #   .. ..@ Id:<externalptr> 
  #     $ packageName : chr "TxDb.Hsapiens.UCSC.hg19.knownGene"
  #   $ .chrom      : chr [1:93] "chr1" "chr2" "chr3" "chr4" ...
  #   $ isActiveSeq : logi [1:93] TRUE TRUE TRUE TRUE TRUE TRUE ...
  #   $ seqnameStyle: chr(0) 
  #   and 13 methods, of which 1 are possibly relevant:
  #     initialize
  #cols(txdb)
  #   [1] "CDSID"      "CDSNAME"    "CDSCHROM"   "CDSSTRAND"  "CDSSTART"
  #   [6] "CDSEND"     "EXONID"     "EXONNAME"   "EXONCHROM"  "EXONSTRAND"
  #   [11] "EXONSTART"  "EXONEND"    "GENEID"     "TXID"       "EXONRANK"
  #   [16] "TXNAME"     "TXCHROM"    "TXSTRAND"   "TXSTART"    "TXEND"
  
  # I use here the list of ENTREZIDs (for my target genes) that I obtained in the previous step
  my.geneid.entrezid <- suppressWarnings(select(txdb, keys=geneid$ENTREZID, cols=cols(txdb) ,keytype="GENEID"))
  print_mes_fullpath("\n[WARNING] This step in fun.genedata() seems to produce: 
                     In .generateExtraRows(tab, keys, jointype) :
                     'select' resulted in 1:many mapping between keys and return rows\n\n", abs_routlogfile)
  
  # Testing
  #summary(my.geneid.entrezid)
  #str(my.geneid.entrezid)
  
  # Add column with GeneSymbols (hgnc_symbol, which equals to = org.Hs.eg.db, keys=genesym, keytype="SYMBOL")
  # corresponding to the equivalent EntrezID in my.geneid.entrezid
  # ------------------------------------------------------------------------------------------------------------
  # Command (in general)
  # my_symbols <- df.a[ pmatch(df.b[,"GENEID"], df.a[,2]), 1 ]
  
  # load the data obtained previously with the correspondence between hgnc_symbol and entrezid
  df.a <- geneid
  #str(df.a)
  
  df.b <- my.geneid.entrezid
  #str(df.b)
  
  a <- as.character(df.a[,"ENTREZID"])
  b <- as.character(df.b[,"GENEID"])
  # Example of match in the testset
  # a: 672
  # b: 672
  #match.b.a <- pmatch(b, a, nomatch=0)
  match.b.a <- pmatch(b, a, duplicates.ok=TRUE)
  # Get the vector with the hgnc_symbols corresponding to those genes. There can be some gaps, like in the case of the KIAA1462 in the testset
  SYMBOL <- df.a[ match.b.a, "SYMBOL" ]
  # join the column to the right
  my.geneid.entrezid <- cbind(my.geneid.entrezid, SYMBOL)
  # --------------- end adding new column with Gene_symbols corresponding to the ENSG (ensembl_gene_id) found
  
  # Write the results at my.geneid.entrezid to the file my_genedata.csv
  file_my_genedata = file=paste(params$log.folder,"/", params$startdate, ".", params$opt$label,".my_genes.data.csv", sep="")
  
  # I remove the warning messages from this call (below) because of the coercion type of message (harmless, afaik, but annoying and polluting output in log files)
  # Write all: data with column names in one go
  #suppressWarnings(write.table(my.geneid.entrezid, file=file_my_genedata, append=F, quote=F, sep="\t", row.names=F, col.names=T))
  suppressWarnings(write.csv(my.geneid.entrezid, file=file_my_genedata, append=F, quote=F, row.names=F, col.names=T))
  
  # ---> Testing - ini-----------
  # This (below) is the table with all values (including the wrong chromosome numbers as gene symbols by mistake)
  # convert in a flat table sorting first by Gene Symbol, and then, by exon TXNAME.
  #  my.geneid.entrezid.table <- ftable(my.geneid.entrezid[1:10000,], row.vars = c("SYMBOL", "TXNAME"), col.vars="EXONRANK")
  #my.geneid.entrezid.table <- ftable(my.geneid.entrezid[1:1000,c("EXONRANK","SYMBOL","TXNAME")], row.vars = c("SYMBOL","TXNAME"))
  #str(my.geneid.entrezid.table)
  #?print.ftable
  #
  # Testing with melt and cast, from reshape package
  #   library(reshape)
  #   a <- array(1:24, c(2,3,4))
  #   melt(a)
  #   melt(a, varnames=c("X","Y","Z"))
  #   dimnames(a) <- lapply(dim(a), function(x) LETTERS[1:x])
  #   melt(a)
  #   melt(a, varnames=c("X","Y","Z"))
  #   dimnames(a)[1] <- list(NULL)
  #   melt(a)
  #   data <- my.geneid.entrezid
  #   str(data)
  #   mdata <- melt(data)
  #   length(data)
  #   length(mdata$SYMBOL)
  #   length(mdata$TXNAME)
  #   dim(mdata)
  #melt.array(data, varnames = c("SYMBOL", "TXNAME", "EXONRANK"))
  #cdata <- cast(mdata, SYMBOL ~ TXNAME, length)
  # <--- Testing - end-----------
  
  # # We don't do check2clean here  since the output are results
  print_done("run");
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
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
  check2showcommand(params$opt$showc, command, file2process.my2);
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
  #   file2process.my2 <-"sample_a_merged12.sam"
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

  # Define samtools.bit.wise.flag.param. 
  # -------------------------------------
  # This part is for filtering out unmmaped reads, which were causing issues in snpEff later on (MAPQ should be zero for unmapped reads).
  # To understand how this works we first need to inspect the SAM format. 
  #   The SAM format includes a bitwise FLAG field described here: http://picard.sourceforge.net/explain-flags.html 
  #   The -f/-F options to the samtools command allow us to query based on the presense/absence of bits in the FLAG field.
  #   So -f 4 only output alignments that are unmapped (flag 0×0004 is set) 
  #   and -F 4 only output alignments that are not unmapped (i.e. flag 0×0004 is not set),
  #   hence these would only include mapped alignments.
  # More information: 
  # http://left.subtree.org/2012/04/13/counting-the-number-of-reads-in-a-bam-file/
  # http://www.biostars.org/p/55830/ 
  # http://seqanswers.com/forums/showpost.php?p=57348&postcount=2
  if (params$opt$only.m.r == 1 || params$opt$only.m.r == "y") {
    # Show only mapped
    samtools.bitwise.flag.param <- " -F 4 " 
  } else if (params$opt$only.m.r == -1 || params$opt$only.m.r == "u") {
    # Show only unmapped (-1 to imply "the opposite of", or "u" for unmapped)
    samtools.bitwise.flag.param <- " - f 4 " 
  } else {
    # Show all (no -F/-f param with bitwise param: include all mapped and unmapped)
    samtools.bitwise.flag.param <- " "
  }
  options00 = paste(" view -bS ", samtools.bitwise.flag.param,  
                    " -t ", params$path_genome, ".fai ", file_in, " 2>> ", file_stderr, # Captured the type of line "[samopen] SAM header is present: NN sequences" as appended to the sample log file since it was sent through the stderr
                    " | ", command00, " sort - ", file_out,   " 2>> ", file_stderr, sep="");

  # direct command line call for testing other things:
  # samtools  view -bS file_in | samtools sort - file_in.sorted
  
# XXX ToDo : In theory, the -u option is better (faster) for piped processes, since it does not compress/uncrompress data.... but untested yet.
#  options00 = paste(" view -buS ", file_in, " | ", command00, " sort - ", file_out,   " 2>> ", file_stderr, sep="");
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  check2clean(file_in, file2process.my2);
  create.hard.link(file2process.my2, step.my, 
                   suffix_file_from=".sam.sorted.bam",
                   suffix_file_to=".sam.sorted.edited.bam")
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

##########################
### FUNCTION create.hard.link
###
### Function to create a hardlink between the processed file 
### and the file with common name for further processing. This way, the space on disk is occupied only once, 
### for all hard-linked files 
###
### Arguments:
### file_in <- source file
### file_out <- destination file name
##########################

create.hard.link <- function(file2process.my2, step.my, suffix_file_from, suffix_file_to) {

  # Report that the hard link is going to be created
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "... Creating File alias (hardlink) from ", suffix_file_from, " to ", suffix_file_to, ". Sample: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  # Create also a hard link with a shorter file name that will be he base for the next processing
  # The param "prefix_path" is needed because when running with relative directories (like when doing test-mode runs),
  # we need to indicate that the path is relative. When using absolute paths (like when processing real data), 
  # paths use to be absolute and no prefix is needed (that's why it's null in that case)  
  if   (params$path_input_absolute == 1) {
    prefix_path <- ""
  } else { 
    # path is relative
    prefix_path <- "./"
  }
  # Set the params for the hardlink
  file_from  <- paste(prefix_path, params$directory_out, "/", file2process.my2, suffix_file_from, sep="");
  file_to    <- paste("", params$directory_out, "/", file2process.my2, suffix_file_to, sep="");
  # Check if hard link exists
  if ( file.exists(file_to) ) {
    # Remove it (so that we can get a new time stamp similar to the file linked to
    # which has been very recently generated, etc)
    file.remove(file_to)
  }
  # Create the hardlink link (it uses space only once for that file and it's hard linked file names - aliases)
  file.link(file_from, file_to) 
  
  gc() # Let's clean ouR garbage if possible
  return() # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.samtools.fixmate
###
###   Fill in mate coordinates, ISIZE and mate related flags from a name-sorted alignment.
###
###   These is needed later by samtools rmdup to remove potential PCR duplicates: 
###   if multiple read pairs have identical external coordinates, only retain the pair with highest mapping quality. 
###   This command ONLY works with FR orientation and requires ISIZE is correctly set.
##########################

fun.samtools.fixmate <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Fixmate info and Isize in .bam to allow removing PCR duplicates later on: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.bam", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.1.fixmate.bam", sep="");
  command00 = "samtools "; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" fixmate ", file_in, " ", file_out,  " 2>> ", file_stderr, sep="");
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  check2clean(file_in, file2process.my2);
  create.hard.link(file2process.my2, step.my, 
                   suffix_file_from=".sam.sorted.edited.1.fixmate.bam",
                   suffix_file_to=".sam.sorted.edited.bam")
  print_done(file2process.my2);
  

  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.picard.mark.dup
###
###   Mark duplicates with Picard
##########################

fun.picard.mark.dup <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Mark duplicates using Picard: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.bam", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.2.dupmarked.bam", sep="");
  file_metrics = paste(params$directory_out, "/", file2process.my2, ".bam.metrics.txt", sep="");
  # java -jar ${picard_dir}/MarkDuplicates.jar I=${name}_bwa.bam O=${name}_bwa_dedup.bam M=${name}_bwa_dedup_metrics.txt
  command00 = " java  -Xmx4g -jar "; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  
  options00 = paste(" ", params$path_picard, " I=", file_in, " O=", file_out, " M=", file_metrics, 
                    "  REMOVE_DUPLICATES=false  AS=true ", " 2>> ", file_stderr, sep="");
  # AS=true tells picard to assume that the bam file is coordinate sorted.
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  check2clean(file_in, file2process.my2);
  create.hard.link(file2process.my2, step.my, 
                   suffix_file_from=".sam.sorted.edited.2.dupmarked.bam",
                   suffix_file_to=".sam.sorted.edited.bam")
  print_done(file2process.my2);
  
  # direct command line call for testing other things:
  # samtools  rmdup -s file_in.sam.sorted.bam file_in.sam.sorted.fixmate.dupmarked.noDup.bam
  
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

##########################
### FUNCTION fun.remove.pcr.dup
###
### 	Remove possible PCR duplicates
###   if multiple read pairs have identical external coordinates, only retain the pair with highest mapping quality. 
###   This command ONLY works with FR orientation and requires ISIZE is correctly set.
##########################

fun.remove.pcr.dup <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Remove possible PCR duplicates: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.bam", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.3.noDup.bam", sep="");
  command00 = "samtools"; # next command.
  ###   -s   Remove duplicate for single-end reads. By default, the command works for paired-end reads only.
  ###   -S   Treat paired-end reads as single-end reads.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  if ( (params$opt$bwa == 1) || (params$opt$bwa == 3) ) {
    rmdup.param <- " -s " # Case for single end
  } else if (params$opt$bwa == 2) {
    # In theory we could use no param (for paired end data) or param -S to tread reads as single end.
    # Which as the consequences of doing so?:
    ##
    ##  Quote From http://seqanswers.com/forums/showpost.php?p=101616&postcount=3
    ##
    ##  "If you have one pair of reads where read 1 starts at position 100, and the other end starts at position 200, 
    ##   and a second pair of reads where read 1 starts at position 100, and read 2 starts at position 250, 
    ##   those came from different fragments of DNA. You can tell because the read 2 start is different, 
    ##   even though the read 1 start is the same.
    ##
    ##   When treating the reads as paired end, none of those reads should be deleted as PCR duplicates.
    ##   However, if you ran rmdup -S, the software will not check to see if read 2 has a different start coordinate, 
    ##   so one of those read 1 reads will be treated as a duplicate, and deleted."
    ##
    
    #rmdup.param <- " -S " # Case for paired end, treating reads as single end. Otherwise, it doesn't work with samtools rmdup
    rmdup.param <- " " # Case for paired end, in theory, to remove dups as paired end. But it doesn't work.
  } else {
    cat("\n\nn[ERROR] Wrong value in param opt$bwa or p_bwa. Only values 1, 2 or 3 are allowed\n\n")
  }

  options00 = paste(" rmdup ", rmdup.param, " ", file_in, " ", file_out,  " 2>> ", file_stderr, sep="");
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  check2clean(file_in, file2process.my2);
  create.hard.link(file2process.my2, step.my, 
                   suffix_file_from=".sam.sorted.edited.3.noDup.bam",
                   suffix_file_to=".sam.sorted.edited.bam")
  print_done(file2process.my2);

  # direct command line call for testing other things:
  # samtools  rmdup -s file_in.sam.sorted.bam file_in.sam.sorted.fixmate.dupmarked.noDup.bam
  
  
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
  check2showcommand(params$opt$showc, command, file2process.my2);
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

  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.bam", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.4.intervals", sep="");
  command00 = "java -jar "; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(params$path_gatk, " -T RealignerTargetCreator -R ", params$path_genome, 
                    " -I ", file_in, " --known:dbsnp,vcf ", params$path_dbSNP,
#                    " -L ", params$path_exon_capture_file ,
                    " -o ", file_out, 
                    " -et NO_ET -K ", params$path_gatk_key,
                    " 2>&1 | tee -a ", file_stderr, # Send stderr to stdout and save it to file (-a = append) while keeping it in stdout also
                    sep="");
    # Former option -B:dbsnp,vcf has been converted into -known:dbsnp,vcf. See http://seqanswers.com/forums/showthread.php?t=14013  

  # For manual debugging, see this example to be run in the command line:
  # java -Xmx4G -jar /path_gatk/GenomeAnalysisTKLite.jar -T RealignerTargetCreator -R /path_refgenome/hg19.fa -I /pathtest_out/sample.sam.sorted.fixmate.dupmarked.noDup.bam --known:dbsnp,vcf /path_dbSNP/dbsnp132_20101103_gatk.vcf -o /pathtest_out/sample.sam.sorted.fixmate.dupmarked.noDup.bam.intervals -et NO_ET -K /pathkey/ueb_vhir.org.key  2>&1 | tee -a /pathtest_out/foo.txt
  
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
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  check2clean(file_in, file2process.my2);
  create.hard.link(file2process.my2, step.my, 
                   suffix_file_from=".sam.sorted.edited.4.intervals",
                   suffix_file_to=".sam.sorted.edited.intervals")
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
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.bam", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.5.intervals", sep="");
  command00 = "java -jar "; # next command.
  # options00 = ...
  
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  check2clean(file_in, file2process.my2);
  create.hard.link(file2process.my2, step.my, 
                   suffix_file_from=".sam.sorted.edited.5.intervals",
                   suffix_file_to=".sam.sorted.edited.intervals")
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
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.bam", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.6.intervals", sep="");
  command00 = "java -jar "; # next command.
  # options00 = ...
  
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  check2clean(file_in, file2process.my2);
  create.hard.link(file2process.my2, step.my, 
                   suffix_file_from=".sam.sorted.edited.6.intervals",
                   suffix_file_to=".sam.sorted.edited.intervals")
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
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.bam", sep="");
  #  file_out = paste(file_in, ".foo", sep="");
  command00 = "samtools"; # next command.
  options00 = paste(" index ", file_in, sep="");
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  # Don't check for check2clean("$file_in") since we still need it to do some stats upon it
  print_done(file2process.my2);
  
  # direct command line call for testing other things:
  # samtools  index file_in.sam.sorted.fixmate.dupmarked.noDup.bam
  
  
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
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.bam", sep="");
  file_metrics = paste(params$directory_out, "/", file2process.my2, ".bam.metrics.txt", sep="");
  #  file_out = paste(file_in, ".foo", sep="");
  command00 = "samtools"; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");

#   # Substep 1a: by samtools idxstats - total reads
#   #---------------
#   # Reads can be nicely counted from bam index by using samtools idxstats (or picard BamIndexStats):
#   # samtools idxstats in.bam | awk '{s+=$3+$4} END {print s}' // total reads
#   options00 = paste(" idxstats ", file_in,  " | awk '{s+=$3+$4} END {print s}' ", 
#                     ">> ", file_stderr,  " 2>> ", file_stderr, sep="");
#   command = paste(command00, " ", options00, sep="");
#   check2showcommand(params$opt$showc, command, file2process.my2);
#   print_doc(paste("Total reads: ", sep=""), file2process.my2);
#   system(command);
# 
#   # Substep 1b: by samtools idxstats - mapped reads 
#   #---------------
#   # Reads can be nicely counted from bam index by using samtools idxstats (or picard BamIndexStats):
#   # samtools idxstats in.bam | awk '{s+=$3} END {print s}' // mapped reads
#   options00 = paste(" idxstats ", file_in,  " | awk '{s+=$3} END {print s}' ", 
#                     ">> ", file_stderr,  " 2>> ", file_stderr, sep="");
#   command = paste(command00, " ", options00, sep="");
#   check2showcommand(params$opt$showc, command, file2process.my2);
#   print_doc(paste("Mapped reads: ", sep=""), file2process.my2);
#   system(command);
  
  # Substep 2: by samtools flagstat 
  #---------------
  options00 = paste(" flagstat ", file_in,  " >> ", file_metrics,  " 2>> ", file_stderr, sep="");
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  # write a header in the file_metrics to indicate where does it come from
  mess.my <- paste("\n[COMMAND RUN]----->\n", command, "\n<-----\n", sep="");
  print_mes(mess=paste("\n[CONTENTS INSERTED HERE FROM FILE: ", file_metrics,
                     "]-----> \n ", sep=""), filename.my1=file2process.my2)
  #print_mes_fullpath(mess=mess.my, filename.my1=file_metrics)
  
  # Run the command
  system(command);

  # End of contents inserted from the file on disk with the metrics
  print_mes_fullpath(mess="\n<-----\n", filename.my1=file_metrics)
  
  # Copy the contents from the file "metrics" into the log file for the processing of this run
  system(paste("cat ", file_metrics, " >> ", file_stderr, " 2>>", file_stderr, sep=""))
  
  # Don't check for check2clean("$file_in") since we still need it for the variant calling
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

##########################
### FUNCTION fun.addleading0.ids
###
### By Josep LLuis Mosquera, UEB-VHIR. jl.mosquera at vhir.org, & 
###    Xavier de Pedro, UEB-VHIR. xavier.depedro at vhir.org
###
##########################
##
## Description:
##
##   Add leading zeros to ids so that it can be sorted naturally for humans
##
## Parameters:
##
##          x   : numeric or character vector with ids nn of differnt number of characters (like string_nn) 
##          sep : separator character to split by. E.g. "_", " ", "-" ...
##          nd  : number of digits needed in the end after adding as many leading zeroes (o) as required (integer value: 2, 3, etc)
##

fun.addleading0.ids <- function(x, sep, nd)
{
  
  # First, some data massaging...
  out.list <- strsplit(x, split = sep)
  out.df <- do.call("rbind", out.list)
  out.df2 <-out.df
  
  # Get the indexes of values containing NA
  idx.nona <- which(!is.na(out.df[,2]))
  
  # Add as many leading zeroes (0) to the left of the number to have 'nd' digits (2, 3, ...), in all numbers that are no NA
  # so that even if those numbers were treated as characters (in the variable class in R) they would get sorted properly
  # also as characters. And exons normally are numbered in tens or hundreds, so that nd should be 2 or 3, repectively (from 01 to 99, or from 001 to 999)
  out.df2[idx.nona,2] <- unlist(lapply(out.df[idx.nona,2], function(x) sprintf(paste("%0", nd ,"d", sep=""), as.numeric(x) ) ) )
  
  # join the two parts again with the same character splitter
  out.df2 <- paste(out.df2[,1], out.df2[,2], sep=sep)
  # Fix NA strings (they also became NA_NA). Add other cases here when needed in the future  
  out.df2 <- gsub("NA_NA", "NA", out.df2)
  
  return(out.df2)
}
##########################

##########################
### FUNCTION fun.snpeff.count.reads
###
###   Count Reads through snpEff software [optional]
##########################

fun.snpeff.count.reads <- function(file2process.my2, step.my) {
  # Manual debugging
  # file2process.my2 <- "/mnt/magatzem02/tmp/run_sara_293a/dir_in_test_cr_ind1/s_1_m11_143b_merged12"
  
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "a. Count Reads using snpEff: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  # If for whatever reason, the filetoprocess.my2 comes with the base path (from dir_in or dir_out) 
  # as a prefix, remove it.
  if ( length(grep(params$directory_in, file2process.my2)) ) {
    file2process.my2 <- sub(paste(params$directory_in, "/", sep=""), "", file2process.my2)
  } else if ( length(grep(params$directory_out, file2process.my2)) ) {
    file2process.my2 <- sub(paste(params$directory_out, "/", sep=""), "", file2process.my2)
  }
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.bam", sep="");
  file_out = paste(file_in, ".cr.txt", sep="");
  file_bed.my = paste(params$log.folder,"/", params$startdate, ".", params$opt$label,".my_genes.bed", sep="")
  
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  
  command00 = "java -Xmx4g -jar "; # next command.
#  options00 = paste(params$path_snpEff, "snpEff.jar  -c ", params$path_snpEff, "snpEff.config countReads ", params$opt$genver," ", file_in, " > ", file_out, sep="");
#  options00 = paste(params$path_snpEff, "snpEff.jar  countReads ", params$opt$genver," ", file_in, " > ", file_out, sep="");
  
  # Since April 2013, 26, count reads will count only for the inteervals of interested for the researcher (limited to the intervals providedd in the bed file) 
  options00 = paste(params$path_snpEff, "snpEff.jar  countReads ",  
                    # Temporarily disabled until feedback from snpEff author about potential bug here with the -i param for the bed file
                    # " -i '", file_bed.my, "' ", 
                    params$opt$se_db_rg, " ", file_in, " > ", file_out, " 2>> ", file_stderr, sep="");
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  
  # Manual debugging XXX
  # file2process.my2 <- "sgs7a_merged12"
  # file_in <- "./test_out2/sgs7a_merged12.sam.sorted.edited.bam"
  # file_out <- "./test_out2/sgs7a_merged12.sam.sorted.edited.bam.cr.txt"
  # file_in <- "/mnt/magatzem02/tmp/run_sara_293a/dir_out_all/test_out/s_4_m11_146b_merged12.sam.sorted.edited.bam"
  # file_out <- "/mnt/magatzem02/tmp/run_sara_293a/dir_out_all/s_4_m11_146b_merged12.sam.sorted.edited.bam.cr.txt"
  
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "b0. Generate cr.table.csv using R: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  # load the file created as file2process.countreads (aka: f2p.cr)
  f2p.cr <- read.table(file_out, header=T, sep="\t")
  # head(f2p.cr)
  #dim(f2p.cr)
  x.my <- as.character(f2p.cr$IDs)
  
  # Do all the processing only if there is some data to process. To avoid the program to break when snpEff fails to count reads
  # dues to the issues with "MAPQ should be zero for unmmaped reads", etc. 
  if (length(x.my) > 0) {
    
    # Split IDs by ; and create the new columns on the right with separated unitN, ENST (ENSEMBL Transcript) & ENSG (ensembl_gene_id) codes.
    out.my <- fun.splitAnnot(x.my)
    colnames(out.my) <- c("unitN", "ENST","ENSG")  # unitN stands for exonNumber or IntronNumber or similar
    f2p.cr <- cbind(f2p.cr, out.my)
    
    # convert column 8 into character vectors and apply the function to add leading zeros for correct ordering as text
    x <- as.character(f2p.cr[,8])
    # Add leading zeros whre needed to have up to 3 digits for all numbers (from exon_001 to exon_999)
    f2p.cr[,8] <- fun.addleading0.ids(x, "_", 3)
    
    print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "b1. Get hgnc_symbols for the corresponding ensembl_gene_id: ", file2process.my2, " ###\n", sep=""), file2process.my2);
    
    # Convert ENSG (ensembl_gene_id) to Gene_symbols (hgnc_symbol) and add it as new column to f2p.cr$Gene_Symbol
    # ------------------------------------------------------------------------------------------------------------
    file_my_genenames = file=paste(params$log.folder,"/", params$startdate, ".", params$opt$label,".my_genes.names.txt", sep="")
    # Command (in general)
    # my_symbols <- df.a[ pmatch(df.b[,"ensembl_gene_id"], df.a[,2]), 1 ]
    
    # load the file created previously with the correspondence between hgnc_symbol and ensembl_gene_id (while working with biomaRt to generate the bed file)
    # f2p.my_genenames
    df.a <- read.table(file_my_genenames, header=T, sep="\t")
    #str(df.a)
    
    df.b <- f2p.cr
    #str(df.b)
    
    b <- as.character(df.b[,"ENSG"])
    a <- as.character(df.a[,"ensembl_gene_id"])
    # Example of match in the testset
    # a: ENSG00000012048
    # b: ENSG00000012048
    #match.b.a <- pmatch(b, a, nomatch=0)
    match.b.a <- pmatch(b, a, duplicates.ok=TRUE)
    # Get the vector with the hgnc_symbols corresponding to those genes. There can be some gaps, like in the case of the KIAA1462 in the testset
    GeneSymbol <- df.a[ match.b.a, "hgnc_symbol" ]
    # length(my_hgnc_symbols)
    # dim(df.b)
    # dim(f2p.cr)
    f2p.cr <- cbind(f2p.cr, GeneSymbol)
    #str(f2p.cr)
    # --------------- end adding new column with Gene_symbols corresponding to the ENSG (ensembl_gene_id) found
    
    # Filter results by the genes of interest only
    #
    # BEWARE that subset function can lead to confusion in some cases. Subset is useful for interactive programming, not for automatic scripts.
    # See:
    # (1) The easy subset way: http://www.ats.ucla.edu/stat/r/faq/subset_R.htm
    # (2) The potential problems: see 
    #   http://stackoverflow.com/questions/9860090/in-r-why-is-better-than-subset 
    #   https://github.com/hadley/devtools/wiki/Evaluation
    
    # We could use params$opt$filter.c since it's already clean (added in March 2013)
    #  but we do clean params$opt$filter here still  
    # ---------------------------------------------
    # Get first the list of Gene Symbols for the "target genes" of interest for the researcher (tgenes)
      # Split values by pipe (it will create some empty values also, but we will remove them later)
      tgenes <- strsplit(params$opt$filter, "|", fixed=TRUE)
      tgenes <- unlist(tgenes) # Convert those values from a list into a character vector 
      # Remove non-intended characters (all except numbers and letters)
        ## This could be performed with a fairly simple regular expression, such as: 
        #tgenes <- gsub(pattern="[^A-Za-z0-9]", replacement="",  x=tgenes, fixed=FALSE)
        ## But I'm not sure that some error would be introduced in the future by removing some otehr valid character.
        ## Therefore, just in case, I remove the other sets of characters by steps in a FIXED form
    
        # Remove non-intended characters (it will create some empty values also, but we will remove them later)
        tgenes <- gsub(pattern=":", replacement="",  x=tgenes, fixed=TRUE)
        tgenes <- gsub(pattern="^", replacement="",  x=tgenes, fixed=TRUE)
        tgenes <- gsub(pattern="\t", replacement="",  x=tgenes, fixed=TRUE)
        tgenes <- gsub(pattern="\\", replacement="",  x=tgenes, fixed=TRUE)
        tgenes <- gsub(pattern="\"", replacement="",  x=tgenes, fixed=TRUE)
        tgenes <- gsub(pattern=" ", replacement="",  x=tgenes, fixed=TRUE)
    
      # Remove duplicates of GeneSymbols or ENSEMBL GENES
      tgenes <- unique(tgenes)
      # Remove the case of an empty value
      tgenes <- tgenes[-match("", tgenes)]
    # --------------------------------------------------
    print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "b2. Filter results by a subset with only target genes: ", file2process.my2, " ###\n", sep=""), file2process.my2);
    

    ## Filter results by the genes of interest only WITH A SUBSET
      ## Works easily with one value
      # f2p.cr.tg <- f2p.cr[f2p.cr$GeneSymbol == "WASH7P",]
      ## For multiple values like "1" and "4", we can use the syntax "x.sub5 <- x.df[x.df$y %in% c(1, 4), ]"
      ## Example for a couple of gene Symbols
      #f2p.cr.tg <- f2p.cr[f2p.cr$GeneSymbol %in% c("WASH7P", "OR4F5"), ]
    # Do the filtering for the genes of interest (in the list of target genes: tgenes) 
    f2p.cr.tg <- f2p.cr[f2p.cr$GeneSymbol %in% tgenes, ]
    # for the sake of simplicity and effectiveness, we assign the subset of count reads 
    # for the genes of interest ("target genes", aka, "tg") to the whole list of count reads in the file to process (f2p.cr)
    f2p.cr <- f2p.cr.tg
    
    # Get the last three columns as Gene symbols and exon and intron numbers
    f2p.cr.genes <- f2p.cr[,c(8,9,11)]
    #summary(f2p.cr.genes)
    
    ## Convert into true/false
    #f2p.cr.genes.tf <- suppressWarnings(is.na(as.numeric(as.character(unlist(f2p.cr.genes[2])))) )
    
    # We want to apply the table function to the subset of values of the data frames (since it seems to be much more complicated
    # to create a subset of values once the object of type "table" is created)
    # We do the subset through getting the indexes of these GeneSymbol values (and not the numbers from chromosomes) 
    f2p.cr.genes.idx <- which(suppressWarnings(is.na(as.numeric(as.character(unlist(f2p.cr.genes["GeneSymbol"]))))) )
    
    # this is the list of values with gene symbols only, and not the values the chromosome number as gene symbol
    f2p.cr.genes2 <- f2p.cr.genes[f2p.cr.genes.idx,]
    #str(f2p.cr.genes2)
    #summary(f2p.cr.genes[f2p.cr.genes.idx,])
    
    # We fix the levels of the variable Genesymbol from the data frame, which still hold bad names from chromosome numbers
    # we can't assign the unique value of f2p.cr.genes2$GeneSymbol to the levels of f2p.cr.genes2$GeneSymbol because they differ in length
    # therefore we apply this trick (thanks jl.moquera!)
    f2p.cr.genes2$GeneSymbol <- as.factor(as.character(f2p.cr.genes2$GeneSymbol))
    #str(f2p.cr.genes2)
    
    print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "b3. Apply function ftable in R: ", file2process.my2, " ###\n", sep=""), file2process.my2);
    
    # (Approach 1) Implementation with ftable ------------->
    #startcr <- Sys.time();

    # Therefore, this (below) is the table with all values (including the wrong chromosome numbers as gene symbols by mistake)
    # convert in a flat table sorting first by Gene Symbol, and then, by exon number.
    f2p.cr.table <- ftable(f2p.cr.genes2, row.vars = 2:1)
    #head(f2p.cr.genes2)
    #str(f2p.cr.table)
    #?print.ftable
    
    print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "b4a. Convert ftable into dataframe (R): ", file2process.my2, " ###\n", sep=""), file2process.my2);
    
    # [SOLVED] as.data.frame.table or as.data.frame.matrix !!!!!!
    f2p.cr.dft <- as.data.frame.table(f2p.cr.table)
    
    print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "b4b. Re-order results in R: ", file2process.my2, " ###\n", sep=""), file2process.my2);
    
    # We need to re-sort in a similar way to what the ftable was
    f2p.cr.dft <- f2p.cr.dft[order(f2p.cr.dft$GeneSymbol, f2p.cr.dft$unitN),]
    
    print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "b4c. Remove zeros from counts in the data frame (R): ", file2process.my2, " ###\n", sep=""), file2process.my2);
    
    # And last, we remove all row with 0 in the column Counts
    f2p.cr.dft <- f2p.cr.dft[!f2p.cr.dft$Freq==0,]
  
    #durationftable <- Sys.time() - startcr
    # (Approach 1) <------ Implementation with ftable

#     # (Approach 2) Implementation with aggregate ------------->
#     startcr <- Sys.time();
#     
#     f2p.cr.df2 <- aggregate(ENST ~ GeneSymbol + unitN, data=f2p.cr.genes, FUN=length)
#     #class(f2p.cr.df2)
#     f2p.cr.df2 <- f2p.cr.df2[order(f2p.cr.df2$GeneSymbol, f2p.cr.df2$unitN),]
#     f2p.cr.df2 <- f2p.cr.df2[!f2p.cr.df2$unitN=="NA",]
#     #str(f2p.cr.table)
#     #?head(f2p.cr.table)
#     
#     durationaggr <- Sys.time() - startcr
#     # (Approach 2) <------ Implementation with aggregate
    
    print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "b5. Write the file  cr.table.csv using R: ", file2process.my2, " ###\n", sep=""), file2process.my2);
    
    ## as.data.frame.matrix was useful when f2p.cr.table was another type of object, but not nowadays
    #f2p.cr.dfm <- as.data.frame.matrix(f2p.cr.table)
    
    file_out.table = paste(file_in, ".cr.table.csv", sep="");
    # Write the first line with the column names
    write("\"ENST\",\"unitN\",\"GeneSymbol\",\"Count\"", file=file_out.table, append = F, sep = "")
    #  write.table(f2p.cr.table, file=file_out.table, append=T, quote=T, sep=",", row.names=T, col.names=F)
    
    #tapply(as.numeric(colnames(f2p.cr.table)), 1:9, FUN=is.numeric)
    
    # I remove the warning messages from this call (below) because of the coercion type of message (harmless, afaik, but annoying and polluting output in log files)
    # Write the rest: all data without column names
    write.table(f2p.cr.dft, file=file_out.table, append=T, quote=T, sep=",", row.names=F, col.names=F)
    #write.ftable(f2p.cr.table, file=file_out.table, append=T, quote=T)
    
  } else {
    print_mes("XXX Count Reads failed XXX; no results saved.", file2process.my2)
  }
  
  # Don't check for check2clean("$file_in") since we still need it for the variant calling
  print_done(file2process.my2);
  
  # direct command line call for testing other things:
  # java -Xmx4g -jar /home/ueb/snpEff/snpEff.jar countReads hg19 file_in.sam.sorted.fixmate.dupmarked.noDup.bam > file_in.sam.sorted.fixmate.dupmarked.noDup.bam.cr.txt

  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


############################################################################
### FUNCTION fun.variants.locate.r
###
###   Locate variants using R [optional]. Unused notes as of April 2013
###   Stub, in case we need to follow this track in the future
############################################################################

fun.variants.locate.r <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "Locate Variants using R: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  # Load the required packages  (after installing them if necessary)
  if(!require(Homo.sapiens)){ biocLite("Homo.sapiens") }
  library(Homo.sapiens, quietly = TRUE)

  if(!require(VariantAnnotation)){ biocLite("VariantAnnotation") }
  library(VariantAnnotation)
  
  if(!require(SNPlocs.Hsapiens.dbSNP.20120608)){ biocLite("SNPlocs.Hsapiens.dbSNP.20120608") }
  library("SNPlocs.Hsapiens.dbSNP.20120608")
  
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".f.vcf", sep="");
  file_out = paste(file_in, ".f.locations.txt", sep="");
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  
  command00 = " "; # next command.
  options00 = paste(params$path_snpEff, " foo "," ", file_in, " > ", file_out, 
                    " 2>> ", file_stderr, sep="");
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  
  # Manual debugging XXX
  # file2process.my2 <- "sgs7a_merged12"
  # file_in <- "./test_out2/sgs7a_merged12.f.vcf"
  # file_out <- "./test_out2/sgs7a_merged12.f.locations.txt"
  
  # <-----------------------------------
  # Code snippets taken from bioconductor list
  rsID <- c("rs1929842", "rs10448261", "rs16942913", "rs9530156",
            "rs9543238", "rs3757718", "rs17564689","rs10167958", 
            "rs10777752", "rs10072700", "rs10842099","rs11024171", "rs797516",
            "rs2046545", "rs12996997", "rs6135128","rs2143072", "rs12585354"
            ,"rs797515",  "rs17073211", "rs10496454","rs17197639", "rs4704128",
            "rs2143071", "rs3748997", "rs2292100")
  library(Homo.sapiens)
  library(VariantAnnotation)
  library("SNPlocs.Hsapiens.dbSNP.20120608")
  SNPs.gr  <- rsidsToGRanges(rsID)
  seqlevels(SNPs.gr)
  SNPs.gr@seqnames
  str(SNPs.gr@seqnames)
  SNPs.gr@seqnames@values
  # renameSeqlevels(SNPs.gr, ????)
  seqlevels()
  
  length(SNPs.gr@seqnames@values)
  vars <- locateVariants(SNPs.gr, TxDb.Hsapiens.UCSC.hg19.knownGene,
                         AllVariants())
  
#  Error in .mk_isActiveSeqReplacementValue(x, value) :
#    the names of the supplied 'isActiveSeq' must match the names of the
#  current 'isActiveSeq'
  
  # Solution tips: from Valerie (VariantAnnotation maintainer)
  #   The seqlevels (chromosome names) in the txdb and SNPlocs packages don't match.
  # 
  # > rsID <- c("rs1929842", "rs10448261", "rs16942913", "rs9530156")
  # > SNPs.gr  <- rsidsToGRanges(rsID)
  # > seqlevels(SNPs.gr)
  #  [1] "ch1"  "ch2"  "ch3"  "ch4"  "ch5"  "ch6"  "ch7"  "ch8"  "ch9"
  # ...
  # 
  # >   library(TxDb.Hsapiens.UCSC.hg19.knownGene)
  # >   txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
  # > seqlevels(txdb)
  #  [1] "chr1"                  "chr2"                 "chr3"
  #
  
  # str(seqlevels(txdb))
  # str(seqlevels(SNPs.gr))
  # ...
  # 
  # You can rename seqlevels in your GRanges using renameSeqlevels(), 
  # paste(), or seqlevels()<-. 
  # See any of ?locateVariants, ?renameSeqlevels, ?seqlevels for examples and details.
  
  unlist(mget(vars$GENEID, org.Hs.egSYMBOL))
  
  # ----------------------------------->
  
  # Don't check for check2clean("$file_in") since we still need it for the variant calling
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

########################################################################

##
## fun.splitAnnot
##
## Description:
##
##    Split the elements of a character vector ‘x’ into substrings according to the
##    matches to substring ";" within them.
##
## Parameters:
##
##          x : vector of characters to be splitted.
##    myNames : vector of characters with the names of the output columns in the new data.frame
##
## Details:
##
##    It is expected that the input character vector containing one or two ";" symbols, thereby
##    generating a character vector with three or two components respectively. When the generated
##    vector has two components, it will be modified to have three components by adding a NA as a
##    first component.
##
## Example:
##
##            Generating data
##
##            annot.1 <- paste(sample(c("Intron", "Exon"), size = 3, replace = TRUE),
##                             c("NM_007294.3", "NM_007297.3", "NM_007298.3"),
##                             rep("BRCA1", 3),
##                             sep = ";")
##            annot.2 <- paste(c("NM_007299.3", "NM_007300.3"), rep("BRCA1", 2), sep = ";")
##            annot.3 <- paste(sample(c("Intron", "Exon"), size = 5, replace = TRUE),
##                             c("NM_007295.2", "NM_007296.2", "NM_007301.2", "NM_007302.2", "NM_007303.2"),
##                             rep("BRCA1", 5),
##                             sep = ";")
##            annot.4 <- paste("NM_007305.2", "BRCA1", sep = ";")
##            annot.5 <- paste(sample(c("Intron", "Exon"), size = 1), "NM_007306.2", "BRCA1", sep = ";")
##
##            annot <- data.frame(myData = c(annot.1, annot.2, annot.3, annot.4, annot.5))
##            print(annot)
##
##            Apply fun.splitAnnot function
##
##            nms <- c("Location", "RefSeq", "Symbol")
##            annot2split <- as.character(annot$myData)
##
##            annot.df <- splitAnnot(x = annot2split, myNames = nms)
##
##            print(annot.df)

fun.splitAnnot <- function(x, myNames) {

  x.list <- strsplit(x, split = ";")
  head(x.list)
  idx2change <- which(unlist(lapply(x.list, length))<3)
  # x.list[which(unlist(lapply(x.list, length))<3)]
  # Add NA to col5 (IDs) files when no data in any of the 3 columns
  idx <- which(unlist(lapply(x.list, length))==0)
  if (length(idx) > 0) {
    x.list2change <- lapply(x.list[idx], function(y){ c(NA, NA, NA) })
    x.list[idx] <- x.list2change
  }
  
  # Add NA to col5 (IDs) files when no data in the first 2 columns (unitN or NM)
  idx <- which(unlist(lapply(x.list, length))==1)
  if (length(idx) > 0) {
    x.list2change <- lapply(x.list[idx], function(y){ c(NA, NA, y) })
    x.list[idx] <- x.list2change
  }

  # Add NA to col5 (IDs) files when no data in the unitN or NM columns
  idx <- which(unlist(lapply(x.list, length))==2)
  if (length(idx) > 0) {
    x.list2change <- lapply(x.list[idx], function(y){ c(NA, y) })
    x.list[idx] <- x.list2change
  }
  
  out <- do.call("rbind", x.list)
  #head(f2p.cr)
  #colnames(f2p.cr)
  #dim(out)
  #head(out)
  #dim(f2p.cr)
  #summary(out2)
  #--------
 
  return(out)
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
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.bam", sep="");
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
  
  
  # Jan 9th, 2013: 
  ## added params in samtools mpileup: 
  ##  -C50: for short reads (recommended)
  ##      One may consider to add -C50 to mpileup if mapping quality is overestimated for reads containing excessive mismatches. 
  ##      Applying this option usually helps BWA-short but may not other mappers.
  ##  -E: extended BAQ calculation
  ##  -D: Output per-sample read depth
  ##  -S: write down strand-bias p-value: "Output per-sample Phred-scaled strand bias P-value"
  ##  -q: minimum mapping quality of the reads to be considered in the variant calling process. "Minimum mapping quality for an alignment to be used [0]"
  ##  -d: At a position, read maximally this amount of reads per input BAM
  ## Tunning the paramters: (from http://samtools.sourceforge.net/mpileup.shtml )
  ## * Given multiple technologies, apply -P to specify which technologies to use for collecting initial INDEL candidates. 
  ##    It is recommended to find INDEL candidates from technologies with low INDEL error rate, such as Illumina. 
  ##    When this option is in use, the value(s) following the option must appear in the PL tag in the @RG header lines.
  ## * Use `-BQ0 -d10000000 -f ref.fa' if the purpose is to get the precise depth of coverage rather than call SNPs. 
  ##    Under this setting, mpileup will count low-quality bases, process all reads (by default the depth is capped at 8000), 
  ##    and skip the time-demanding BAQ calculation.
  ## * Adjust -m and -F to control when to initiate indel realignment (requiring r877+). 
  ##    Samtools only finds INDELs where there are sufficient reads containing the INDEL at the same position. 
  ##    It does this to avoid excessive realignment that is computationally demanding. 
  ##    The default works well for many low-coverage samples but not for, say, 500 exomes. 
  ##    In the latter case, using -m 3 -F 0.0002 (3 supporting reads at minimum 0.02% frequency) is necessary to find singletons.
  ## Added params in bcftools view: 
  ##  -A: Retain all possible alternate alleles at variant sites. By default, the view command discards unlikely alleles.
  
  if (params$opt$bychr != 1) {
    # -----------------------
    # Variant Calling without splitting chromosomes in parallel
    # -----------------------
    # Do the variant calling in one go, without splitting by chromosomes
    options00 = paste(" mpileup -uf ", params$path_genome, " ", file_in, " -C50 -EDS -q50 -d10000",
                      " -Q", params$opt$st.vf.Q, 
                      " | bcftools view -vcg - >  ", file_out, sep="");

    # changed on Jan 8th, 2013, to test the potential reduction of false positives (adding -C50 and -B , see documentation above)
    #options00 = paste(" mpileup -uf ", params$path_genome, " ", file_in, " | bcftools view -vcg - >  ", file_out, sep="");
    #  options00 = paste(" mpileup -uf -C50 -EDS -q50 -d10000  ", params$path_genome, " ", file_in, " | bcftools view -Avcg - >  ", file_out, sep="");
    # Since all this together doesn't work, let's try one by one.
    #  options00 = paste(" mpileup -uf      -EDS", params$path_genome, " ", file_in, " | bcftools view -Avcg - >  ", file_out, sep="");
    #  options00 = paste(" mpileup -uf           -q50 ", params$path_genome, " ", file_in, " | bcftools view -Avcg - >  ", file_out, sep="");
    #  options00 = paste(" mpileup -uf                -d10000  ", params$path_genome, " ", file_in, " | bcftools view -Avcg - >  ", file_out, sep="");
    #  options00 = paste(" mpileup -uf                         ", params$path_genome, " ", file_in, " | bcftools view -vcg - >  ", file_out, sep="");
    # Those params needed to be called after the file_in!!!
    #  options00 = paste(" mpileup -uf ", params$path_genome, " ", file_in, " -C50 -EDS -q50 -d10000 | bcftools view -Avcg - >  ", file_out, sep="");
    ## March 18th: Removing the -A from bcftools to see whether the weird X letters in alternative nucleotide go away from vcf files. 
    
    command = paste(command00, " ", options00, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
    system(command);
    
  } else if (params$opt$bychr == 1) {

    # -----------------------
    # Variant Calling in Parallel by Chr
    # http://www.research.janahang.com/2013/01/02/efficient-way-to-generate-vcf-files-using-samtools/
    # http://www.biostars.org/p/48781/
    #
    # Xavier de Pedro, April 23rd, 2013: 
    #   Unluckily, my first tests with our small test datase (covering variants in 3 chromosomes) indicate that
    #   when running splitting the job by chromosomes, there is one variant that is recorded with a lower 
    #   QUAL and mapping quality (MQ) than the one indicated in the bam file, for some misterious reason.
    #   Running samtools mpileup without splitting by chromosome records Qual and MQ properly.
    #
    #chr13  32889762	.	G	T	15.1	.	DP=1;AF1=1;AC1=2;DP4=0,0,1,0;MQ=45;FQ=-30	GT:PL:DP:SP:GQ	1/1:45,3,0:1:0:6
    #vs
    #chr13  32889762	.	G	T	30	.	DP=1;AF1=1;AC1=2;DP4=0,0,1,0;MQ=60;FQ=-30	GT:PL:DP:SP:GQ	1/1:60,3,0:1:0:6
    #
    #The second corresponds to the properly mapped one, as it equals the values shown in igv for that position from the bam file
    #
    # Moreover for this small test set, running in parallel (splitted by chromosomes), 
    # takes twice as much time (34sec aprox) than running all in a row (17sec aprox)
    
    # -----------------------
      # Variant Calling in Parallel by Chr - step 1
      # http://www.research.janahang.com/2013/01/02/efficient-way-to-generate-vcf-files-using-samtools/
      # -----------------------
      # 1) Create a list of chromosomes from your BAM header and use ‘P’ for creating number of 
      #   parallel processes to run the mplieup. Change the mpileup filtering parameters accordingly
      files_tmp = paste(params$directory_out, "/", file2process.my2, ".tmp.{}.vcf", sep="");
      options00 = paste(" view -H ", file_in, "  | grep \"\\@SQ\" | sed 's/^.*SN://g' | cut -f 1 ",
                        " |xargs -I {} -n 1 -P 4 sh -c ", 
                        " \"", # start of the section of options that go surrounded by double quotes 
                            " samtools mpileup  -C50 -EDS -q50 -d10000 -Q", params$opt$st.vf.Q,
                            " -uf ", params$path_genome, " -r {} ",  file_in, " ",
                            " | bcftools view -vcg - > ", files_tmp, 
                        " \"", # end of the section of options that go surrounded by double quotes 
                        sep="") # end of paste.
      command = paste(command00, " ", options00, sep="");
      check2showcommand(params$opt$showc, command, file2process.my2);
      system(command);
    
      # -----------------------
      # Variant Calling in Parallel by Chr - step 2
      # -----------------------
      # 2) Merge the vcf files
      # First remove any previous file with that same name from previous runs before appending new content
      file.remove(file_out)
      # Now merge the tmp vcf files in file_out
      files_tmp2 = paste(params$directory_out, "/", file2process.my2, ".tmp.$F[0].vcf", sep="");
      options00 = paste(" view -H ", file_in, "  | grep \"\\@SQ\" | sed 's/^.*SN://g' | cut -f 1 ",
                      " | perl -ane 'system(", # nested system call 
                      "\"", # start of the section of options that go surrounded by double quotes 
                          "cat ", files_tmp2, " >> ", file_out, ".tmp",
                      "\"", # end of the section of options that go surrounded by double quotes 
                      ")'; ", # end of nested system call 
                      sep="") # end of paste.
      command = paste(command00, " ", options00, sep="");
      check2showcommand(params$opt$showc, command, file2process.my2);
      system(command);

      # -----------------------
      # Variant Calling in Parallel by Chr - step 3
      # -----------------------
      # 3) Count the number of header lines; number to be used in a step below
      #file_out <- "test_out2/sgs7b_merged12.sam.sorted.edited.bam.samtools.var.raw.vcf"
      file_tmp3 <-  paste(file_out, ".tmp", sep="");
      tmp3 <- readLines(file_tmp3)
      if (length(grep("#CHROM", tmp3)) > 0) {
        last.header.line.number <- grep("#CHROM", tmp3)[1]
      } else {
        # We could chose something else, but in case we can't find #CHROM, we get 
        # and arbritary num,ber of header lines here, which should be safe, upon the risk of repeating some info.
        last.header.line.number <- 40
      }

      
      # -----------------------
      # Variant Calling in Parallel by Chr - step 4
      # -----------------------
      # 4) Remove the unwanted headers from vcf files generated. 
      #   Each temporary file generated will have a header and the idea hear is to keep
      #   the first header and remove the rest.
      
      command00 = "cat "
      options00 = paste(" ", file_out, ".tmp  | sed -e 1,", last.header.line.number, 
                        "b -e '/^#/d' > ", file_out, "_new.vcf",
                        sep="") # end of paste.
      command = paste(command00, " ", options00, sep="");
      check2showcommand(params$opt$showc, command, file2process.my2);
      system(command);
      
      # -----------------------
      # Variant Calling in Parallel by Chr - step 5
      # -----------------------
      # file2process.my2 <- "sgs7a_merged12"
      # 5) Delete the temporary files
      setwd(paste("./", params$directory_out, sep="")) # move to the directory of the tmp files
      command00 = paste("ls ", file2process.my2,"* | grep -E tmp |xargs rm ", sep="")
      command = command00
      system(command);
      setwd("..") # Return to the previous directory where the whole pipeline runs
      check2showcommand(params$opt$showc, command, file2process.my2);
      
       # -----------------------
  }
     
  check2clean(file_in, file2process.my2);
  create.hard.link(file2process.my2, step.my, 
                   suffix_file_from=".sam.sorted.edited.bam.samtools.var.raw.vcf_new.vcf",
                   suffix_file_to=".sam.sorted.edited.vcf")
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.variant.filtering
###
### 	Variant Filtering, with BCFTOOLS vcfutils.pl varFilter, from Samtools package
###   http://samtools.sourceforge.net/samtools.shtml
##########################

fun.variant.filtering <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, "b. Variant Filtering with vcfutils.pl (Samtools): ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  file_in = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.bam.samtools.var.raw.vcf", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.edited.bam.samtools.var.filtered.vcf", sep="");
  command00 = params$path_vcfutils ; # next command.

  ## This line below contain the new params set as default for the EVA pipeline, as of March 13th, 2013.
  #former hardcoded params in 1 line: options00 = paste(" varFilter -Q10 -d10 -a2 -D10000000 -S1000 ", file_in, " > ", file_out, sep=""); 
  options00 = paste(" varFilter ",
                    " -Q", params$opt$st.vf.Q, # by default:        10
                    " -d", params$opt$st.vf.d, # by default:        10
                    " -a", params$opt$st.vf.a, # by default:         2
                    " -D", params$opt$st.vf.D, # by default:  10000000
                    " -S", params$opt$st.vf.S, # by default:      1000
                    " ", file_in, " > ", file_out, sep=""); 
  ## ToDo: set this values as params that can be set from the command line at pipeline run time (opt$...)
        ## Former params used in other test cases in the past while finetunning the pipeline
        ##   options00 = paste(" varFilter -Q 10 -d 15 -a 5 ", file_in, " > ", file_out, sep="");
        ##   options00 = paste(" varFilter -Q1 -d15 -D10000000 -a2 -S1 ", file_in, " > ", file_out, sep="");
    #
    # varFilter" params (SAMTOOLS vcftools.pl):
    # ------------------------------------------
    # (1) From "path_vcfutils" (usually in /usr/share/samtools/vcfutils.pl but path might depend on your system)
    # * Default values used by vcfutils.pl
    #     d=>2, D=>10000000, a=>2, W=>10, Q=>10, w=>3, p=>undef, 
    #     1=>1e-4, 2=>1e-100, 3=>0, 4=>1e-4, G=>0, S=>1000, e=>1e-4);
    #
    # (2) From "BCFTOOLS COMMANDS AND OPTIONS": http://samtools.sourceforge.net/samtools.shtml#4 )
    # * Minimum base quality for a base to be considered (param "Q"): 10 (90% accuracy)|20 (99.9%)|30 (99.99%)|40 (99.999%)|50 (99.9999%). See Wikipedia. We filtered out variants with less than this value in quality (so nothing was left out due to low quality at this stage).
    # * Minimum read depth (coverage) to call a SNP (param "d"): 10-15. We filtered out variants with less than this number of reads for that variant.
    # * Minimum number of alternate bases (param "a"): 2-3.
    # * Maximum read depth (coverage) to call a SNP (param "D"): 10.000.000
    #     The -D option of varFilter controls the maximum read depth, 
    #     which should be adjusted to about twice the average read depth.
    # * min P-value for strand bias (given PV4, param "1"). By default: 1e-4.
    # * min P-value for baseQ bias (param "2"). By default: 1e-100.
    # * min P-value for mapQ bias (param "3"). By default: 0.
    # * min P-value for end distance bias (param "4"). By default: 1e-4.
    # * min indel score for nearby SNP filtering (param "G"). By default: 0.
    # * minimum SNP quality (param "S"). By default: 1000.| the smaller, the better (more precise, more quality in the SNP). High values are too permissive.
    # * min P-value for HWE (plus F<0; param "e"). By default: 1e-4.
  
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);

  
  create.hard.link(file2process.my2, step.my, 
                   suffix_file_from=".sam.sorted.edited.bam.samtools.var.filtered.vcf",
                   suffix_file_to=".sam.sorted.edited.vcf")
  
  create.hard.link(file2process.my2, step.my, 
                suffix_file_from=".sam.sorted.edited.bam.samtools.var.filtered.vcf",
                suffix_file_to=".f.vcf")
  
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
  check2showcommand(params$opt$showc, command, file2process.my2);
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
  
#  file_in =  paste(params$directory_out, "/", file2process.my2, ".sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.filtered.vcf", sep="");
  # Since the hard link is created already, we can use the much shorter file name for input .f.vcf instead of .sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.filtered.vcf
  file_in =  paste(params$directory_out, "/", file2process.my2, ".f.vcf", sep="");
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
  check2showcommand(params$opt$showc, command, file2process.my2);
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
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  # We don't do check2clean here  either since we will still use the .vcf.annovar file in the next steps
  print_done(file2process.my2);
  
  # a mà la instrucció al mainhead és:
  # perl /home/ueb/annovar/annotate_variation.pl -geneanno --buildver hg19 /home/xavi/repo/peeva/dir_out/Gutierrez_B_Sure.sequence_m50.sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.f.vcf4 /home/ueb/annovar/humandb/
  # perl /home/ueb/annovar/annotate_variation.pl -geneanno --buildver hg19 /home/ueb/estudis/ngs/2011-08-SGutierrez-VHIO-207/111224_peeva_dir_out/Gutierrez_B_Sure.sequence.sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.f.vcf4 /home/ueb/annovar/humandb/
  
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
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  # We don't do check2clean here  either since we will still use the .vcf.annovar file in the next steps (summarizing annotations and filtering)
  print_done(file2process.my2);
  
  # a mà la instrucció al mainhead és:
  # perl /home/ueb/annovar/annotate_variation.pl -filter --buildver hg19 -dbtype snp132 /home/xavi/repo/peeva/dir_out/vhir_sample_a_sure_1e6.sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.f.vcf4 /home/ueb/annovar/humandb/
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.variant.annotation.summarize
###
### 	Variant Annotation with Annovar: summarize_annovar.pl
###     Given a list of variants from whole-exome or whole-genome sequencing, it will generate an Excel-compatible file with gene annotation, amino acid change annotation, SIFT scores, PolyPhen scores, LRT scores, MutationTaster scores, PhyloP conservation scores, GERP++ conservation scores, dbSNP identifiers, 1000 Genomes Project allele frequencies, NHLBI-ESP 5400 exome project allele frequencies and other information.
###     http://www.openbioinformatics.org/annovar/annovar_accessary.html#excel
###     
##########################

fun.variant.annotation.summarize <- function(file2process.my2, step.my) {
  # These commands need to be run in advanced, to fetch all requried databases for annovar to work (some of them are only needed when the version that they refer to).
  # perl ./annotate_variation.pl -buildver hg19 -downdb refgene humandb/
  # perl ./annotate_variation.pl -buildver hg19 -downdb phastConsElements46way humandb/
  # perl ./annotate_variation.pl -buildver hg19 -downdb genomicSuperDups humandb/
  # perl ./annotate_variation.pl -buildver hg19 -downdb 1000g2010nov -webfrom annovar humandb/
  # perl ./annotate_variation.pl -buildver hg19 -downdb 1000g2012apr -webfrom annovar humandb/
  # perl ./annotate_variation.pl -buildver hg19 -downdb snp132 -webfrom annovar humandb/
  # perl ./annotate_variation.pl -buildver hg19 -downdb snp135 -webfrom annovar humandb/
  # perl ./annotate_variation.pl -buildver hg19 -downdb snp137 -webfrom annovar humandb/
  # perl ./annotate_variation.pl -buildver hg19 -downdb avsift -webfrom annovar humandb/
  # perl ./annotate_variation.pl -buildver hg19 -downdb ljb_all -webfrom annovar humandb/
  # perl ./annotate_variation.pl -buildver hg19 -downdb esp6500_all -webfrom annovar humandb/
    
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Variant Annotation (summarize annotations in .csv): ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  # TODO: 	if ($options{s}) # case to summarize annotation results in a single .csv file ...
  file_in = paste(params$directory_out, "/", file2process.my2, ".f.vcf4", sep=""); 
  file_out = paste(file_in, ".sum", sep=""); # summarize_annovar.pl adds the extension .exome_summary.csv, and many other partial .csv files (hardcoded in annovar). 
  command00 = "perl"; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_summarize_annovar, " --buildver ", params$opt$genver, "  -remove -alltranscript ", 
                    " --verdbsnp ", params$opt$dbsnp," -ver1000g 1000g2012apr -veresp 6500 ",
                    file_in, " ", params$path_annotate_humandb, " --outfile ", file_out, " 2>> ", file_stderr, sep="");
            # Example: summarize_annovar.pl -out myanno -buildver hg19 -verdbsnp 135 -ver1000g 1000g2012apr -veresp 6500 -remove -alltranscript example/ex1_hg19.human humandb/
            # r99 (28/03/11 - 18:11h) Added  -alltranscript to print out all isoforms for exonic variants and to fix slight problems in variants_reduction.pl
            # The final command run SUMMARIZE_ANNOVAR, using dbSNP version 137, 1000 Genomes Project 2012 April version, ESP6500 database,
            # and remove all temporary files, and print out all transcriptional isoforms (rather than one isoform), 
            # and generates two output files: myanno.exome_summary.csv and myanno.genome_summary.csv. 
  
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  # # We don't do check2clean here  either since we will still use the .vcf.annovar file in the next steps (filtering)
  print_done(file2process.my2);
  
  ### Call to the function to fix INFO fields in columns from the csv generated by annovar
  ##fun.variant.annotation.summary.call.fixcolumns( file2process.my2, step.my)
  # Removed call from here since it will be called from eva_analysis_wrappers.R after  
  # fun.variant.annotation.summarize is finished
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.split.csv.info.varN.fields
###
###   Edit one variable with n (e.g. 4) values into n (four) variables with one value each
###   Arguments:
###     string.my: variable such as DP4 or VP4 with 4 values to be splited, or others with less
###     tmp.my: vector with the contents where DP4, VP4, etc can be found
###     nfields.my: number of values inside the contents (usually either 2, 3 or 4)
###     
##########################

fun.split.csv.info.varN.fields <- function(string.my, tmp.my, nfields.my) { 
  
  # Manual debugging
  # tmp.my <- as.character(unlist(strsplit(raw.df[1], ";")))
  # string.my <- "PV4"
  # string.my <- "PC2"
  # tmp.my <- tmp
  # nfields.my <- "2"
  # ensure that nfields.my is treated as number
  nfields.my <- as.numeric(nfields.my)
  
  # Split DP4 and VP4 into four columns to allow fine grained filtering on the spreadsheet by the researcher
  # Get their indexes if found
  varN.tmp.idx <- grep(string.my, tmp.my) # for instance, string.my <- "DP4"
  
  #  Check if the variable is found. Otherwise, skip.
  if ( length(varN.tmp.idx) > 0) {
    
    # Split content in two (left of =, and right of =)
    varN.tmp <- as.character(unlist(strsplit(tmp.my[varN.tmp.idx], "=")))
    # SPlit value in as many as the value of the param nfields.my
    varN.tmp.values <- unlist(strsplit(as.character(varN.tmp[2]), ","))
    varN.tmp.labels <- paste(string.my, ".", 1:nfields.my, "=", sep="")
  
    #cat("\nvarN.tmp.idx: ")
    #cat(varN.tmp.idx)
    #cat("\n")
    
    # prepare the 3 pieces to be joined later
    if (varN.tmp.idx > 1) {
      varN.tmp.a <- tmp.my[1:(varN.tmp.idx-1)]
      } else if (varN.tmp.idx == 1) { # The first value
      varN.tmp.a <- tmp.my[1]
      }
    
      varN.tmp.b <- paste(varN.tmp.labels, varN.tmp.values, sep="" )
    
      if (length(tmp.my) != as.numeric(varN.tmp.idx)) {
        # It was not the last value, add the remaining columns at the end
        varN.tmp.c <- tmp.my[as.numeric(varN.tmp.idx+1) : as.numeric(length(tmp.my))]
      } else {
        # It was the last value; don't add anything at the end
        varN.tmp.c <- NULL
      }
  
    # Assign the new content (splitted by columns) to tmp again for further processing 
    tmp.my <- c(varN.tmp.a, varN.tmp.b, varN.tmp.c)
  }
  

  # Return results
  return(tmp.my)
  
}

##########################
### FUNCTION fun.split.csv.info.fields
###
###   Split the contents in the INFO field of the csv files produced by samtools 
###   so that each variable is splitted into one column and the value of the variable its value in that cell
###
###   Arguments:
###     s.my: character to split strings by (usually, "=")
###     v.my: vector (from the source data frame) to be converted into values
###     x.my: data frame with all values to be saved to, once splitted by variables
###     n.my: number of elements in the dataframe to be processed one by one (vector by vector) 
###     m.my: number of variables=value in the string to split by semicolon
###     
##########################

fun.split.csv.info.fields <- function(raw.df, s.my, n.my, x.my, m.my) { 
  
  # Manual debugging
  # raw.df <- f2p.se$V35
  # s.my <- "="
  # n.my <- n
  # m.my <- m
  # x.my <- x
  # i<- 1
  
  # Convert the contents of the data frame from factors into text
  raw.df <- as.character(raw.df)
  
  # If there are indels ("INDEL;"), replace with "INDEL=1".
  raw.df <- gsub("INDEL;", "INDEL=1;", raw.df, ignore.case = FALSE, fixed=TRUE)
  
  # Loop to process the whole data frame
 # start1 <- Sys.time();
  for (i in 1:n.my) {
    
    # Manual debugging 
    # raw.df <- as.character(f2p.se$V35)
    #cat("\nn.my: ")
    #cat(n.my)
    #cat("\n")
    # s.my <- "="
    # i<- 1
    # i <- i +1
    # class(raw.df)
    
    # First, split by semicolon
    tmp <- as.character(unlist(strsplit(raw.df[i], ";")))

    #length(tmp)
    tmp <- fun.split.csv.info.varN.fields("DP4", tmp, nfields=4)
    tmp <- fun.split.csv.info.varN.fields("PV4", tmp, nfields=4)
    tmp <- fun.split.csv.info.varN.fields("G3", tmp, nfields=3)
    tmp <- fun.split.csv.info.varN.fields("PC2", tmp, nfields=2)
    
    
    # Then, split by the character which divides the variable and the value (usually var=value, therefore s.my: "=")
    tmp <- strsplit(tmp, s.my)
    
    ## Four ways to convert a list into a data frame
    # http://stackoverflow.com/questions/4227223/r-list-to-data-frame
    ## (1) Convert the list into a data frame. Titles are weird
    #do.call(rbind.data.frame, tmp)
    ## (2) This doesn't work as expected. http://stackoverflow.com/questions/4227223/r-list-to-data-frame
    #library(plyr)
    #tmp <- ldply(tmp, data.frame)
    # (3) This works fine, even if will be difficult to remember, maybe
    #tmp <- data.frame(matrix(unlist(tmp), nrow=m.my, byrow=T))
    # (4) This also works fine (c seems to be a function to display the contents???)
    # and maybe is the most intuitive or easy to remember (If I succeed to remember the "c" function)
    tmp <- data.frame(t(sapply(tmp, c)))
    
    etiq <- tmp[1]
    valor <- tmp[2]
    #length(tmp)
    #Manual debugging
    # m.my <- m
    # i <- 1 # Rows
    # j # Columns
    for (j in 1:nrow(etiq)) {
      #Manual debugging
      # j <-1
      #j <- j+1
      #x.my["DP"][i,1]
      x.my[as.character(etiq[j,1])][i,1] <- as.numeric(as.character(valor[j,1]))
      #x.my
    }
  } # end of for loop
#  duration1 <- Sys.time()-start1; cat(duration1)
  
#   # Second way to do it, withoput loops but Apply'ies
#   # ---------------------------------------
#   start2 <- Sys.time();
#   # First, split by semicolon
#   tmp <- sapply(strsplit(as.character(raw.df), "\\;"), "[[", 2)
#   head(raw.df)
#   head(tmp)
#   #length(tmp)
# 
#   library(stringr)
#   tmp <- do.call(rbind, str_split(raw.df, ";"))
#   head(tmp)
#   class(tmp)
#   #tmp <- fun.split.csv.info.varN.fields("DP4", tmp, nfields=4)
#   #tmp <- fun.split.csv.info.varN.fields("PV4", tmp, nfields=4)
#   #tmp <- fun.split.csv.info.varN.fields("G3", tmp, nfields=3)
#   #tmp <- fun.split.csv.info.varN.fields("PC2", tmp, nfields=2)
#   
#   
#   str_split(tmp, s.my)
#   # Then, split by the character which divides the variable and the value (usually var=value, therefore s.my: "=")
#   tmp <- do.call(rbind, str_split(tmp, s.my))
#   head(tmp)
#   sapply(strsplit(as.character(tmp), s.my), "[[", 2)
#   
#   tmp <- strsplit(tmp, s.my)
#   
#   head(tmp)
#   
#   ## Four ways to convert a list into a data frame
#   # http://stackoverflow.com/questions/4227223/r-list-to-data-frame
#   ## (1) Convert the list into a data frame. Titles are weird
#   #do.call(rbind.data.frame, tmp)
#   ## (2) This doesn't work as expected. http://stackoverflow.com/questions/4227223/r-list-to-data-frame
#   #library(plyr)
#   #tmp <- ldply(tmp, data.frame)
#   # (3) This works fine, even if will be difficult to remember, maybe
#   #tmp <- data.frame(matrix(unlist(tmp), nrow=m.my, byrow=T))
#   # (4) This also works fine (c seems to be a function to display the contents???)
#   # and maybe is the most intuitive or easy to remember (If I succeed to remember the "c" function)
#   tmp <- data.frame(t(sapply(tmp, c)))
#   
#   etiq <- tmp[1]
#   valor <- tmp[2]
#   #length(tmp)
#   #Manual debugging
#   # m.my <- m
#   # i <- 1 # Rows
#   # j # Columns
#   for (j in 1:nrow(etiq)) {
#     #Manual debugging
#     # j <-1
#     #j <- j+1
#     #x.my["DP"][i,1]
#     x.my[as.character(etiq[j,1])][i,1] <- as.numeric(as.character(valor[j,1]))
#     #x.my
#   }
#   duration2 <- Sys.time()-start2; cat(duration2)
  
  return  (x.my) 
}  # end fo function

##########################
### FUNCTION fun.variant.annotation.summary.call.fixcolumns
###
###   Call the function to fix INFO Field colums, from the csv files produced by Annovar (summarize_annovar.pl) in a previous step, 
###     so that OTHER INFO is split in columns filterable from simple spreadsheets by the researcher
###     
##########################

fun.variant.annotation.summary.call.fixcolumns <- function(file2process.my2, step.my) {
  
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Call the fixing of INFO columns for EXOME csv files from annovar: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  # Fix INFO cols for exome_summary csv file
  file_in1 = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.my_genes.exome_summary.csv", sep=""); 
  file_out1 = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.my_genes.colsfixed.exome_summary.csv", sep="");
  fun.variant.annotation.summary.fixcolumns(file2process.my2, step.my, "EXOME", file_in1, file_out1)

  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Call the fixing of INFO columns for GENOME csv files from annovar: ", file2process.my2, " ###\n", sep=""), file2process.my2);

  # # We don't do check2clean here  either since we will still use the .vcf.annovar file in the next steps (filtering)
  print_done(file2process.my2);
  
  # Fix INFO cols for genome_summary csv file
  file_in2 = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.my_genes.genome_summary.csv", sep=""); 
  file_out2 = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.my_genes.colsfixed.genome_summary.csv", sep="");
  fun.variant.annotation.summary.fixcolumns(file2process.my2, step.my, "GENOME", file_in2, file_out2)
  
  # # We don't do check2clean here  either since we will still use the .vcf.annovar file in the next steps (filtering)
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}


##########################
### FUNCTION fun.variant.annotation.summary.fixcolumns
###
###   Edit the csv files produced by Annovar (summarize_annovar.pl) in a previous step, 
###     so that OTHER INFO is split in columns filterable from simple spreadsheets by the researcher
###     
##########################

fun.variant.annotation.summary.fixcolumns <- function(file2process.my2, step.my, label, file_in.my, file_out.my) {
  
  ## Manual debugging
  # params$directory_out <- "/home/ueb/repo/peeva/test_out2"
  # file2process.my2 <- "sgs7a_merged12"
  # file2process.my2 <- "00test"
  # file_in1 = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.exome_summary.csv", sep=""); 
  # file_out1 = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.colsfixed.exome_summary.csv", sep="");
  # file_in.my <- file_in1
  # file_out.my <- file_out1
  # step.my <- step
  # step.my$tmp <- "2"
  # params$log.folder <- params$directory_out
  # params$startdate <- "130424"
  # params$opt$label <- "foo"
  
  # update step number
  #step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Fix cols for the ", label, " csv file: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");

  # load the file created as file2process.sum.edition (aka: f2p.sr)
  f2p.se <- read.csv(file_in.my, header=F, skip=1)

  # Do all the processing only if there is some data to process. To avoid the program to break when snpEff fails to count reads
  # dues to the issues with "MAPQ should be zero for unmmaped reads", etc. 
  if (length(f2p.se) > 0) {
    
    # Load the header, add missing column names with default names so far, and assign them as column names
    con.my <- file(file_in.my)
    f2p.se.header.ini <- readLines(con.my, 1);
    close(con.my)
    f2p.se.header.ini <- unlist(strsplit(f2p.se.header.ini, ","))
    f2p.se.header.end <- paste("V", (length(f2p.se.header.ini)+1):length(f2p.se), sep="")
    f2p.se.header <- c(f2p.se.header.ini, f2p.se.header.end)
    colnames(f2p.se) <- f2p.se.header
    
    #f2p.se$V35
    #length(f2p.se$V35)
    V38 <- strsplit(as.character(f2p.se$V35), ";")
    n <- length(V38)
    
    # Calculate the number "m" of columns in the specific vector to process right now
    m <- max(sapply(V38, length))

    # Create the data.frame as place holder of results, for the 18 potential columns in the vcf generated by samtools.
    x <- as.data.frame(matrix(0, n, ncol=27))   
    
    # ToDo: extend the list of potential column names
    colnames(x) <- c("DP", "DP4.1", "DP4.2", "DP4.3", "DP4.4",
                     "MQ", "FQ", "AF1", "AC1", "G3.1", "G3.2", "G3.3", 
                     "HWE", "CLR", "UGT", "CGT", 
                     "PV4.1","PV4.2","PV4.3","PV4.4",
                     "INDEL", "PC2.1", "PC2.2", "PCHI2", "QCHI2", "PR", "VDB")
    
    x <- fun.split.csv.info.fields(f2p.se$V35, "=", n, x, m)
    
    # Replace column 35 by its contents splitted in one column per variable
    f2p.se.a <- f2p.se[,1:34] # First part
    f2p.se.b <- x # Second part
    f2p.se.c <- f2p.se[,36: ncol(f2p.se)] # Third part
    # Re-assign everything to the file to process
    f2p.se <- cbind(f2p.se.a, f2p.se.b, f2p.se.c)
    
    colnames(f2p.se)[28:34] <- c("CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER")
    
    # I remove the warning messages from this call (below) because of the coercion type of message (harmless, afaik, but annoying and polluting output in log files)
    # Write the rest: all data without column names
    write.table(f2p.se, file=file_out.my, append=F, quote=T, sep=",", row.names=F, col.names=T)
    #write.ftable(f2p.cr.table, file=file_out.table, append=T, quote=T)
    
  } else {
    print_mes("XXX Reading the csv file failed.", file2process.my2)
  }
  
  # # We don't do check2clean here  either since we will still use the .vcf.annovar file in the next steps (filtering)
  print_done(file2process.my2);
  
  # a mà la instrucció al mainhead és:
  # perl /home/ueb/annovar/summarize_annovar.pl --buildver hg19  --verdbsnp 137 /home/xavi/repo/peeva/dir_out/vhir_sample_a_sure_1e6.sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.f.vcf4 /home/ueb/annovar/humandb/ --outfile sample_a_sure_sum
  
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
    file_in  = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.", params$opt$genver ,"_snp", params$opt$dbsnp, "_filtered", sep=""); 
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
      file_out_g_csv = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.my_genes.genome_summary.csv", sep=""); 
      command01 = "head -1";
      command02 = command00;
      options01 = paste(" ", file_in_g_csv, " > ", file_out_g_csv, sep="");
      options02 = paste(" '", params$opt$filter,"' ", file_in_g_csv, " >> ", file_out_g_csv, sep="");

      #######################
      # 2nd - sample.*.exome_summary.csv
      #-------------------------------
      file_in_e_csv  = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.exome_summary.csv", sep=""); # summarize_annovar.pl adds the extension .exome_summary.csv (hardcoded in annovar).
      file_out_e_csv = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.my_genes.exome_summary.csv", sep=""); 
      command03 = "head -1";
      command04 = command00;
      options03 = paste(" ", file_in_e_csv, " > ", file_out_e_csv, sep="");
      options04 = paste(" '", params$opt$filter,"' ", file_in_e_csv, " >> ", file_out_e_csv, sep="");
    }
  } else { # skip the searching for specific target genes 
    command00 = "echo '  ...skipped...'"; # next command.
    options00 = "";
  }
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  if (!is.null(params$opt$filter) && (params$opt$filter != "") && !is.null(params$opt$summarize) ) # case to have a file with summarized annotations to search also for specific target genes 
  {
    command = paste(command01, " ", options01, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
    system(command);
    command = paste(command02, " ", options02, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
    system(command);
    command = paste(command03, " ", options03, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
    system(command);
    command = paste(command04, " ", options04, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
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
### FUNCTION fun.variant.fii.pre.snpeff
###
###     Filter Intersecting Intervals to get only values for the target genes in vcf files
###     Get a list of the genetic intervals of those target genes (in BED format): targetgenes.bed
###     Use "SnpSift intidx" to extract those regions: 
###
###     More details here: http://snpeff.sourceforge.net/SnpSift.html#intidx
###
##########################

fun.variant.fii.pre.snpeff <- function(file2process.my2, step.my) {
  
# - Get a list of the genetic intervals of those genes (in BED format): ...my_genes.bed
# - Use "SnpSift intidx" to extract those regions: 
#   java -jar SnpSift.jar intidx variants.vcf my_genes.bed > variants_intersecting_intervals.vcf
# 
# More details here: http://snpeff.sourceforge.net/SnpSift.html#intidx

  #   #Manual debugging - ini
  #   file2process.my2 <-"sample_a_merged12.sam"
  #   step.my <- data.frame(10, 0)
  #   colnames(step.my) <- c("n","tmp")
  #   step.my$tmp <- 0
  #   #Manual debugging - end
  
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Filter Intersecting Intervals in f.vcf from target genes (SnpSift using ...my_genes.bed): ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  # Example of process in the command line
  #   java -jar SnpSift.jar intidx variants.vcf my_genes.bed > variants_intersecting_intervals.vcf
#  file_in  = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.filtered.vcf", sep=""); 
  # Since the hard link is created already, we can use the much shorter file name for input .f.vcf instead of .sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.filtered.vcf
  file_in =  paste(params$directory_out, "/", file2process.my2, ".f.vcf", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".f.my_genes.vcf", sep="");
  # "ssfii" stands for SnpSift Filtered Interseting Intervals. But was later renamed to "my_genes"
  
  # As defined in the previous step
  file_my_bed = file=paste(params$log.folder,"/", params$startdate, ".", params$opt$label,".my_genes.bed", sep="")
  
  command00 = "java -jar"; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_snpEff, "SnpSift.jar intidx ", file_in," ",
                    file_my_bed, " > ", file_out, " 2>> ", file_stderr, sep="");
  
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  
  # # We don't do check2clean here  since the output are results
  print_done(file2process.my2);
  
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
  
}

##########################
### FUNCTION fun.variant.filter.pre.snpeff
###
###     We annotate using dbSnp before using SnpEff in order to have 'known' and 'unknown' statistics in SnpEff's summary page.
###     Those stats are based on the presence of an ID field. If the ID is non-empty, then it is assumed to be a 'known variant'. 
###     From: http://snpeff.sourceforge.net/examples.html
##########################

fun.variant.filter.pre.snpeff <- function(file2process.my2, step.my) {
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

  #   Annotate dbNSFP database
  #   From http://snpeff.sourceforge.net/SnpSift.html#dbnsfp
  #   -----------------------------------------------------------
  #
  #   The dbNSFP is an integrated database of functional predictions from multiple algorithms (SIFT, Polyphen2, LRT and MutationTaster, PhyloP and GERP++, etc.). One of the main advantages is that you can annotate using multiple prediction tools with just one command. Here is the link to dbNSFP database website.
  #   In order to use this command, you need to download the dbNSFP database file from our 'databases' directory. The file is named 'dbNSFP-version.txt.gz', where 'version' is the version number (e.g. 'dbNSFP2.0b3.txt.gz').
  #   Another option is to download from the original dbNSFP site, and create a single file from the individual chromosome files (a simple 'cat dbNSFP2.0b3_variant.chr* | grep -v "^#" > dbNSFP2.0b3.txt' command will be enough).
  #   
  #   Here is a full example how to perform annotations:
  #     
  #     # Donload and uncompress database (you need to do this only once):
  #     # WARNING: The database is 3Gb when compressed and 30Gb uncompressed.
  #     wget http://sourceforge.net/projects/snpeff/files/databases/dbNSFP2.0b3.txt.gz
  #   gunzip dbNSFP2.0b3.txt.gz
  #   
  #   # Annotate using dbNSFP
  #   java -jar SnpSift.jar dbnsfp -v dbNSFP2.0b3.txt myFile.vcf > myFile.annotated.vcf
  #   Note: You can now specify which fields you want to use for annotation using the '-f' command line option followed by a comma separated list of field names.
  #   Defaults fileds are "Ensembl_transcriptid,Uniprot_acc,Interpro_domain,SIFT_score,Polyphen2_HVAR_pred,GERP++_NR,GERP++_RS,29way_logOdds,1000Gp1_AF,1000Gp1_AFR_AF,1000Gp1_EUR_AF,1000Gp1_AMR_AF,1000Gp1_ASN_AF,ESP5400_AA_AF,ESP5400_EA_AF".
  #   ---------------------------
  
  #   #Manual debugging - ini
  #   file2process.my2 <-"sample_a_merged12.sam"
  #   step.my <- data.frame(10, 0)
  #   colnames(step.my) <- c("n","tmp")
  #   step.my$tmp <- 0
  #   #Manual debugging - end
  
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Variant Filtration with SnpSift: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  # SubStep 1 # Annotate ID field using SnpSift & dbSnp
  # ----------------------------------------------------
  # Display substep name
  print_doc(paste(" Step ", step.my$n, ".", step.my$tmp, "a) Annotate ID field using SnpSift & dbSnp: ", file2process.my2, " ###\n", sep=""), file2process.my2);

  # Example of process in the command line
  #   java -jar SnpSift.jar annotate -v dbSnp.vcf.gz file.vcf > file.dbSnp.vcf
#  file_in  = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.filtered.vcf", sep=""); 
  # Since the hard link is created already, we can use the much shorter file name for input .f.vcf instead of .sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.filtered.vcf
  file_in =  paste(params$directory_out, "/", file2process.my2, ".f.vcf", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".f.ssann.vcf", sep=""); 
  # "ssann" in "...filtered.ssann.vcf" stands for annotated (ann) by SnpSift (ss)
  command00 = "java -jar"; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_snpEff, "SnpSift.jar annotate -v ", params$path_dbSNP," ",
                    file_in, " > ", file_out, " 2>> ", file_stderr, sep="");
  
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);

  # SubStep 2 # Annotate using SnpEff
  # ------------------------------------------
  # Display substep name
  print_doc(paste(" Step ", step.my$n, ".", step.my$tmp, "b) Annotate using SnpEff the file from the previous step: ", file2process.my2, " ###\n", sep=""), file2process.my2);

  # # Do this only if you don't already have the database installed.
  #     java -jar snpEff.jar download -v GRCh37.66
  #       or this if you are behind a proxy:
  #     java -DproxySet=true -DproxyHost=yourproxy -DproxyPort=portnumber -jar snpEff.jar download -v GRCh37.66

  # # Annotate the file
  # java -Xmx4g -jar snpEff.jar eff -v GRCh37.66 file.dbSnp.vcf > file.eff.vcf
  file_in  = paste(params$directory_out, "/", file2process.my2, ".f.ssann.vcf", sep=""); 
  file_out = paste(params$directory_out, "/", file2process.my2, ".f.ssann.eff.vcf", sep=""); 
  # "ssann" in "...filtered.ssann.vcf" stands for annotated (ann) by SnpSift (ss)
  command00 = "java -jar"; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_snpEff, "snpEff.jar eff -t  -c ", params$path_snpEff, "snpEff.config ",
                    " -v ", params$opt$se_db_rg," ",
                    file_in, " > ", file_out, " 2>> ", file_stderr, sep="");
  
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);


  # SubStep 3 # Generate file with my_rs codes from target genes 
  # ------------------------------------------------------------
  # Display substep name
#  print_doc(paste(" Step ", step.my$n, ".", step.my$tmp, "c) Generate file with my_rs codes from target genes", file2process.my2, " ###\n", sep=""), file2process.my2);

  # From http://snpeff.sourceforge.net/SnpSift.html#filter
  #   I want to keep samples where the ID matches a set defined in a file:
  #
  #     cat variants.vcf | java -jar SnpSift.jar filter --set my_rs.txt "ID in SET[0]" > filtered.vcf
  #
  #   The file my_rs.txt has one string per line, e.g.:
  #   rs58108140
  #   rs71262674
  #   rs71262673
  ##################### Another potential approach, maybe? #############
  # So far, I'm getting this list by hand from   
  # http://www.scandb.org/newinterface/index.html
  # I upload there the list of genes with 
  ##################### Another potential approach, maybe? #############
  
  
#   # SubStep 4 # Filter using SnpSift for target genes (my_rs.txt) 
#   # ------------------------------------------
#   # Display substep name
#   print_doc(paste(" Step ", step.my$n, ".", step.my$tmp, "c) Filter using SnpSift for target genes (my_rs.txt): ", file2process.my2, " ###\n", sep=""), file2process.my2);
#   #     cat variants.vcf | java -jar SnpSift.jar filter --set my_rs.txt "ID in SET[0]" > filtered.vcf
#   file_in  = paste(params$directory_out, "/", file2process.my2, ".f.ssann.eff.vcf", sep=""); 
#   file_out = paste(params$directory_out, "/", file2process.my2, ".f.ssann.eff.fss.vcf", sep=""); # fss stands for Filtered by SnipSift
#   # "ssann" in "...f.ssann.vcf" stands for annotated (ann) by SnpSift (ss)
#   # "fss" in "....f.ssann.eff.fss.vcf" stands for Filtered by SnipSift
#   command00 = paste("cat ", file_in, " | java -jar", sep=""); # next command.
#   # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
#   file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
#   options00 = paste(" ", params$path_snpEff, "SnpSift.jar filter --set ", 
#                     paste(params$directory_in, "/", params$opt$f_my_rs, sep=""), 
#                     " \"ID in SET[0]\" ", " > ", file_out, " 2>> ", file_stderr, sep="");
#   
#   command = paste(command00, " ", options00, sep="");
#   check2showcommand(params$opt$showc, command, file2process.my2);
#   system(command);
  
  
  # SubStep 5 # Filter ID field using SnpSift
  # ------------------------------------------
  # Display substep name
  print_doc(paste(" Step ", step.my$n, ".", step.my$tmp, "c) Filter ID field using SnpSift: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  # Filter out variants that have a non-empty ID field. These variants are the ones that are NOT in dbSnp, since we annotated the ID field using rs-numbers from dbSnp in step 1.
  # java -jar SnpSift.jar filter -f file.eff.vcf "! exists ID" > file.eff.not_in_dbSnp.vcf
  # 
  # Note: The expression using to filter the file is "! exists ID". This means that the ID field does not exists (i.e. the value is empty) which is represented as a dot (".") in a VCF file. 
  
  file_in  = paste(params$directory_out, "/", file2process.my2, ".f.ssann.vcf", sep=""); 
  file_out = paste(params$directory_out, "/", file2process.my2, ".f.ssann.not_in_dbSnp.vcf", sep=""); 
  # "ssann" in "...filtered.ssann.vcf" stands for annotated (ann) by SnpSift (ss)
  command00 = "java -jar"; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_snpEff, "SnpSift.jar filter -f ", 
                    file_in, " \"! exists ID\" ", " > ", file_out, " 2>> ", file_stderr, sep="");
  
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  
  # Show errors (if any)
  obj.file_stderr <- read.delim(file_stderr, header = FALSE, sep=":")
  obj.file_stderr <- unlist(obj.file_stderr, use.names = FALSE)[obj.file_stderr$V1=="Error"]
  # If we found any error, print it.
  if (length(obj.file_stderr) > 2) {
    print_doc(paste(obj.file_stderr[1:2], "\n", sep=""),  file2process.my2);
    # If this shows NA, then there was only 1 error message
    print_doc(paste(obj.file_stderr[3:4], "\n", sep=""), file2process.my2);  
  } # Display the error message
  # # We don't do check2clean here  since the output are results
  print_done(file2process.my2);
  
 
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
  
  
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
### FUNCTION fun.grep.pre.snpeff.report
###
###   Select variants (based on grep calls) for the target genes
###   in the vcf file before the snpEff report is called 

##########################

fun.grep.pre.snpeff.report <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Select variants for the target genes based on grep calls before the snpEff report: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  if (!is.null(params$opt$filter) && (params$opt$filter != "")) {
    #######################
    # 1st part - sample.*.snpEff.[params$opt$snpeff.of] (= .vcf, .txt, ...)
    #-------------------------------
#    file_in  = paste(params$directory_out, "/", file2process.my2, ".f.ssann.eff.vcf", sep=""); 
#    file_out = paste(params$directory_out, "/", file2process.my2, ".f.ssann.eff.my_genes.vcf", sep=""); 

#    file_in  = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.filtered.vcf", sep=""); 
    # Since the hard link is created already, we can use the much shorter file name for input .f.vcf instead of .sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.filtered.vcf
    file_in =  paste(params$directory_out, "/", file2process.my2, ".f.vcf", sep="");
    file_out = paste(params$directory_out, "/", file2process.my2, ".f.my_genes.vcf", sep=""); 
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
    check2showcommand(params$opt$showc, command, file2process.my2);
    system(command);
    
    command = paste(command02, " ", options02, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
    system(command);
    
  } else { # skip the searching for specific target genes 
    
    command00 = "echo '  ...skipped...'"; # next command.
    options00 = "";
    command = paste(command00, " ", options00, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
    system(command);
  }
  
  #check2clean(file_in, file2process.my2);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
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
#   file2process.my2 <-"sample_a_merged12.sam"
#   step.my <- data.frame(10, 0)
#   colnames(step.my) <- c("n","tmp")
#   step.my$tmp <- 0
#   #Manual debugging - end
  
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Variant Annotation & Effect prediction with snpEff: ", file2process.my2, " ###\n", sep=""), file2process.my2);

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
  
  # Substep 1
  # Do the report with the variants in the target genes only
  # ---------------------------------------------------------
    print_doc(paste("    Substep ", step.my$n, ".", step.my$tmp, "a). Report using snpEff with the target genes only: ", file2process.my2, " ###\n", sep=""), file2process.my2);

  file_in  = paste(params$directory_out, "/", file2process.my2, ".f.my_genes.vcf", sep="");   
  file_out = paste(params$directory_out, "/", file2process.my2, ".f.my_genes.snpEff.", params$opt$snpeff.of, sep=""); 
  file_out_base = paste(params$directory_out, "/", file2process.my2, ".f.my_genes.snpEff", sep="");   
  
  command00 = "java -jar"; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_snpEff, "snpEff.jar -c ", params$path_snpEff, "snpEff.config -v ", params$opt$se_db_rg," ", file_in, 
                    " -a 0 -i vcf -o ", params$opt$snpeff.of," -chr chr -stats ", file_out_base,"_summary.html > ", file_out,
                    " 2>> ", file_stderr, sep="");
  
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
  system(command);
  
  # Show errors (if any)
  # --------------------
  obj.file_stderr <- read.delim(file_stderr, header = FALSE, sep=":")
  obj.file_stderr <- unlist(obj.file_stderr, use.names = FALSE)[obj.file_stderr$V1=="Error"]
  # If we found any error, print it.
  if (length(obj.file_stderr) > 2) {
    print_doc(obj.file_stderr[1:2],  file2process.my2);
    # If this shows NA, then there was only 1 error message
    print_doc(obj.file_stderr[3:4],  file2process.my2);  
  } # Display the error message
  
  
  
 # Substep 2
 # Do the report with the whole list of variants, and not only the ones from the target genes
 # ---------------------------
 print_doc(paste("    Substep ", step.my$n, ".", step.my$tmp, "b). Report using snpEff with the whole list of genes: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
#  file_in  = paste(params$directory_out, "/", file2process.my2, ".sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.filtered.vcf", sep=""); 
  # Since the hard link is created already, we can use the much shorter file name for input .f.vcf instead of .sam.sorted.fixmate.dupmarked.noDup.bam.samtools.var.filtered.vcf
  file_in =  paste(params$directory_out, "/", file2process.my2, ".f.vcf", sep="");
  file_out = paste(params$directory_out, "/", file2process.my2, ".f.snpEff.", params$opt$snpeff.of, sep=""); 
  file_out_base = paste(params$directory_out, "/", file2process.my2, ".f.snpEff", sep="");     

  command00 = "java -jar"; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, ".", params$opt$label,".", file2process.my2, ".txt", sep="");
  #  options00 = paste(" ", params$path_snpEff, "snpEff.jar -t -c ", params$path_snpEff, "snpEff.config -v ", params$opt$se_db_rg," ", file_in, 
  # Multithread (-t) needs to be disabled for the stats to be performed and html file produced.
  options00 = paste(" ", params$path_snpEff, "snpEff.jar -c ", params$path_snpEff, "snpEff.config -v ", params$opt$se_db_rg," ", file_in, 
                    " -a 0 -i vcf -o ", params$opt$snpeff.of," -chr chr -stats ", file_out_base,"_summary.html > ", file_out,
                    " 2>> ", file_stderr, sep="");
  
  command = paste(command00, " ", options00, sep="");
  check2showcommand(params$opt$showc, command, file2process.my2);
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
  
  # ------------------------------ (end of the two substeps)
  
  # We don't do check2clean here  since the output are results
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
  
}


##########################
### FUNCTION fun.grep.post.snpeff.report
###
###   Select variants (based on grep calls) for the target genes
###   in the vcf file after the snpEff report is called 

### XXX to be revised
##########################

fun.grep.post.snpeff.report <- function(file2process.my2, step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1
  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Select variants for the target genes based on grep calls after the snpEff report: ", file2process.my2, " ###\n", sep=""), file2process.my2);
  
  if (!is.null(params$opt$filter) && (params$opt$filter != "")) {
    #######################
    # 1st part - sample.*.snpEff.[params$opt$snpeff.of] (= .vcf, .txt, ...)
    #-------------------------------
    file_in  = paste(params$directory_out, "/", file2process.my2, ".f.snpEff.", params$opt$snpeff.of, sep=""); 
    file_out = paste(params$directory_out, "/", file2process.my2, ".f.snpEff.my_genes.", params$opt$snpeff.of, sep=""); 
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
    check2showcommand(params$opt$showc, command, file2process.my2);
    system(command);
    
    command = paste(command02, " ", options02, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
    system(command);

    #######################
    # 2nd part- sample.*.snpEff_summary.genes.txt
    #-------------------------------
    file_in  = paste(params$directory_out, "/", file2process.my2, ".f.snpEff_summary.genes.txt", sep=""); 
    file_out = paste(params$directory_out, "/", file2process.my2, ".f.snpEff_summary.genes.my_genes.txt", sep=""); 
    # Redo the filter from the clean list of genes, and splitted with "\|" when in the command line
    # therefore, they need to be splitted with '\\|' here in R.
    filter.c2 <- gsub(" ", "\\|", params$opt$filter.c, fixed=TRUE)
    # Remember that the values in the previous opt$filter variable needs to be like: 'BRCA1\|BRCA2' 
    # in order to end up performing a command like:
    # grep 'BRCA1\|BRCA2' dir_out/Gutierrez_A_*.exonic* 
    
    command01 = "grep ^\\#";
    command02 = "grep -w "; # next command. With -w we indicate it to match only the whole word. BRCA1 would not be matched against BRCA, for instance.
    options01 = paste(" ", file_in, " > ", file_out, sep="");
    options02 = paste(" '", filter.c2,"' ", file_in, " >> ", file_out, sep="");
    
    command = paste(command01, " ", options01, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
    system(command);
    
    command = paste(command02, " ", options02, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
    system(command);
    
  } else { # skip the searching for specific target genes 
    
    command00 = "echo '  ...skipped...'"; # next command.
    options00 = "";
    command = paste(command00, " ", options00, sep="");
    check2showcommand(params$opt$showc, command, file2process.my2);
    system(command);
  }
  
  #check2clean(file_in, file2process.my2);
  print_done(file2process.my2);
  
  gc() # Let's clean ouR garbage if possible
  return(step.my) # return nothing, since results are saved on disk from the system command
}

