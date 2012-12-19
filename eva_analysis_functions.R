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
  write(mess, file=paste(params$log.folder,"/log.",params$startdate, params$opt$label,".", filename.my2, ".txt", sep=""), append = TRUE, sep = "");
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
	system(paste("touch ", params$opt$output, "/", "log.",params$startdate, params$opt$label, ".", filename.my2, ".lock", sep=""), TRUE)
}

##########################
### FUNCTION w.checklock.allsamples.pe
###
###   Check if the lock file is present in the file system when using paired end data (it will indicate that this sample is still being processed and limiting
###   the continuation of the pipeline; this sample have not yet had the reads merged from both strands (_1_sequence.fastq & _2_sequence.fastq) into one single file (_12.sam)
##########################

w.checklock.allsamples.pe <- function(file_list.my2)
{
  any(file.exists(paste(params$opt$output, "/", "log.",params$startdate, params$opt$label, ".",
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
	print_mes(paste("Trying to unlock: ", params$opt$output, "/", "log.",params$startdate, params$opt$label, ".", filename.my2, ".lock", sep=""), filename.my2)
  system(paste("rm ", params$opt$output, "/", "log.",params$startdate, params$opt$label, ".", filename.my2, ".lock", sep=""), TRUE)
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
  cat("\n         for bwa: 2, use sufixes: *_1_sequence.fastq, *_2_sequence.fastq       \n")
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

fun.index.reference.genome <- function(step.my) {
  # update step number
  step.my$tmp <- step.my$tmp + 1

  print_doc(paste(" ### Step ", step.my$n, ".", step.my$tmp, ". Map against reference genome: Index the reference genome (if needed) ###\n", sep=""), "Index the reference genome");
  if ((params$opt$index) & (step.my$n == 0)) { # case to index the reference genome (time consuming, do only when really needed as requested)
    # Index the reference genome, if requested with argument -n and only for the first file if more than one sample to process
    command00 <- "bwa index"; # next command
    options00 <- paste("  -a bwtsw ", params$path_genome, sep="");
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
    # Write the lock file to indicate that this sample is being processed from paired end reads dual file-set.
    w.lock.sample.pe(file2process.my2)

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
    mate1.unfinished <- file.exists(paste(params$opt$output, "/", "log.",params$startdate, params$opt$label, ".",
                      file2process.mate1, ".lock", sep=""))  
    
    
    # Condition returns TRUE when matched the string and there is no lock file for the first sample of the paired-end set
    if ( length(grep("_2_sequence", file2process.my2)) == 1 ) # returns TRUE when matched the string (2nd sample of the pair)
    {
      if (mate1.unfinished) {
        # Report the user that sam is not finished for mate1 of the sample pair
        print_doc(paste(" ### Waiting for sam file for the mate1 of the pair to finish ###\n", sep=""), file2process.my2);
        # If A lock file exists; wait a while and check again until no lock file from samples exist
        while (file.exists(paste(params$opt$output, "/", "log.", params$startdate, params$opt$label, ".",
                                 file2process.mate1, ".lock", sep=""))) {
            cat(".")
            Sys.sleep(60) # Wait 60 seconds while for the creation of the sam file for the mat1 sample file to finish, and check again
        }       
      } 

      # Provide temporal shorter base names for the 2 files of the paired-end set (remove "_1_sequence" and "_2_sequence")
      f2pbase <- gsub("_2_sequence", "", file2process.my2)

      # Example - 2nd part
      #bwa sampe database.fasta aln_sa.sai short_read.fastq > aln.sam
      file_in_sai1 = paste(params$directory_out, "/", f2pbase, "_1_sequence", ".sai", sep="");
      file_in_sai2 = paste(params$directory_out, "/", f2pbase, "_2_sequence", ".sai", sep="");
      file_in_fq1 = paste(params$directory_in, "/", f2pbase, "_1_sequence", ".fastq", sep="");
      file_in_fq2 = paste(params$directory_in, "/", f2pbase, "_2_sequence", ".fastq", sep="");
      file_out = paste(params$directory_out, "/", f2pbase, "_merged12.sam", sep="");
      command00 = "bwa sampe"; # next command.
      options00 = paste(params$path_genome, " ", file_in_sai1, " ", file_in_sai2, " ", file_in_fq1, " ", file_in_fq2, " > ", file_out, sep="");
      command = paste(command00, " ", options00, sep="");
      # Annotate the subprocess start time; launch the subprocess; and annotate the end time & duration
      start.my <- Sys.time(); system(command); duration <- Sys.time()-start.my;
      # Show the duration of this subprocess
      cat("\nRelative duration since last step: "); print(duration);

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
  params$filename_list <- paste(params$opt$output, "/", "log.",params$startdate, params$opt$label, ".merged12_input_list.txt", sep="")
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
  file_stderr = paste(params$log.folder,"/log.",params$startdate, params$opt$label,".", file2process.my2, ".txt", sep="");
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
  file_stderr = paste(params$log.folder,"/log.",params$startdate, params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_annotate_variation, " -geneanno --buildver hg19 ", file_in, " ", params$path_annotate_humandb, " 2>> ", file_stderr, sep="");
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
  file_stderr = paste(params$log.folder,"/log.",params$startdate, params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_annotate_variation, " -filter --buildver hg19 -dbtype snp132 ", file_in, " ", params$path_annotate_humandb, " 2>> ", file_stderr, sep="");
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
  file_stderr = paste(params$log.folder,"/log.",params$startdate, params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_summarize_annovar, " --buildver hg19 --verdbsnp 132 ", file_in, " ", params$path_annotate_humandb, " --outfile ", file_out, " 2>> ", file_stderr, sep="");
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
      #######################
      # 1st part- sample.*.exome_summary.csv
      #-------------------------------
      file_in  = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.genome_summary.csv", sep=""); # summarize_annovar.pl adds the extension .genome_summary.csv (hardcoded in annovar).
      file_out = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.genome_summary.fg.csv", sep=""); 
      command01 = "head -1";
      command02 = command00;
      options01 = paste(" ", file_in, " > ", file_out, sep="");
      options02 = paste(" '", params$opt$filter,"' ", file_in, " >> ", file_out, sep="");

      #######################
      # 2nd - sample.*.exome_summary.csv
      #-------------------------------
      file_in  = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.exome_summary.csv", sep=""); # summarize_annovar.pl adds the extension .exome_summary.csv (hardcoded in annovar).
      file_out = paste(params$directory_out, "/", file2process.my2, ".f.vcf4.sum.exome_summary.fg.csv", sep=""); 
      command03 = "head -1";
      command04 = command00;
      options03 = paste(" ", file_in, " > ", file_out, sep="");
      options04 = paste(" '", params$opt$filter,"' ", file_in, " >> ", file_out, sep="");
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
  file_out = paste(params$directory_out, "/", file2process.my2, ".f.snpEff.vcf", sep=""); 
  file_out_base = paste(params$directory_out, "/", file2process.my2, ".f.snpEff", sep="");     
  command00 = "java -jar"; # next command.
  # DOC: file_stderr is the file to store the output of standard error from the command, where meaningful information was being shown to console only before this output was stored on disk 
  file_stderr = paste(params$log.folder,"/log.",params$startdate, params$opt$label,".", file2process.my2, ".txt", sep="");
  options00 = paste(" ", params$path_snpEff, "/snpEff.jar -c ", params$path_snpEff, "/snpEff.config hg19 ", file_in, 
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
    # 1st part - sample.*.snpEff.vcf
    #-------------------------------
    file_in  = paste(params$directory_out, "/", file2process.my2, ".f.snpEff.vcf", sep=""); 
    file_out = paste(params$directory_out, "/", file2process.my2, ".f.snpEff.fg.vcf", sep=""); 
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
  convert.file.list.pe              <- params_w2pps$p_convert.file.list.pe
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
  variant.dbsnp.pre.snpeff          <- params_w2pps$p_variant.dbsnp.pre.snpeff
  variant.eff.report                <- params_w2pps$p_variant.eff.report
  grep.post.snpeff.variants          <- params_w2pps$p_grep.post.snpeff.variants
  
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

  
  # In cases of paired end data (p_bwa = 2), check if all files have been processed 
  # to create the corresponding 1 sam file for each pair of files from paired end data 
  # This can be checked against the presence of lock files in the file system.
  # By default, there is one lock file for the whole run (whan you have paired data),
  # and one other lock file per sample when each sample is being processed.
  if (params$opt$bwa == 2 && convert.file.list.pe) {
    
  	# Check if general lock file exist.   
#	  if ( file.exists(paste(params$abs_path_to_script, "/", params$filename_list, ".lock", sep="")) ) {
	  if ( file.exists(paste(params$filename_list, ".lock", sep="")) ) {
	      
        # lock for the whole run still found.
        # check if any sample lock file is still present
	      check_sample_lock_exists <- w.checklock.allsamples.pe(params$file_list) 
      
	      if (check_sample_lock_exists) { # Suspend execution of R expressions for a given number of seconds
          # Report the user that some process is still working in the background
	        print_doc(paste(" ### Waiting for the creation of all sam files from all samples ###\n", sep=""), file2process.my1);
	        # If A lock file exists; wait a while and check again until no lock file from samples exist
	        while (w.checklock.allsamples.pe(params$file_list)) {
            cat(".")
            Sys.sleep(60) # Suspend execution of R expressions for a given number of seconds
	        }
	      } else {
	        # No lock file from samples exist any more; clean the general lock file
	        # clean the lock file
#	        system(paste("rm ", params$opt$output, "/", "log.",params$startdate, params$opt$label, ".fastq_pe_tmp.txt.lock", sep=""), TRUE)
	        print_mes(paste(" ### Removing the general lock file ###\n", sep=""), file2process.my1);
	        system(paste("rm ", params$filename_list, ".lock", sep=""), TRUE)
	      } # end of process to clean the general lock file 

    } # end of check for parent lock file. No parent lock file left (removed).
    
	  # Remake the file list with the defintive filenames with merged reads (_merged12.sam), and not just all .fastq files
    
	  if (params$opt$bwa == 2 && (params_wseq$p_map.on.reference.genome.sequential || params_wseq$p_map.on.reference.genome.parallel)) {
	    # Next Step
	    list.collected <- fun.convert.file.list.pe(file2process.my2  = routlogfile,
	                                 step.my  = step)
	    print_mes(paste(" ### 2nd call to fun.convert.file.list.pe ###\n", sep=""), routlogfile);
	    step.my           <- list.collected[[1]]
	    params$file_list  <- list.collected[[2]]
	    params$n_files    <- list.collected[[3]]
	    file2process.my1 <- params$file_list[datastep.my2]
	    
	  }
    
	} # end of the case bwa=2 (paired end) ####################################

  
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

  if (variant.dbsnp.pre.snpeff) {
    # Next Step
    step <- fun.variant.dbsnp.pre.snpeff(file2process.my2  = file2process.my1,
                                         step.my  = step)
  }
  
  if (variant.eff.report) {
    # Next Step
    step <- fun.variant.eff.report(file2process.my2  = file2process.my1,
                                             step.my  = step)
  }
  
  if (grep.post.snpeff.variants) {
    # Next Step
    step <- fun.grep.post.snpeff.variants(file2process.my2  = file2process.my1,
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
    print_mes("\n################################################################################\n\n", file2process.my1);
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
