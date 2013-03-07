#!/home/ueb/repo/peeva/eva_analysis_wrappers.R
#
# SCRIPT: eva_analysis_wrappers.R
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
### WRAPPERS OF FUNCTIONS
##########################

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
  map.on.reference.genome.sequential.mt               <- params_wseq$p_map.on.reference.genome.sequential.mt
  # -----------------------------
  
  
  # Get the file name to process now
  file2process.my1 <- params$file_list[datastep.my]
  
  # Re-set working directory while in child worker, just in case
  setwd(params$wd)
  step <- data.frame(datastep.my, 0)
  colnames(step) <- c("n","tmp")
  
  # Re-set the log file, if it exists already and log is requested. Create it.
  if (params$log) { 
    write(paste("\n", sep=""), file=paste(params$log.folder,"/log.", params$startdate, ".", params$opt$label, ".", file2process.my1, ".txt", sep=""), append = FALSE, sep = "");
  }
  
  # Re-set the log file, if it exists already and log is requested
  if (params$log && map.on.reference.genome.sequential.mt) { 
    write(paste("\n", sep=""), file=paste(params$log.folder,"/log.", params$startdate, ".", params$opt$label, ".", file2process.my1, ".txt", sep=""), append = TRUE, sep = "");
    print_mes("\n################################################################################\n", file2process.my1);
    print_mes(paste("  		Part A. SEQUENTIAL. ", params$n_files, " files; Current: *** ", file2process.my1, " ***\n", sep=""), file2process.my1);
    print_mes("################################################################################\n\n", file2process.my1);
    
  }
  
  print_doc(paste("### Start processing file #", datastep.my, " (", file2process.my1, ") ... ###\n", sep=""), file2process.my1);
  
  
  #--- Sequential Pipeline steps into wrapper.sequential function ###----------------------------------------
  
  if (map.on.reference.genome.sequential.mt) { 
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
  bowtie2sam                        <- params_w2pps$p_bowtie2sam
  sam2bam.and.sort	 	              <- params_w2pps$p_sam2bam.and.sort
  remove.pcr.dup		                <- params_w2pps$p_remove.pcr.dup
  gatk.sortbyref                    <- params_w2pps$p_gatk.sortbyref
  gatk.local.realign.step1          <- params_w2pps$p_gatk.local.realign.step1
  gatk.local.realign.step2          <- params_w2pps$p_gatk.local.realign.step2
  gatk.local.realign.step3          <- params_w2pps$p_gatk.local.realign.step3
  index.bam.file		                <- params_w2pps$p_index.bam.file
  stats			                        <- params_w2pps$p_stats
  snpeff.count.reads                <- params_w2pps$p_snpeff.count.reads
  exon.coverage    	                <- params_w2pps$p_exon.coverage
  variant.calling		                <- params_w2pps$p_variant.calling
  variant.filtering		              <- params_w2pps$p_variant.filtering
  gatk.combine.vcfs                 <- params_w2pps$p_gatk.combine.vcfs
  convert2vcf4		                  <- params_w2pps$p_convert2vcf4
  variant.annotation.geneb	        <- params_w2pps$p_variant.annotation.geneb
  variant.annotation.regionb	      <- params_w2pps$p_variant.annotation.regionb
  variant.annotation.filterb	      <- params_w2pps$p_variant.annotation.filterb
  variant.annotation.summarize      <- params_w2pps$p_variant.annotation.summarize
  grep.variants		                  <- params_w2pps$p_grep.variants
  visualize.variants		            <- params_w2pps$p_visualize.variants
  variant.dbsnp.pre.snpeff          <- params_w2pps$p_variant.dbsnp.pre.snpeff
  variant.eff.report                <- params_w2pps$p_variant.eff.report
  grep.post.snpeff.variants         <- params_w2pps$p_grep.post.snpeff.variants
  
  # -----------------------------
  
  # Get the file name to process now
  file2process.my1 <- params$file_list[datastep.my2]
  
  # Re-set working directory while in child worker, just in case
  setwd(params$wd)
  step <- data.frame(datastep.my2, 0)
  colnames(step) <- c("n","tmp")
  
  # Continue with the log file when/where needed
  if (params$log && map.on.reference.genome.parallel) { 
    write(paste("\n", sep=""), file=paste(params$log.folder,"/log.", params$startdate, ".", params$opt$label, ".", file2process.my1, ".txt", sep=""), append = TRUE, sep = "");
    print_mes("\n################################################################################\n", file2process.my1);
    print_mes(paste("	Part A. PARALLELIZED. ", params$n_files, " files; Current: *** ", file2process.my1, " ***\n", sep=""), file2process.my1);
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
    write(paste("\n>>> NEW SAMPLE TO PROCESS (", Sys.Date(),") <<<\n\n", sep=""), 
                          file=paste(params$log.folder,"/log.", params$startdate, ".", params$p_label, ".",
                          file2process.my1, ".txt", sep=""), append = FALSE, sep = "");
    print_mes("\n################################################################################\n", file2process.my1);
    print_mes(paste(" Part B. PARALLELIZABLE. ", params$n_files, " files; Current: *** ", file2process.my1, " ***\n", sep=""), file2process.my1);
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
#  if (params$opt$bwa == 2 && (!params_wseq$p_map.on.reference.genome.sequential.mt && !params_wseq$p_map.on.reference.genome.parallel)
#     && p_convert.file.list.pe) {
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
        #	        system(paste("rm ", params$opt$output, "/", "log.",params$startdate, ".", params$opt$label, ".fastq_pe_tmp.txt.lock", sep=""), TRUE)
        print_mes(paste("\n ### Both single sam files found from this set of paired samples: Removing the general lock file in order to continue processing this paired sample###\n\n", sep=""), file2process.my1);
        system(paste("rm ", params$filename_list, ".lock", sep=""), TRUE)
      } # end of process to clean the general lock file 
      
    } # end of check for parent lock file. No parent lock file left (removed).
    
    # Remake the file list with the definitive filenames with merged reads (_merged12.sam), and not just all .fastq files
    
    if (params$opt$bwa == 2 && (params_wseq$p_map.on.reference.genome.sequential.mt || params_wseq$p_map.on.reference.genome.parallel)) {
      # Next Step
      list.collected <- fun.convert.file.list.pe(file2process.my2  = routlogfile,
                                                 step.my  = step)
      print_mes(paste("\n ### 2nd call to fun.convert.file.list.pe ###\n\n", sep=""), routlogfile);
      step.my           <- list.collected[[1]]
      params$file_list  <- list.collected[[2]]
      params$n_files    <- list.collected[[3]]
      file2process.my1 <- params$file_list[datastep.my2]
      
    }
    
  } # end of the case bwa=2 (paired end) ####################################
  
    
  if (bowtie2sam) {
    # Next Step
    step <- fun.bowtie2sam(file2process.my2  = file2process.my1,
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
  
  if (gatk.sortbyref) {
    # Next Step
    step <- fun.gatk.sortbyref(file2process.my2  = file2process.my1,
                                         step.my  = step)
  }
  
  if (gatk.local.realign.step1) {
    # Next Step
    step <- fun.gatk.local.realign.step1(file2process.my2  = file2process.my1,
                                         step.my  = step)
  }
  
  if (gatk.local.realign.step2) {
    # Next Step
    step <- fun.gatk.local.realign.step2(file2process.my2  = file2process.my1,
                                         step.my  = step)
  }
  
  if (gatk.local.realign.step3) {
    # Next Step
    step <- fun.gatk.local.realign.step3(file2process.my2  = file2process.my1,
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
  
  if (snpeff.count.reads) {
    # Next Step
    step <- fun.snpeff.count.reads(file2process.my2  = file2process.my1,
                                   step.my  = step)
  }
  
  if (exon.coverage) {
    # Next Step
    step <- fun.exon.coverage(file2process.my2  = file2process.my1,
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
  
  if (gatk.combine.vcfs) {
    # Next Step
    step <- fun.gatk.combine.vcfs(file2process.my2  = file2process.my1,
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
  print_doc(paste("	Part B. End of processing this file: ", file2process.my1, "\n", sep=""), file2process.my1);
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
    print_mes(paste("	Part C. PARALLELIZABLE also. ", params$n_files, " files.", sep=""), file2process.my1);
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
  print_doc(paste("	End", "\n", sep=""), file2process.my1);
  
  # XXX...
  
  
  # Last step of wrapper
  #  gc() # Let's clean ouR garbage if possible
  return(NULL) # return nothing, since results are saved on disk from the perl script
} # end of wrapper function
