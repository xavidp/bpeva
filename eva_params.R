#!/home/ueb/repo/peeva/eva_params.R
#
# SCRIPT: eva_params.R
# SCOPE: to be called from eva_main.R
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

##############################################################

# Roadmap (Accepted ToDo things):
# ----------------------
# (See also http://ueb.vhir.org/PendentsEVA )
##
## * get snpEff countReads fixed [DONE]
## * SnpSift Filter: replace the basic filtering based on grep on the annotated results and filter after the vcf is created using SnpSift
##   * add dbNSFP annotation
##   * add GWAS Catalog annotation
## * implement first steps with GATK successfully
## * assess adding a new step for local realignment with InGap
##


# 0. Basic startup params
# ---------------------------------------------------------------------
startdate <- paste(format(Sys.Date(), "%y%m%d"), sep="")

p_test     = 1 # 1/0; ### Is this a test run? ###
                # 1 = test run, so that use the predefined values for a test run; 
                # 0 = normal run
if (p_test==1) {
  path_input_absolute <- "0" # Define whether the p_input is absolute or relative
  p_input    <- "test_in2" #"/mnt/magatzem02/tmp/run_sara_293a/dir_in_293a3" #"test_in2" # "../test_in2"  # "test_in"
  p_in.suffix <- "_sequence" #"_sequence" # "" # This is the suffix of all input filenames (without extension) used for the pipeline to process
  p_in.ext    <- ".fastq" #".fastq" # ".fa" ".sam" ".bam" # This is the .extension of all files used as input for the pipeline to process
  p_output   <- "test_out" #"/mnt/magatzem02/tmp/run_sara_293a/dir_out_293a3b" #"test_out2" # "../test_out2" # "test_out"
  p_label    <- "testset0" #"testrunGATK1" # "testsnpEffCountReads_a" "test-121002" # "test-foo"        # Run Label for output filenames
  p_desc     <- "Testing the overall pipeline starting from scratch" # "Testing the new testset1 from the source merged sam file. See http://ueb.vhir.org/PendentsEVA and http://ueb.vhir.org/sheet3" #"Testing gatk files etc"# Testing The issue in snpEff Count Reads with first set: s_7_m11_149b_merged12_00-01M.sam"
  p_keep     <- TRUE # Enable if run through editor and you want to keep temp files
  p_showc    <- TRUE # Enable if you want to see the commands literally that are run in the command line
  #p_filter   <- ""   
  p_filter   <- "BRCA1|\\|BRCA2|\\|CHEK2|\\|PALB2|\\|BRIP1|\\|TP53|\\|PTEN|\\|STK11|\\|CDH1|\\|ATM|\\|BARD1|\\|APC|\\|MLH1|\\|MRE11|\\|MSH2|\\|MSH6|\\|MUTYH|\\|NBN|\\|PMS1|\\|PMS2|\\|RAD50|\\|RAD51D|\\|RAD51C|\\|XRCC2|\\|UIMC1|\\|FAM175A|\\|ERCC4|\\|RAD51|\\|RAD51B|\\|XRCC3|\\|FANCA|\\|FANCB|\\|FANCC|\\|FANCD2|\\|FANCE|\\|FANCF|\\|FANCG|\\|FANCI|\\|FANCL|\\|FANCM|\\|SLX4|\\|CASP8|\\|FGFR2|\\|TOX3|\\|MAP3K1|\\|MRPS30|\\|SLC4A7|\\|NEK10|\\|COX11|\\|ESR1|\\|CDKN2A|\\|CDKN2B|\\|ANKRD16|\\|FBXO18|\\|ZNF365|\\|ZMIZ1|\\|BABAM1|\\|LSP1|\\|ANKLE1|\\|TOPBP1|\\|BCCIP|\\|53BP1|\\|BRCA1:\\|BRCA2:\\|CHEK2:\\|PALB2:\\|BRIP1:\\|TP53:\\|PTEN:\\|STK11:\\|CDH1:\\|ATM:\\|BARD1:\\|APC:\\|MLH1:\\|MRE11:\\|MSH2:\\|MSH6:\\|MUTYH:\\|NBN:\\|PMS1:\\|PMS2:\\|RAD50:\\|RAD51D:\\|RAD51C:\\|XRCC2:\\|UIMC1:\\|FAM175A:\\|ERCC4:\\|RAD51:\\|RAD51B:\\|XRCC3:\\|FANCA:\\|FANCB:\\|FANCC:\\|FANCD2:\\|FANCE:\\|FANCF:\\|FANCG:\\|FANCI:\\|FANCL:\\|FANCM:\\|SLX4:\\|CASP8:\\|FGFR2:\\|TOX3:\\|MAP3K1:\\|MRPS30:\\|SLC4A7:\\|NEK10:\\|COX11:\\|ESR1:\\|CDKN2A:\\|CDKN2B:\\|ANKRD16:\\|FBXO18:\\|ZNF365:\\|ZMIZ1:\\|BABAM1:\\|LSP1:\\|ANKLE1:\\|TOPBP1:\\|BCCIP:\\|53BP1:\\|^BRCA1\t\\|^BRCA2\t\\|^CHEK2\t\\|^PALB2\t\\|^BRIP1\t\\|^TP53\t\\|^PTEN\t\\|^STK11\t\\|^CDH1\t\\|^ATM\t\\|^BARD1\t\\|^APC\t\\|^MLH1\t\\|^MRE11\t\\|^MSH2\t\\|^MSH6\t\\|^MUTYH\t\\|^NBN\t\\|^PMS1\t\\|^PMS2\t\\|^RAD50\t\\|^RAD51D\t\\|^RAD51C\t\\|^XRCC2\t\\|^UIMC1\t\\|^FAM175A\t\\|^ERCC4\t\\|^RAD51\t\\|^RAD51B\t\\|^XRCC3\t\\|^FANCA\t\\|^FANCB\t\\|^FANCC\t\\|^FANCD2\t\\|^FANCE\t\\|^FANCF\t\\|^FANCG\t\\|^FANCI\t\\|^FANCL\t\\|^FANCM\t\\|^SLX4\t\\|^CASP8\t\\|^FGFR2\t\\|^TOX3\t\\|^MAP3K1\t\\|^MRPS30\t\\|^SLC4A7\t\\|^NEK10\t\\|^COX11\t\\|^ESR1\t\\|^CDKN2A\t\\|^CDKN2B\t\\|^ANKRD16\t\\|^FBXO18\t\\|^ZNF365\t\\|^ZMIZ1\t\\|^BABAM1\t\\|^LSP1\t\\|^ANKLE1\t\\|^TOPBP1\t\\|^BCCIP\t\\|^53BP1\t\\|\"BRCA1\"\\|\"BRCA2\"\\|\"CHEK2\"\\|\"PALB2\"\\|\"BRIP1\"\\|\"TP53\"\\|\"PTEN\"\\|\"STK11\"\\|\"CDH1\"\\|\"ATM\"\\|\"BARD1\"\\|\"APC\"\\|\"MLH1\"\\|\"MRE11\"\\|\"MSH2\"\\|\"MSH6\"\\|\"MUTYH\"\\|\"NBN\"\\|\"PMS1\"\\|\"PMS2\"\\|\"RAD50\"\\|\"RAD51D\"\\|\"RAD51C\"\\|\"XRCC2\"\\|\"UIMC1\"\\|\"FAM175A\"\\|\"ERCC4\"\\|\"RAD51\"\\|\"RAD51B\"\\|\"XRCC3\"\\|\"FANCA\"\\|\"FANCB\"\\|\"FANCC\"\\|\"FANCD2\"\\|\"FANCE\"\\|\"FANCF\"\\|\"FANCG\"\\|\"FANCI\"\\|\"FANCL\"\\|\"FANCM\"\\|\"SLX4\"\\|\"CASP8\"\\|\"FGFR2\"\\|\"TOX3\"\\|\"MAP3K1\"\\|\"MRPS30\"\\|\"SLC4A7\"\\|\"NEK10\"\\|\"COX11\"\\|\"ESR1\"\\|\"CDKN2A\"\\|\"CDKN2B\"\\|\"ANKRD16\"\\|\"FBXO18\"\\|\"ZNF365\"\\|\"ZMIZ1\"\\|\"BABAM1\"\\|\"LSP1\"\\|\"ANKLE1\"\\|\"TOPBP1\"\\|\"BCCIP\"\\|\"53BP1\"\\|BRCA1\t|BRCA2\t|CHEK2\t|PALB2\t|BRIP1\t|TP53\t|PTEN\t|STK11\t|CDH1\t|ATM\t|BARD1\t|APC\t|MLH1\t|MRE11\t|MSH2\t|MSH6\t|MUTYH\t|NBN\t|PMS1\t|PMS2\t|RAD50\t|RAD51D\t|RAD51C\t|XRCC2\t|UIMC1\t|FAM175A\t|ERCC4\t|RAD51\t|RAD51B\t|XRCC3\t|FANCA\t|FANCB\t|FANCC\t|FANCD2\t|FANCE\t|FANCF\t|FANCG\t|FANCI\t|FANCL\t|FANCM\t|SLX4\t|CASP8\t|FGFR2\t|TOX3\t|MAP3K1\t|MRPS30\t|SLC4A7\t|NEK10\t|COX11\t|ESR1\t|CDKN2A\t|CDKN2B\t|ANKRD16\t|FBXO18\t|ZNF365\t|ZMIZ1\t|BABAM1\t|LSP1\t|ANKLE1\t|TOPBP1\t|BCCIP\t|53BP1  "
  p_mail.send <- 0 # 0=FALSE, 1=TRUE ; Indicate whether we want an email sent when the run is finished
} else {
  path_input_absolute <- "1" # Define whether the p_input is absolute or relative
#  p_input    <- "/mnt/magatzem02/tmp/run_sara_293a/dir_in_293a2" # "../dir_in" # "test_in"   # "dir_in"     
#  p_output   <- "/mnt/magatzem02/tmp/run_sara_293a/dir_out_293a2" #../dir_out_293" # "../dir_out_293" # "test_out"	 # "dir_out_293"
#  p_label    <-  "sg293a2b2.snpeff.greped" # "test-121002" # ".sg293_qa_sg3sg4"   # "test-121002" ".sara207_4s4cpu"        # Run Label for output filenames
  p_input     <- "/backups_disk_03/tmp/bam_mv311" # "../dir_in" # "test_in"   # "dir_in"     
  p_in.suffix <- "_sequence" #  "_sequence" # "" # This is the suffix of all input filenames (without extension) used for the pipeline to process
  p_in.ext    <-  ".fastq" #".fastq" # ".fa" ".sam" ".bam" # This is the .extension of all files used as input for the pipeline to process
  p_output   <- "/backups_disk_03/tmp/bam_mv311" #"/home/xavi/Estudis/eva_bowtie/dir_out" #../dir_out_293" # "../dir_out_293" # "test_out"   # "dir_out_293"
  p_label    <-   "mv311c3" #sg293a3_test_count_reads" # ".sg293a3_ind7_mc15g3seq" # mc=minimum cover; g3: 3rd filter version for the grep; "test-121002" # ".sg293_qa_sg3sg4"   # "test-121002" ".sara207_4s4cpu"        # Run Label for output filenames
                  # p_desc = Description in longer format of the run. DEscribe that for you to understand in the future what were the conditions and params of this run. 
  p_desc     <-   "c3: processat d'un fastq de prova fins a sam i bam amb el pipeline eva per a ratoli rn4."
  p_keep     <- TRUE # Enable if run through editor and you want to keep temp files
  p_showc    <- FALSE #TRUE # Enable if you want to see the commands literally that are run in the command line
#  p_filter   <- "BRCA"
#  p_filter   <- "BRCA1\\|BRCA2\\|CHEK2\\|PALB2\\|BRIP1\\|TP53\\|PTEN\\|STK11\\|CDH1\\|ATM\\|BARD1\\|APC\\|MLH1\\|MRE11\\|MSH2\\|MSH6\\|MUTYH\\|NBN\\|PMS1\\|PMS2\\|RAD50\\|RAD51D\\|RAD51C\\|XRCC2\\|UIMC1\\|FAM175A\\|ERCC4\\|RAD51\\|RAD51B\\|XRCC3\\|FANCA\\|FANCB\\|FANCC\\|FANCD2\\|FANCE\\|FANCF\\|FANCG\\|FANCI\\|FANCL\\|FANCM\\|SLX4\\|CASP8\\|FGFR2\\|TOX3\\|MAP3K1\\|MRPS30\\|SLC4A7\\|NEK10\\|COX11\\|ESR1\\|CDKN2A\\|CDKN2B\\|ANKRD16\\|FBXO18\\|ZNF365\\|ZMIZ1\\|BABAM1\\|LSP1\\|ANKLE1\\|TOPBP1\\|BCCIP\\|53BP1"            
  # for p_filter, keep in mind that we want to find 'foo1', and there is one 'foo10', we need to indicate somehow to grep for the whole word
  # and that's why we grep for "foo1|", "foo1:", "\"foo\"" anywhere, or "foo\t" at the begining of the line, since the "|" is the character splitter in snpEff results, 
  # ":" in annovar results, and at the begining of the string before a tab (\t) in *snpEff_genes.txt  results file.
  # We are also doing a system (gnu) grep with a -P flag (to indicate to read PERL-like syntax) for "foo\t" at teh snpEff.txt output files wich are tab deliimted.
#  p_filter   <- "BRCA1|\\|BRCA2|\\|CHEK2|\\|PALB2|\\|BRIP1|\\|TP53|\\|PTEN|\\|STK11|\\|CDH1|\\|ATM|\\|BARD1|\\|APC|\\|MLH1|\\|MRE11|\\|MSH2|\\|MSH6|\\|MUTYH|\\|NBN|\\|PMS1|\\|PMS2|\\|RAD50|\\|RAD51D|\\|RAD51C|\\|XRCC2|\\|UIMC1|\\|FAM175A|\\|ERCC4|\\|RAD51|\\|RAD51B|\\|XRCC3|\\|FANCA|\\|FANCB|\\|FANCC|\\|FANCD2|\\|FANCE|\\|FANCF|\\|FANCG|\\|FANCI|\\|FANCL|\\|FANCM|\\|SLX4|\\|CASP8|\\|FGFR2|\\|TOX3|\\|MAP3K1|\\|MRPS30|\\|SLC4A7|\\|NEK10|\\|COX11|\\|ESR1|\\|CDKN2A|\\|CDKN2B|\\|ANKRD16|\\|FBXO18|\\|ZNF365|\\|ZMIZ1|\\|BABAM1|\\|LSP1|\\|ANKLE1|\\|TOPBP1|\\|BCCIP|\\|53BP1|\\|BRCA1:\\|BRCA2:\\|CHEK2:\\|PALB2:\\|BRIP1:\\|TP53:\\|PTEN:\\|STK11:\\|CDH1:\\|ATM:\\|BARD1:\\|APC:\\|MLH1:\\|MRE11:\\|MSH2:\\|MSH6:\\|MUTYH:\\|NBN:\\|PMS1:\\|PMS2:\\|RAD50:\\|RAD51D:\\|RAD51C:\\|XRCC2:\\|UIMC1:\\|FAM175A:\\|ERCC4:\\|RAD51:\\|RAD51B:\\|XRCC3:\\|FANCA:\\|FANCB:\\|FANCC:\\|FANCD2:\\|FANCE:\\|FANCF:\\|FANCG:\\|FANCI:\\|FANCL:\\|FANCM:\\|SLX4:\\|CASP8:\\|FGFR2:\\|TOX3:\\|MAP3K1:\\|MRPS30:\\|SLC4A7:\\|NEK10:\\|COX11:\\|ESR1:\\|CDKN2A:\\|CDKN2B:\\|ANKRD16:\\|FBXO18:\\|ZNF365:\\|ZMIZ1:\\|BABAM1:\\|LSP1:\\|ANKLE1:\\|TOPBP1:\\|BCCIP:\\|53BP1"  
#  p_filter   <- "BRCA1|\\|BRCA2|\\|CHEK2|\\|PALB2|\\|BRIP1|\\|TP53|\\|PTEN|\\|STK11|\\|CDH1|\\|ATM|\\|BARD1|\\|APC|\\|MLH1|\\|MRE11|\\|MSH2|\\|MSH6|\\|MUTYH|\\|NBN|\\|PMS1|\\|PMS2|\\|RAD50|\\|RAD51D|\\|RAD51C|\\|XRCC2|\\|UIMC1|\\|FAM175A|\\|ERCC4|\\|RAD51|\\|RAD51B|\\|XRCC3|\\|FANCA|\\|FANCB|\\|FANCC|\\|FANCD2|\\|FANCE|\\|FANCF|\\|FANCG|\\|FANCI|\\|FANCL|\\|FANCM|\\|SLX4|\\|CASP8|\\|FGFR2|\\|TOX3|\\|MAP3K1|\\|MRPS30|\\|SLC4A7|\\|NEK10|\\|COX11|\\|ESR1|\\|CDKN2A|\\|CDKN2B|\\|ANKRD16|\\|FBXO18|\\|ZNF365|\\|ZMIZ1|\\|BABAM1|\\|LSP1|\\|ANKLE1|\\|TOPBP1|\\|BCCIP|\\|53BP1|\\|BRCA1:\\|BRCA2:\\|CHEK2:\\|PALB2:\\|BRIP1:\\|TP53:\\|PTEN:\\|STK11:\\|CDH1:\\|ATM:\\|BARD1:\\|APC:\\|MLH1:\\|MRE11:\\|MSH2:\\|MSH6:\\|MUTYH:\\|NBN:\\|PMS1:\\|PMS2:\\|RAD50:\\|RAD51D:\\|RAD51C:\\|XRCC2:\\|UIMC1:\\|FAM175A:\\|ERCC4:\\|RAD51:\\|RAD51B:\\|XRCC3:\\|FANCA:\\|FANCB:\\|FANCC:\\|FANCD2:\\|FANCE:\\|FANCF:\\|FANCG:\\|FANCI:\\|FANCL:\\|FANCM:\\|SLX4:\\|CASP8:\\|FGFR2:\\|TOX3:\\|MAP3K1:\\|MRPS30:\\|SLC4A7:\\|NEK10:\\|COX11:\\|ESR1:\\|CDKN2A:\\|CDKN2B:\\|ANKRD16:\\|FBXO18:\\|ZNF365:\\|ZMIZ1:\\|BABAM1:\\|LSP1:\\|ANKLE1:\\|TOPBP1:\\|BCCIP:\\|53BP1:\\|^BRCA1\t\\|^BRCA2\t\\|^CHEK2\t\\|^PALB2\t\\|^BRIP1\t\\|^TP53\t\\|^PTEN\t\\|^STK11\t\\|^CDH1\t\\|^ATM\t\\|^BARD1\t\\|^APC\t\\|^MLH1\t\\|^MRE11\t\\|^MSH2\t\\|^MSH6\t\\|^MUTYH\t\\|^NBN\t\\|^PMS1\t\\|^PMS2\t\\|^RAD50\t\\|^RAD51D\t\\|^RAD51C\t\\|^XRCC2\t\\|^UIMC1\t\\|^FAM175A\t\\|^ERCC4\t\\|^RAD51\t\\|^RAD51B\t\\|^XRCC3\t\\|^FANCA\t\\|^FANCB\t\\|^FANCC\t\\|^FANCD2\t\\|^FANCE\t\\|^FANCF\t\\|^FANCG\t\\|^FANCI\t\\|^FANCL\t\\|^FANCM\t\\|^SLX4\t\\|^CASP8\t\\|^FGFR2\t\\|^TOX3\t\\|^MAP3K1\t\\|^MRPS30\t\\|^SLC4A7\t\\|^NEK10\t\\|^COX11\t\\|^ESR1\t\\|^CDKN2A\t\\|^CDKN2B\t\\|^ANKRD16\t\\|^FBXO18\t\\|^ZNF365\t\\|^ZMIZ1\t\\|^BABAM1\t\\|^LSP1\t\\|^ANKLE1\t\\|^TOPBP1\t\\|^BCCIP\t\\|^53BP1\t"  
#wrong#  p_filter   <- "BRCA1|\\|BRCA2|\\|CHEK2|\\|PALB2|\\|BRIP1|\\|TP53|\\|PTEN|\\|STK11|\\|CDH1|\\|ATM|\\|BARD1|\\|APC|\\|MLH1|\\|MRE11|\\|MSH2|\\|MSH6|\\|MUTYH|\\|NBN|\\|PMS1|\\|PMS2|\\|RAD50|\\|RAD51D|\\|RAD51C|\\|XRCC2|\\|UIMC1|\\|FAM175A|\\|ERCC4|\\|RAD51|\\|RAD51B|\\|XRCC3|\\|FANCA|\\|FANCB|\\|FANCC|\\|FANCD2|\\|FANCE|\\|FANCF|\\|FANCG|\\|FANCI|\\|FANCL|\\|FANCM|\\|SLX4|\\|CASP8|\\|FGFR2|\\|TOX3|\\|MAP3K1|\\|MRPS30|\\|SLC4A7|\\|NEK10|\\|COX11|\\|ESR1|\\|CDKN2A|\\|CDKN2B|\\|ANKRD16|\\|FBXO18|\\|ZNF365|\\|ZMIZ1|\\|BABAM1|\\|LSP1|\\|ANKLE1|\\|TOPBP1|\\|BCCIP|\\|53BP1|\\|BRCA1:\\|BRCA2:\\|CHEK2:\\|PALB2:\\|BRIP1:\\|TP53:\\|PTEN:\\|STK11:\\|CDH1:\\|ATM:\\|BARD1:\\|APC:\\|MLH1:\\|MRE11:\\|MSH2:\\|MSH6:\\|MUTYH:\\|NBN:\\|PMS1:\\|PMS2:\\|RAD50:\\|RAD51D:\\|RAD51C:\\|XRCC2:\\|UIMC1:\\|FAM175A:\\|ERCC4:\\|RAD51:\\|RAD51B:\\|XRCC3:\\|FANCA:\\|FANCB:\\|FANCC:\\|FANCD2:\\|FANCE:\\|FANCF:\\|FANCG:\\|FANCI:\\|FANCL:\\|FANCM:\\|SLX4:\\|CASP8:\\|FGFR2:\\|TOX3:\\|MAP3K1:\\|MRPS30:\\|SLC4A7:\\|NEK10:\\|COX11:\\|ESR1:\\|CDKN2A:\\|CDKN2B:\\|ANKRD16:\\|FBXO18:\\|ZNF365:\\|ZMIZ1:\\|BABAM1:\\|LSP1:\\|ANKLE1:\\|TOPBP1:\\|BCCIP:\\|53BP1:\\|^BRCA1\t\\|^BRCA2\t\\|^CHEK2\t\\|^PALB2\t\\|^BRIP1\t\\|^TP53\t\\|^PTEN\t\\|^STK11\t\\|^CDH1\t\\|^ATM\t\\|^BARD1\t\\|^APC\t\\|^MLH1\t\\|^MRE11\t\\|^MSH2\t\\|^MSH6\t\\|^MUTYH\t\\|^NBN\t\\|^PMS1\t\\|^PMS2\t\\|^RAD50\t\\|^RAD51D\t\\|^RAD51C\t\\|^XRCC2\t\\|^UIMC1\t\\|^FAM175A\t\\|^ERCC4\t\\|^RAD51\t\\|^RAD51B\t\\|^XRCC3\t\\|^FANCA\t\\|^FANCB\t\\|^FANCC\t\\|^FANCD2\t\\|^FANCE\t\\|^FANCF\t\\|^FANCG\t\\|^FANCI\t\\|^FANCL\t\\|^FANCM\t\\|^SLX4\t\\|^CASP8\t\\|^FGFR2\t\\|^TOX3\t\\|^MAP3K1\t\\|^MRPS30\t\\|^SLC4A7\t\\|^NEK10\t\\|^COX11\t\\|^ESR1\t\\|^CDKN2A\t\\|^CDKN2B\t\\|^ANKRD16\t\\|^FBXO18\t\\|^ZNF365\t\\|^ZMIZ1\t\\|^BABAM1\t\\|^LSP1\t\\|^ANKLE1\t\\|^TOPBP1\t\\|^BCCIP\t\\|^53BP1\t\\|\\"BRCA1\\"\\|\\"BRCA2\\"\\|\\"CHEK2\\"\\|\\"PALB2\\"\\|\\"BRIP1\\"\\|\\"TP53\\"\\|\\"PTEN\\"\\|\\"STK11\\"\\|\\"CDH1\\"\\|\\"ATM\\"\\|\\"BARD1\\"\\|\\"APC\\"\\|\\"MLH1\\"\\|\\"MRE11\\"\\|\\"MSH2\\"\\|\\"MSH6\\"\\|\\"MUTYH\\"\\|\\"NBN\\"\\|\\"PMS1\\"\\|\\"PMS2\\"\\|\\"RAD50\\"\\|\\"RAD51D\\"\\|\\"RAD51C\\"\\|\\"XRCC2\\"\\|\\"UIMC1\\"\\|\\"FAM175A\\"\\|\\"ERCC4\\"\\|\\"RAD51\\"\\|\\"RAD51B\\"\\|\\"XRCC3\\"\\|\\"FANCA\\"\\|\\"FANCB\\"\\|\\"FANCC\\"\\|\\"FANCD2\\"\\|\\"FANCE\\"\\|\\"FANCF\\"\\|\\"FANCG\\"\\|\\"FANCI\\"\\|\\"FANCL\\"\\|\\"FANCM\\"\\|\\"SLX4\\"\\|\\"CASP8\\"\\|\\"FGFR2\\"\\|\\"TOX3\\"\\|\\"MAP3K1\\"\\|\\"MRPS30\\"\\|\\"SLC4A7\\"\\|\\"NEK10\\"\\|\\"COX11\\"\\|\\"ESR1\\"\\|\\"CDKN2A\\"\\|\\"CDKN2B\\"\\|\\"ANKRD16\\"\\|\\"FBXO18\\"\\|\\"ZNF365\\"\\|\\"ZMIZ1\\"\\|\\"BABAM1\\"\\|\\"LSP1\\"\\|\\"ANKLE1\\"\\|\\"TOPBP1\\"\\|\\"BCCIP\\"\\|\\"53BP1\\""
#  p_filter   <- "BRCA1|\\|BRCA2|\\|CHEK2|\\|PALB2|\\|BRIP1|\\|TP53|\\|PTEN|\\|STK11|\\|CDH1|\\|ATM|\\|BARD1|\\|APC|\\|MLH1|\\|MRE11|\\|MSH2|\\|MSH6|\\|MUTYH|\\|NBN|\\|PMS1|\\|PMS2|\\|RAD50|\\|RAD51D|\\|RAD51C|\\|XRCC2|\\|UIMC1|\\|FAM175A|\\|ERCC4|\\|RAD51|\\|RAD51B|\\|XRCC3|\\|FANCA|\\|FANCB|\\|FANCC|\\|FANCD2|\\|FANCE|\\|FANCF|\\|FANCG|\\|FANCI|\\|FANCL|\\|FANCM|\\|SLX4|\\|CASP8|\\|FGFR2|\\|TOX3|\\|MAP3K1|\\|MRPS30|\\|SLC4A7|\\|NEK10|\\|COX11|\\|ESR1|\\|CDKN2A|\\|CDKN2B|\\|ANKRD16|\\|FBXO18|\\|ZNF365|\\|ZMIZ1|\\|BABAM1|\\|LSP1|\\|ANKLE1|\\|TOPBP1|\\|BCCIP|\\|53BP1|\\|BRCA1:\\|BRCA2:\\|CHEK2:\\|PALB2:\\|BRIP1:\\|TP53:\\|PTEN:\\|STK11:\\|CDH1:\\|ATM:\\|BARD1:\\|APC:\\|MLH1:\\|MRE11:\\|MSH2:\\|MSH6:\\|MUTYH:\\|NBN:\\|PMS1:\\|PMS2:\\|RAD50:\\|RAD51D:\\|RAD51C:\\|XRCC2:\\|UIMC1:\\|FAM175A:\\|ERCC4:\\|RAD51:\\|RAD51B:\\|XRCC3:\\|FANCA:\\|FANCB:\\|FANCC:\\|FANCD2:\\|FANCE:\\|FANCF:\\|FANCG:\\|FANCI:\\|FANCL:\\|FANCM:\\|SLX4:\\|CASP8:\\|FGFR2:\\|TOX3:\\|MAP3K1:\\|MRPS30:\\|SLC4A7:\\|NEK10:\\|COX11:\\|ESR1:\\|CDKN2A:\\|CDKN2B:\\|ANKRD16:\\|FBXO18:\\|ZNF365:\\|ZMIZ1:\\|BABAM1:\\|LSP1:\\|ANKLE1:\\|TOPBP1:\\|BCCIP:\\|53BP1:\\|^BRCA1\t\\|^BRCA2\t\\|^CHEK2\t\\|^PALB2\t\\|^BRIP1\t\\|^TP53\t\\|^PTEN\t\\|^STK11\t\\|^CDH1\t\\|^ATM\t\\|^BARD1\t\\|^APC\t\\|^MLH1\t\\|^MRE11\t\\|^MSH2\t\\|^MSH6\t\\|^MUTYH\t\\|^NBN\t\\|^PMS1\t\\|^PMS2\t\\|^RAD50\t\\|^RAD51D\t\\|^RAD51C\t\\|^XRCC2\t\\|^UIMC1\t\\|^FAM175A\t\\|^ERCC4\t\\|^RAD51\t\\|^RAD51B\t\\|^XRCC3\t\\|^FANCA\t\\|^FANCB\t\\|^FANCC\t\\|^FANCD2\t\\|^FANCE\t\\|^FANCF\t\\|^FANCG\t\\|^FANCI\t\\|^FANCL\t\\|^FANCM\t\\|^SLX4\t\\|^CASP8\t\\|^FGFR2\t\\|^TOX3\t\\|^MAP3K1\t\\|^MRPS30\t\\|^SLC4A7\t\\|^NEK10\t\\|^COX11\t\\|^ESR1\t\\|^CDKN2A\t\\|^CDKN2B\t\\|^ANKRD16\t\\|^FBXO18\t\\|^ZNF365\t\\|^ZMIZ1\t\\|^BABAM1\t\\|^LSP1\t\\|^ANKLE1\t\\|^TOPBP1\t\\|^BCCIP\t\\|^53BP1\t\\|\"BRCA1\"\\|\"BRCA2\"\\|\"CHEK2\"\\|\"PALB2\"\\|\"BRIP1\"\\|\"TP53\"\\|\"PTEN\"\\|\"STK11\"\\|\"CDH1\"\\|\"ATM\"\\|\"BARD1\"\\|\"APC\"\\|\"MLH1\"\\|\"MRE11\"\\|\"MSH2\"\\|\"MSH6\"\\|\"MUTYH\"\\|\"NBN\"\\|\"PMS1\"\\|\"PMS2\"\\|\"RAD50\"\\|\"RAD51D\"\\|\"RAD51C\"\\|\"XRCC2\"\\|\"UIMC1\"\\|\"FAM175A\"\\|\"ERCC4\"\\|\"RAD51\"\\|\"RAD51B\"\\|\"XRCC3\"\\|\"FANCA\"\\|\"FANCB\"\\|\"FANCC\"\\|\"FANCD2\"\\|\"FANCE\"\\|\"FANCF\"\\|\"FANCG\"\\|\"FANCI\"\\|\"FANCL\"\\|\"FANCM\"\\|\"SLX4\"\\|\"CASP8\"\\|\"FGFR2\"\\|\"TOX3\"\\|\"MAP3K1\"\\|\"MRPS30\"\\|\"SLC4A7\"\\|\"NEK10\"\\|\"COX11\"\\|\"ESR1\"\\|\"CDKN2A\"\\|\"CDKN2B\"\\|\"ANKRD16\"\\|\"FBXO18\"\\|\"ZNF365\"\\|\"ZMIZ1\"\\|\"BABAM1\"\\|\"LSP1\"\\|\"ANKLE1\"\\|\"TOPBP1\"\\|\"BCCIP\"\\|\"53BP1\""
  p_filter   <- "BRCA1|\\|BRCA2|\\|CHEK2|\\|PALB2|\\|BRIP1|\\|TP53|\\|PTEN|\\|STK11|\\|CDH1|\\|ATM|\\|BARD1|\\|APC|\\|MLH1|\\|MRE11|\\|MSH2|\\|MSH6|\\|MUTYH|\\|NBN|\\|PMS1|\\|PMS2|\\|RAD50|\\|RAD51D|\\|RAD51C|\\|XRCC2|\\|UIMC1|\\|FAM175A|\\|ERCC4|\\|RAD51|\\|RAD51B|\\|XRCC3|\\|FANCA|\\|FANCB|\\|FANCC|\\|FANCD2|\\|FANCE|\\|FANCF|\\|FANCG|\\|FANCI|\\|FANCL|\\|FANCM|\\|SLX4|\\|CASP8|\\|FGFR2|\\|TOX3|\\|MAP3K1|\\|MRPS30|\\|SLC4A7|\\|NEK10|\\|COX11|\\|ESR1|\\|CDKN2A|\\|CDKN2B|\\|ANKRD16|\\|FBXO18|\\|ZNF365|\\|ZMIZ1|\\|BABAM1|\\|LSP1|\\|ANKLE1|\\|TOPBP1|\\|BCCIP|\\|53BP1|\\|BRCA1:\\|BRCA2:\\|CHEK2:\\|PALB2:\\|BRIP1:\\|TP53:\\|PTEN:\\|STK11:\\|CDH1:\\|ATM:\\|BARD1:\\|APC:\\|MLH1:\\|MRE11:\\|MSH2:\\|MSH6:\\|MUTYH:\\|NBN:\\|PMS1:\\|PMS2:\\|RAD50:\\|RAD51D:\\|RAD51C:\\|XRCC2:\\|UIMC1:\\|FAM175A:\\|ERCC4:\\|RAD51:\\|RAD51B:\\|XRCC3:\\|FANCA:\\|FANCB:\\|FANCC:\\|FANCD2:\\|FANCE:\\|FANCF:\\|FANCG:\\|FANCI:\\|FANCL:\\|FANCM:\\|SLX4:\\|CASP8:\\|FGFR2:\\|TOX3:\\|MAP3K1:\\|MRPS30:\\|SLC4A7:\\|NEK10:\\|COX11:\\|ESR1:\\|CDKN2A:\\|CDKN2B:\\|ANKRD16:\\|FBXO18:\\|ZNF365:\\|ZMIZ1:\\|BABAM1:\\|LSP1:\\|ANKLE1:\\|TOPBP1:\\|BCCIP:\\|53BP1:\\|^BRCA1\t\\|^BRCA2\t\\|^CHEK2\t\\|^PALB2\t\\|^BRIP1\t\\|^TP53\t\\|^PTEN\t\\|^STK11\t\\|^CDH1\t\\|^ATM\t\\|^BARD1\t\\|^APC\t\\|^MLH1\t\\|^MRE11\t\\|^MSH2\t\\|^MSH6\t\\|^MUTYH\t\\|^NBN\t\\|^PMS1\t\\|^PMS2\t\\|^RAD50\t\\|^RAD51D\t\\|^RAD51C\t\\|^XRCC2\t\\|^UIMC1\t\\|^FAM175A\t\\|^ERCC4\t\\|^RAD51\t\\|^RAD51B\t\\|^XRCC3\t\\|^FANCA\t\\|^FANCB\t\\|^FANCC\t\\|^FANCD2\t\\|^FANCE\t\\|^FANCF\t\\|^FANCG\t\\|^FANCI\t\\|^FANCL\t\\|^FANCM\t\\|^SLX4\t\\|^CASP8\t\\|^FGFR2\t\\|^TOX3\t\\|^MAP3K1\t\\|^MRPS30\t\\|^SLC4A7\t\\|^NEK10\t\\|^COX11\t\\|^ESR1\t\\|^CDKN2A\t\\|^CDKN2B\t\\|^ANKRD16\t\\|^FBXO18\t\\|^ZNF365\t\\|^ZMIZ1\t\\|^BABAM1\t\\|^LSP1\t\\|^ANKLE1\t\\|^TOPBP1\t\\|^BCCIP\t\\|^53BP1\t\\|\"BRCA1\"\\|\"BRCA2\"\\|\"CHEK2\"\\|\"PALB2\"\\|\"BRIP1\"\\|\"TP53\"\\|\"PTEN\"\\|\"STK11\"\\|\"CDH1\"\\|\"ATM\"\\|\"BARD1\"\\|\"APC\"\\|\"MLH1\"\\|\"MRE11\"\\|\"MSH2\"\\|\"MSH6\"\\|\"MUTYH\"\\|\"NBN\"\\|\"PMS1\"\\|\"PMS2\"\\|\"RAD50\"\\|\"RAD51D\"\\|\"RAD51C\"\\|\"XRCC2\"\\|\"UIMC1\"\\|\"FAM175A\"\\|\"ERCC4\"\\|\"RAD51\"\\|\"RAD51B\"\\|\"XRCC3\"\\|\"FANCA\"\\|\"FANCB\"\\|\"FANCC\"\\|\"FANCD2\"\\|\"FANCE\"\\|\"FANCF\"\\|\"FANCG\"\\|\"FANCI\"\\|\"FANCL\"\\|\"FANCM\"\\|\"SLX4\"\\|\"CASP8\"\\|\"FGFR2\"\\|\"TOX3\"\\|\"MAP3K1\"\\|\"MRPS30\"\\|\"SLC4A7\"\\|\"NEK10\"\\|\"COX11\"\\|\"ESR1\"\\|\"CDKN2A\"\\|\"CDKN2B\"\\|\"ANKRD16\"\\|\"FBXO18\"\\|\"ZNF365\"\\|\"ZMIZ1\"\\|\"BABAM1\"\\|\"LSP1\"\\|\"ANKLE1\"\\|\"TOPBP1\"\\|\"BCCIP\"\\|\"53BP1\"\\|BRCA1\t|BRCA2\t|CHEK2\t|PALB2\t|BRIP1\t|TP53\t|PTEN\t|STK11\t|CDH1\t|ATM\t|BARD1\t|APC\t|MLH1\t|MRE11\t|MSH2\t|MSH6\t|MUTYH\t|NBN\t|PMS1\t|PMS2\t|RAD50\t|RAD51D\t|RAD51C\t|XRCC2\t|UIMC1\t|FAM175A\t|ERCC4\t|RAD51\t|RAD51B\t|XRCC3\t|FANCA\t|FANCB\t|FANCC\t|FANCD2\t|FANCE\t|FANCF\t|FANCG\t|FANCI\t|FANCL\t|FANCM\t|SLX4\t|CASP8\t|FGFR2\t|TOX3\t|MAP3K1\t|MRPS30\t|SLC4A7\t|NEK10\t|COX11\t|ESR1\t|CDKN2A\t|CDKN2B\t|ANKRD16\t|FBXO18\t|ZNF365\t|ZMIZ1\t|BABAM1\t|LSP1\t|ANKLE1\t|TOPBP1\t|BCCIP\t|53BP1  "
  p_mail.send <- 1 # 0=FALSE, 1=TRUE ; Indicate whether we want an email sent when the run is finished
}
p_genver    <- "hg19" # hg19 is the only one supported throughout the whole pipeline still, as of January 2013.
p_index     <- FALSE #FALSE # TRUE         
p_log       <- TRUE        
p_dbsnp     <- "132" # 132 for dbsnp132 is the one supported throughout the whole pipeline still, regarding current annovar version, as of January 2013.
p_summarize <- TRUE  
p_snpeff.of <- "txt" # Output format for snpEff. Possible values: txt, vcf, gatk, bed, bedAnn (txt will be deprecated, but it can be ocasionally useful still in the meantime)
p_cpus      <- 4             
p_parallel  <- TRUE # TRUE #FALSE #FALSE #TRUE # Do you want to allow running some parallelized processes at all? (which ones will be specified elsewhere in the code)
p_bwa       <- 2          # Algorythm for mapping with bwa - http://bio-bwa.sourceforge.net/bwa.shtml
                        # 1: bwa aln      + samse  (short reads, single ends, low errors);
                        # 2: bwa aln (x2) + sampe  (short reads, paired ends, low errors);
                        # 3: bwa bwasw             (longer reads, single end only) 
p_convert.file.list.pe        <- TRUE #FALSE #TRUE # Keep as TRUE if you have paired end samples (sampe; p_bwa=2)
                                      # --and you are re-processing just some steps--, since you will need
                                      # the input file list 1_sequence.fastq etc converted into the _merged12.sam 
                                      # IMPORTANT: 
                                      # Since revision bzr 70'ish (mid Jan'2013), this might need to be enabled as TRUE in both cases 
                                      # of running the whole pipeline or just some steps, when using short reads paired end (sampe; p_bwa=2)

# Reporting by email at the end of the run
p_from <- "xavier.depdedro@vhir.org"
p_to <- "xdpedro@ir.vhebron.net"
p_subject <- paste("EVA Pipeline run finished - ", p_label, sep="")
p_body <- paste(p_subject, " - from ", program_ueb," - See some log information attached", sep="")                   
p_smtp="smtp.ir.vhebron.net"



# Set flags as ON (TRUE) or OFF (FALSE) for all processes from function:
#  wrapper.sequential (wseq)
#----------------------------------
#####
runParam <- TRUE #######################
p_map.on.reference.genome.sequential.mt     <- runParam # In case we run the mapping sequentially, in multi-thread mode for each sample, with as many threads as cpus indicated in the input params (opt$cpus) 

runParam <- FALSE # !runParam ####################### The opposite to map in sequential mode
p_map.on.reference.genome.parallel       <- runParam # In case we run the mapping in parallel for n (p_cpus) samples at a time

# Set all params inside a list, so that it's easier to send from main to functions
# wseq : for function wrapper.sequential
params_wseq <- list()
params_wseq <- list(
  p_map.on.reference.genome.sequential.mt  = p_map.on.reference.genome.sequential.mt, 
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
p_bowtie2sam                  <- runParam
#####
runParam <- TRUE #######################
####
p_sam2bam.and.sort		        <- runParam
p_remove.pcr.dup		          <- runParam
p_gatk.sortbyref              <- FALSE # runParam # Not working properly yet. Error message: Can not open temporary file 1080: Too many open files at /home/ueb/Data/gatk-data-2.3/SortByRef.pl line 95, <$INPUT> line 50737751.
p_gatk.local.realign.step1    <- FALSE # TRUE # FALSE # runParam
p_gatk.local.realign.step2    <- FALSE # runParam # Unfinished work
p_gatk.local.realign.step3    <- FALSE # runParam # Unfinished work
p_index.bam.file		          <- runParam
p_stats			                  <- runParam
p_snpeff.count.reads          <- runParam # Apparently, Working properly since early 2013 March
p_exon.coverage   	          <- FALSE # runParam # Unfinished work
#####
runParam <- FALSE #######################
####
p_variant.calling		          <- runParam
p_variant.filtering		        <- runParam
p_gatk.combine.vcfs           <- FALSE # runParam # Non-started work (place holder only)
p_convert2vcf4		            <- runParam
p_variant.annotation.geneb	  <- runParam
p_variant.annotation.regionb	<- FALSE # runParam # Non-started work (place holder only)
p_variant.annotation.filterb	<- runParam
p_variant.annotation.summarize<- runParam
p_grep.variants		            <- runParam
p_visualize.variants		      <- FALSE # runParam # Non-started work (place holder only)
p_variant.dbsnp.pre.snpeff    <- FALSE # runParam # Non-started work (place holder only)
p_variant.eff.report          <- runParam
p_grep.post.snpeff.variants    <- runParam
#####
runParam <- FALSE #######################
####


# Set all params inside a list, so that it's easier to send from main to functions
# w2pps : for function wrapper2.parallelizable.per.sample
#params_w2pps <- list()
params_w2pps <- list(
  p_map.on.reference.genome.parallel  = p_map.on.reference.genome.parallel,
  p_quality.control                   = p_quality.control,
  p_convert.file.list.pe              = p_convert.file.list.pe,
  p_bowtie2sam                        = p_bowtie2sam,
  p_sam2bam.and.sort                  = p_sam2bam.and.sort,
  p_remove.pcr.dup                    = p_remove.pcr.dup,
  p_gatk.sortbyref                    = p_gatk.sortbyref,
  p_gatk.local.realign.step1          = p_gatk.local.realign.step1,
  p_gatk.local.realign.step2          = p_gatk.local.realign.step2,
  p_gatk.local.realign.step3          = p_gatk.local.realign.step3,
  p_index.bam.file                    = p_index.bam.file,
  p_stats                             = p_stats,
  p_snpeff.count.reads                = p_snpeff.count.reads,
  p_exon.coverage                     = p_exon.coverage,
  p_variant.calling                   = p_variant.calling,
  p_variant.filtering                 = p_variant.filtering,
  p_gatk.combine.vcfs                 = p_gatk.combine.vcfs,
  p_convert2vcf4                      = p_convert2vcf4,
  p_variant.annotation.geneb          = p_variant.annotation.geneb,
  p_variant.annotation.regionb        = p_variant.annotation.regionb,
  p_variant.annotation.filterb        = p_variant.annotation.filterb,
  p_variant.annotation.summarize      = p_variant.annotation.summarize,
  p_grep.variants                     = p_grep.variants,
  p_visualize.variants                = p_visualize.variants,
  p_variant.dbsnp.pre.snpeff          = p_variant.dbsnp.pre.snpeff,
  p_variant.eff.report                = p_variant.eff.report,
  p_grep.post.snpeff.variants         = p_grep.post.snpeff.variants
)

# 9b. Set flags as ON (TRUE) or OFF (FALSE) for all processes from function:
#  wrapper2.parallelizable.final (w2pf)
#----------------------------------
#####
runParam <- FALSE #######################
####
p_build.html.report  <- FALSE # runParam # Non-started work (place holder only)


# 9b. Distribute final Parallelizable calculation 
# w2pf : for function wrapper2.parallelizable.final
params_w2pf <- list()
params_w2pf <- list(
  # p_foo_w2pf = p_foo_w2pf, # dummy param to attempt to prevent error message "Error in cut.default(i, breaks) : 'breaks' are not unique" https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14898
  p_build.html.report             = p_build.html.report
)


# Define path params for all runs
#----------------------------------
# p_server = Choose machine where to get the paths for
# 1 for MainHead,
# 2 for B52,
p_server <- 1 # Set the server number (see codes above)

if (p_server==1) { # MainHead server
  # A. Reference Data file paths
    # A.i) Human Genome
      path_genome1 = "/home/ueb/Data/Data_Genomes/hg19/hg19.fa"  # chromosome names with prefix "chr"
      path_genome2 = "/home/ueb/Data/Data_Genomes/hg19_Broad_Reference_Genome/Homo_sapiens_assembly19.fasta" # chromosome names without prefix "chr"
      path_genome3 = "/home/ueb/Data/gatk-data-2.3/hg19/ucsc.hg19.fasta"
      path_genome4 = "/home/ueb/Data/Data_Genomes/forGATK/hg19_rCRSchrm.fa" # from http://epigenome.usc.edu/publicationdata/bissnp2011/utilies.html
      path_genome5 = "/home/ueb/Data/Data_Genomes/hg19/chr/hg19gatk.fa"  # chromosome names with prefix "chr" and in the order for gatk indicated in http://seqanswers.com/wiki/How-to/exome_analysis
    path_genome = path_genome1
    # A.ii) dbSNP
#      path_dbSNP1 = "/home/ueb/Data/dbSNP/dbsnp132_20101103.vcf" # chromosome names without prefix "chr"
      path_dbSNP1 = "/home/ueb/Data/dbSNP/dbsnp132_20101103_gatk.vcf" # chromosome names with the prefix "chr" being added by Xavi (Jan 23, 2013)
      path_dbSNP2 = "/home/ueb/Data/Data_Genomes/hg19_Broad_Reference_Genome/dbsnp_135_human_9606_v4.0_00-All.vcf" # chromosome names without prefix "chr"
      path_dbSNP3 = "/home/ueb/Data/gatk-data-2.3/hg19/dbsnp_137.hg19.vcf"
      path_dbSNP4 = "/home/ueb/Data/Data_Genomes/forGATK/dbsnp_135.hg19.sort.vcf"
    path_dbSNP = path_dbSNP1
    # A.iii) Exon Capture File (Intervals list)
      path_exon_capture_file1 = "/home/ueb/Data/BED/TruSeq_exome_targeted_regions.hg19.bed.chr"
      path_exon_capture_file2 = "/home/ueb/Data/BED/all_captured_exomes_from_ucsc_hg19.bed"
      path_exon_capture_file3 = "/home/ueb/Data/Data_Genomes/forGATK/whole_genome_interval_list.hg19.bed"
      path_exon_capture_file4 = "/home/ueb/Data/Data_Genomes/forGATK/whole_genome_interval_list.hg19.bed"
    path_exon_capture_file = path_exon_capture_file4 
  # B. Program file paths
  path_fastq = "/home/ueb/fastqc/fastqc" 
  path_vcfutils = "/usr/share/samtools/vcfutils.pl"
  path_convert2annovar = "/home/ueb/annovar/convert2annovar.pl"
  path_annotate_variation = "/home/ueb/annovar/annotate_variation.pl"
  path_annotate_humandb = "/home/ueb/annovar/humandb/"
  path_summarize_annovar = "/home/ueb/annovar/summarize_annovar.pl"           
  path_snpEff = "/home/ueb/snpEff/" # end with trailing slash but no script, since the same folder is used for several files          
  path_gatk = "/home/ueb/GenomeAnalysisTKLite-2.3-4-gb8f1308/GenomeAnalysisTKLite.jar" # end with the jar file since it can be lite or not
  path_gatk_key = "/home/ueb/GenomeAnalysisTKLite-2.3-4-gb8f1308/ueb_vhir.org.key" # key to avoid gatk 'calling home' (to gatk authors) on each run. See http://gatkforums.broadinstitute.org/discussion/1250/what-is-phone-home-and-how-does-it-affect-me
  path_gatk_sortbyref = "/home/ueb/Data/gatk-data-2.3/SortByRef.pl" # see http://gatkforums.broadinstitute.org/discussion/1328/script-for-sorting-an-input-file-based-on-a-reference-sortbyref-pl

} else if (p_server==2) { # B52 server
    
  path_fastq = "/home/ueb/software/FastQC/fastqc"
  path_genome = "/home/ueb/Data/Data_Genomes/hg19.fa" 
  #path_genome = "/home/ueb/Data/Data_Genomes/rn4/rn4.fa"
  path_vcfutils = "/usr/share/samtools/vcfutils.pl"
  path_convert2annovar = "/home/ueb/software/annovar/convert2annovar.pl"
  path_annotate_variation = "/home/ueb/software/annovar/annotate_variation.pl"
  path_annotate_humandb = "/home/ueb/software/annovar/humandb/"
  path_summarize_annovar = "/home/ueb/software/annovar/summarize_annovar.pl"           
  path_snpEff = "/home/ueb/snpEff/" # end with trailing slash but no script, since the same folder is used for several files           
  path_gatk = "/home/ueb/GenomeAnalysisTKLite-2.3-4-gb8f1308/GenomeAnalysisTKLite.jar" # end with the jar file since it can be lite or not
  path_gatk_key = "/home/ueb/GenomeAnalysisTKLite-2.3-4-gb8f1308/ueb_vhir.org.key" # key to avoid gatk 'calling home' (to gatk authors) on each run. See http://gatkforums.broadinstitute.org/discussion/1250/what-is-phone-home-and-how-does-it-affect-me
  path_dbSNP1 = "/home/ueb/Data/dbSNP/dbsnp132_20101103.vcf"
  path_dbSNP = path_dbSNP1
  path_exon_capture_file1 = "/home/ueb/Data/BED/TruSeq_exome_targeted_regions.hg19.bed.chr"
  path_exon_capture_file2 = "/home/ueb/Data/BED/all_captured_exomes_from_ucsc_hg19.bed"  
  path_exon_capture_file = path_exon_capture_file2
}
