#!/usr/bin/perl -w
#
# SCRIPT: eva_ueb_for_r.pl # Adapted version to work from an R script (eva_ueb.R) to get parallelization
# Input: Directories of input and output
# Output: List of Variants
# Autor: Xavier de Pedro (2012)
#        xavier.depedro@vhir.org
# Acknowledgements: Alex Sanchez, Enrique Blanco.
# License: GNU/GPL.
###################################################

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

##########################
### Main Program
##########################

# Enable the strict declaration of variables
# ---------------------------------------------------------------------
use strict;
use Term::ANSIColor;
use Getopt::Std; # to allow using 'getopts' for argument handling in perl from the command line call
				# For more info, see: http://www.devdaily.com/perl/perl-getopts-command-line-options-flags-in-perl
				#  or http://www.vromans.org/johan/articles/getopt.html

# Define and initialize a few vars.
# ---------------------------------------------------------------------
my $program_ueb = "eva_ueb.pl";
my %options=();
my $directory_in = ".";
my $directory_out = ".";
# Fetch the revision number from the branch of the bazaar revision control system
my $revision = `bzr revno`; # the use of back ticks (`) instead of the usual system call ("system()") are for indicating that the output of the system call is to be saved into a perl variable.
my $step_n = 0; # Step number for the loop inside each file to process
my $file_n = 0; # File number for the loop of file to process
#my $indexing = 0; # Param to indicate whether indexing the reference gnome is needed or not 
my $step_tmp = $step_n; # dummy counter for the number of step when inside the files loop
my $command00;
my $now = nownice(time);
my $path_fastq = "/home/ueb/fastqc/fastqc";
my $path_genome = "/home/xavi/Data/Data_Genomes/hg19/hg19.fa";
my $path_vcfutils = "/usr/share/samtools/vcfutils.pl";
my $path_convert2annovar = "/home/ueb/annovar/convert2annovar.pl";
my $path_annotate_variation = "/home/ueb/annovar/annotate_variation.pl";
my $path_annotate_humandb = "/home/ueb/annovar/humandb/";
my $path_summarize_annovar = "/home/ueb/annovar/summarize_annovar.pl";
#my $targetgenes = ""; # not used as such; fetched directly from the argument value

# Argument handling of the program call
# ---------------------------------------------------------------------
# # declare the perl command line flags/options we want to allow
getopts("hi:o:nf:l:sk", \%options);
# Arguments
# h: show help on usage # Optional
# i: Directory with __I__nput data files # Compulsory
# o: Directory for __O__utput data files # Compulsory
# n: i__N__dex the reference genome # Optional
# f: __F__ilter results for these target genes # Optional
# l: __L__og info about the process into a log file # Optional
# s: __S__summarize results in a single .csv file with annotations # Optional
# k: __K__eep temporal dummy files after they have been used # Optional

# test for the existence of the options on the command line.
# in a normal program you'd do more than just print these.
print "-h $options{h}\n" if defined $options{h};
print "-i $options{i}\n" if defined $options{i};
print "-o $options{o}\n" if defined $options{o};
print "-n $options{n}\n" if defined $options{n};
print "-f $options{f}\n" if defined $options{f};
print "-l $options{l}\n" if defined $options{l};
print "-s $options{s}\n" if defined $options{s};
print "-k $options{k}\n" if defined $options{k};

# other things found on the command line
print "Other things found on the command line:\n" if $ARGV[0];
foreach (@ARGV)
{
  print "$_\n";
}

## Header shown at program execution
# ---------------------------------------------------------------------
print_doc("##############################################################################");
print_doc("### $program_ueb: Pipeline for 'Exome Variant Analysis' (EVA) at the UEB     ###");
print_doc("###   r$revision###                                                                        ###");
print_doc("###   ( using dbSNP 132 & hg 19 )                                          ###");
print_doc("### (c) 2011-12 UEB - GPL licensed - http://ueb.vhir.org/tools             ###");
print_doc("##############################################################################\n");

# Manage showing help if requested
# ---------------------------------------------------------------------
if ($options{h}) # show help info
{
  show_help();
} else {  # Show instead the tip on how to call the help info
	print("Use '$program_ueb -h' for help\n\n");
}

## Step 1. Fetch the directory name
# ---------------------------------------------------------------------
$step_n = 1;
if ($options{i}) # there is some input directory
{
  $directory_in = $options{i};
} else {
	print_error("Missing directory with input files (-i)");
}
if ($options{o}) # there is some output directory 
{
  $directory_out = $options{o};
} else {
	print_error("Missing directory for ourput files (-o)");
}

			#my ($directory_in,$directory_out,$indexing,$n_arguments);
			##$n_arguments = scalar(keys @ARGV); # with 'scalar(@ARGV);' I was misscounting 3 when 3 arguments where passed; however, only working in perl 5.12 (ubuntu 11.10), not on perl 5.10 (ubuntu 10.04) 
			#$n_arguments = scalar(@ARGV); # misscounts argumetns, but at least it works on perl 5.10 on ubuntu 10.04
			
			#if ($n_arguments < 2 || $n_arguments > 3) {
				#print_error("Wrong number of parameters: source and destination directories missing");
			#} elsif ($n_arguments == 2) {
				#($directory_in,$directory_out) = @ARGV;
				#$indexing = "off"; # Set indexing to 0 (off)
			#}else{ # case when there are 3 arguments
				#($directory_in,$directory_out,$indexing) = @ARGV;
			#}

print_doc("Step $step_n. Reading parameters");
print_done();

## Step 2. Accesing directory
# ---------------------------------------------------------------------
$step_n = $step_n+1;
# Definition of internal variables
my ($file,$file_in,$file_out,$sum_file_in,$sum_file_out);
my ($name,$filter,$command,$options00,$options01);

opendir(DIR,$directory_in);
			#opendir(DIR,$directory_in) or die "Cannot open $directory_in: !";
			#opendir my $dh, $dir_to_process or die "Cannot open $dir_to_process: $!";
print_doc("Step $step_n. Accesing directory: $directory_in");
print_done();

while ($file = readdir(DIR))
{
	## Control over directories . & ..
	next if ($file =~ /^\./);
	## Skip files without .fastq extension
	next if ($file !~ /\.fastq/);
	$file_n = $file_n + 1;
	$step_n = $step_n+1;
	$step_tmp = 0;
	print_doc("$now - ########## Step $step_n. Processing file #$file_n ($file) ... ##########");

	## Step 3. Quality Control and Preprocessing (using fastqc)
	# ---------------------------------------------------------------------
	# See http://www.bioinformatics.bbsrc.ac.uk/projects/fastqc/INSTALL.txt
	$step_tmp = $step_tmp + 1;
	# Remove .fastq (substitute it with nothing) from file names
	($name = $file) =~ s/\.fastq//;
	print_doc("$now -   Step $step_n.$step_tmp Quality Control and Preprocessing: $name ...");
	$file_in = "$directory_in/$file";
#	$file_out = "$directory_out/$name.txt";
	$command00 = "$path_fastq"; # path to fastqc binary; adapt to your case.
#	$command00 = "ls"; # next command.
	$options00 = "$file_in --outdir=$directory_out ";
	$command = "$command00 $options00";
	system($command);
	print_done();



	## Next Step. Map against reference genome: Index the reference genome (if needed)
	# ---------------------------------------------------------------------
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Map against reference genome: Index the reference genome (if needed): $name ...");
			#	# Index the reference genome, if requested with param "-index"
			##	if (($ARGV[2] eq '-index') || ($ARGV[3] eq '-index')) { # index the reference genome
			##	if ($n_arguments == 3) { # only when there are 3 arguments use $indexing; otherwise,  a warning of uninitialized $indexing is shown 
			#		if ((($indexing eq '-index') || ($indexing eq '-Index')) && ($file_n == 1)) { # index the reference genome when requested by the -index param but only once if more than one sample to process
			#		#	$file_in = "$directory_in/$name";
			#		#	$file_out = "$directory_out/$name.txt";
			#			$command00 = "bwa index"; # next command
			#			$options00 = " -a bwtsw $path_genome";
			#		}
			#		else { # skip the indexing of the reference genome
			#			$command00 = "echo '  ...skipped...'"; # next command.
			#			$options00 = "";
			#		}
			##	} # end of condition for when there are 3 arguments
	
	#print $options{n};
	
	if (($options{n}) && ($file_n == 1)) # case to index the reference genome (time consuming, do only when really needed as requested)
	{
		# Index the reference genome, if requested with argument -n and only for the first file if more than one sample to process
		$command00 = "bwa index"; # next command
		$options00 = " -a bwtsw $path_genome";
#		$command00 = "echo '  ...fake indexing XXX...'"; # next command.
	} else 	{ # skip the indexing of the reference genome
		$command00 = "echo '  ...skipped...'"; # next command.
		$options00 = "";
	}
	$command = "$command00 $options00";
	system($command);
	print_done();

			#die(); #### For Debuging only XXXX
			
			## XXX debugging ini...
			##print_doc("@ARGV"); # xxx debug
			#print_doc("@ARGV[0]"); # xxx debug
			#print_doc("@ARGV[1]"); # xxx debug
			#print_doc("@ARGV[2]"); # xxx debug
			##$n_arguments = scalar(keys @ARGV); # xxx debug
			#print_doc("$n_arguments") ; # xxx debug
			#exit(0); # XXX debugging end...


	## Next Step. Map against reference genome: do the mapping
	# ---------------------------------------------------------------------
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Map against reference genome: do the mapping: $name ...");
	$file_in = "$directory_in/$file";
	$file_out = "$directory_out/$name.sam";
	$command00 = "bwa bwasw"; # next command.
	$options00 = " $path_genome $file_in > $file_out";
	$command = "$command00 $options00";
	system($command);
	print_done();
	# At this step we don't do check2clean since that is the input file, and not temporary output files.


	## Next Step. Convert sam to bam and sort it
	# ---------------------------------------------------------------------
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Convert sam to bam and sort it: $name ...");
	$file_in = $file_out;
	$file_out = "$directory_out/$name.sam.sorted";
	$command00 = "samtools"; # next command.
	$options00 = " view -bS $file_in | samtools sort - $file_out"; # Attention: it seems that the real file generated is a .bam file
	$command = "$command00 $options00";
	system($command);
	print_done();
	check2clean("$file_in");



	## Next Step. Remove possible PCR duplicates
	# ---------------------------------------------------------------------
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Remove possible PCR duplicates: $name ...");
	$file_in = "$directory_out/$name.sam.sorted.bam";
	$file_out = "$directory_out/$name.sam.sorted.noDup.bam";
	$command00 = "samtools"; # next command.
	$options00 = " rmdup -s $file_in $file_out";
	$command = "$command00 $options00";
	system($command);
	print_done();
	check2clean("$file_in");



	## Next Step. Index the bam file
	# ---------------------------------------------------------------------
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Index the bam file: $name ...");
	$file_in = "$directory_out/$name.sam.sorted.noDup.bam";
#	$file_out = "";
	$command00 = "samtools"; # next command.
	$options00 = " index $file_in";
	$command = "$command00 $options00";
	system($command);
	print_done();
	# Don't check for check2clean("$file_in") since we still need it to do some stats upon it



	## Next Step. Get some stats using the samtools [optional]
	# ---------------------------------------------------------------------
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Get some stats using the samtools [optional]: $name ...");
	$file_in = "$directory_out/$name.sam.sorted.noDup.bam";
	$file_out = "";
	$command00 = "samtools"; # next command.
	$options00 = " flagstat $file_in";
	$command = "$command00 $options00";
	system($command);
	print_done();
	# Don't check for check2clean("$file_in") since we still need it for the variant calling



	## Next Step. Variant calling
	# ---------------------------------------------------------------------
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Variant calling: $name ...");
	$file_in = "$directory_out/$name.sam.sorted.noDup.bam";
	$file_out = "$directory_out/$name.sam.sorted.noDup.bam.samtools.var.raw.vcf";
	$command00 = "samtools"; # next command.
	$options00 = " mpileup -uf $path_genome $file_in | bcftools view -vcg - > $file_out";
	$command = "$command00 $options00";
	system($command);
	print_done();
	check2clean("$file_in");
	
			# testing
			# samtools mpileup -uf $path_genome ./dir_out/prova1.sam.sorted.noDup.bam | bcftools view -vcg - > ./dir_out/prova1.sam.sorted.noDup.bam.samtools.var.raw.vcf


	## Next Step. Variant Filtering
	# ---------------------------------------------------------------------
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Variant Filtering: $name ...");
	$file_in = $file_out; # get the previous output file as input for this step
	$file_out = "$directory_out/$name.sam.sorted.noDup.bam.samtools.var.filtered.vcf";
	$command00 = "$path_vcfutils"; # next command.
	$options00 = " varFilter -Q 10 -d 15 -a 5 $file_in > $file_out";
	$command = "$command00 $options00";
	system($command);
	print_done();
	check2clean("$file_in");



	## Next Step. Convert files to Annnovar vcf4 format
	# ---------------------------------------------------------------------
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Convert files to Annnovar vcf4 format: $name ...");
	$file_in = $file_out; # get the previous output file as input for this step
	$file_out = "$directory_out/$name.sam.sorted.noDup.bam.samtools.var.filtered.vcf.annovar";
	$command00 = "$path_convert2annovar"; # next command.
	$options00 = " $file_in -format vcf4 > $file_out";
	$command = "$command00 $options00";
	system($command);
	print_done();
	#check2clean("$file_in"); # Commented out so that samtools standard .vcf files (and not only the converted to .vcf4 - .vcf.annovar - format) are also always kept.
	
	## Next Step. Variant Annotation
	# ---------------------------------------------------------------------
		## Alternative annotation procedure 1: SeattleSeq Website
		# See: http://snp.gs.washington.edu/SeattleSeqAnnotation134/

		## Alternative annotation procedure 2: VariantAnnotation Bioconductor package for R 2.14
		# See: http://www.bioconductor.org/packages/devel/bioc/html/VariantAnnotation.html
		
	# We will be mainly annotating by (1) gene, (2) region or (3) filtering specific nucleotide changes (the 3 methos of annovar)
	# See: http://www.openbioinformatics.org/annovar/

	# (1) Variant Annotation with Annovar: Gene-based
	#     Identify whether SNPs or CNVs cause protein coding changes and the amino acids that are affected. Users can flexibly use RefSeq genes, UCSC genes, ENSEMBL genes, GENCODE genes, or many other gene definition systems.
	#     http://www.openbioinformatics.org/annovar/annovar_gene.html
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Variant Annotation (gene-based): $name ...");
	$file_in = $file_out; # get the previous output file as input for this step
	$file_out = "$directory_out/$name.sam.sorted.noDup.bam.samtools.var.filtered.vcf.exonic_variant_function_gene"; # XXX this extension ".exonic_variant_function" is hardcoded in annovar. It's written here to be given for the next steep as input file name 
	$command00 = "perl $path_annotate_variation"; # next command.
	$options00 = " -geneanno --buildver hg19 $file_in $path_annotate_humandb";
	$command = "$command00 $options00";
	system($command);
	print_done();
	# We don't do check2clean here  either since we will still use the .vcf.annovar file in the next steps

			# a mà la instrucció al mainhead és:
			# perl /home/ueb/annovar/annotate_variation.pl -geneanno --buildver hg19 /home/xavi/repo/peeva/dir_out/Gutierrez_B_Sure.sequence_m50.sam.sorted.noDup.bam.samtools.var.filtered.vcf.annovar /home/ueb/annovar/humandb/
			# perl /home/ueb/annovar/annotate_variation.pl -geneanno --buildver hg19 /home/ueb/estudis/ngs/2011-08-SGutierrez-VHIO-207/111224_peeva_dir_out/Gutierrez_B_Sure.sequence.sam.sorted.noDup.bam.samtools.var.filtered.vcf.annovar /home/ueb/annovar/humandb/

	# (2) Variant Annotation with Annovar: Region-based
	# 	  Identify variants in specific genomic regions, for example, conserved regions among 44 species, predicted transcription factor binding sites, segmental duplication regions, GWAS hits, database of genomic variants, DNAse I hypersensitivity sites, ENCODE H3K4Me1/H3K4Me3/H3K27Ac/CTCF sites, ChIP-Seq peaks, RNA-Seq peaks, or many other annotations on genomic intervals
	#     Skipped so far (May 2012)
	#     http://www.openbioinformatics.org/annovar/annovar_region.html

	# (3) Variant Annotation with Annovar: Filter-based
	#     Identify variants that are reported in dbSNP, or identify the subset of common SNPs (MAF>1%) in the 1000 Genome Project, or identify subset of non-synonymous SNPs with SIFT score>0.05, or many other annotations on specific mutations
	#     http://www.openbioinformatics.org/annovar/annovar_filter.html
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Variant Annotation (filter based): $name ...");
	$file_in = "$directory_out/$name.sam.sorted.noDup.bam.samtools.var.filtered.vcf.annovar"; 
	$file_out = "$directory_out/$name.sam.sorted.noDup.bam.samtools.var.filtered.vcf.exonic_variant_function_filter"; # XXX this extension ".exonic_variant_function" is hardcoded in annovar. It's written here to be given for the next steep as input file name 
	$command00 = "perl $path_annotate_variation"; # next command.
	$options00 = " -filter --buildver hg19  -dbtype snp132 $file_in $path_annotate_humandb";
	$command = "$command00 $options00";
	system($command);
	print_done();
	# We don't do check2clean here  either since we will still use the .vcf.annovar file in the next steps (summarizing annotations and filtering)


		# a mà la instrucció al mainhead és:
		# perl /home/ueb/annovar/annotate_variation.pl -filter --buildver hg19 -dbtype snp132 /home/xavi/repo/peeva/dir_out/vhir_sample_a_sure_1e6.sam.sorted.noDup.bam.samtools.var.filtered.vcf.annovar /home/ueb/annovar/humandb/

	# (extra) Variant Annotation with Annovar: summarize_annovar.pl
	#     Given a list of variants from whole-exome or whole-genome sequencing, it will generate an Excel-compatible file with gene annotation, amino acid change annotation, SIFT scores, PolyPhen scores, LRT scores, MutationTaster scores, PhyloP conservation scores, GERP++ conservation scores, dbSNP identifiers, 1000 Genomes Project allele frequencies, NHLBI-ESP 5400 exome project allele frequencies and other information.
	#     http://www.openbioinformatics.org/annovar/annovar_accessary.html#excel
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Variant Annotation (summarize annotations in .csv): $name ...");
	if ($options{s}) # case to summarize annotation results in a single .csv file 
		{
			$file_in = "$directory_out/$name.sam.sorted.noDup.bam.samtools.var.filtered.vcf.annovar"; 
			$sum_file_out = "$directory_out/$name.sum"; # summarize_annovar.pl adds the extension .exome_summary (hardcoded in annovar). 
			$command00 = "perl $path_summarize_annovar"; # next command.
			$options00 = " --buildver hg19 --verdbsnp 132 $file_in $path_annotate_humandb --outfile $file_out";
		} else { # skip 
			$command00 = "echo '  ...skipped...'"; # next command.
			$options00 = "";
		}
	$command = "$command00 $options00";
	system($command);
	print_done();
	# We don't do check2clean here  either since we will still use the .vcf.annovar file in the next steps (filtering)


		# a mà la instrucció al mainhead és:
		# perl /home/ueb/annovar/summarize_annovar.pl --buildver hg19  --verdbsnp 132 /home/xavi/repo/peeva/dir_out/vhir_sample_a_sure_1e6.sam.sorted.noDup.bam.samtools.var.filtered.vcf.annovar /home/ueb/annovar/humandb/ --outfile sum

	## Next Step. Filter variants for the target genes
	# ---------------------------------------------------------------------
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Filter variants for the target genes: $name ...");
	if ($options{f}) # case to search for specific target genes 
		{
			$file_in =  $file_out; # get the previous output file as input for this step
			$file_out = "$directory_out/$name.results_".get_timestamp().".txt";
			$command00 = "grep"; # next command.
			$options00 = "  '$options{f}' $file_in > $file_out";
			# Remember that the values in the previous $options{f} variable needs to be like: 'BRCA1\|BRCA2' 
			# in order to end up performing a command like:
			# grep 'BRCA1\|BRCA2' dir_out/Gutierrez_A_*.exonic* 
			if ($options{s}) # case to have a file with summarized annotations to search also for specific target genes 
				{
					$sum_file_in = $sum_file_out;  # get the previous summary output file as input for this step
					$sum_file_out = "$directory_out/$name.sum_results_".get_timestamp().".txt";
					$options01 = "  '$options{f}' $sum_file_in > $sum_file_out";
				}
		} else { # skip the searching for specific target genes 
			$command00 = "echo '  ...skipped...'"; # next command.
			$options00 = "";
		}
	$command = "$command00 $options00";
	system($command);
	if ($options{f} && $options{s}) # case to have a file with summarized annotations to search also for specific target genes 
		{
			$command = "$command00 $options01";
			system($command);
		}
	print_done();
	check2clean("$file_in"); # here we check if the user requested to clean the .vcf.annovar nowadays that it's not being used any more.

	## Next Step. Visualization of Variants
	# ---------------------------------------------------------------------
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Visualization of Variants: $name ...");
	$file_in = "";
	$file_out = "";
	$command00 = "echo '  ...skipped...'"; # next command.
	$options00 = "";
	$command = "$command00 $options00";
	system($command);
	print_done();



	### Step N. Template for the next step. Clone this section before adding a new section
	#$step_tmp = $step_tmp + 1;
	#print_doc("$now -   Step $step_n.$step_tmp Next step: $name ...");
	#$file_in = "";
	#$file_out = "";
	#$command00 = "echo '  ...skipped...'"; # next command.
	#$options00 = "";
	#$command = "$command00 $options00";
	#system($command);
	#print_done("$now -\t\t\t\t\t");

}

# Close directory
closedir(DIR);

# closing....
# ---------------------------------------------------------------------
$step_n = $step_n+1;
print_done("$now - Step $step_n. End of EVA UEB pipeline");
exit(0);

##########################
### FUNCTIONS
##########################
sub get_timestamp 
{
	# ---------------------------------------------------------------------
   my($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
   if ($mon < 10) { $mon = "0$mon"; }
   if ($hour < 10) { $hour = "0$hour"; }
   if ($min < 10) { $min = "0$min"; }
   if ($sec < 10) { $sec = "0$sec"; }
   $year=$year+1900;

#   return $year . '_' . $mon . '_' . $mday . '__' . $hour . '_' . $min . '_' . $sec;
   sprintf '%04d-%02d-%02d_%02d-%02d-%02d', $year, $mon, $mday, $hour, $min, $sec;

}

sub print_doc
{
	# ---------------------------------------------------------------------
	my $mess;
	$mess = $_[0];
	$now = nownice(time);

	print STDOUT color("bold cyan"),
		"$mess\n";
	print STDOUT color(" reset ");
}

sub print_done
{
	# ---------------------------------------------------------------------
#	my $mess;
#	$mess = $_[0];
	$now = nownice(time);

	print STDOUT color("bold green"),
#		"$now - \t\t\t\t\t $mess\t\t[DONE]\n\n";
		"$now - \t\t\t\t\t\t\t[DONE]\n\n";
	print STDOUT color(" reset ");
}

sub print_error
{
	# ---------------------------------------------------------------------
	my $mess;
	$mess = $_[0];
	$now = nownice(time);

	print STDERR color("bold red"),
		"$mess\t\t[ERROR]\n\n";
	print STDERR color(" reset ");
	die();
}

sub nownice
{
	# ---------------------------------------------------------------------
	my ($mess, $yearcurrent);
	$mess = $_[0];
	my( $year, $month, $day, $hour, $minute ) = (localtime)[5,4,3,2,1];
	$yearcurrent = 1900 + $year;
	sprintf '%04d-%02d-%02d %02d:%02d', $yearcurrent, $month, $day, $hour, $minute;
}

sub check2clean
{
	# ---------------------------------------------------------------------
	my $var;
	$var = $_[0];
	if ($options{k}) # keep temporary files if requested by the user 
	{ 
		# do nothing
	} else { # clean temporary files not from this but from the previous step
		system("rm  $var");
	}
}

sub show_help {
  # ---------------------------------------------------------------------
  print "The help info needs to be updated to the new argument system for calls to the $program_ueb.\n";
  print_doc("\nThis program $program_ueb accepts 3 arguments passed to the program call:
  -i: directory with source .fastq files \t\t\t *** required ***
  -o: directory to save output files \t\t\t\t *** required ***
  -n: -index (indexing of the reference genome)\t\t\t    [optional]
  -s: summarize results with annotations in a single .csv file \t    [optional]
  -f: filter results for these target genes \t\t\t    [optional]
      with this syntax for one gene:
       -f BRCA1 
      or
       -f 'BRCA1\|BRCA2\|unknown' 
      for more than one gene or string to filter results
  -k: keep temporary files after they have been used \t\t    [optional] 
  -h: show this help text \t\t\t\t\t    [optional]\n
  -l: log results (optional, to be coded ;-). In the mean time, use the standard unix utilities such as:\n
  Example1: perl $program_ueb -i ./dir_in -o ./dir_out -s -f 'BRCA1\|BRCA2\|unknown' > ./logs/log_stdout.txt 2> ./logs/log_stderr.txt
  Example2: perl $program_ueb -i ./test_in -o ./test_out -s -k > ./logs/log_both.txt 2>&1
  Example3: perl $program_ueb -i ./test_in -o ./test_out -s -k | tee /dev/tty ./logs/log_both.txt\n");
  print_doc("##############################################################################\n");
}
	
