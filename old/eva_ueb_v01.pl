#!/usr/bin/perl -w
#
# SCRIPT: eva_ueb.pl
# Input: Directories of input and output
# Output: List of Variants
# Autor: Xavier de Pedro, Alex Sanchez (2011)
#        xavier.depedro@vhir.org
###################################################


##########################
### Main Program
##########################

# Activar la declaracion estricta de variables
use strict;
use Term::ANSIColor;

my $program_ueb = "eva_ueb_v01.pl";
my $step_n = 0; # Step number for the loop inside each file to process
my $file_n = 0; # File number for the loop of file to process
my $step_tmp = $step_n; # dummy counter for the number of step when inside the files loop
my $command00;
my $now = nownice(time);


## Header shown at program execution
print_doc("##############################################################################");
print_doc("### $program_ueb: Pipeline for 'Exome Variant Analysis' (EVA) at the UEB ###");
print_doc("###                                                                        ###");
print_doc("### (c) 2011 UEB - GPL licensed - http://ueb.ir.vhebron.net/tools          ###");
print_doc("##############################################################################\n");

## Step 1. Fetch the directory name
$step_n = 1;
my ($directory_in,$directory_out,$n_arguments);

$n_arguments = scalar(@ARGV);
if ($n_arguments != 1 && $n_arguments != 2) {
	print_doc("This program $program_ueb accepts 2 arguments passed to the program call:
  1) directory with source .fastq files (required)
  2) directory to save output files     [optional]\n
  Example: perl $program_ueb ./dir_in ./dir_out\n");
	print_error("Wrong number of parameters: at least source directory is needed");
}
($directory_in,$directory_out) = @ARGV;
print_ok("$now - Step $step_n. Reading parameters");

## Step 2. Accesing directory
$step_n = $step_n+1;
my ($file,$file_in,$file_out);
my ($name,$filter,$command,$options);

opendir(DIR,$directory_in);
#opendir(DIR,$directory_in) or die "Cannot open $directory_in: !";
#opendir my $dh, $dir_to_process or die "Cannot open $dir_to_process: $!";
print_ok("$now - Step $step_n. Accesing directory: $directory_in");

while ($file = readdir(DIR))
{
	## Control sobre los directories . y ..
	next if ($file =~ /^\./);
	$file_n = $file_n + 1;
	$step_n = $step_n+1;
	$step_tmp = 0;
	print_doc("$now - Step $step_n. Processing file #$file_n ($file) ...");

	## Step 3. Quality Control and Preprocessing (using fastqc)
	# See http://www.bioinformatics.bbsrc.ac.uk/projects/fastqc/INSTALL.txt
	$step_tmp = $step_tmp + 1;
	($name = $file) =~ s/\.fastq//;
	print_doc("$now -   Step $step_n.$step_tmp Quality Control and Preprocessing: $name ...");
	$file_in = "$directory_in/$file";
##	$file_out = "$directory_out/$name.txt";
#	$options = "--outdir=$directory_out";
#	$command00 = "~/fastqc/fastqc"; # path to fastqc binary; adapt to your case.
	$command00 = "ls"; # next command.
	$options = "$file_in";
	$command = "$command00 $options";
	system($command);
	print_ok("$now -   Step $step_n.$step_tmp Quality Control and Preprocessing: $name");

	## Next Step. Map against reference genome: Index the reference genome (if needed)
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Map against reference genome: Index the reference genome (if needed): $name ...");
#	$file_in = "$directory_in/$name";
#	$file_out = "$directory_out/$name.txt";
	$command00 = "bwa index"; # next command.
	$options = " -a bwtsw hg19.fa";
	$command = "$command00 $options";
	system($command);
	print_ok("$now -   Step $step_n.$step_tmp Map against reference genome: Index the reference genome (if needed): $name");


	## Next Step. Map against reference genome: do the mapping
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Map against reference genome: do the mapping: $name ...");
	$file_in = "$directory_in/$name";
	$file_out = "$directory_out/$name.sam";
	$command00 = "bwa bwasw"; # next command.
	$options = " /home/xavi/Data/Data_Genomes/hg19/hg19.fa $file_in > $file_out";
	$command = "$command00 $options";
	system($command);
	print_ok("$now -");


	## Next Step. Convert sam to bam and sort it
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Convert sam to bam and sort it: $name ...");
	$file_in = $file_out;
	$file_out = "$directory_out/$name.sam.sorted";
	$command00 = "samtools"; # next command.
	$options = " view -bS $file_in | samtools sort - $file_out"; # Attention: it seems that the real file generated is a .bam file
	$command = "$command00 $options";
	system($command);
	print_ok("$now -");


	## Next Step. Remove possible PCR duplicates
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Remove possible PCR duplicates: $name ...");
	$file_in = "$directory_out/$name.sam.sorted.bam";
	$file_out = "$directory_out/$name.sam.sorted.noDup.bam";
	$command00 = "samtools"; # next command.
	$options = " rmdup -s $file_in $file_out";
	$command = "$command00 $options";
	system($command);
	print_ok("$now -");


	## Next Step. Index the bam file
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Index the bam file: $name ...");
	$file_in = "$directory_out/$name.sam.sorted.noDup.bam";
#	$file_out = "";
	$command00 = "samtools"; # next command.
	$options = " index $file_in";
	$command = "$command00 $options";
	system($command);
	print_ok("$now -");


	## Next Step. Get some stats using the samtools [optional]
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Get some stats using the samtools [optional]: $name ...");
	$file_in = "$directory_out/$name.sam.sorted.noDup.bam";
	$file_out = "";
	$command00 = "samtools"; # next command.
	$options = " flagstat $file_in";
	$command = "$command00 $options";
	system($command);
	print_ok("$now -");


	## Next Step. Variant calling
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Variant calling: $name ...");
	$file_in = "$directory_out/$name.sam.sorted.noDup.bam";
	$file_out = "$directory_out/$name.sam.sorted.noDup.bam.samtools.var.raw.vcf";
	$command00 = "samtools"; # next command.
	$options = " mpileup -uf /home/xavi/Data/Data_Genomes/hg19/hg19.fa $file_in | bcftools view -vcg - > $file_out";
	$command = "$command00 $options";
	system($command);
	print_ok("$now -");


	## Next Step. Variant Filtering
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Variant Filtering: $name ...");
	$file_in = $file_out; # get the previous output file as input for this step
	$file_out = "$directory_out/$name.sam.sorted.noDup.bam.samtools.var.filtered.vcf";
	$command00 = "/usr/share/samtools/vcfutils.pl"; # next command.
	$options = " varFilter -Q 10 -d 15 -a 5 $file_in > $file_out";
	$command = "$command00 $options";
	system($command);
	print_ok("$now -");


	## Next Step. Variant Annotation
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Variant Annotation: $name ...");
	$file_in = $file_out; # get the previous output file as input for this step
	$file_out = "$directory_out/$name.sam.sorted.noDup.bam.samtools.var.filtered.vcf.annovar";
	$command00 = "/home/ueb/annovar/convert2annovar.pl"; # next command.
	$options = " $file_in -format vcf4 > $file_out";
	$command = "$command00 $options";
	system($command);
	print_ok("$now -");


	## Next Step. Visualization of Variants
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Visualization of Variants: $name ...");
	$file_in = "";
	$file_out = "";
	$command00 = "echo '  ...skipped...'"; # next command.
	$options = "";
	$command = "$command00 $options";
	system($command);
	print_ok("$now -");


	## Step N. Template for the next step. Clone this section before adding a new section
	$step_tmp = $step_tmp + 1;
	print_doc("$now -   Step $step_n.$step_tmp Next step: $name ...");
	$file_in = "";
	$file_out = "";
	$command00 = "echo '  ...skipped...'"; # next command.
	$options = "";
	$command = "$command00 $options";
	system($command);
	print_ok("$now -");
}

# Cerrar el directory
closedir(DIR);

$step_n = $step_n+1;
print_ok("$now - Step $step_n. End of EVA UEB pipeline");
exit(0);

##########################
### FUNCIONES
##########################
sub print_doc
{
	my $mess;
	$mess = $_[0];
	$now = nownice(time);

	print STDERR color("bold cyan"),
		"$mess\n";
	print STDERR color(" reset ");
}

sub print_ok
{
	my $mess;
	$mess = $_[0];
	$now = nownice(time);

	print STDERR color("bold green"),
		"$mess\t\t[OK]\n\n";
	print STDERR color(" reset ");

}

sub print_error
{
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
	my $mess;
	$mess = $_[0];
	my( $year, $month, $day, $hour, $minute ) = (localtime)[5,4,3,2,1];
	sprintf '%2d-%02d-%02d %02d:%02d', $year, $month, $day, $hour, $minute;
}