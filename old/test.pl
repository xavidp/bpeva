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
	print_doc("This program accepts 2 arguments passed to the program call:
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