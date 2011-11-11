#!/usr/bin/perl -w
#
# SCRIPT: eva_ueb_v00.pl
# Input: Directories of input and output
# Output: List of Variants
# Autor: Xavier de Pedro (2011)
###################################################

use strict;
use Term::ANSIColor;

##########################
### PROGRAMA PRINCIPAL
##########################

print_ok("Inicio correcto");
if (scalar(@ARGV) == 0)
{	print_ok("Parametros");	}
else
{	print_error("Parametros");}
print_ok("Final correcto");

# Abrir el directorio de secuencias
opendir(GENOMA,"genoma");

while ($cromosoma = readdir(GENOMA))
{
    # Abrir el fichero actual
    open(CROMOSOMA,$cromosoma);

    # Proceso 1

    @argumentos = preparar_argumentos($cromosoma);
    $resultado = preparar_salida($cromosoma);
    $comando = "programa1 @argumentos > $resultado";
    system($comando);
    #...

    # Proceso N
    @argumentos = preparar_argumentos($cromosoma);
    $resultado = preparar_salida($cromosoma);
    $comando = "programaN @argumentos > $resultado";
    system($comando);

    # Guardar los resultados finales
    guardar(@resultados,$resultado);

    # Cerrar el fichero actual
    close(CROMOSOMA);
}

# Cerrar el directorio
closedir(GENOMA);

# Generar informe final
volcar_resultados(@resultados);


###################################################
# Activar la declaracion estricta de variables
use strict;
use Term::ANSIColor;

my
my
my
my
$PREDICTOR
$CATALOG
$PLOTTER
$COLORES
=
=
=
=
"matscan";
"matrices.pfm";
"gff2ps";
"gff2ps.param";
17
18
19
## Paso 1. Adquirir el nombre del directorio
my ($directorio_in,$directorio_out,$n_argumentos);
20
21
22
23
24
25
$n_argumentos = scalar(@ARGV);
($n_argumentos == 2) or
print_error("Numero de argumentos incorrectos");
($directorio_in,$directorio_out) = @ARGV;
print_ok("Paso 1. Lectura de parametros");
26
27
28
29
## Paso 2. Abrir el directorio de secuencias
my ($fichero,$fichero_in,$fichero_out);
my ($nombre,$filtro,$comando,$opciones);
30
31
32
opendir(DIR,$directorio_in);
print_ok("Paso 2. Acceso al directorio: $directorio_in");
33
34
35
36
37
while ($fichero = readdir(DIR))
{
## Control sobre los directorios . y ..
next if ($fichero =~ /^\./);
38
## Paso 3. Prediccion de sitios de union a factores
($nombre = $fichero) =~ s/\.fa//;
$fichero_in = "$directorio_in/$fichero";
$fichero_out = "$directorio_out/$nombre.gff";
$filtro = " | grep MatScan | sort -k 4n ";
$comando = "$PREDICTOR $fichero_in
$CATALOG $filtro > $fichero_out";
system($comando);
print_ok("Paso 3. Prediccion de sitios: $nombre");
39
40
41
42
43
44
45
46
47
48
## Paso 4. Representacion grafica de los resultados
$fichero_in = $fichero_out;
$fichero_out = "$directorio_out/$nombre.ps";
$opciones = " -av -C $COLORES";
$comando = "$PLOTTER $opciones $fichero_in > $fichero_out";
system($comando);
# Conversion a formato PNG
$fichero_in = $fichero_out;
$fichero_out = "$directorio_out/$nombre.png";
$comando = " convert -rotate 90 $fichero_in $fichero_out ";
system($comando);
print_ok("Paso 4. Representacion grafica: $nombre");
49
50
51
52
53
54
55
56
57
58
59
60
61
}
62
63
64
# Cerrar el directorio
closedir(DIR);
65
66
67
print_ok("Paso 5. Final del proceso");
exit(0);

##########################
### FUNCIONES
##########################
sub print_ok
{
	my $mess;
	$mess = $_[0];

	print STDERR color("bold green"),
		"$mess\t\t[OK]\n\n";
	print STDERR color(" reset ");
}

sub print_error
{
	my $mess;
	$mess = $_[0];

	print STDERR color("bold red"),
		"$mess\t\t[ERROR]\n\n";
	print STDERR color(" reset ");
	die();
}

