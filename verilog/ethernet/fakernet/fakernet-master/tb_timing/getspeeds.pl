#!/usr/bin/perl

use strict;
use warnings;

sub dumpspeeds();

my $target = ();
my $minperiod = ();
my $luts = ();
my $ffs = ();
my $srpfile = ();

while (my $line = <>) {

    if ($line =~ /# TARGET (.*?) /) {
	dumpspeeds();
	$target = $1;
	$minperiod = ();
	$luts = ();
	$ffs = ();
	$srpfile = ();
    }

    # Do stuff with each $line.
    if ($line =~ /Minimum period:\s*([\d\.]*)\s*ns/) {
	$minperiod = $1;
    }
    if ($line =~ /Number of.*LUTs:\s*([\d]*)\s*out/) {
	$luts = $1;
    }
    if ($line =~ /Number of.*Flip Flops:\s*([\d]*)\s*out/) {
	$ffs = $1;
    }
    if ($line =~ /Number of.*Registers:\s*([\d]*)\s*out/) {
	$ffs = $1;
    }
    if ($line =~ /\.srp:\s(.*)/) {
	$srpfile = $1;
    }
    if ($line =~ /\.sta.rpt:\s(.*)/) {
	$srpfile = $1;
    }

    if ($line =~ /;\s*([\d\.]*)\s*MHz\s*;.*;\s*clk\s*;/) {
	my $period = 1000.0 / $1;
	#print " --- $1 -> $period";
	if (!defined $minperiod ||
	    $period > $minperiod) {
	    $minperiod = $period;
	}
    }
    if ($line =~ /Total logic elements\s*:\s*([\d,]*)\s*\//) {
	$luts = $1;
    }
    if ($line =~ /Logic utilization.*:\s*([\d,]*)\s*\//) {
	$luts = $1;
    }
    if ($line =~ /Total registers.*:\s*([\d,]*)\s*/) {
	$ffs = $1;
    }
}

dumpspeeds();

sub dumpspeeds()
{
    if (!defined $target) {
	return;
    }

    $luts =~ s/,//g;
    $ffs =~ s/,//g;

    if (!defined $srpfile) {
	$srpfile = "-";
    }

    print sprintf("%-20s  %7.3f  %7.3f  %4d %4d  %s\n",
		  $target,$minperiod,1000./$minperiod,$luts,$ffs,$srpfile);
}
