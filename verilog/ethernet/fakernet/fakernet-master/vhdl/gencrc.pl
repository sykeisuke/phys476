#!/usr/bin/perl

# Copyright (C) 2020, Haakan T. Johansson
# All rights reserved.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of the authors nor the
#       names of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

use strict;
use warnings;

# https://www.xilinx.com/support/documentation/application_notes/xapp209.pdf

# 32 26 23 22 16 12 11 10 8 7 5 4 2 1 + 1
# 0x82608EDB

# my $poly = 0x82608EDB;
# my $poly = 0xDB710641;
my $poly = 0x04C11DB7;

my @cval = ();
my @dval = ();

my $replaceagain = 0; # Brought no improvement; more LUTs used.

for (my $i = 0; $i < 32; $i++)
{
    push @cval,(1<<$i);
    push @dval,0;
}

sub crc_round($)
{
    my $d = shift;

    my @ncval = ();
    my @ndval = ();

    my $cinj = $cval[31];
    my $dinj = $dval[31] ^ (1 << $d);

    for (my $i = 1; $i < 32; $i++)
    {
	$ncval[$i] = $cval[$i-1];
	$ndval[$i] = $dval[$i-1];
	if ($poly & (1 << ($i)))
	{
	    $ncval[$i] ^= $cinj;
	    $ndval[$i] ^= $dinj;
	    #print "$i ";
	}
    }
    #print "\n";
    $ncval[0] = $cinj;
    $ndval[0] = $dinj;

    @cval = @ncval;
    @dval = @ndval;

    #for (my $i = 0; $i < 32; $i++)
    #{
    #	print sprintf("%2d: 0x%08x 0x%04x\n",$d,$cval[$i],$dval[$i]);
    #}
}

my @outname = ();
my @output = ();
my @outhash = ();
my @outcount = ();

sub assign_output($)
{
    my $prefix = shift;

    for (my $i = 0; $i < 32; $i++)
    {
	print sprintf("  -- %s %2d 0x%08x 0x%04x\n",
		      $prefix,$i,$cval[$i],$dval[$i]);

	my @list = ();
	my %hash = ();

	for (my $d = 0; $d < 16; $d++)
	{
	    if ($dval[$i] & (1 << $d))
	    {
		my $item = sprintf("d(%2d)",$d);
		push @list,$item;
		$hash{$item} = 1;
	    }
	}
	for (my $c = 0; $c < 32; $c++)
	{
	    if ($cval[$i] & (1 << $c))
	    {
		my $item = sprintf("c(%2d)",$c);
		push @list,$item;
		$hash{$item} = 1;
	    }
	}
	# printf ("\n");
	push @outname, sprintf("%s(%2d)",$prefix,$i);
	push @output, \@list;
	push @outhash, \%hash;
	push @outcount, 1;
    }
}

for (my $d = 0; $d < 8; $d++) # change 16 -> 32 to make 32-bit data version
{
    crc_round($d);
}

assign_output("o8");

for (my $d = 8; $d < 16; $d++) # change 16 -> 32 to make 32-bit data version
{
    crc_round($d);
}

assign_output("o");

my $numout = $#outname+1;

#

sub countset(@)
{
    my @set = @_;

    my $matches = 0;

    for (my $i = 0; $i < ($replaceagain ? $#outname+1 : $numout); $i++)
    {
	my $hashref = $outhash[$i];

	my $cnt = 0;
	foreach my $s (@set)
	{
	    if ($$hashref{$s}) { $cnt++; }
	}
	if ($cnt == $#set+1)
	{
	    $matches++;
	}
    }

    return $matches;
};

my $nsubexpr = 0;

sub replaceset($@)
{
    my $nummatch = shift;
    my @set = @_;

    if ($nummatch <= 1) { return 0; }

    $nsubexpr++;
    my $repl = sprintf("p(%2d)",$nsubexpr);
    print "  -- $repl <= ".join(' xor ',@set)."; -- $nummatch\n";

    my %sethash = ();
    foreach my $s (@set)
    {
	$sethash{$s} = 1;
    }

    push @outname, $repl;
    push @output, \@set;
    push @outhash, \%sethash;
    push @outcount, $nummatch;

    for (my $i = 0; $i < ($replaceagain ? $#outname+1 : $numout); $i++)
    {
	my $hashref = $outhash[$i];

	my $cnt = 0;
	foreach my $s (@set)
	{
	    if ($$hashref{$s}) { $cnt++; }
	}
	if ($cnt == $#set+1)
	{
	    foreach my $s (@set)
	    {
		$$hashref{$s} = 0;
	    }
	    my @newout = ();

	    $$hashref{$repl} = 1;
	    push @newout,$repl;

	    my $outref = $output[$i];
	    foreach my $o (@$outref)
	    {
		if (!$sethash{$o})
		{
		    push @newout,$o;
		}
	    }
	    $output[$i] = \@newout;
	}
    }
    return 1;
}

sub combinations($$$@);

sub combinations($$$@)
{
    my $first = shift;
    my $last = shift;
    my $depth = shift;
    my @set = @_;

    for (my $i = $first; $i < $last; $i++)
    {
	if (!$depth)
	{
	    #my $matches = countset(@set);

	    # print "$i1 $i2 $i3 $i4, $items , $matches\n";

	    #if ($matches > $bestmatch)
	    #{
	#	@bestset = @set;
	#	$bestmatch = $matches;
	    #}
	}
	else
	{
	    my @callset = @set;
	    push @callset,0;
	    combinations($i+1,$last,$depth,@set);
	}
    }
}

sub bestsubexpr6()
{
    my @bestset = ();
    my $bestmatch = 1;

    # Find all sets of four symbols
    for (my $i = 0; $i < $numout; $i++)
    {
	my $out = $output[$i];
	my @list = @$out;

	my $items = $#list+1;

	for (my $i1 = 0; $i1 < $items-5; $i1++) {
	    for (my $i2 = $i1+1; $i2 < $items-4; $i2++) {
		for (my $i3 = $i2+1; $i3 < $items-3; $i3++) {
		  for (my $i4 = $i3+1; $i4 < $items-2; $i4++) {
		    for (my $i5 = $i4+1; $i5 < $items-1; $i5++) {
		      for (my $i6 = $i5+1; $i6 < $items; $i6++) {

			my @set =
			    ($list[$i1], $list[$i2], $list[$i3],
			     $list[$i4], $list[$i5], $list[$i6]);

			my $matches = countset(@set);

			# print "$i1 $i2 $i3 $i4, $items , $matches\n";

			if ($matches > $bestmatch)
			{
			    @bestset = @set;
			    $bestmatch = $matches;
			}
		      }
		    }
		  }
		}
	    }
	}
    }

    return replaceset($bestmatch,@bestset);
}

sub bestsubexpr()
{
    my @bestset = ();
    my $bestmatch = 1;

    # Find all sets of four symbols
    for (my $i = 0; $i < $numout; $i++)
    {
	my $out = $output[$i];
	my @list = @$out;

	my $items = $#list+1;

	for (my $i1 = 0; $i1 < $items-3; $i1++) {
	    for (my $i2 = $i1+1; $i2 < $items-2; $i2++) {
		for (my $i3 = $i2+1; $i3 < $items-1; $i3++) {
		    for (my $i4 = $i3+1; $i4 < $items; $i4++) {

			my @set =
			    ($list[$i1], $list[$i2], $list[$i3], $list[$i4]);

			my $matches = countset(@set);

			# print "$i1 $i2 $i3 $i4, $items , $matches\n";

			if ($matches > $bestmatch)
			{
			    @bestset = @set;
			    $bestmatch = $matches;
			}
		    }
		}
	    }
	}
    }

    return replaceset($bestmatch,@bestset);
}

sub bestsubexpr3()
{
    my @bestset = ();
    my $bestmatch = 1;

    # Find all sets of four symbols
    for (my $i = 0; $i < $numout; $i++)
    {
	my $out = $output[$i];
	my @list = @$out;

	my $items = $#list+1;

	for (my $i1 = 0; $i1 < $items-2; $i1++) {
	    for (my $i2 = $i1+1; $i2 < $items-1; $i2++) {
		#for (my $i3 = $i2+1; $i3 < $items; $i3++) {
		    #for (my $i4 = $i3+1; $i4 < $items; $i4++) {

			my @set =
			    ($list[$i1], $list[$i2]);#, $list[$i3]);

			my $matches = countset(@set);

			# print "$i1 $i2 $i3 $i4, $items , $matches\n";

			if ($matches > $bestmatch)
			{
			    @bestset = @set;
			    $bestmatch = $matches;
			}
		    #}
		#}
	    }
	}
    }

    return replaceset($bestmatch,@bestset);
}

print "\n";

if (1) {
 findsubexpr6:
for ( ; ; )
{
    if (!bestsubexpr6())
    { last findsubexpr6; }
}
}

if (1) {
 findsubexpr:
for ( ; ; )
{
    if (!bestsubexpr())
    { last findsubexpr; }
}

 findsubexpr3:
for ( ; ; )
{
    if (!bestsubexpr3())
    { last findsubexpr3; }
}
}

print "\n";

for (my $i = $numout; $i < $#outname+1; $i++)
{
    my $out = $output[$i];
    my $name = $outname[$i];
    my $count = $outcount[$i];;
    print "  $name <= ".join(' xor ',@$out)."; -- $count\n";
}

print "\n";

for (my $i = 0; $i < $numout; $i++)
{
    my $out = $output[$i];
    my $name = $outname[$i];
    print "  $name <= ".join(' xor ',@$out).";\n";
}
