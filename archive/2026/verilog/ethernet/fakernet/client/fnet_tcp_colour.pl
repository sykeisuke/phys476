#!/usr/bin/perl

use strict;
use warnings;

use Term::ANSIColor qw(:constants);

my $hex = "[0-9a-zA-Z]";

while (my $line = <>)
{
    chomp $line;

    if ($line =~ s/^(UART-(\d+:))//)
    {
	print BLUE, $1;

	if ($line =~ /^\s*UBX/)
	{
	    print RED, BOLD, $line, RESET;
	}
	else
	{	
	    print BOLD, $line, RESET;
	}
    }
    elsif ($line =~ s/^((NEXT|PPS)-(\d+:))//)
    {
	print MAGENTA, $1;
	print BOLD, $line, RESET;
    }
    elsif ($line =~ s/^(TRACK-(\d+:))//)
    {
	print GREEN, $1;
	print BOLD, $line, RESET;
    }
    elsif ($line =~ s/^(Sampler-(\d+:))(\s+)($hex+)(\s+)($hex+)(\s+)($hex+)(\s+\()/$9/)
    {
	print CYAN, $1;         # Sampler
	print $3;
	print UNDERLINE, $4;    # 1st data word (fine time)
	print $5;
	print $6, RESET, CYAN;  # 2nd data word (coarse time)
	print $7;
	print BOLD, $8, RESET;  # calculated timestamp
	print $line;            # remaining line
    }
    elsif ($line =~ s/^(XADC:)//)
    {
	print RED, $1;
	print BOLD, $line, RESET;
    }
    else
    {
	print $line;
    }
	 
    print "\n";
}
