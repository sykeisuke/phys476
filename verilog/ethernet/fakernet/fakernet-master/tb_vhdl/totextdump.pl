#!/usr/bin/perl

my @inpacket = ();
my @outpacket = ();

sub dumppacket($@);

while (my $line = <>)
{
    if ($line =~ /NEW-PACKET/ ||
	$line =~ /FIN-PACKET/)
    {
	dumppacket("in",@inpacket);
	@inpacket = ();
    }

    if ($line =~
	/INPUT: 0x([0-9a-fA-F][0-9a-fA-F])([0-9a-fA-F][0-9a-fA-F]) = /)
    {
	my $b1 = $1;
	my $b2 = $2;

	push @inpacket, $b1;
	push @inpacket, $b2;
    }

    if ($line =~ /END-PACKET/)
    {
	dumppacket("out",@outpacket);
	@outpacket = ();
    }

    if ($line =~ /NEW-DYN-IP/)
    {
	print sprintf ("# $line\n\n");
    }


    if ($line =~
	/OUTPUT: 0x([0-9a-fA-F][0-9a-fA-F])([0-9a-fA-F][0-9a-fA-F]) = /)
    {
	my $b1 = $1;
	my $b2 = $2;

	push @outpacket, $b1;
	push @outpacket, $b2;
    }

}

dumppacket("in",@inpacket);
dumppacket("out",@outpacket);

sub dumppacket($@)
{
    my $descr  = shift;
    my @packet = @_;

    if ($#packet >= 0)
    {
	printf ("\n$descr:");

	for ($off = 0; $off <= $#packet; $off++)
	{
	    if ($off % 16 == 0)
	    {
		print sprintf ("\n%06x ", $off);
	    }
	    print sprintf (" %s", $packet[$off]);
	}
    }
    printf ("\n");
    printf ("\n");
}
