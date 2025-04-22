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

# First read all options that should be handled.

my %options_type  = ();
my %options_index = ();

my %type_flags = (
    'd' => 0x01,        # decimal
    'p' => 0x01 | 0x08, # decimal/ip
    'x' => 0x02,        # hexadecimal
    'b' => 0x03,        # binary
    's' => 0x04,        # string
    );

my %char_codes = ();

for (my $char = 0; $char < 26; $char++) {
    $char_codes{chr(ord('a') + $char)} = $char + 1;
}
for (my $char = 0; $char < 26; $char++) {
    $char_codes{chr(ord('A') + $char)} = $char + 1; # case insensitive
}
for (my $char = 0; $char < 10; $char++) {
    $char_codes{chr(ord('0') + $char)} = 32 + $char;
}
$char_codes{'_'} = 31;

# print keys %char_codes;

while (my $line = <>)
{
    $line =~ s/#.*//; # Remove comments.
    chomp $line;
    $line =~ s/\s*$//; # Remove trailing whitespace.

    if ($line =~ /^([A-Za-z_][A-Za-z0-9_]*)\s+([dxbsp])\s+([0-9]+)$/)
    {
	my $option = lc($1);
	my $type   = $2;
	my $index  = $3;

	if (!defined($type_flags{$type}))
	{
	    die "Internal error: unhandled option type '$type'.";
	}

	if (defined($options_type{$option}))
	{
	    die "Option '$option' already defined.";
	}

	if ($index >= (1 << 25))
	{
	    die "Option '$option' index ($index) too large.";
	}

	$options_type{$option}  = $type;
	$options_index{$option} = $index;
    }
    elsif ($line =~ /^$/)
    {
	;
    }
    else
    {
	die "Unhandled option specification: $line";
    }
}

my $tree_top_ref = {};

sub insert_tree($$);
sub insert_branch($$);
sub offsets_tree($$);
sub generate_tree($$);
sub print_header();
sub print_value_index();
sub print_array_header();
sub print_footer();

foreach my $option (sort keys %options_type)
{
    # print "$option\n";

    my $node_ref = $tree_top_ref;

    for (my $i = 0; $i < length($option); $i++)
    {
	$node_ref = insert_tree($node_ref, substr($option, $i, 1));
    }

    insert_branch($node_ref, $option);
}

my $max_jump_value = 0;
my $table_offset = 0;

print_header();
print_value_index();
print_array_header();
offsets_tree($tree_top_ref, 0);
generate_tree($tree_top_ref, 0);
print_footer();

if ($max_jump_value > 0x1ff)
{
    die "Maximum jump value out of range: $max_jump_value\n";
}

sub insert_tree($$)
{
    my $node_ref = shift;
    my $char = shift;

    if (defined ($node_ref->{$char}))
    {
	# print "$char -\n";
	return $node_ref->{$char};
    }

    my $new_node_ref = {};

    $node_ref->{$char} = $new_node_ref;

    # print "$char\n";

    return $new_node_ref;
}

sub insert_branch($$)
{
    my $node_ref = shift;
    my $option = shift;

    if (defined ($node_ref->{"OPTION"}))
    {
	die "Option '$option' already inserted in tree.";
    }

    $node_ref->{"OPTION"} = $option;
}

sub offsets_tree($$)
{
    my $node_ref = shift;
    my $depth = shift;

    $node_ref->{"OFFSET"} = $table_offset;

    my $first_offset = $table_offset;

    # Any options (leafs) end up first.

    if (defined(my $option = $node_ref->{"OPTION"}))
    {
	# print sprintf ("[%3d] Option: %*s $option\n",
	# 	       $table_offset, $depth, "");

	$table_offset++; # Option slot.
	$table_offset++; # High index slot.
    }

    # The sub-branches (jumps) come next.

    my $last_branch = ();

    foreach my $branch (sort keys %$node_ref)
    {
	# Single-letter keys are subtrees.
	if (length($branch) == 1)
	{
	    # print sprintf ("[%3d] Branch: %*s $branch\n",
	    #		   $table_offset, $depth, "");

	    $table_offset++; # Character slot.
	    $last_branch = $branch;
	}
    }

    $node_ref->{"LAST_BRANCH"} = $last_branch;

    if ($first_offset == $table_offset)
    {
	# There must be at least one option or a branch.  Otherwise we
	# end up empty, and actually have same location As whatever
	# follows.  No good.  This branch should not have been
	# generated.
	die "Bad tree-branch - no option or branch.";
    }

    # Followed by the actual sub-trees.

    foreach my $branch (sort keys %$node_ref)
    {
	if (length($branch) == 1)
	{
	    offsets_tree($node_ref->{$branch}, $depth+1);
	}
    }
}

sub generate_tree($$)
{
    my $node_ref = shift;
    my $depth = shift;

    my $offset = $node_ref->{"OFFSET"};

    my $last_branch = $node_ref->{"LAST_BRANCH"};

    if (defined(my $option = $node_ref->{"OPTION"}))
    {
	my $index = $options_index{$option};
	my $flags = $type_flags{$options_type{$option}};

	my $is_last_branch = (!defined($last_branch)) ? 1 : 0;

	my $index_lo = ($index     ) & 0x000001ff;
	my $index_hi = ($index >> 9) & 0x0000ffff;

	# Should ahve been caught by check at start.
	if ($index - $index_lo - ($index_hi << 9) != 0)
	{ die "Internal index bug ($index, $index_lo, $index_hi)."; }

	my $entry =
	    ($is_last_branch << 15) | ($index_lo << 6) | 0x0030 | $flags;

	print sprintf ("    i2slv16(16#%04x#), -- [x%02x] 0x%04x ".
		       "(%3d %2d)  Option:%*s $option\n",
		       $entry, $offset, $entry,
		       $index, $flags,
		       $depth < 10 ? $depth : 10, "");

	$offset++;

	$entry = $index_hi;

	print sprintf ("    i2slv16(16#%04x#), -- [x%02x] 0x%04x\n",
		       $entry, $offset, $entry);

	$offset++;
    }

    foreach my $branch (sort keys %$node_ref)
    {
        if (length($branch) == 1)
        {
	    my $jumpto_ref = $node_ref->{$branch};
	    my $jumpto = $jumpto_ref->{"OFFSET"};

	    my $char_code = $char_codes{$branch};

	    my $is_last_branch = ($branch eq $last_branch) ? 1 : 0;

	    my $jump_value = ($jumpto - 1);

	    if ($jump_value > $max_jump_value) {
		$max_jump_value = $jump_value;
	    }

	    my $entry =
		($is_last_branch << 15) | (($jumpto - 1) << 6) | $char_code;

	    print sprintf ("    i2slv16(16#%04x#), -- [x%02x] 0x%04x ".
			   "(x%02x %2d) Branch: %*s $branch\n",
                           $entry, $offset, $entry,
			   $jumpto, $char_code,
			   $depth < 10 ? $depth : 10, "");

	    $offset++;
	}
    }

    foreach my $branch (sort keys %$node_ref)
    {
	if (length($branch) == 1)
	{
	    generate_tree($node_ref->{$branch}, $depth+1);
	}
    }
}

sub print_value_index()
{
    foreach my $option (sort { $options_index{$a} <=>
				   $options_index{$b} } keys %options_index)
    {
	my $vhdloption = $option;

	# VHDL identifiers cannot have two _ in a row.
	$vhdloption =~ s/^_/x_/;
	$vhdloption =~ s/__/_x_/g;

	print "  constant tcfg_index_$vhdloption : integer := ".
	    "$options_index{$option};\n";
    }
}

sub print_header()
{
    print <<END_MESSAGE;
--
-- This file is automatically generated.  Editing is useless!
--

library ieee;
use ieee.std_logic_1164.all;
use work.fnet_util_pkg.all;

package text_config_data is

  constant text_config_data_size : integer := 2**8;

  type text_config_array_type is array(0 to text_config_data_size-1) of
    std_logic_vector(15 downto 0);

END_MESSAGE
}

sub print_array_header()
{
    print <<END_MESSAGE;

constant text_config_array : text_config_array_type := (
END_MESSAGE
}

sub print_footer()
{
    print <<END_MESSAGE;
    --
    others => (others => '0')
    );

end text_config_data;
END_MESSAGE
}
