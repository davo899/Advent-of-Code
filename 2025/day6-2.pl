#!/usr/bin/perl
use strict;
use warnings;

use Path::Tiny;
use autodie; # die if problem reading or writing a file
use List::Util 'reduce';

my $dir = path(".");

my $file = $dir->child("day6.txt");

my $content = $file->slurp_utf8();

my $file_handle = $file->openr_utf8();

my @lines;
while( my $line = $file_handle->getline() ) {
    $line =~ s/\s+$//;
    push @lines, $line;
}

my $total = 0;
my @ns;
for (my $col = length($lines[0]) - 1; $col >= 0; $col--) {
    my $n = "";
    for (my $row = 0; $row < 4; $row++) {
        my $c = substr $lines[$row], $col, 1;
        $n = "$n$c";
    }
    if ($n eq "    ") {
        next;
    }
    push @ns, $n;

    my $op = substr $lines[4], $col, 1;
    if ($op eq "+") {
        $total += reduce { $a + $b } 0, @ns;
        @ns = ();
    } elsif ($op eq "*") {
        $total += reduce { $a * $b } 1, @ns;
        @ns = ();
    }
}
print $total;
