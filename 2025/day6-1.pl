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
    my @bits = split / /, $line;
    @bits = grep { $_ ne "" } @bits;
    push @lines, \@bits;
}

my $total = 0;
for (my $i = 0; $i < 1000; $i++) {
    my @ns;
    for (my $j = 0; $j < 4; $j++) {
        push @ns, $lines[$j][$i];
    }

    my $n;
    my $op = $lines[4][$i];
    if ($op eq "+") {
        $n = reduce { $a + $b } 0, @ns;
    } elsif ($op eq "*") {
        $n = reduce { $a * $b } 1, @ns;
    }
    $total += $n;
}
print $total;
