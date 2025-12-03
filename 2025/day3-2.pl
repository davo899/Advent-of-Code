#!/usr/bin/perl
use strict;
use warnings;

use Path::Tiny;
use autodie; # die if problem reading or writing a file

my $dir = path(".");

my $file = $dir->child("day3.txt");

my $content = $file->slurp_utf8();

my $file_handle = $file->openr_utf8();

sub min {
    my ($x, $y) = @_;
    return $x < $y ? $x : $y;
}

my $k = 12;

my $joltage = 0;
while( my $line = $file_handle->getline() ) {

    $line =~ s/\s+$//;
    my @ns = split //, $line;

    my @digits = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

    while (@ns) {
        my $n = shift @ns;
        my $remaining = @ns;
        my $increased = 0;
        for (my $i = min($remaining, $k - 1); $i >= 0; $i--) {
            if ($increased) {
                $digits[$i] = 0;
            } elsif ($n > $digits[$i]) {
                $digits[$i] = $n;
                $increased = 1;
            }
        }
    }

    my $num = 0;
    while (@digits) {
        $num *= 10;
        $num += pop @digits;
    }
    $joltage += $num;
}
print $joltage;