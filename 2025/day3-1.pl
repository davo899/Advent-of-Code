#!/usr/bin/perl
use strict;
use warnings;

use Path::Tiny;
use autodie; # die if problem reading or writing a file

my $dir = path(".");

my $file = $dir->child("day3.txt");

my $content = $file->slurp_utf8();

my $file_handle = $file->openr_utf8();

my $joltage = 0;
while( my $line = $file_handle->getline() ) {

    $line =~ s/\s+$//;

    my $first = 0;
    my $second = 0;

    for (my $i = 0; $i < length($line) - 1; $i++) {
        my $n = substr($line, $i, 1);
        if ($n > $first) {
            $first = $n;
            $second = 0;
            next;
        }
        if ($n > $second) {
            $second = $n;
        }
    }

    my $last = substr($line, length($line) - 1, 1);
    if ($last > $second) {
        $second = $last;
    }
    $joltage += ($first * 10) + $second;
}
print $joltage;