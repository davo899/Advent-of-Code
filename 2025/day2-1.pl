#!/usr/bin/perl
use strict;
use warnings;

use Path::Tiny;
use autodie; # die if problem reading or writing a file

my $dir = path(".");

my $file = $dir->child("day2.txt");

my $content = $file->slurp_utf8();

my $file_handle = $file->openr_utf8();

my $line = $file_handle->getline();
my @ranges = split(/,/, $line);

my $count = 0;
for (my $i = 0; $i < @ranges; $i++) {
    my @ends = split(/-/, $ranges[$i]);
    my $start = $ends[0];
    my $end = $ends[1];
    for (my $j = $start; $j <= $end; $j++) {
        my $str = "$j";
        my $length = length($str);
        if ($length % 2 != 0) {
            next;
        }
        my $head = substr($str, 0, $length / 2);
        my $tail = substr($str, $length / 2, $length / 2);
        if ($head eq $tail) {
            $count += $j;
        }
    }
}
print $count;
