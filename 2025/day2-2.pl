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
        for (my $k = 1; $k <= length($str) / 2; $k++) {
            if (length($str) % $k != 0) {
                next;
            }

            my $chunk = substr($str, 0, $k);
            my $failed = 0;
            for (my $n = $k; $n + $k <= length($str); $n += $k) {
                if ($chunk ne substr($str, $n, $k)) {
                    $failed = 1;
                    last;
                }
            }

            if (!$failed) {
                $count += $j;
                last;
            }
        }
    }
}
print $count;
