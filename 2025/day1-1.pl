#!/usr/bin/perl
use strict;
use warnings;

use Path::Tiny;
use autodie; # die if problem reading or writing a file

my $dir = path(".");

my $file = $dir->child("day1.txt");

my $content = $file->slurp_utf8();

my $file_handle = $file->openr_utf8();

my $dial = 50;
my $count = 0;
while( my $line = $file_handle->getline() ) {
    my $way = substr $line, 0, 1;
    my $turns = (substr $line, 1) + 0;
    if ($way eq 'L') {
        $dial -= $turns;
    } elsif ($way eq 'R') {
        $dial += $turns;
    }
    $dial %= 100;
    if ($dial == 0) {
        $count++;
    }
}
print $count;
