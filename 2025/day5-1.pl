#!/usr/bin/perl
use strict;
use warnings;

use Path::Tiny;
use autodie; # die if problem reading or writing a file

my $dir = path(".");

my $file = $dir->child("day5.txt");

my $content = $file->slurp_utf8();

my $file_handle = $file->openr_utf8();

my @ranges;

while( my $line = $file_handle->getline() ) {
    $line =~ s/\s+$//;
    if ($line eq "") {
        last;
    }
    my ($start, $end) = split /-/, $line;
    push @ranges, [$start, $end];
}

my $fresh = 0;
while( my $line = $file_handle->getline() ) {
    $line =~ s/\s+$//;
    foreach my $range (@ranges) {
        if ($range->[0] <= $line && $line <= $range->[1]) {
            $fresh++;
            last;
        }
    }
}

print $fresh;