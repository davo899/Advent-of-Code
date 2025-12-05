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

@ranges = sort { $a->[0] <=> $b->[0] } @ranges;

my $count = 0;
my $head = 0;

for (my $i = 0; $i < @ranges; $i++) {
    my $start = $ranges[$i][0];
    my $end = $ranges[$i][1];
    if ($head > $end) {
        next;
    }
    if ($head < $start) {
        $head = $end + 1;
        $count += $head - $start;
        next; 
    }
    $count += $end - $head + 1;
    $head = $end + 1;
}
print $count;
