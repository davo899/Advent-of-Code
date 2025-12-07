#!/usr/bin/perl
use strict;
use warnings;

use Path::Tiny;
use autodie; # die if problem reading or writing a file
use List::Util 'reduce';

my $dir = path(".");

my $file = $dir->child("day7.txt");

my $content = $file->slurp_utf8();

my $file_handle = $file->openr_utf8();

my @lines;
while( my $line = $file_handle->getline() ) {
    $line =~ s/\s+$//;
    push @lines, $line;
}

my %beams;
my $splits = 0;
foreach my $line (@lines) {
    for (my $i = 0; $i < length $line; $i++) {
        my $c = substr $line, $i, 1;
        if ($c eq "S") {
            $beams{$i} = 1;
        } elsif ($c eq "^" && exists $beams{$i}) {
            $beams{$i + 1} = 1;
            $beams{$i - 1} = 1;
            delete $beams{$i};
            $splits++;
        }
    }
}
print $splits;