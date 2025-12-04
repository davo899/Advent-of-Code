#!/usr/bin/perl
use strict;
use warnings;

use Path::Tiny;
use autodie; # die if problem reading or writing a file

my $dir = path(".");

my $file = $dir->child("day4.txt");

my $content = $file->slurp_utf8();

my $file_handle = $file->openr_utf8();

my $count = 0;

my %rolls;

my $row = 0;
while( my $line = $file_handle->getline() ) {
    $line =~ s/\s+$//;
    for (my $i = 0; $i < length($line); $i++) {
        my $c = substr($line, $i, 1);
        if ($c eq "@") {
            $rolls{"$row,$i"} = 1;
        }
    }
    $row++;
}

my @dirs = (
    [1, 0],
    [1, 1],
    [0, 1],
    [-1, 1],
    [-1, 0],
    [-1, -1],
    [0, -1],
    [1, -1]
);

my $removed = 1;
while ($removed) {
    $removed = 0;
    for my $key (keys %rolls) {
        my ($r, $c) = split /,/, $key;
        my $neighbours = 0;
        for my $dir (@dirs) {
            my $x = $r + @$dir[0];
            my $y = $c + @$dir[1];
            if (exists $rolls{"$x,$y"}) {
                $neighbours++;
            }
        }
        if ($neighbours < 4) {
            $count++;
            delete $rolls{$key};
            $removed = 1;
        }
    }
}

print $count;