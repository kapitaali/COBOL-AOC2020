#!/usr/bin/perl

use strict;
use warnings;

# preprocess day2 input for COBOL
# just perl prep2.pl > day2.txt to get preprocessed input

sub main {
	open(fh, './day2input.txt');
	my @passut;
	my $i;
	while($i = readline(fh)) {
		# lines are of the form 5-8 m: mmghmcwm
        my @a = split(/:/, $i); # @a[0] limits+char, @a[1] passwd
        my @b = split(/\s/, $a[0]); # @b[0] limits, @b[1] char
        my ($eka, $toka) = split(/-/, $b[0]);
        my @passu = split(//, $a[1]);
        my $paste;
        $paste = sprintf("%02d", $eka) . '-' . sprintf("%02d", $toka) . ' ' . $b[1] . ': ' . $a[1];
        print $paste;
	}		
	close(fh);

}


main()


