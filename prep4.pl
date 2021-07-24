#!/usr/local/bin/perl
use strict;
use warnings;
use Data::Dumper;

# I actually removed the newlines by hand, because somehow it did not work the last time
# so this script takes in a file that has one passport record per line
# and arranges them

my %i;
my @lines;

my $file = "./day4input.txt";
 open (FH, "+< $file");
while (<FH>)
{
	push @lines, $_;
}
close FH;


foreach my $data (@lines) {
	chomp($data);
	%i = split(/[: ]/, $data);

	print defined $i{byr}  ? "byr:" . $i{byr} : "byr:    ";
	print defined $i{cid}  ? " cid:" . sprintf("%03d", $i{cid}) : " cid:   ";
	if(defined $i{ecl}) {
	if (length $i{ecl} > 3) { print " ecl:" . substr $i{ecl},0,3; }
	else { print " ecl:" . $i{ecl}; }
	} else { print " ecl:   ";  }
	print defined $i{eyr}  ? " eyr:" . sprintf("%04d", $i{eyr}) : " eyr:    ";
	if(defined $i{hcl}) {
	if(length $i{hcl} < 7) { print " hcl:" . $i{hcl} . ' ' x (7 - length $i{hcl})}
	else { print " hcl:" . $i{hcl}; }
	}
	else { print " hcl:       "; }
	if(defined $i{hgt}) {
	if(length $i{hgt} < 5) { print " hgt:" . $i{hgt} . ' ' x (5 - length $i{hgt})}
	else { print " hgt:" . $i{hgt}; }
	}
	else { print " hgt:     "; }
	print defined $i{iyr}  ? " iyr:" . sprintf("%04d", $i{iyr}) : " iyr:    ";
	print defined $i{pid}  ? " pid:" . $i{pid} . "\n" : " pid:         \n";

}
