#! /usr/bin/env perl

use 5.040;

my %swaps;
sub swap {
  @swaps{my ($a,$b) = @_} = 1;
  s/$a$/tmp/;
  s/$b$/$a/;
  s/tmp$/$b/;
}

say "digraph {";

my $opc = 0;
while (<>) {
  chomp;
  next unless length;
  next if /:/;

  swap 'z07', 'rts';
  swap 'z12', 'jpj';
  swap 'z26', 'kgj';
  swap 'vvw', 'chv';

  my ($v1,$op,$v2,$dst) = /^(...) (\w+) (...) -> (...)$/;
  my $label =
    $op eq 'AND' ? '&'
    : $op eq 'OR' ? '≥1'
    : $op eq 'XOR' ? '=1'
    : die "Unknown op $op";
  say "op$opc [ label=\"$label\" shape=\"square\"]";
  say "$v1 -> op$opc";
  say "$v2 -> op$opc";
  say "op$opc -> $dst";
  $opc++;
}

say "}";

say STDERR join ',', sort keys %swaps;
