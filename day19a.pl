#! /usr/bin/env perl

use 5.040;
use warnings;

my @patterns = split /, /, <>;
say 0+ grep /^(@{[join '|', @patterns]})+$/o, <>;
