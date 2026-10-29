use strict;
use warnings;

my @list = map { int(rand(100)) } 1 .. 10;
print "Original list: @list\n";

# swap neighbours in the wrong order until a pass swaps none
my $sorted = 0;
until ($sorted) {
    $sorted = 1;
    for my $i (0 .. $#list - 1) {
        if ($list[$i] > $list[$i + 1]) {
            @list[$i, $i + 1] = @list[$i + 1, $i];
            $sorted = 0;
        }
    }
}
print "Sorted list: @list\n";
