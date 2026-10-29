use strict;
use warnings;

print "Please enter the number to factorise:\n";
my $number = <STDIN>;
chomp $number;

# try every divisor from 2 on; a divisor that fits is a prime factor
my @factors;
my $divisor = 2;
while ($number > 1) {
    if ($number % $divisor == 0) {
        push @factors, $divisor;
        $number /= $divisor;
    } else {
        $divisor++;
    }
}
print "Prime factors: @factors\n";
