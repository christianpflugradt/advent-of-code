open FH, '2018-03.txt';

while (<FH>) {
    /.+?(\d+).+?(\d+).+?(\d+).+?(\d+).+?(\d+)/;
    $z{$1}++;
    for $x ($2..$2+$4-1) {
        for $y ($3..$3+$5-1) {
            push @{ $x{"$x $y"} }, $1;
        }
    }
}
delete $z{$_} for map @$_ > 1 ? @$_ : (), values %x;

print +(keys %z)[0], "\n";
