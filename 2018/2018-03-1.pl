open FH, '2018-03.txt';
while (<FH>) {
    /.+?\d+.+?(\d+).+?(\d+).+?(\d+).+?(\d+)/;
    for $x ($1..$1+$3-1) {
        for $y ($2..$2+$4-1) {
            $x{"$x $y"}++;
        }
    }
}
print scalar (grep $_ > 1, values %x), "\n";