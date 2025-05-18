open FH, '2018-02.txt';
chomp(@x = <FH>);
X:for $y (0..$#x) {
    for $z ($y+1..$#x) {
        @y = split //, $x[$y];
        @z = split //, $x[$z];
        $rc = 0, $r = '';
        for $x (0..$#y) {
            $r .= $y[$x] if $y[$x] eq $z[$x];
            $rc++        unless $y[$x] eq $z[$x];
        }
        last X if $rc == 1;
    }
}
print $r, "\n";