open FH, '2018-02.txt';

chomp(@x = <FH>);
X:for $y (0..$#x) {
    for $z ($y+1..$#x) {
        @y = split //, $x[$y];
        @z = split //, $x[$z];
        $r = 0, $result = '';
        for $x (0..$#y) {
            $result .= $y[$x] if $y[$x] eq $z[$x];
            $r++              unless $y[$x] eq $z[$x];
        }
        last X if $r == 1;
    }
}

print "$result\n";
