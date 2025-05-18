open FH, '2018-02.txt';
while (<FH>) {
    %x = ();
    $x{$_}++ for split //;
    $y++     if grep $_ == 2, values %x;
    $z++     if grep $_ == 3, values %x;
}
print $y * $z, "\n";