open FH, '2018-02.txt';

@x = <FH>;
for (@x) {
    %x = ();
    $x{$_}++ for split //;
    $y++     if grep $_ == 2, values %x;
    $z++     if grep $_ == 3, values %x;
}
$result = $y * $z;

print "$result\n";
