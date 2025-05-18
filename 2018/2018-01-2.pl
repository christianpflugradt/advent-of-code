open FH, '2018-01.txt';
@x = <FH>;
$x = 0;
%x = (0 => 1);
X:while (1) {
    for (@x) {
        $x += $_;
        last X if $x{$x}++
    }
}
print $x, "\n";