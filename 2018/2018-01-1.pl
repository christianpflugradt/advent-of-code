open FH, '2018-01.txt';

$result = eval join '', <FH>;

print "$result\n";