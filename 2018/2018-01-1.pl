open FH, '2018-01.txt';
print +(eval join '', <FH>), "\n";