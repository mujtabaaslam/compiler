echo "===== test1 ====="
./cli -help hello this better work
echo "===== test2 ====="
./cli this should print line by line
echo "===== test3 ====="
./cli -length now give me lengths of this
echo "===== test4 ====="
./compiler "example.txt"
echo "===== test5 ====="
./compiler -lex "example.txt"
echo "===== test6 ====="
./compiler -parse "example.txt"
