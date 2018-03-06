echo "===== test1 ====="
./compiler.native example.txt
echo "===== test2 ====="
./compiler.native -lex example.txt
echo "===== test3 ====="
./compiler.native -parse example.txt
echo "===== test4 ====="
./compiler.native test/test1.txt
echo "===== test5 ====="
./compiler.native -lex test/test1.txt
echo "===== test6 ====="
./compiler.native -parse test/test1.txt
echo "===== test7 ====="
./compiler.native test/test2.txt
echo "===== test8 ====="
./compiler.native -lex test/test2.txt
echo "===== test9 ====="
./compiler.native -parse test/test2.txt 
