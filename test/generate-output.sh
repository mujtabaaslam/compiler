echo "===== test1 ====="
./compiler.native example.txt
echo "===== test2 ====="
./compiler.native -lex example.txt
echo "===== test3 ====="
./compiler.native -parse example.txt
echo "===== test4 ====="
./compiler.native -step example.txt
echo "===== test5 ====="
./compiler.native test/test1.txt
echo "===== test6 ====="
./compiler.native -lex test/test1.txt
echo "===== test7 ====="
./compiler.native -parse test/test1.txt
echo "===== test8 ====="
./compiler.native -step test/test1.txt
echo "===== test9 ====="
./compiler.native test/test2.txt
echo "===== test10 ====="
./compiler.native -lex test/test2.txt
echo "===== test11 ====="
./compiler.native -parse test/test2.txt
echo "===== test12 ====="
./compiler.native -step test/test2.txt
echo "===== test13 ====="
./compiler.native test/test3.txt
echo "===== test14 ====="
./compiler.native -lex test/test3.txt
echo "===== test15 ====="
./compiler.native -parse test/test3.txt
echo "===== test16 ====="
./compiler.native -step test/test3.txt
echo "===== test17 ====="
./compiler.native test/test4.txt
echo "===== test18 ====="
./compiler.native -lex test/test4.txt
echo "===== test19 ====="
./compiler.native -parse test/test4.txt
echo "===== test20 ====="
./compiler.native -step test/test4.txt
