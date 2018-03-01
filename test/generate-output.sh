echo "===== test1 ====="
./compiler.native example.txt 
echo "===== test2 ====="
./compiler.native -lex example.txt 
echo "===== test3 ====="
./compiler.native -parse example.txt 

