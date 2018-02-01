CAESAR_TESTS="$(ls caesar-test*.in)"
VIGENERE_TESTS="$(ls vigenere-test*.in)"

for test in ${CAESAR_TESTS}; do
  echo "===== $test ====="
  cat $test | java CaesarCipher
done

for test in ${VIGENERE_TESTS}; do
  echo "===== $test ====="
  cat $test | java VigenereCipher
done
