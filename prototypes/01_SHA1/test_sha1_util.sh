#!/bin/bash

# This script will test the file 'sha1_util.c' via 'sha1_util_test.c'.
# It will compute the hashes of a test file ($FILE_TEST) with sha1_util_test.c
# and compare the result with a computing made by sha1sum on
# the splitted parts of the test file.

# If all gone fine the '[OK]' is displayed and the script return 0.
# If a problem appears then an error is displayed and the results are not deleted.

FILE_EXEC=test_sha1
FILE_TEST=01.wmv
CHUNK_SIZE=2097152

gcc sha1.c sha1_util.c sha1_util_test.c -o $FILE_EXEC

split -b $CHUNK_SIZE < $FILE_TEST

sha1sum xa* | egrep -e "\w{40}" -o > FILE_TEST_SHA1SUM
./$FILE_EXEC $FILE_TEST > FILE_TEST_C

rm xa*

diff FILE_TEST_SHA1SUM FILE_TEST_C
if (( $? ))
then
   echo "Error : files FILE_TEST_SHA1SUM and FILE_TEST_C are not equal"
   exit 1
else
   echo "[OK]"
   rm FILE_TEST_SHA1SUM
   rm FILE_TEST_C
   exit 0
fi
