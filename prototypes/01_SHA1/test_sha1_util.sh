#!/bin/bash

# This script will test this prototype against the common sha1sum.
# It will compute the hashes of a test file ($FILE_TEST)
# and compare the result with a computing made by sha1sum on
# the splitted parts of the test file.

# If all gone fine the '[OK]' is displayed and the script return 0.
# If a problem appears then an error is displayed and the results are not deleted.

FILE_EXEC=qt_sha1
FILE_TEST=01.wmv
CHUNK_SIZE=2097152

# Compile the project.
qmake qt_sha1.pro
make

split -b $CHUNK_SIZE < $FILE_TEST

sha1sum xa* | egrep -e "\w{40}" -o > FILE_TEST_SHA1SUM
./$FILE_EXEC chunk $FILE_TEST > FILE_TEST_C

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
