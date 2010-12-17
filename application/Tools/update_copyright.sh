#!/usr/bin/env bash

# This script will integrate the license '../license.txt' to all source (*.h, *.cpp)
# If the license already exists in a source it will be replaced.

set -o errexit

cd ..

LICENSE_FILENAME="license.txt"
LICENSE_SIZE=`wc -l $LICENSE_FILENAME | awk '{print $1}'`
LICENSE=`cat "$LICENSE_FILENAME"`

echo -e "Current copyright and license header :\n$LICENSE"
echo

NB_FILE_UPDATED=0

for CURRENT_FILE in `find . -not -path "./Libs*" -and -not -path "*/.tmp*" -and \( -name '*.cpp' -or -name '*.h' \)`
do
   echo "Current File : $CURRENT_FILE"
   TMP_FILE="${CURRENT_FILE}.tmp"
   if [ "$LICENSE" != "`head -n $LICENSE_SIZE $CURRENT_FILE`" ]
   then
      NB_FILE_UPDATED=`expr $NB_FILE_UPDATED + 1`
      # We only match a part (lines 5 to 8) of the copyright to know if the header exists or not in the current file.
      if [ "`sed -n '5,8p' $LICENSE_FILENAME`" = "`sed -n '5,8p' \"$CURRENT_FILE\"`" ]
      then
         echo "$LICENSE" > "$TMP_FILE"
         sed "1,${LICENSE_SIZE}d" "$CURRENT_FILE" >> "$TMP_FILE"
         mv "$TMP_FILE" "$CURRENT_FILE"
      else
         echo "$LICENSE" | cat - "$CURRENT_FILE" > "$TMP_FILE"
         mv "$TMP_FILE" "$CURRENT_FILE"
      fi
   else
      echo "Nothing to do"
   fi
done

echo "Number of file updated : $NB_FILE_UPDATED"
