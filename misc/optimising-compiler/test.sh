#!/bin/bash

BASEDIR=$(dirname "$0")
DIFF=$(which diff)

cd "$BASEDIR"

if [ ! -x "$DIFF" ]; then
  echo "Cannot find the diff command. Exiting."
  exit 1
fi

if [ -x "$BASEDIR/../../eff" ]; then
  EFF="$BASEDIR/../../eff"
elif [ -x "$BASEDIR/../../eff.native" ]; then
  EFF="$BASEDIR/../../eff.byte"
elif [ -x "$BASEDIR/../../eff.byte" ]; then
  EFF="$BASEDIR/../../eff.byte"
else
  echo "Cannot find the eff executable. Compile eff first."
  exit 1
fi

VALIDATE=0
if [ "$1" = "-v" ]; then
  VALIDATE=1
fi

for FILE in "$BASEDIR"/*.eff; do
  "$EFF" --explicit-subtyping -n --compile "$FILE" &>/dev/null
  mv "$FILE.ml" "$FILE.ml.out"
  if [ -f "$FILE.ml.ref" ]; then
    RESULT=$("$DIFF" "$FILE.ml.out" "$FILE.ml.ref")
    if [ "$?" = "0" ]; then
      echo "Passed:  $FILE"
      rm "$FILE.ml.out"
    else
      echo "FAILED:  $FILE"
      if [ $VALIDATE = "1" ]; then
        "$DIFF" "$FILE.ml.out" "$FILE.ml.ref"
        read -pr "Validate $FILE.ml.out as new $FILE.ml.ref? (y/n) [n] " ans
        if [ "$ans" = "y" ] || [ "$ans" = "Y" ]; then
          mv "$FILE.ml.out" "$FILE.ml.ref"
          echo "Validated: $FILE"
        fi
      fi
    fi

  else
    mv "$FILE.ml.out" "$FILE.ml.ref"
    echo "Created: $FILE.ml.ref"
  fi
done
