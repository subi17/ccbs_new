#!/bin/bash

if test -a ${1}
   then rm ${1}
fi

for a in `ls -1 DOC1BR_${2}*.txt*`; do
   cat $a >> ${1};
done
