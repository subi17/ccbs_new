#!/bin/bash

IFS=$'\n';
for row in `hg heads | grep ^branch | uniq -c`; do
	number=`echo $row | sed 's/^[ \t]*\(.*\) branch:.*/\1/g'`
	branch=`echo $row | sed 's/^[ \t]*.* branch\: *\(.*\)/\1/g'`  
	if [ $number -ne "1" ] ; then
		echo "======================================================================================"
		echo "Trying to push $number heads in branch $branch, try run "hg merge" before it"
		echo "======================================================================================"
		exit 1
	fi
done
number=`hg heads default | grep ^changeset | wc -l`
if [ $number -ne "1" ] ; then
	echo "======================================================================================"
	echo "Trying to push $number heads in default, try run "hg merge" before it"
	echo "======================================================================================"
	exit 1
fi
exit 0
