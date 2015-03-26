#!/bin/bash

for f in *; do 
	echo -n "$f " 
	a=`find $f -name "*.[pi]" | xargs cat | wc -l`
	echo $a
done
a=`find . -name "*.[pi]" | xargs cat | wc -l`
echo $a
