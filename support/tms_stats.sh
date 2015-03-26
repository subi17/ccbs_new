#!/bin/bash

p_rows=`find . -name *.p | xargs wc -l | tail -n 1 | sed -e "s/[a-zA-Z ]//g"`
p_files=`find . -name *.p | wc -l`
i_rows=`find . -name *.i | xargs wc -l | tail -n 1 | sed -e "s/[a-zA-Z ]//g"`
i_files=`find . -name *.i | wc -l`
cls_rows=`find . -name *.cls | xargs wc -l | tail -n 1 | sed -e "s/[a-zA-Z ]//g"`
cls_files=`find . -name *.cls | wc -l`
cui_rows=`grep  -i -e "ALERT-BOX" -e "BROWSE:" -e "FORM:" -e "ehto = "  * -Rl | xargs wc -l | tail -n 1 | sed -e "s/[a-zA-Z ]//g"`
cui_files=`grep  -i -e "ALERT-BOX" -e "BROWSE:" -e "FORM:" -e "ehto = " * -Rl | wc -l`
p_files=`echo $p_files`
i_files=`echo $i_files`
cls_files=`echo $cls_files`
cui_files=`echo $cui_files`
total=$((p_rows+i_rows+cls_rows))
total_files=$((p_files+i_files+cls_files))
echo -e "    .p lines: $(printf %7d $p_rows) ($((p_rows*100/total))%) ($p_files files)"
echo -e "    .i lines: $(printf %7d $i_rows) ($((i_rows*100/total))%) ($i_files files)"
echo -e "  .cls lines: $(printf %7d $cls_rows) ($((cls_rows*100/total))%) ($cls_files files)"
echo -e "   cui lines: $(printf %7d $cui_rows) ($((cui_rows*100/total))%) ($cui_files files)"
echo ""
echo "per directory:"
for dir in $(ls -d1 */);
do
	rows=`find $dir -type f | xargs wc -l | tail -n 1 | sed -e "s/[a-zA-Z \/\\\.]//g"`
	echo -e "$(printf %12s $dir): $(printf %7d $rows) ($((rows*100/total))%)"
done
echo ""
echo "   total LOC:  $total ($total_files files)"
