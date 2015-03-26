mach=/store/riftp/mach
while read filename
do
   cp $mach/test/processed/$filename $mach/production/outgoing
   echo "Copied " $mach/test/processed/$filename " to " $mach/production/outgoing
done < $1

