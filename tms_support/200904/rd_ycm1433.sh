loc=/store/simcards/processed
while read filename
do
     wc -l $loc/$filename 
 
done < $1

