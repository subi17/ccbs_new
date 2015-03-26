for progdb in `ls -1 *.db`
do 
   echo From $progdb	
   for dbuser in `proshut $progdb -C list | grep $1 | cut -c 1-3` 
   do 
      proshut $progdb -C disconnect $dbuser
   done
done
