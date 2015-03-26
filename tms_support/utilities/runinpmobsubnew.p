{/home/harrim/utilities/ttinpmobsub.i}
fAddOption("DefaultImport", "FALSE").
fAddOption("ImportMobCDR", "TRUE").
fAddOption("ImportPrepCDR", "TRUE").

run /home/harrim/utilities/inpmobsubnew.p("/home/harrim/dumps/d200901/dumps/hierarch/for_merak/", 
 0,0,0,"/home/harrim/dumps/d200901/dumps/hierarch/for_merak/clis_logs.txt",
 "/home/harrim/dumps/d200901/dumps/hierarch/for_merak/merak_clis.txt", TABLE ttinpmobsub).

