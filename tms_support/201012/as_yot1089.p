input from /store/riftp/topupfat/incoming/processed/topup_fat_622FF-20101119.txt.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream sout.
output stream sout to as_yot1089.log.

def stream sbak.
output stream sbak to as_yot1089_fatime.d.
      
put stream sout "MSISDN|FAT group|Amount removed" skip.

FATLOOP:
repeat:
   import unformatted lcLine.
   lcCli = entry(2,lcLine,";").

   find mobsub where
      mobsub.cli = lcCli NO-LOCK.

   i = 0.
   FOR EACH fatime NO-LOCK where
      fatime.brand = "1" and
      fatime.msseq = mobsub.msseq and
      fatime.period = 201011 and
      fatime.FTGrp = "CPFAT"  and
      fatime.invnum = 0:
      i = i + 1.
   end.

   if i > 1 then next. 

   FOR EACH fatime EXCLUSIVE-LOCK where
      fatime.brand = "1" and
      fatime.msseq = mobsub.msseq and
      fatime.period = 201011 and
      fatime.FTGrp = "CPFAT"  and
      fatime.invnum = 0:
      put stream sout unformatted mobsub.cli "|" fatime.ftGrp "|" fatime.amt skip.
      export stream sbak fatime.
      delete fatime.
   end.

end.
