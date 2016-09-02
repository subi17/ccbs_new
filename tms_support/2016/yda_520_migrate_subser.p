DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEF VAR k AS INT NO-UNDO. 

def stream sout.
output stream sout to yda_520_subser_migration.txt append.

def stream sout2.
output stream sout2 to yda_520_subser_migration_exluded.d append.

FOR EACH subser NO-LOCK use-index ServCom where
   subser.msseq > 2140271:
   
   if lookup(subser.servcom,"BAIC,BAOC,BOIC,CAMPAIGN,CCGW,CLIP,CLIR,IVR,KEEP,MMS,NTFNBR,NUMBERINQ,OBA,OBI,OBO,OBOPRE,OBOPRI,OBR,TS21,TS22,BCG,RSA") > 0 then do:
      export stream sout2 subser.
      k = k + 1.
      next.
   end.
   
   i = i + 1.
   
   if i mod 2000 = 0 THEN DO:
      disp subser.msseq i k j with frame a.
      pause 2 no-message.
   END.

   FIND FIRST subser_new NO-LOCK where
              subser_new.MsSeq     = subser.msseq and
              subser_new.ServCom   = subser.servcom and
              subser_new.SSDate    = subser.ssdate no-error.
   IF AVAIL subser_new then do:
      put stream sout unformatted
         subser.msseq ";"
         subser.servcom ";"
         subser.ssdate ";"
         "SKIPPED:Subser already exists" skip.
      j = j + 1.
      next.
   end.

   do trans:
      create subser_new.
      buffer-copy subser to subser_new.
      release subser_new.
   end.

end.

disp i j k.
