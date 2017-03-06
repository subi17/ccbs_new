{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".

{Func/timestamp.i}

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCommLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeActStamp AS DECIMAL NO-UNDO.

input from /apps/snet/200901/as_yts1272.input.
def stream slog.
output stream slog to /apps/snet/200901/as_yts1272.log append.

ldeActStamp = fMakeTS().

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

repeat:
   
   import unformatted lcLine.

   find termmobsub where 
      cli = lcLine and
      clitype = "contrd1" no-lock.

   lcCommLine = "MODIFY,MSISDN=34" + termmobsub.cli + 
   ",ICC=" + TermMobsub.ICC + 
   ",IMSI=" + TermMobsub.IMSI + 
   ",PAYTYPE=POSTPAID,OPERATOR=YOIGO,NW=ERICSSON,COS=2,SHAPER=0,REQUESTID=0". 
  
 /* 
  disp lcCommLine view-as editor size 60 by 60. */
   
   CREATE Solog.
    ASSIGN
        Solog.Solog = NEXT-VALUE(Solog).
              
    ASSIGN
        Solog.CreatedTS    = ldeActStamp
        Solog.MsSeq        = termmobSub.MsSeq 
        Solog.CLI          = termmobSub.Cli 
        Solog.Stat         = 0     /* just created */
        Solog.Brand        = gcBrand
        Solog.Users        = katun.
   ASSIGN     
        Solog.TimeSlotTMS  = ldeActStamp
        Solog.ActivationTS = ldeActStamp
        Solog.MSrequest    = 0.

    ASSIGN 
       Solog.Commline = STRING(Solog.Solog) + " " + lcCommLine.  
       
   put stream slog unformatted termmobsub.cli "|" Solog.Solog "|" lcCommLine skip.
  

end.

input close.
output stream slog close.
