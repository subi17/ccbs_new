{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".

{Func/timestamp.i}

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCommLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeActStamp AS DECIMAL NO-UNDO.
def stream slog.
output stream slog to /apps/snet/200804/as_msisdn_fix.log append.

lcCommLine = "MODIFY,MSISDN=34622206123->34600007332,ICC=8934040408009692079,IMSI=214040101415007,OPERATOR=YOIGO,NW=ERICSSON,COS=2,PROFILE=1,PAYTYPE=POSTPAID,REQUESTID=0".

ldeActStamp = fMakeTS().

CREATE Solog.
 ASSIGN
     Solog.Solog = NEXT-VALUE(Solog).
           
ASSIGN
  Solog.CreatedTS    = ldeActStamp
  Solog.MsSeq        = 1226219 
  Solog.CLI          = "600007332" 
  Solog.Stat         = 0     /* just created */
  Solog.Brand        = gcBrand
  Solog.Users        = katun.

ASSIGN     
     Solog.TimeSlotTMS  = ldeActStamp
     Solog.ActivationTS = ldeActStamp
     Solog.MSrequest    = 0.

ASSIGN 
   Solog.Commline = STRING(Solog.Solog) + " " + lcCommLine.  

put stream slog unformatted  Solog.Solog  "|" Solog.CommLine  skip.

/*output stream slog_create close.*/
output stream slog close.
