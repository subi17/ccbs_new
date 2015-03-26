{commpaa.i} katun = "SOG". gcbrand = "1".
{fsubser.i}
{timestamp.i}

def stream slog.
output stream slog to /apps/snet/200901/as_yts1250.log.

DEF VAR lcPaytype AS CHAR NO-UNDO.
DEF VAR lcProfile as CHAR NO-UNDO.
DEF VAR lcCommline AS CHAR NO-undo.
def var lcservices as char no-undo.
DEF VAR lcActStamp as dec no-undo.

def stream sin.
input stream sin from /apps/snet/200901/as_yts1250.input.

DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 

REPEAT:

   import stream sin unformatted lcCli.

   lcservices = "".
   lcCommline = "".
   
   /*find mobsub where cli = "605504630" no-lock no-error. */

   find mobsub where cli = lcCli no-lock no-error.

   FIND Imsi WHERE Imsi.Imsi = MobSub.Imsi NO-LOCK NO-ERROR.

   FOR EACH SubSer where SubSer.MsSeq = mobsub.msseq and servpac = "" no-lock break by servcom by servcom.
   if first-of(servcom) then do:
   lcServices = lcServices + "," + servcom + "=" + STRING(fCurrSubSer(MobSub.MsSeq,subser.servcom)) .
   end.
   end.

   IF INT(MobSub.PayType) = 1 THEN 
   ASSIGN lcPayType = "POSTPAID"
          lcProfile = "1".
      ELSE 
   ASSIGN lcPayType = "PREPAID"
         lcProfile = "2".

   lcActStamp = fMakeTS().

   lcCommline = " CREATE,MSISDN=34" + MobSub.Cli + ",ICC=" + 
                Imsi.ICC + ",IMSI=" + Imsi.IMSI + 
                ",OPERATOR=YOIGO,NW=ERICSSON,ADKEY=1,PAYTYPE="+ lcPaytype +
                 ",PROFILE=" + lcProfile 
                 + ",KI=" + Imsi.ki + ",COS=2,ORGID=1,".
    IF MobSub.MNPChannel > 0 THEN lcCommline = lcCommline + "MNP=1".
                       
   lcCommline =  lcCommLine +  lcservices  + " ,REQUESTID=0".

   /*
                 disp substr(lcCommline,150,78) format "X(78)".

   CREATE Solog.
    ASSIGN
        Solog.Solog = NEXT-VALUE(Solog).
              
    ASSIGN
        Solog.CreatedTS    = lcActStamp
        Solog.MsSeq        = MobSub.MsSeq    /* Mobile Subscription No.    */
        Solog.CLI          = MobSub.Cli      /* MSISDN                     */
        Solog.Stat         = 0               /* just created               */
        Solog.Brand        = gcBrand
        Solog.Users        = katun.
   ASSIGN     
        Solog.TimeSlotTMS  = lcActStamp
        Solog.ActivationTS = lcActStamp
        Solog.MSrequest    = 0.

    ASSIGN 
       Solog.Commline = STRING(Solog.Solog) + lcCommLine.  
       
       disp Solog.Solog. */
   
   put stream slog unformatted mobsub.cli " " /*Solog.Solog " "*/ lcCommLine skip.

END.

input stream sin close.
output stream slog close.
