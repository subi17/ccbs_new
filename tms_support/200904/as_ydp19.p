{commpaa.i}
katun = "anttis".
gcBrand = "1".

{timestamp.i}

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCommLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeActStamp AS DECIMAL NO-UNDO.

input from /apps/snet/200904/as_ydp19.input2.

def stream slog_unclear.
output stream slog_unclear to /apps/snet/200904/as_ydp19.unclear.log.

def stream slog_term.
output stream slog_term to /apps/snet/200904/as_ydp19.terminated_6.log append.
/*
def stream slog_create.
output stream slog_create to /apps/snet/200904/as_ydp19.created.log append.
*/

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcICC AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIMSI AS CHARACTER NO-UNDO. 
     
DEFINE TEMP-TABLE ttCLI
FIELD cli AS CHAR 
FIELD clitype AS CHAR 
INDEX i IS PRIMARY UNIQUE cli. 

DEFINE TEMP-TABLE ttOper
FIELD cli AS CHAR
FIELD msseq as int
INDEX msseq IS PRIMARY UNIQUE msseq. 

repeat:
   import unformatted lcLine.
   find ttCLI where ttCLI.cli = entry(1,lcLine,"|") NO-LOCK NO-ERROR.
   IF NOT AVAIL ttCLI then do:
      create ttCLI.
      assign 
         ttCLI.cli = trim(entry(1,lcLine,"|"))
         ttCLI.clitype = trim(entry(2,lcLine,"|")).
   end.
end.
ldeActStamp = fMakeTS().

FILE_LOOP:
FOR EACH ttCli NO-LOCK:
   
   find mobsub where 
      mobsub.cli = ttCli.cli NO-LOCK NO-ERROR.
   
   IF AVAIL mobsub then do:
      if mobsub.clitype = "contrd1" then next FILE_LOOP. 
   /*   put stream slog_term unformatted mobsub.cli "|" mobsub.msseq "|" mobsub.clitype skip. */
   end.
   else next FILE_LOOP.
   
   
   find msrequest where
      msrequest.msseq = mobsub.msseq and
      msrequest.reqtype = 0 and
      msrequest.reqcparam1 = "contrd1" NO-LOCK NO-ERROR.
   if avail msrequest then do:
      /*put stream slog_unclear unformatted mobsub.cli "|" mobsub.msseq "|" mobsub.clitype "|STC|" msrequest.reqstat "|" msrequest.actstamp skip. 
      next FILE_LOOP. */
   end.
   else next FILE_LOOP.
   
   if mobsub.activationts >= 20090414 then do:
  /*    put stream slog_term unformatted mobsub.cli "|" mobsub.msseq "|IS NEW" skip.*/
    next FILE_LOOP.
   end.
   
   i = i + 1.
   if i <= 1 then next.   
/*   if i > 1 then leave.     */
   
/*
   find termmobsub where 
      termmobsub.cli = ttCli.cli  no-lock no-error.
   
   IF NOT AVAIL termmobsub then do:
      
      find first termmobsub where 
         termmobsub.cli = ttCli.cli  no-lock no-error.
      
      if avail termmobsub then do:
         put stream slog_unclear unformatted ttCli.cli "|CONTRD1|NOT UNIQUE MSISDN" skip.
         next FILE_LOOP.
      end.
      else do:
        put stream slog_unclear unformatted ttCli.cli "|CONTRD1|ERROR - unknown msisdn" skip.
      end.

   end.
   else next.
  */ 

   find first msowner where
      msowner.cli = ttCli.cli and
      msowner.cliType = "contrd1" NO-LOCK use-index cli_s NO-ERROR.
   if not avail msowner then do:
      put stream slog_term unformatted ttCli.cli "|NO MSOWNER" skip.
      next FILE_LOOP.
   end.
   
   if lookup(msowner.cli,"622067320 622513384 622617800 622688857 622714378 622732372 622856979 633000237 661967863") > 0 then do:
      put stream slog_term unformatted msowner.cli "|" msowner.msseq "|" msowner.tsbegin "|NO SERVCOM" skip.
      next FILE_LOOP.
   end.
   
   find first imsi where imsi.imsi = msowner.imsi NO-LOCK NO-ERROR.
   IF NOT AVAIL imsi then do:
      put stream slog_term unformatted msowner.cli "|" msowner.msseq "|" msowner.imsi "|" msowner.tsbegin "|NO IMSI" skip.
      next FILE_LOOP.
   end.

   lcCommLine = "MODIFY,MSISDN=34" + msowner.cli + 
   ",ICC=" + imsi.icc + 
   ",IMSI=" + msowner.imsi + 
   ",PAYTYPE=POSTPAID,OPERATOR=YOIGO,NW=ERICSSON,COS=2,SHAPER=0,REQUESTID=0". 
 
   CREATE Solog.
    ASSIGN
        Solog.Solog = NEXT-VALUE(Solog).
              
    ASSIGN
        Solog.CreatedTS    = ldeActStamp
        Solog.MsSeq        = msowner.MsSeq 
        Solog.CLI          = msowner.Cli 
        Solog.Stat         = 0     /* just created */
        Solog.Brand        = gcBrand
        Solog.Users        = katun.
   ASSIGN     
        Solog.TimeSlotTMS  = ldeActStamp
        Solog.ActivationTS = ldeActStamp
        Solog.MSrequest    = 0.

    ASSIGN 
       Solog.Commline = STRING(Solog.Solog) + " " + lcCommLine.  
   
    put stream slog_term unformatted msowner.cli "|" msowner.msseq "|" msowner.tsbegin "|DELETED|" Solog.Solog  "|" Solog.CommLine  skip.

end. 

DEFINE VARIABLE ldeTimeLimit AS DECIMAL NO-UNDO. 

ldeTimeLimit = 20090414.

/* CREATE MISSING SHAPERS */
/*
FOR EACH mobsub where
   mobsub.brand = "1" and
   mobsub.clitype = "CONTRD1" NO-LOCK:

   if mobsub.activationts >= 20090414 then next.

   find ttCli where ttCli.cli = mobsub.cli NO-LOCK NO-ERROR.
   if avail ttCli then next.

   i = i + 1.
   if i <= 4 then next.
/*   if i > 4 then leave. */
   
   if lookup(mobsub.cli,"622067320 622513384 622617800 622688857 622714378 622732372 622856979 633000237 661967863") > 0 then do:
      put stream slog_create unformatted mobsub.cli "|" mobsub.msseq "|" mobsub.activationts "|NO SERVCOM" skip.
      next.
   end.
   
   lcCommLine = "MODIFY,MSISDN=34" + mobsub.cli + 
   ",ICC=" + mobsub.ICC + 
   ",IMSI=" + mobsub.IMSI + 
   ",PAYTYPE=POSTPAID,OPERATOR=YOIGO,NW=ERICSSON,COS=2,SHAPER=CONTRD1,REQUESTID=0". 
   
   CREATE Solog.
    ASSIGN
        Solog.Solog = NEXT-VALUE(Solog).
              
    ASSIGN
        Solog.CreatedTS    = ldeActStamp
        Solog.MsSeq        = mobSub.MsSeq 
        Solog.CLI          = mobSub.Cli 
        Solog.Stat         = 0     /* just created */
        Solog.Brand        = gcBrand
        Solog.Users        = katun.
   ASSIGN     
        Solog.TimeSlotTMS  = ldeActStamp
        Solog.ActivationTS = ldeActStamp
        Solog.MSrequest    = 0.

    ASSIGN 
       Solog.Commline = STRING(Solog.Solog) + " " + lcCommLine.  

   put stream slog_create unformatted mobsub.cli "|" mobsub.msseq "|" mobsub.activationts "|CREATED|" Solog.Solog  "|" Solog.CommLine skip.
     

END.
*/
input close.
/*output stream slog_create close.*/
output stream slog_term close.
output stream slog_unclear close.
