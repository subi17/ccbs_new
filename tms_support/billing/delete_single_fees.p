{commpaa.i}
assign gcbrand = "1"
       katun = "Qvantel".
{timestamp.i}
{eventval.i}

def stream sin.
def stream sout.

def var lcline        as char no-undo.
def var lccli         as char no-undo.
def var lcdel         as char no-undo INIT "|".
DEF VAR ldaTermDate   AS DATE NO-UNDO.
DEF VAR liTermTime    AS INT  NO-UNDO.
DEF VAR liTermPeriod  AS INT  NO-UNDO.

DEFINE BUFFER bMsRequest     FOR MsRequest.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {lib/eventlog.i}
END.

def stream sbak.
output stream sbak to /apps/yoigo/tms_support/billing/delete_single_fees.d append.

FUNCTION fDeleteSingleFee RETURNS LOGICAL (INPUT iiTermPeriod AS INT):

   FIND FIRST SingleFee USE-INDEX Custnum WHERE
              SingleFee.Brand = gcBrand AND
              SingleFee.Custnum = MobSub.CustNum AND
              SingleFee.HostTable = "Mobsub" AND
              SingleFee.KeyValue = STRING(MobSub.MsSeq) AND
              SingleFee.BillPeriod = iiTermPeriod AND
              SingleFee.BillCode = "TERMPERIOD" EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL SingleFee THEN DO:
      IF NOT SingleFee.Billed THEN DO:
         IF llDoEvent THEN
            RUN StarEventMakeDeleteEventWithMemo(
               (BUFFER SingleFee:HANDLE),
               katun,
               "YOT-2783").

         put stream sout unformatted lcline lcdel "Single Fee deleted successfully with price: " +
             STRING(SingleFee.Amt) skip.

         export stream sbak SingleFee.
         DELETE SingleFee.
      END.
      ELSE DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                            "MobSub",
                            STRING(MobSub.MsSeq),
                            MobSub.CustNum,
                            "Single Fee",
                            "TERMPERIOD Single fee is already billed " +
                            "so can not be deleted").
   END. /* IF AVAIL SingleFee THEN DO: */
   
   RETURN TRUE.
END FUNCTION.

input stream sin from "/apps/yoigo/tms_support/billing/delete_single_fees.txt".
output stream sout to "/apps/yoigo/tms_support/billing/delete_single_fees.log" append.


DEFINE VARIABLE i AS INTEGER NO-UNDO. 
repeat trans:
   import stream sin unformatted lcline.
   if lcline = "" or lcline = ? then next.

   lccli = trim(entry(1,lcline,lcdel)).

   i = i + 1.
   if i <= 100 then next.
/*   if i > 100 then leave. */
   if i mod 10 = 0 THEN DO:
      disp i with frame a.
      pause 0.
   END.
   
   FIND mobsub NO-LOCK WHERE
        mobsub.cli = lccli NO-ERROR.
   IF NOT AVAILABLE mobsub THEN DO:
      put stream sout unformatted lcline lcdel "ERROR:MobSub not found" skip.
      next.
   END.

   FIND FIRST MsRequest WHERE
              MsRequest.MsSeq = MobSub.MsSeq AND
              MsRequest.ReqType = 81 AND
              MsRequest.ReqStatus = 2 AND
              MsRequest.ActStamp = 20131201 AND
              MsRequest.ReqCparam1 = "CONTS30" AND
              MsRequest.ReqCparam2 = "CONTS25" NO-LOCK NO-ERROR.
   IF NOT AVAIL MsRequest THEN DO:
      put stream sout unformatted lcline lcdel "ERROR:Original BTC request not found" skip.
      next.
   END.

   FIND FIRST bMsRequest WHERE
              bMsRequest.OrigRequest = MsRequest.MsRequest AND
              bMsRequest.MsSeq = MobSub.MsSeq AND
              bMsRequest.ReqType = 9 AND
              bMsRequest.ReqStatus = 2 AND
              bMsRequest.ReqCparam3 BEGINS "TERM" NO-LOCK NO-ERROR.
   IF NOT AVAIL bMsRequest THEN DO:
      put stream sout unformatted lcline lcdel "ERROR:Term Contract termination request not found" skip.
      next.
   END.

   fSplitTS(MsRequest.ActStamp,OUTPUT ldaTermDate,OUTPUT liTermTime).

   ASSIGN ldaTermDate  = ldaTermDate - 1
          liTermPeriod = YEAR(ldaTermDate) * 100 + MONTH(ldaTermDate).

   fDeleteSingleFee(INPUT liTermPeriod).

end.

input stream sin close.
output stream sout close.
