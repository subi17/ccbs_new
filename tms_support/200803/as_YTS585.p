{commpaa.i} 
katun = "anttis".
gcBrand = "1".
{msreqfunc.i}
{daycampaign.i}
{eventval.i}
{fmakeservlimit.i}

def buffer m for msrequest.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE  ldCoefficient AS DECIMAL NO-UNDO.   
DEFINE VARIABLE liDaysPassed AS INTEGER NO-UNDO. 
DEF VAR lcDCEvent     AS CHAR NO-UNDO.

def stream sout.
output stream sout to /home/anttis/fee_fix.txt.

FOR EACH msrequest where 
         msrequest.brand = "1" and 
         msrequest.reqtype = 9 and 
         msrequest.reqstatus = 2 and 
         msrequest.actstamp >= 20080326 and 
         msrequest.reqcparam3 eq "term18" and 
         msrequest.createfees = false NO-LOCK:
   
   FIND FIRST m where m.msseq = msrequest.msseq and m.reqtype = 18 and 
   /*m.reqiparam4 = 1 and*/ m.reqsource ne "3" NO-LOCK no-error.
   if not avail m then next.
   
   FIND FIRST termmobsub where termmobsub.msseq = msrequest.msseq and termmobsub.clitype begins "cont" NO-LOCK no-error.
  if avail termmobsub then i = i + 1. else next.
 
   /* day campaign id */
   lcDCEvent = MsRequest.ReqCParam3.

   fSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).

   FIND FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE
              MsOwner.MsSeq  = MsRequest.MsSeq AND
              MsOwner.TsEnd >= 99999999 NO-ERROR.
   IF NOT AVAILABLE MsOwner THEN 
   FIND FIRST MsOwner NO-LOCK WHERE
              MsOwner.MsSeq  = MsRequest.MsSeq NO-ERROR.
   IF NOT AVAIL MsOwner THEN NEXT.
  
   FIND FIRST DCCLI WHERE
             DCCLI.Brand      = gcBrand         AND
             DCCLI.DCEvent    = lcDCEvent       AND
             DCCLI.MsSeq      = MsRequest.MsSeq AND
             DCCLI.ValidTo   >= ldtActDate      AND
             DCCLI.ValidFrom <= ldtActDate
   NO-LOCK NO-ERROR.
   if not avail dccli then next.

   /* Count the days the contract has been valid. */
   liDaysPassed = ldtActDate - DCCLI.ValidFrom.

 
   /* create a penalty fee for termination */
   
 /*  IF MsRequest.CreateFees THEN DO:*/

      ldCoefficient = 1 - ((liDaysPassed + 1) / 547).
      IF ldCoefficient < 0 THEN ldCoefficient = 0.

      RUN creasfee(MsOwner.CustNum,
                    MsRequest.MsSeq,
                    ldtActDate,
                    "MobSub",
                    "TERM_PERIOD",
                    1,
                    /* memo   */
                    DCCLI.DCEvent + " terminated " +
                       STRING(ldtActDate,"99.99.9999") +
                       "¤¤¤" + STRING(ldCoefficient),
                    TRUE,          /* messages to screen */
                    OUTPUT lcReqChar).

      put stream sout unformatted msrequest.msrequest lcReqChar skip.
/*   END.*/
 
END.

output stream sout close.
disp i.
