/* ----------------------------------------------------------------------
  MODULE .......: payterm_short_action.p
  TASK .........: Shortening Payterm contract(DCCLI) to match same length than
                  respective FixedFee end periods. Also possible SingleFees(Q25) 
                  are adjusted to match FixedFee end periods.

  INSTRUCTION ..: Define dates for eventlog
                  and simulation flag to simulate(default) 
                  when flag is false, program does changes
  APPLICATION ..: 
  AUTHOR .......: Janne Tourunen
  CREATED ......: 28/06/2016
  CHANGED ......: 
  Version ......: 1.00
  ---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "Qvantel".
{date.i}
{tmsconst.i}

DEFINE VARIABLE ldaBegin AS DATE NO-UNDO.
DEFINE VARIABLE ldaEnd   AS DATE NO-UNDO. 
DEFINE VARIABLE liCustNum AS INTEGER NO-UNDO.
DEFINE VARIABLE liMsSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE liEPeriod AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcline AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldapercont AS DATE NO-UNDO. 
DEFINE VARIABLE ldaffee AS DATE NO-UNDO. 
DEFINE VARIABLE liEventLogs AS INTEGER NO-UNDO. 
DEFINE VARIABLE lifoundFFees AS INTEGER NO-UNDO. 
DEFINE VARIABLE lidif_FFvsDCCLI AS INTEGER NO-UNDO. 
DEFINE VARIABLE liSFBPeriod AS INTEGER NO-UNDO.
DEFINE VARIABLE ldanow AS DATE NO-UNDO.
DEFINE VARIABLE lcnow AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcfile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcErrfile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llSimulate AS LOG NO-UNDO. 

/* DEFINE DAYS, Last searched period:
 31.5.2016 - 9.6.2016 */

ASSIGN
   ldaBegin    = 5/31/2016  /* Define this */
   ldaEnd      = 6/9/2016   /* Define this */
   llSimulate  = TRUE       /* True=Simulation, False=action */
   ldanow      = TODAY
   lcnow       = STRING(DAY(ldanow),"99") + STRING(MONTH(ldanow),"99")
               + STRING(YEAR(ldanow),"9999")
   lcfile      = "payterm_action_" + lcnow + ".log"
   lcErrfile   = "payterm_action_errors_" + lcnow + ".log".

DEF STREAM slog.
OUTPUT STREAM slog TO VALUE(lcErrfile).

DEF STREAM sout.
OUTPUT STREAM sout TO VALUE(lcfile).
PUT STREAM sout UNFORMATTED
   "MSISDN;MsSeq;DCEvent;ValidFrom;ValidTo;FFPeriod;Diff;NewValidTo;NewSinFeePer"
   SKIP.
   
DISP " -- Payterm shortening program -- " SKIP
     "This collect & sync up cases, where " SKIP
     "payterms and monthlyfees(FF) are not in sync" SKIP(1)
     "The session:" 
     IF llSimulate THEN "[SIMULATION MODE]" 
      ELSE "[ACTION MODE]" FORMAT "X(20)"
     SKIP "Begin Date.: " ldaBegin NO-LABEL SKIP
     "End Date...: " ldaEnd NO-LABEL SKIP
     "Send final report to Yoigo" WITH FRAME a.
PAUSE 0.

/* First is checked FF shortening events */
FOR EACH EventLog NO-LOCK USE-INDEX EventDate WHERE
         EventLog.EventDate >= ldaBegin AND
         Eventlog.EventDate < ldaEnd AND
         EventLog.TableName = "FixedFee" AND
         Eventlog.Usercode NE "PerCont" AND
         EventLog.Action = "Modify":

   ASSIGN
      liCustNum = INT(ENTRY(2,EventLog.Key,CHR(255)))
      liMsSeq = INT(ENTRY(4,EventLog.Key,CHR(255)))
      liEPeriod = INT(ENTRY(5,EventLog.Key,CHR(255))).

   liEventLogs = liEventLogs + 1.
   IF liEventLogs MOD 1 = 0 THEN DO:
      DISP liEventLogs lifoundFFees lidif_FFvsDCCLI WITH FRAME b.
      PAUSE 0.
   END.
   FIND FIRST Mobsub NO-LOCK WHERE
              Mobsub.MsSeq = liMsSeq NO-ERROR.
   IF NOT AVAIL MobSub THEN DO:
      PUT STREAM slog UNFORMATTED
      liMsSeq ";" liCustNum ";" liEPeriod "; MobSub was not found" SKIP.
      NEXT.
   END.

   /* FixedFee custnum must be same than current Mobsub.custnum. */
   /* FixedFee search via Eventlog information */
   FIND FixedFee NO-LOCK WHERE
        FixedFee.Brand = "1" AND
        FixedFee.CustNum = MobSub.CustNum AND
        FixedFee.HostTable = "Mobsub" AND
        FixedFee.KeyValue = STRING(liMsSeq) AND
        FixedFee.EndPeriod = liEPeriod AND
        FixedFee.billcode BEGINS "PAYTERM" AND
        FixedFee.SourceTable = "DCCLI" NO-ERROR.

   IF NOT AVAIL FixedFee THEN DO:
      PUT STREAM slog UNFORMATTED
      Mobsub.MsSeq ";" Mobsub.CLI ";" Mobsub.CustNum ";"
      liEPeriod "; FixedFee Not found" SKIP.
      NEXT.
   END.

   /* Payterm contract search with FixedFee SourceKey */
   FIND FIRST DCCLI EXCLUSIVE-LOCK  WHERE
              DCCLI.PerContractID = INT(FixedFee.SourceKey) AND
              DCCLI.MsSeq = Mobsub.MsSeq NO-ERROR.
   IF NOT AVAIL DCCLI THEN DO:
      PUT STREAM slog UNFORMATTED
       Mobsub.MsSeq ";" Mobsub.CLI ";" Mobsub.CustNum ";"  
       FixedFee.SourceKey ";" FixedFee.FFNum "; DCCLI not found"
       SKIP.
      NEXT.
   END.

   /* Skip if DCCLI contract is ended */ 
   IF AVAIL DCCLI AND DCCLI.ValidTo < TODAY THEN DO:
      PUT STREAM slog UNFORMATTED
       Mobsub.MsSeq ";" Mobsub.CLI ";" Mobsub.CustNum ";"  
       FixedFee.SourceKey ";" DCCLI.ValidTo
       "; DCCLI contract has been ended before today" SKIP.
      NEXT.
   END.

   /* Skip if DCCLI contract has been terminated */ 
   IF AVAIL DCCLI AND DCCLI.TermDate <> ? THEN DO:
      PUT STREAM slog UNFORMATTED
       Mobsub.MsSeq ";" Mobsub.CLI ";" Mobsub.CustNum ";"  
       FixedFee.SourceKey ";" DCCLI.TermDate
       "; DCCLI contract has been terminated" SKIP.
      NEXT.
   END.

   /* Check is there difference between FixedFee 
      payment months and length of Payterm contract */
   lifoundFFees = lifoundFFees + 1.

   ldapercont = fLastDayOfMonth(DCCLI.ValidTo).
   IF ldapercont EQ ? THEN NEXT.

   fTS2Date(DEC(FixedFee.EndPeriod * 100 + 1), OUTPUT ldaffee).
   ldaffee = fLastDayOfMonth(ldaffee).
   lidif_FFvsDCCLI = 0.

   lcline = Mobsub.CLI + ";" + STRING(Mobsub.MsSeq) + ";" + DCCLI.DCEvent +
            ";" + STRING(DCCLI.ValidFrom) + ";" + STRING(DCCLI.ValidTo) + ";" + 
            STRING(ldaffee).
   REPEAT:
      IF ldaffee = ldapercont THEN LEAVE.
      ELSE
         ASSIGN
            ldapercont = ADD-INTERVAL(ldapercont,-1,"months")
            ldapercont = fLastDayOfMonth(ldapercont)
            lidif_FFvsDCCLI = lidif_FFvsDCCLI + 1.
   END. /* REPEAT loop */

   /* If difference, update DCCLI.ValidTo to match FixedFee, add to report log */
   IF lidif_FFvsDCCLI >= 1 THEN DO:
      /* ValidTo population only in action mode */
      IF NOT llSimulate THEN
         DCCLI.ValidTo = ldaffee.
      lcline = lcline + ";" + STRING(lidif_FFvsDCCLI) + ";" + STRING(ldaffee).
   END.
   ELSE
      NEXT.  /* no changes */
   
   /* Search if there are Q25 Residual Value(RVTERM) SingleFees
      In case there is, update RVTERM SF to match FixedFee */
   FIND FIRST SingleFee EXCLUSIVE-LOCK  WHERE
              SingleFee.Brand = "1" AND
              SingleFee.CustNum = Mobsub.CustNum AND
              SingleFee.HostTable = "Mobsub" AND
              SingleFee.Keyvalue = STRING(Mobsub.MsSeq) AND
              SingleFee.SourceTable = "DCCLI" AND
              SingleFee.sourcekey = STRING(DCCLI.PercontractID) AND
              LOOKUP(SingleFee.Billcode, {&TF_RVTERM_BILLCODES}) > 0 NO-ERROR. 
   IF AVAIL SingleFee AND SingleFee.InvNum = 0 THEN DO:
         liSFBPeriod = YEAR(ldaffee + 1) * 100 + MONTH(ldaffee + 1).
         /* BillPeriod population only in action mode */
         IF NOT llSimulate THEN DO:
            SingleFee.BillPeriod = liSFBPeriod.
            SingleFee.Concerns[1] = YEAR(ldaffee + 1) * 10000 +
                                    MONTH(ldaffee + 1) * 100  + 
                                    DAY(ldaffee + 1).
         END. 
         PUT STREAM sout UNFORMATTED
            lcline ";" liSFBPeriod /* SingleFee.Billcode */ SKIP.
   END.
   ELSE PUT STREAM sout UNFORMATTED lcline SKIP.
END.

