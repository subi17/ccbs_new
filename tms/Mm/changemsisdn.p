/* ----------------------------------------------------------------------
  MODULE .......: changemsisdn.p
  TASK .........: Change MSISDN Numbers
  APPLICATION ..: 
  AUTHOR .......: jp
  CREATED ......: 
  CHANGED ......: 16.08.07/aam clean eventlog
                  27.08.07/aam move balances (custbal)
  Version ......: 
  ---------------------------------------------------------------------- */


{Syst/commali.i}
{Func/msisdn.i}
{Func/fcustbal.i}
{Func/msreqfunc.i}
{Syst/eventval.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/create_eventlog.i}

DEF INPUT  PARAMETER iiMSrequest AS INT  NO-UNDO.

DEF VAR ldBalance  AS DEC  NO-UNDO.
DEF VAR lcBalTypes AS CHAR NO-UNDO.
DEF VAR liCnt      AS INT  NO-UNDO.
DEF VAR liLogType  AS INT  NO-UNDO.
DEF VAR lcResponse AS CHAR NO-UNDO. 
DEF VAR lcResult   AS CHAR NO-UNDO. 
DEF VAR liReq AS INTEGER NO-UNDO. 

DEF BUFFER new-MSISDN  FOR MSISDN.
DEF BUFFER Old-Msowner FOR Msowner.

FIND FIRST MSRequest WHERE
           MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 19 THEN DO:
   RETURN "ERROR".
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMobSub AS HANDLE NO-UNDO.
   lhMobSub = BUFFER MobSub:HANDLE.
   RUN StarEventInitialize(lhMobSub).

   DEFINE VARIABLE lhMSOwner AS HANDLE NO-UNDO.
   lhMSOwner = BUFFER Old-MSOwner:HANDLE.
   RUN StarEventInitialize(lhMSOwner).

   DEFINE VARIABLE lhMSISDN AS HANDLE NO-UNDO.
   lhMSISDN = BUFFER MSISDN:HANDLE.
   RUN StarEventInitialize(lhMSISDN).
 
   DEFINE VARIABLE lhDCCLI AS HANDLE NO-UNDO.
   lhDCCLI = BUFFER DCCLI:HANDLE.
   RUN StarEventInitialize(lhDCCLI).
END.

RUN pChangeMSISDN.

fCleanEventObjects().

RETURN RETURN-VALUE.


PROCEDURE pChangeMSISDN:

   DEF VAR liArrivalStatus AS INT  NO-UNDO.
   
   liArrivalStatus = MsRequest.ReqStatus.
   
   IF LOOKUP(STRING(liArrivalStatus),"6,8") = 0 THEN RETURN.
   
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN DO:
      RETURN "ERROR".
   END.

   FIND FIRST new-MSISDN WHERE
              new-MSISDN.CLI = MSRequest.ReqCParam2  AND 
              new-MSISDN.ValidTo > fMakeTS() No-LOCK NO-ERROR.

   IF NOT AVAIL new-MSISDN THEN DO:
      fReqError("Unknown new msisdn number:" +  MSRequest.ReqCParam2).
      RETURN.
   END.

   IF New-MSISDN.StatusCode NE 2 AND 
      New-MSISDN.StatusCode NE 27 THEN DO:
      fReqError("MSISDN Change not allowed, Wrong statusCode for MSISDN:"  +
                MSRequest.ReqCParam2).
      RETURN.
   ENd.

   FIND MobSub WHERE 
        MobSub.MsSeq = MSRequest.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN DO:
      fReqError("Subscription not available").
      RETURN.
   END.

   FIND CURRENT MobSub EXCLUSIVE-LOCK.

   FIND FIRST Old-MSOWner WHERE 
              Old-MSOWNER.MSSEQ  = Mobsub.MSSeq AND 
              Old-MSOWNER.CLI    = Mobsub.CLI   AND 
              Old-MSOwner.TSEND >= FMakeTS() EXCLUSIVE-LOCK NO-ERROR.

   FIND FIRST MSISDN WHERE 
              MSISDN.CLI  = MobSub.CLI AND 
              MSISDN.ValidTo > fMakeTS() EXCLUSIVE-LOCK NO-ERROR.

   FIND FIRST MSRange WHERE 
              MSRange.Brand    = gcBrand    AND 
              MSRange.CLIFrom <= MSISDN.CLI AND
              MSRange.CLITo   >= MSISDN.CLI
   NO-LOCK NO-ERROR.


   /* What Happens TO old msowner */
   IF AVAIL Old-MSOwner THEN DO:
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsOwner).
      ASSIGN Old-MSOwner.TsEnd = FMakeTS().
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsOwner).
   END.
  
   /* What happens TO the old MSISDN */

   fMakeMsidnHistory(INPUT RECID(MSISDN)).

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSISDN).

   IF  AVAIL MSRange THEN DO:
      ASSIGN
         MSISDN.StatusCode = 4
         MSISDN.CustNum    = MSrange.Custnum
         MSISDN.ACtionDate = TOday.
   END.   
 
   ELSE IF NOT AVAIL MSRange THEN DO:
      ASSIGN
         MSISDN.CustNum    = 0
         MSISDN.StatusCode = 2
         MSISDN.ActionDate = Today.
   END.

   /* Creation of new MSOwner      **/

   CREATE MSOwner.
   BUFFER-COPY old-Msowner except CLI CLIEvent TO Msowner.
   ASSIGN
      MSOwner.TsBegin     = FMakeTS()
      MSOwner.CLI         = new-MSISDN.CLI
      MSOwner.CustNum     = Mobsub.CustNum
      MSOwner.BillTarget  = Mobsub.BillTarget
      MSOwner.MsSeq       = MobSub.MsSeq 
      MSowner.imsi        = mobsub.imsi
      MSowner.brand       = gcBrand 
      MSOWner.TSEnd       = 99999999.99999
      MsOwner.CliType     = mobsub.clitype
      MsOwner.CLIEvent    = "CLI".
   
   IF llDoEvent THEN fMakeCreateEvent((BUFFER MsOwner:HANDLE),
                                      "",
                                      katun,
                                      "").

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobSub).
   Mobsub.Cli = MSowner.CLI .
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobSub).

   /* move balances */
   lcBalTypes = "DP,OP,AP,REF".

   DO liCnt = 1 TO NUM-ENTRIES(lcBalTypes):

       ldBalance = fGetCustBal(MsOwner.InvCust,
                              old-MsOwner.CLI,
                              ENTRY(liCnt,lcBalTypes)).
                           
      IF ldBalance = 0 THEN NEXT.
   
      CASE ENTRY(liCnt,lcBalTypes):
      WHEN "DP"  THEN liLogType = 24.
      WHEN "OP"  THEN liLogType = 25.
      WHEN "AP"  THEN liLogType = 26.
      WHEN "REF" THEN liLogType = 28.
      END CASE.
   
      /* deduct from old msisdn, add to new */
      fCustBal(MsOwner.InvCust,
               old-MsOwner.CLI,
               ENTRY(liCnt,lcBalTypes),
               -1 * ldBalance).

      CREATE OPLog.
      ASSIGN
         OPLog.CreStamp  = fMakeTS()
         OPLog.CustNum   = MsOwner.InvCust
         OPLog.EventDate = TODAY
         OPLog.UserCode  = katun
         OPLog.EventType = liLogType 
         OPLog.InvNum    = 0
         OPLog.Voucher   = 0
         OPLog.Amt       = -1 * ldBalance
         OPLog.Info      = old-MsOwner.CLI.
 
      fCustBal(MsOwner.InvCust,
               MsOwner.CLI,
               ENTRY(liCnt,lcBalTypes),
               ldBalance).

      CREATE OPLog.
      ASSIGN
         OPLog.CreStamp  = fMakeTS()
         OPLog.CustNum   = MsOwner.InvCust
         OPLog.EventDate = TODAY
         OPLog.UserCode  = katun
         OPLog.EventType = liLogType
         OPLog.InvNum    = 0
         OPLog.Voucher   = 0
         OPLog.Amt       = ldBalance
         OPLog.Info      = MsOwner.CLI.
   END.

   /* Finalize new msisdn */ 
   
   fMakeMsidnHistory(INPUT RECID(new-MSISDN)).

   ASSIGN
      MSISDN.CustNum    = Msowner.CustNum
      Msisdn.StatusCode = 3
      MSISDN.ActionDate = Today.

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSISDN).
   
   IF MsRequest.CreateFees THEN 
         RUN Mm/create_charge_comp.p(
            {&REQUEST_SOURCE_MANUAL_TMS},
            Mobsub.MsSeq,   
            MsRequest.UserCode,
            MsRequest.ReqDParam2,  
            (IF MobSub.PayType THEN "MSISDN_PREPAID" ELSE "MSISDN_POSTPAID"),
            MsRequest.MsRequest, 
            OUTPUT liReq) NO-ERROR.

   /* Update periodical contract cli */
   FOR EACH DCCLI WHERE 
      DCCLI.MsSeq = MobSub.MsSeq AND 
      DCCLI.ValidTo > TODAY AND
      DCCLI.CLI NE Mobsub.CLI EXCLUSIVE-LOCK:
      
         IF llDoEvent THEN RUN StarEventSetOldBuffer ( lhDCCLI ).
         DCCLI.CLI = Mobsub.CLI. 
         IF llDoEvent THEN RUN StarEventMakeModifyEvent ( lhDCCLI ).
      
         CREATE Memo.
         ASSIGN
            Memo.CreStamp  = fMakeTs() 
            Memo.Brand     = gcBrand                 
            Memo.HostTable = "MobSub"                
            Memo.KeyValue  = STRING(DCCLI.MsSeq)     
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CreUser   = katun 
         Memo.Memotitle = "Per.Contract Msisdn Changed" 
         Memo.MemoText  = "Periodical contract msisdn was changed due to " +
                          "subscription msisdn change.".
   
      RELEASE Memo.
   END.

   /* Activate the HSDPA with new msisdn, if it was activated before */
   IF MobSub.CLIType BEGINS "TARJ" THEN
   FOR FIRST SubSer NO-LOCK WHERE
             SubSer.MsSeq = MobSub.MsSeq AND
             SubSer.ServCom = "HSDPA" AND
             SubSer.SSDate <= TODAY:

      IF Subser.SSStat > 0 THEN DO:
         IF fServiceRequest (MsRequest.MsSeq,     
                             "HSDPA",
                             1,
                             "",
                             fMakeTS(),
                             "",                /* SalesMan */ 
                             FALSE,             /* fees */
                             FALSE,             /* SMS */ 
                             "",
                             {&REQUEST_SOURCE_SCRIPT},
                             MsRequest.MsRequest,
                             FALSE,
                             OUTPUT lcResult) = 0 THEN DO:
            fReqError("HSDPA reactivation failed:" + lcResult).
            RETURN "ERROR".
         END. /* IF fServiceRequest (MsRequest.MsSeq */
      END. /* IF Subser.SSStat > 0 THEN DO: */
   END. /* FOR FIRST SubSer NO-LOCK WHERE */
 
   /* request handled succesfully */
   fReqStatus(2,""). 

   RETURN "".
   
END PROCEDURE.


