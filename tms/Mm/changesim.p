/* ----------------------------------------------------------------------
  MODULE .......: changesim.p
  TASK .........: Change SIM Numbers
  APPLICATION ..: 
  AUTHOR .......: jp
  CREATED ......: 
  CHANGED ......: 31.01.07 kl MsRequest.ReqStatus = 2
                  16.08.07/aam clean eventlog
  Version ......: 
  ---------------------------------------------------------------------- */


{Syst/commali.i}
{Func/msisdn.i}
{Func/msreqfunc.i}
{Syst/eventval.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/create_eventlog.i}

DEF INPUT  PARAMETER  iiMSrequest AS INT  NO-UNDO.

DEF VAR lcResponse AS CHAR NO-UNDO.
DEF VAR liChargeReqId AS INT NO-UNDO.

DEF BUFFER Old-Msowner FOR Msowner.
DEF BUFFER new-SIM     FOR SIM.

FIND FIRST MSRequest WHERE
           MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.

IF NOT AVAIL MSRequest OR MsRequest.ReqType NE 15 THEN DO:
   RETURN "ERROR:Unknown MSRequest " + STRING(iiMSRequest).
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
 
   DEFINE VARIABLE lhSIM AS HANDLE NO-UNDO.
   lhSIM = BUFFER SIM:HANDLE.
   RUN StarEventInitialize(lhSIM).
END.

RUN pChangeSIM.

fCleanEventObjects().

RETURN RETURN-VALUE.


PROCEDURE pChangeSIM:

   DEF BUFFER bSubMsRequest FOR MsRequest.

   DEF VAR liRequest        AS INT  NO-UNDO.

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN DO:
      RETURN "ERROR".
   END.

   FIND FIRST new-SIM WHERE
              new-SIM.ICC = MSRequest.ReqCParam2 No-LOCK NO-ERROR.

   IF NOT AVAIL new-SIM THEN DO:
      fReqError("Unknown SIM card:" +  MSRequest.ReqCParam2).
      RETURN.
   END.
                            
   FIND FIRST Imsi WHERE 
              Imsi.ICC = NEW-SIM.ICC NO-LOCK NO-ERROR.
           
   IF NOT AVAIL IMSI THEN DO:
      fReqError("Missing IMSI on ICC:" +  MSRequest.ReqCParam2).
      RETURN.
   END.

   FIND MobSub WHERE
        MobSub.MsSeq = MSRequest.MsSeq       EXCLUSIVE-LOCK NO-ERROr.

   IF NOT AVAIL MobSub THEN DO:
      fReqError("Unknown subscription:" +  STRING(MSRequest.Msseq)).
      RETURN.
   END.

   FIND FIRST SIM WHERE
              SIM.ICC  = MobSub.ICC EXCLUSIVE-LOCK NO-ERROR.
           
   IF NOT AVAIL SIM THEN DO:
      fReqError("Unknown SIM card:" +  Mobsub.ICC).
      RETURN.
   END.

   FIND FIRST Old-MSOWner WHERE 
              Old-MSOWNER.MSSEQ  = Mobsub.MSSeq AND 
              Old-MSOWNER.CLI    = Mobsub.CLI   AND 
              Old-MSOwner.TSEND >= FMakeTS() EXCLUSIVE-LOCK NO-ERROR.
            
   IF NOT AVAIL old-Msowner THEN DO:
      FIND FIRST Old-MSOWner WHERE
                 Old-MSOWNER.MSSEQ  = Mobsub.MSSeq AND
                 Old-MSOWNER.CLI    = Mobsub.CLI 
      EXCLUSIVE-LOCK NO-ERROR.           
   END.

   /* What Happens TO old msowner */

   IF AVAIL Old-MSOwner THEN DO:
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsOwner).
      ASSIGN Old-MSOwner.TsEnd = FMakeTS().
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsOwner).
   END.
   
   /* What happens TO the old SIM */

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSIM).
   Sim.SimStat = 14.
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSIM).

   /* Creation of new MSOwner      **/
   CREATE MSOwner.
   BUFFER-COPY old-Msowner except TSend CLIEvent TO Msowner.
   ASSIGN
      MSOwner.TsBegin     = fSecOffSet(Old-MSOwner.TsEnd,1)
      MSowner.imsi        = IMSI.imsi
      MSOWner.TSEnd       = 99999999.99999
      MsOwner.CLIEvent    = "ICC".
   
   IF llDoEvent THEN fMakeCreateEvent((BUFFER MsOwner:HANDLE),
                                      "",
                                      katun,
                                      "").

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobsub).
   
   /* Activate new sim to mobsub */ 
   ASSIGN 
      Mobsub.Imsi = IMSI.IMSI
      Mobsub.ICC  = IMSI.ICC.

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobsub).

   /* Status for new-sim - delivery process handling?*/
 
   IF MsRequest.CreateFees THEN DO:
      
      RUN create_charge_comp.p(
         {&REQUEST_SOURCE_MANUAL_TMS},
         Mobsub.MsSeq,   
         MsRequest.UserCode,
         MsRequest.ReqDParam2,  
         "ICC_" + (IF MobSub.PayType THEN "PREPAID" ELSE "POSTPAID"),
         MsRequest.MsRequest, 
         OUTPUT liChargeReqId) NO-ERROR.

      IF ERROR-STATUS:ERROR OR liChargeReqId = 0 THEN
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.CustNum,
                          "CHARGE CREATION FAILED",
                          STRING(RETURN-VALUE)).

   END.

   /* Activate the BB with new SIM, if it was activated before */
   FOR EACH bSubMsRequest WHERE
            bSubMsRequest.OrigRequest = MsRequest.MsRequest NO-LOCK:
      /* BlackBerry Re-provision */
      IF bSubMsRequest.ReqType    = {&REQTYPE_SERVICE_CHANGE} AND
         bSubMsRequest.ReqCParam1 = "BB" AND
         bSubMsRequest.ReqIParam1 = 0    AND
         bSubMsRequest.ReqStatus  = {&REQUEST_STATUS_DONE} THEN DO:
         FIND FIRST SubSer NO-LOCK WHERE
                    SubSer.MsSeq   = MobSub.MsSeq AND
                    SubSer.ServCom = "BB" AND
                    SubSer.SSDate <= TODAY NO-ERROR.
         IF AVAILABLE SubSer AND Subser.SSStat = 0 THEN DO:
            liRequest = fServiceRequest(MobSub.MsSeq,
                                        SubSer.ServCom,
                                        1,     /* ON */
                                        "",
                                        fMakeTS(),
                                        "",
                                        TRUE, /* fees */
                                        FALSE, /* sms */
                                        "",
                                        MsRequest.ReqSource,
                                        MsRequest.MsRequest,
                                        FALSE,
                                        OUTPUT lcResponse).
            /* Write possible error to a memo */
            IF liRequest = 0 THEN
             DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                              "MobSub",
                              STRING(Mobsub.MsSeq),
                              Mobsub.CustNum,
                              "BB Service Re-provision",
                              "Service " + bSubMsRequest.ReqCParam1 +
                              " request failed " + lcResponse).
         END. /* IF AVAILABLE SubSer AND Subser.SSStat = 2 THEN DO: */
      END. /* IF bSubMsRequest.ReqCParam1 = "BB" AND */
   END. /* FOR EACH bSubMsRequest WHERE */
         
   /* YDR-1561 */
   FIND FIRST SubSer NO-LOCK WHERE
              SubSer.MsSeq   = MobSub.MsSeq AND
              SubSer.ServCom = "BPSUB" NO-ERROR.

   IF AVAIL SubSer AND
      SubSer.SSStat EQ 1 THEN DO:

      liRequest = fServiceRequest(MobSub.MsSeq,
                                  SubSer.ServCom,
                                  1,     /* ON */
                                  "",
                                  fMakeTS(),
                                  "",
                                  TRUE, /* fees */
                                  FALSE, /* sms */
                                  "",
                                  MsRequest.ReqSource,
                                  MsRequest.MsRequest,
                                  FALSE,
                                  OUTPUT lcResponse).
      /* Write possible error to a memo */
      IF liRequest = 0 THEN
       DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                        "MobSub",
                        STRING(Mobsub.MsSeq),
                        Mobsub.CustNum,
                        Subser.Servcom + " service Re-provision",
                        "Service " + SubSer.ServCom +
                        " (" + SubSer.SSPAram +
                        ") request failed " + lcResponse).
   END.

   /* request handled succesfully */
   fReqStatus(2,""). 

   RETURN "".
   
END PROCEDURE.





