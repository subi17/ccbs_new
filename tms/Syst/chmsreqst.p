/* ----------------------------------------------------------------------
  MODULE .......: chmsreqst.p
  TASK .........: /* Change MsRequest status */
  APPLICATION ..: TMS
  AUTHOR .......: jukka 
  CREATED ......: 18.09.07
  CHANGED ......: 16.11.07/aam question to user
                  22.11.07/as  reqtype 18 and 19 cancel handling
  Version ......: xfera
----------------------------------------------------------------------- */
{Func/msreqfunc.i}
{Func/msisdn.i}
{Func/fuserright.i}
{Syst/eventval.i}
{Func/forderstamp.i}
{Func/fmakemsreq.i}
{Func/fmakesms.i}
{Func/fsendsms.i}
{Func/dss_matrix.i}
{Func/fsubstermreq.i}
{Func/add_lines_request.i}
{Func/fixedlinefunc.i}

DEFINE INPUT PARAMETER iiMsRequest  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iiFromStatus AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iiToStatus   AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER icCparam     AS CHARACTER NO-UNDO.

DEF VAR lcError            AS CHARACTER    NO-UNDO.
DEF VAR lcMessage          AS CHARACTER    NO-UNDO.
DEF VAR lcCancelReason     AS CHARACTER    NO-UNDO FORMAT "x(60)".
DEF VAR llOk               AS LOGICAL      NO-UNDO.
DEF VAR llCanUse           AS LOGICAL      NO-UNDO.
DEF VAR lcErrorMessage     AS CHAR         NO-UNDO.
DEF VAR lcDoneMessage      AS CHAR         NO-UNDO.
DEF VAR ldeSMSTime         AS DECIMAL      NO-UNDO. 
DEF VAR lcSender           AS CHARACTER    NO-UNDO. 
DEF VAR liQuarTime         AS INT          NO-UNDO.
DEF VAR liSimStat          AS INT          NO-UNDO.
DEF VAR liMSISDNStat       AS INT          NO-UNDO.
DEF VAR liRequest          AS INT          NO-UNDO.
DEF VAR ldaSecSIMTermDate  AS DATE         NO-UNDO.
DEF VAR liSecSIMTermTime   AS INT          NO-UNDO.
DEF VAR ldeSecSIMTermStamp AS DEC          NO-UNDO.
DEF VAR llgAddSIMTerm      AS LOG          NO-UNDO.
DEF VAR ldActStamp         AS DECIMAL      NO-UNDO.
DEF VAR ldtTdDate          AS DATE         NO-UNDO.
DEF VAR ldeActStamp        AS DEC NO-UNDO. 

DEF BUFFER lbMobSub        FOR Mobsub.
DEF BUFFER bMsRequest      FOR MsRequest.

DEF TEMP-TABLE ttAdditionalSIM NO-UNDO
    FIELD MsSeq    AS INT
    FIELD CustNum  AS INT
    FIELD CLI      AS CHAR.
    
FUNCTION fUpdStatus RETURNS LOGICAL
    (INPUT iTo           AS INT,
     INPUT cMessage      AS CHAR,
     OUTPUT cRMessage AS CHAR).
  
    cRMessage = lcDoneMessage.

    IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                      MsRequest.MsRequest EQ iiMsRequest AND
                      MsRequest.ReqStatus NE iiFromStatus) THEN
       cRMessage = "Error occured during update, status changed before update!".
    ELSE
    /* Update request status with input parameter and MEMO message */
    IF NOT fReqStatus(iTo,cMessage) THEN 
       cRMessage = lcErrorMessage.

END FUNCTION.

ASSIGN
   lcCancelReason = ""
   lcDoneMessage  = "Request status changed from " + 
                    STRING(iiFromStatus)          +
                    " to "                        + 
                    STRING(iiToStatus)            + 
                    " succesfully!"
   lcErrorMessage = "Error occured during update, request was locked!".

/* eventlog not needed here, msrequest.p takes care of it */

/* Get request to buffer */
FIND MsRequest WHERE
     MsRequest.MsRequest = iiMsRequest
NO-LOCK NO-ERROR.

/* user rights */
llCanUse = TRUE.

CASE MsRequest.ReqType:

WHEN {&REQTYPE_CONTRACT_ACTIVATION} THEN DO:

   /* YOT-539 */
   IF MsRequest.ReqStatus = {&REQUEST_STATUS_REJECTED} AND
      iiToStatus EQ {&REQUEST_STATUS_NEW} THEN DO:
      
      IF MSRequest.ReqCParam3 NE "PMDUB" THEN DO:
         MESSAGE {&MSG_NOT_ALLOWED} VIEW-AS ALERT-BOX INFORMATION.
         RETURN.
      END.
   END.
END.

WHEN 23 THEN DO:
   IF MsRequest.ReqStat = 19 THEN
      llCanUse = (fTokenRights(Syst.Var:katun,"CCSUPER") = "RW").
      
   IF iiToStatus = 4 OR iiToStatus = 9 THEN DO:
     IF LOOKUP(STRING(MsRequest.ReqStat),"0,3,16,19") = 0 THEN 
        RETURN "ERROR:Request status doesn't allow cancelling".
   END.    
END.

END CASE.

IF NOT llCanUse THEN DO:
   MESSAGE "You are not authorized to use this function"
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.

/* YPR-112 */
If MsRequest.ReqStat EQ {&REQUEST_STATUS_REJECTED} AND
   iiToStatus EQ {&REQUEST_STATUS_NEW} AND
   LOOKUP(STRING(MsRequest.ReqType),"0,1,13,15,18,19") > 0 AND
   INDEX(MsRequest.Memo,"Rollback failed") > 0 THEN DO:

   MESSAGE "Relaunch not allowed, network command rollback failed"
   VIEW-AS ALERT-BOX.
   RETURN.
END.

/* Confirmation */
llOk = FALSE.
MESSAGE "Change status of the request to" 
       STRING(iiToStatus) +  "?" 
VIEW-AS ALERT-BOX QUESTION
BUTTONS YES-NO
SET llOk.
   
IF NOT llOk THEN DO:
   RETURN.
END.

DO TRANSACTION:
   IF MsRequest.ReqType = 0 AND (iiToStatus = 4 OR iiToStatus = 9) THEN DO:
    
      /* cancel possible renewal pos stc order */
      FIND FIRST Order WHERE
           Order.MsSeq = MsRequest.MsSeq AND
           LOOKUP(Order.OrderChannel,"renewal_pos_stc,retention_stc") > 0 AND
           LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES} + ",12") = 0 NO-LOCK NO-ERROR.
         
      IF AVAIL Order THEN DO:
         
         RUN Mc/closeorder.p(Order.OrderId,TRUE).
      
         IF RETURN-VALUE NE "" THEN DO:
            MESSAGE "Error with renewal order closing:" 
            RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
            UNDO, RETURN.
         END.
         
         CREATE Memo.
         ASSIGN
            Memo.CreStamp  = Func.Common:mMakeTS() 
            Memo.Brand     = Syst.Var:gcBrand
            Memo.HostTable = "Order"
            Memo.KeyValue  = STRING(Order.OrderId)
            Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
            Memo.CreUser   = Syst.Var:katun
            Memo.MemoTitle = "Cancelled due to STC cancellation"
            Memo.MemoText  = "".

      END.
   END.

   CASE iiToStatus:
      
      /* Different actions may require different exceptions per type */

      WHEN 4 THEN DO: /* Cancellation */

         /* STC cancellation */
         IF MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} THEN DO:
            llOk = TRUE.
            MESSAGE "Send cancellation SMS to customer?" 
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            SET llOk.

            IF llOk THEN DO:

               FIND Customer WHERE
                    Customer.Custnum = MsRequest.Custnum NO-LOCK NO-ERROR.

               lcSMSText = fGetSMSTxt("STC_Cancelled",
                                      TODAY,
                                      (IF AVAIL Customer 
                                       THEN Customer.Language ELSE 1),
                                      OUTPUT ldeSMSTime).

               /* both to agreement cust and user */
               IF lcSMSText > "" THEN
                  fMakeSchedSMS2(MsRequest.CustNum,
                                 MsRequest.CLI,
                                 {&SMSTYPE_INFO},
                                 lcSMSText,
                                 ldeSMSTime,
                                 {&STC_SMS_SENDER},
                                 "").
            END.
            
            FIND FIRST MobSub NO-LOCK WHERE 
                       MobSub.Brand = Syst.Var:gcBrand AND
                       MobSub.MsSeq = MsRequest.MsSeq NO-ERROR.

            IF CAN-FIND(
                   FIRST CLIType NO-LOCK WHERE
                         CLIType.Brand    = Syst.Var:gcBrand                         AND
                         CLIType.CLIType  = (IF MobSub.TariffBundle > ""
                                                THEN MobSub.TariffBundle
                                             ELSE MobSub.CLIType)           AND
                         CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL}) AND
               CAN-FIND(
                   FIRST CLIType NO-LOCK WHERE
                         CLIType.Brand   = Syst.Var:gcBrand                       AND
                         CLIType.CLIType = (IF MsRequest.ReqCParam5 > ""
                                               THEN MsRequest.ReqCParam5
                                            ELSE MsRequest.ReqCParam2)   AND
                         CLIType.LineType = {&CLITYPE_LINETYPE_MAIN})    THEN  
               RUN ipMulitSIMTermination(MsRequest.ReqType).
            

            fAddLineSTCCancellation(MsRequest.MsRequest, MsRequest.CustNum).  
                          
         END.

         IF MsRequest.ReqType = 15 THEN DO:
            
            llOk = NO.

            MESSAGE "Release SIM card" Msrequest.ReqCParam2 SKIP
                    "which was assigned to this request?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            TITLE " SIM "
            UPDATE llOk.

            FIND FIRST sim EXCLUSIVE-LOCK WHERE
                       sim.icc = Msrequest.ReqCParam2 NO-ERROR.
            
            IF llOk THEN DO:
               
               IF NOT AVAIL sim THEN DO:
                  MESSAGE "Couldn't release SIM" SKIP
                          "RELEASE ICC: " Msrequest.ReqCParam2 " manually!"
                  VIEW-AS ALERT-BOX.
               END.
               ELSE DO:

                  FIND FIRST Order NO-LOCK USE-INDEX MsSeq WHERE
                             Order.MsSeq = MsRequest.MsSeq AND
                             Order.ICC = SIM.ICC AND
                             Order.OrderType = 2 AND
                             INDEX(Order.OrderChannel,"pos") = 0 NO-ERROR.
                  IF AVAIL Order THEN sim.simstat = 7.
                  ELSE sim.simstat = 1.

                  FIND CURRENT SIM NO-LOCK.

                  IF SIM.SimStat EQ 1 THEN DO:
                     MESSAGE "SIM card set back to available status"
                     VIEW-AS ALERT-BOX
                     TITLE " SIM RELEASED ".
                  END.
                  ELSE DO:
                     MESSAGE "SIM card set to status" sim.simstat
                     VIEW-AS ALERT-BOX
                     TITLE " SIM RELEASED ".
                  END.
               END.

            END.
            ELSE DO:
               IF AVAIL sim THEN sim.simstat = 9.
            END.
            
            RELEASE SIM.
         END.
         
         ELSE IF MsRequest.ReqType EQ 18 THEN DO:

            IF MsRequest.ReqCParam3 EQ "2" THEN DO:
               
               MESSAGE
                  "Termination was ordered by MNP outporting"           SKIP 
                  "process. MNP process cancellation has to be"         SKIP 
                  "accepted before request cancellation."               SKIP
                  SKIP
                  "Do you want to continue Termination cancellation."   
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               TITLE " Termination Cancellation "
               UPDATE llOk.
               
               IF NOT llOk THEN UNDO, RETURN.
            END.

            IF iiFromStatus EQ {&REQUEST_STATUS_CONFIRMATION_PENDING} AND
               NOT fCanTerminateConvergenceTariff(
                     MsRequest.MsSeq,
                     INT(MsRequest.ReqCParam3),
                     OUTPUT lcError) THEN DO:
               MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
               UNDO, RETURN.
            END.
         
            fAddLineSTCCancellation(MsRequest.MsRequest, MsRequest.CustNum).
         END.
         
         /* msisdn change cancel */
         ELSE IF MsRequest.ReqType EQ 19 THEN DO:
            
            DEFINE BUFFER msisdn-back FOR MSISDN.
            DEFINE VARIABLE msisdn-recid AS RECID NO-UNDO.
            
            FIND FIRST MSISDN-back NO-LOCK WHERE
               MSISDN-back.Brand = Syst.Var:gcBrand AND
               MSISDN-back.CLI   = MsRequest.ReqCParam2 USE-INDEX CLI.
            IF MSISDN-back.StatusCode = 27 THEN DO:
               msisdn-recid = recid(msisdn-back).
               FIND NEXT MSISDN-back.
               fMakeMSIDNHistory(msisdn-recid).
               BUFFER-COPY msisdn-back 
                  EXCEPT validfrom validto actiondate
                  TO msisdn.
               msisdn.statuscode = 4.   
            END.
         
         END. 

         /* credit note */
         ELSE IF MsRequest.ReqType EQ 22 THEN DO:
            
            find invoice where
                 invoice.invnum = msrequest.reqiparam1 NO-LOCK no-error.
            
            IF AVAIL invoice then do:
               
               FIND FIRST Order WHERE
                    order.invnum = invoice.invnum NO-LOCK no-error.
               IF AVAIL order and 
                  lookup(order.statuscode,{&ORDER_CLOSE_STATUSES}) > 0 then do:

                  MESSAGE "Order is closed. Cancellation is prohibited"
                  VIEW-AS ALERT-BOX ERROR.
                  UNDO, RETURN.

               END.
            END.
         END. 

         /* Cancel - Terminate DSS Request */
         ELSE IF MsRequest.ReqType EQ {&REQTYPE_DSS}   AND
                 MsRequest.ReqCparam1 EQ "DELETE"      AND
                 iiFromStatus EQ {&REQUEST_STATUS_NEW} AND
                 fIsDSSActive(INPUT MsRequest.CustNum,INPUT Func.Common:mMakeTS())
         THEN DO:

            IF NOT CAN-FIND(FIRST MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq)
            THEN DO:
               MESSAGE
                  "DSS termination request can not be cancelled because " SKIP
                  "subscription " + MsRequest.CLI + " is already terminated."
                  VIEW-AS ALERT-BOX.
               UNDO, RETURN.
            END. /* IF CAN-FIND(FIRST MobSub WHERE MobSub.MsSeq */

            MESSAGE
               "Do you want to keep DSS active to the customer: " +
               STRING(MsRequest.CustNum)
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               TITLE " DSS Termination Cancellation "
               UPDATE llOk.
            
            IF NOT llOk THEN UNDO, RETURN.

            IF NOT fCanDSSKeepActive(INPUT MsRequest.CustNum,
                                     INPUT 0, /* don't exclude current subs */
                                     Func.Common:mMakeTS(),
                                     INPUT MsRequest.ReqCparam3,
                                     OUTPUT lcError) THEN DO:
               MESSAGE
                  "DSS termination request can not be cancelled. " + lcError
                  VIEW-AS ALERT-BOX.
               UNDO, RETURN.
            END. /* IF NOT fCanDSSKeepActive(INPUT MsRequest.CustNum */
         END. /* ELSE IF MsRequest.ReqType EQ {&REQTYPE_DSS} AND */

         ELSE IF MsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE} THEN DO:

            /* Additional SIM Termination logic */
            IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                              CLIType.Brand    = Syst.Var:gcBrand                         AND
                              CLIType.CLIType  = MsRequest.ReqCparam1            AND
                              CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL}) AND
               CAN-FIND(FIRST CLIType NO-LOCK WHERE
                              CLIType.Brand    = Syst.Var:gcBrand                   AND
                              CLIType.CLIType  = MsRequest.ReqCparam2      AND
                              CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN DO:
               
               llgAddSIMTerm = fAdditionalSimTermination(MsRequest.MsSeq,
                                                         {&REQUEST_SOURCE_MANUAL_TMS}).
                                                        
               IF NOT llgAddSIMTerm THEN 
                  MESSAGE "Additional SIM Termination request can not be cancelled."
                  VIEW-AS ALERT-BOX.
            END.
                        
         END.

         UPDATE lcCancelReason
         WITH TITLE " CANCELLATION REASON "
            CENTERED ROW 10
            OVERLAY NO-LABELS
            FRAME formReqCancel.

         HIDE FRAME formReqCancel.
         
         /* send acc rejection message to customer */
         IF MsRequest.ReqType = 10 AND MsRequest.ReqIParam3 NE 1 THEN DO:

            llOk = TRUE.
            MESSAGE "Send cancellation SMS to customer?" 
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            SET llOk.

            IF llOk THEN 
               RUN Mm/acc_sendsms.p(MsRequest.MsRequest,
                               MsRequest.CustNum,
                               "Cancelled",
                               lcCancelReason).
         END.

         /* Send cancellation message to customer if 
            DSS activation request was in queue */
         IF MsRequest.ReqType EQ {&REQTYPE_DSS}   AND
            MsRequest.ReqCparam1 EQ "CREATE"      AND
            iiFromStatus EQ 19 THEN DO:

            llOk = TRUE.
            MESSAGE "Send cancellation SMS to customer?" 
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            SET llOk.

            IF llOk THEN
               RUN pSendSMS(INPUT MsRequest.MsSeq, INPUT 0, INPUT "DSSActCancel",
                            INPUT 9, INPUT "622", INPUT "").
         END. /* IF MsRequest.ReqType EQ {&REQTYPE_DSS} */
         
      END. 
      WHEN 9 THEN 
         IF MsRequest.ReqType = {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE} THEN 
            RUN ipMulitSIMTermination(MsRequest.ReqType).
      
   END CASE.

   IF MsRequest.ReqType = {&REQTYPE_ACTIVATE_EMAIL_INVOICE} AND
      LOOKUP(STRING(iiToStatus),"4,9") > 0 THEN DO:

      FIND FIRST Customer NO-LOCK WHERE
                 Customer.Custnum EQ MsRequest.Custnum NO-ERROR.

      IF AVAIL Customer AND
               Customer.DelType EQ {&INV_DEL_TYPE_EMAIL_PENDING} THEN DO:
         FIND CURRENT Customer EXCLUSIVE-LOCK.
         Customer.DelType = {&INV_DEL_TYPE_ESI}.
         RELEASE Customer.
      END.
   END.

   /* refund */
   IF MsRequest.ReqType = 23 AND LOOKUP(STRING(iiToStatus),"4,9") > 0 THEN DO:
    
      RUN Ar/refundcancel.p(MsRequest.MsRequest,
                       "AP",
                       0,
                       MsRequest.CustNum,
                       lcCancelReason,
                       iiToStatus).
    
      IF RETURN-VALUE BEGINS "ERROR" 
      THEN lcMessage = lcErrorMessage.
      ELSE lcMessage = lcDoneMessage.
   END.

   ELSE DO:
      fUpdStatus(iiToStatus,
                 lcCancelReason,
                 OUTPUT lcMessage).
      IF NOT lcMessage BEGINS "Error" THEN DO:
         IF MsRequest.ReqType = {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE} AND
            LOOKUP(STRING(iiToStatus),"4,9") > 0
         THEN DO:
            fChangeOrderStatus(MsRequest.ReqIParam4, {&ORDER_STATUS_CLOSED}).
            /* cancel pending sms */
            FOR FIRST CallAlarm EXCLUSIVE-LOCK USE-INDEX CLI WHERE
                      CallAlarm.Brand    = Syst.Var:gcBrand       AND
                      CallAlarm.CLI      = MsRequest.CLI AND
                      CallAlarm.DeliStat = 1             AND
                      CallAlarm.DeliPara = "PD":
               CallAlarm.DeliStat = 4.
            END.
         END.
         
         /* if additional line to non-additional line pending STC is cancellled
            and if it doesn't contain any main line then STC request has to be created
            for additional line to CONT9*/
         IF iiToStatus EQ 4 AND (MsRequest.Reqtype EQ 0 OR MsRequest.ReqType EQ 18) THEN 
            fNonAddLineSTCCancellationToAddLineSTC(MsRequest.MsRequest).

         /* set activation date as the 1st of next month */
         IF iiToStatus EQ 0 AND
            iiFromStatus EQ 19 AND
            MsRequest.ReqType = 10 AND
            CAN-FIND(FIRST MobSub WHERE
                           MobSub.MsSeq = MsRequest.MsSeq AND
                    LOOKUP(MobSub.CLIType,{&MOBSUB_CLITYPE_FUSION}) > 0)
            THEN DO:

            IF MONTH(TODAY) = 12
            THEN ldtTdDate = DATE(1,1,YEAR(TODAY) + 1).
            ELSE ldtTdDate = DATE(MONTH(TODAY) + 1,1,YEAR(TODAY)).

            ldActStamp = Func.Common:mMake2DT(ldtTdDate,3600).

            FIND CURRENT MsRequest EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE MsRequest THEN MsRequest.ActStamp = ldActStamp.
            FIND CURRENT MsRequest NO-LOCK NO-ERROR.
         END.
         
         IF iiToStatus EQ 8 AND
            iiFromStatus EQ 19 AND
            MsRequest.ReqType = 0 THEN DO:
         
            IF MsRequest.ReqDParam1 < Func.Common:mMakeTS() THEN
               ldeActStamp = Func.Common:mMake2DT(TODAY + 1, 0).
            ELSE ldeActStamp = MSrequest.ReqDParam1.

            FIND CURRENT MsRequest EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN
               MsRequest.ReqDParam1 = ldeActStamp
               MsRequest.ActStamp = ldeActStamp.
            FIND CURRENT MsRequest NO-LOCK NO-ERROR.
         END.
      END.
      
   END.

   MESSAGE lcMessage VIEW-AS ALERT-BOX.
   IF lcMessage BEGINS "Error" THEN UNDO, RETURN.
END. /* DO TRANSACTION: */

PROCEDURE ipMulitSIMTermination:
   
   DEFINE INPUT PARAMETER iiReqType AS INTEGER NO-UNDO. 

   DEF VAR lcError AS CHAR NO-UNDO. 

    FIND FIRST MobSub WHERE
               MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
   
    IF NOT AVAILABLE MobSub THEN RETURN. 
    
    /* YDR-819 - Create CONTM termination request - MultiSIM */
    IF MobSub.MultiSIMId > 0 AND
       MobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} THEN DO:
   
       FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
                  lbMobSub.Brand        = Syst.Var:gcBrand                 AND
                  lbMobSub.MultiSimID   = MobSub.MultiSimID       AND
                  lbMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
                  lbMobSub.Custnum      = MobSub.Custnum NO-ERROR.
                     
       IF NOT AVAIL lbMobSub THEN DO:
          FIND FIRST TermMobSub NO-LOCK USE-INDEX MultiSIM WHERE
                     TermMobSub.Brand        = Syst.Var:gcBrand                 AND
                     TermMobSub.MultiSimID   = MobSub.MultiSimID       AND
                     TermMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
                     TermMobSub.Custnum      = MobSub.Custnum NO-ERROR.
             
          IF NOT AVAIL TermMobSub THEN
             ldaSecSIMTermDate = TODAY.
          ELSE DO:
             FIND FIRST Msowner WHERE
                        Msowner.MsSeq = TermMobsub.MsSeq NO-LOCK NO-ERROR.
             IF AVAIL Msowner THEN
                Func.Common:mSplitTS(Msowner.TSEnd,
                         OUTPUT ldaSecSIMTermDate,
                         OUTPUT liSecSIMTermTime).
             ELSE ldaSecSIMTermDate = TODAY.
          END. /* ELSE DO: */
    
          fTermAdditionalSim(MobSub.MsSeq,
                             MobSub.CLI,
                             MobSub.CustNum,
                             {&SUBSCRIPTION_TERM_REASON_MULTISIM},
                             ldaSecSIMTermDate,
                             {&REQUEST_SOURCE_MANUAL_TMS},
                             0,
                             OUTPUT lcError).
    
       END. /* IF NOT AVAIL lbMobSub THEN DO: */
    END. /* IF AVAIL MobSub AND MobSub.MultiSIMId > 0 AND */
    /* STC is created for Additional lines when main line is terminated 
       YDR-1847 (Mentioned in comments) */
    ELSE DO:
     /* TODO: cannot use this function in this case*/
     /*
      fAdditionalLineSTC(MsRequest.MsRequest,
                         Func.Common:mMake2DT(TODAY + 1, 0),
                         "").
     */
    END. /* ELSE DO: */

END PROCEDURE.

