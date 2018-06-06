/* fsubstermreq.i         25.03.08/aam 

   create a request for subscription termination
*/

&IF "{&FSUBSTERMREQ_I}" NE "YES"
&THEN

&GLOBAL-DEFINE FSUBSTERMREQ_I YES
   
{Func/flimitreq.i}
{Func/fixedlinefunc.i}
{Func/multitenantfunc.i}

FUNCTION fTerminationRequest RETURNS INTEGER
   (INPUT  iiMsSeq        AS INT,    /* subscription         */
    INPUT  idActStamp     AS DEC,    /* when request should be handled */
    INPUT  iiMSISDNStatus AS INT,    /* new MSISDN status */
    INPUT  iiSIMStatus    AS INT,    /* new SIM status */
    INPUT  iiQuarantine   AS INT,    /* quarantine time (days) */
    INPUT  iiPenaltyFee   AS INT,    /* penalty fee calculated (1=yes) */
    INPUT  icOutOperator  AS CHAR,   /* operator to which outported */ 
    INPUT  icTermReason   AS CHAR,   /* reason for termination */
    INPUT  icSource       AS CHAR,   /* source of request */
    INPUT  icCreator      AS CHAR,   /* who made the request */
    INPUT  iiOrigReq      AS INT,    /* father request */
    INPUT  icTermType     AS CHAR,   /* Termination type full, Partial */
    OUTPUT ocResult       AS CHAR):

   DEF VAR liReqCreated AS INT NO-UNDO.
   DEF BUFFER bMsRequest FOR MsRequest.
   DEF BUFFER bOrder FOR Order.   

   ocResult = fChkRequest(iiMsSeq,
                          18,
                          "",
                          icCreator).

   IF ocResult > "" THEN RETURN 0.            

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = Func.Common:mMakeTS().

   /* Add some time interval so that sub-requests of 
      subs. creation request handled properly */
   IF CAN-FIND(FIRST bMsRequest WHERE
               bMsRequest.MsSeq   = iiMsSeq AND
               bMsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_CREATE} AND
               bMsRequest.DoneStamp > Func.Common:mSecOffSet(idActStamp,-240)) THEN
      idActStamp = Func.Common:mSecOffSet(idActStamp,240). /* 4 min delay */

   fCreateRequest(18,
                  idActStamp,
                  icCreator,
                  FALSE,    /* create fees */
                  FALSE).   /* sms */

   ASSIGN
      bCreaReq.ReqCParam1  = "DELETE"
      bCreaReq.ReqCParam2  = icOutOperator
      bCreaReq.ReqCParam3  = icTermReason
      bCreaReq.ReqIParam1  = iiMSISDNStatus
      bCreaReq.ReqIParam2  = iiSIMStatus
      bCreaReq.ReqIParam3  = iiQuarantine
      bCreaReq.ReqIParam4  = iiPenaltyFee
      bCreaReq.ReqCParam6  = icTermType
      bCreaReq.ReqSource   = icSource
      bCreaReq.OrigReq     = iiOrigReq
      liReqCreated         = bCreaReq.MsRequest.

   IF (fHasConvergenceTariff(iiMsSeq) AND
       icTermType = {&TERMINATION_TYPE_FULL}) THEN DO:

      /* Close mobile part order when termination request is created. */
      FOR FIRST bOrder NO-LOCK WHERE
             bOrder.MsSeq = iiMsSeq AND
             (bOrder.StatusCode = {&ORDER_STATUS_PENDING_MOBILE_LINE} OR 
              bOrder.StatusCode = {&ORDER_STATUS_MNP} OR
              bOrder.StatusCode = {&ORDER_STATUS_MNP_REJECTED}):          

         RUN Mc/closeorder.p(bOrder.OrderId, TRUE).

      END.
                    
      /* Do not change the memo text (used by DWH) */
      IF icTermReason EQ STRING({&SUBSCRIPTION_TERM_REASON_MNP}) THEN
         bCreaReq.Memo = "Fixed line need to be terminated by Yoigo BO".
   END.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

FUNCTION fDeleteMsValidation RETURNS INTEGER 
(INPUT iiMsSeq AS INTEGER,
 INPUT iiTerminationReason AS INT,
 OUTPUT ocError AS CHARACTER):

   DEF BUFFER bMsTermReq FOR MsRequest.

   /* Check that mobsub is available */
   FIND FIRST MobSub WHERE
              MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN DO:
      ocError = "System Error ! Mobile Subscription not available".
      RETURN 1.
   END.

   /* Check that msisdn is available */
   IF (NOT CAN-FIND(FIRST MSISDN WHERE 
                   MSISDN.CLI      = MobSub.CLI  AND 
                   MSISDN.ValidTo >= Func.Common:mMakeTS()) AND
      MobSub.MsStatus NE {&MSSTATUS_MOBILE_PROV_ONG} AND
      MobSub.msStatus NE {&MSSTATUS_MOBILE_NOT_ACTIVE}) THEN DO: 
      
      ocError = "System Error ! Unknown MSISDN number : "
                + STRING(MobSub.MsSeq) + " : " + MobSub.CLI.
      RETURN 2.
   END.
   
   /* Check that no other termination requests is under work*/
   FIND FIRST bMsTermReq WHERE
              bMsTermReq.MsSeq     = MobSub.MsSeq AND
              bMsTermReq.ReqType   = 18           AND
              bMsTermReq.ReqStatus NE 2           AND
              bMsTermReq.ReqStatus NE 4           AND
              bMsTermReq.ReqStatus NE 9
   NO-LOCK NO-ERROR.
   IF AVAIL bMsTermReq THEN DO: 
      ocError = 
      "There is already a scheduled KILL request"  + CHR(10) +
      "for Mobile Subscription " + MobSub.CLI      + CHR(10) +
      "Saved by user '" + bMsTermReq.UserCode + "'" + CHR(10) +
      "Proposed time of deactivation " + Func.Common:mTS2HMS(bMsTermReq.ActStamp) + CHR(10)
      + "Status " + STRING(bMsTermReq.ReqStatus).
      RETURN 3.
   END.

   /* Check that sim is available */ 
   
   IF (MobSub.MsStatus NE {&MSSTATUS_MOBILE_PROV_ONG} AND
       MobSub.MsStatus NE {&MSSTATUS_MOBILE_NOT_ACTIVE}) AND
      NOT CAN-FIND(IMSI WHERE
                   IMSI.IMSI = MobSub.IMSI) THEN DO: 
      ocError = "System Error ! Mobile Subscription doesn't have any SIM card.".
      RETURN 4.
   END.
   
   IF Mnp.MNPOutGoing:mIsMNPOutOngoing(mobsub.cli) THEN DO:
      ocError = "Ongoing MNP OUT Process".
      RETURN 5.
   END.
   
   /* check ongoing convergent orders */
   IF NOT fCanTerminateConvergenceTariff(MobSub.MsSeq,
                                        iiTerminationReason,
                                        OUTPUT ocError)
      THEN RETURN 6.

   RETURN 0. /* ok */

END FUNCTION. 
   
FUNCTION fCheckBillingPermission RETURNS INT
(iiMsSeq AS INT,
 OUTPUT ocError AS CHAR):
   
   FIND MobSub NO-LOCK WHERE MobSub.MsSeq = iiMsSeq.
   
   fGetLimit(MobSub.InvCust, MobSub.MsSeq, {&LIMIT_TYPE_BILLPERM}, 0, 0, TODAY).

   IF AVAIL Limit THEN DO:
      IF Limit.LimitAmt = 1 THEN DO:
         ocError = "Termination not allowed with billing permission status Suspended".
         RETURN 1.
      END.
      ELSE IF Limit.LimitAmt = 2 THEN DO:
         ocError = "Note! Subscription's billing permission status is Prohibited. Do you want to change the status to Allowed? Changing the status allows invoicing for subscription's uninvoiced events.".
         RETURN 2.
      END.
   END.

   ocError = "".
   RETURN 0. /* ok */

END FUNCTION.
           
FUNCTION fCheckKillTS RETURNS INT            
(iiOrderer AS INT,
ideKillTS AS DECIMAL,
OUTPUT ocError AS CHAR):
   
   DEFINE VARIABLE lcKillTS AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcTime AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hh AS INTEGER NO-UNDO. 
   DEFINE VARIABLE mm AS INTEGER NO-UNDO. 
   
   IF ideKillTS < Func.Common:mMakeTS() THEN DO:
      ocError = "Kill time cannot be in the past!".
      RETURN 1.
   END.
   
   IF iiOrderer EQ 2 THEN DO:
      lcKillTS = Func.Common:mTS2HMS(ideKillTS).
      lcTime = ENTRY(2,lcKillTS," ").
      hh = int(entry(1,lcTime,":")).
      mm = int(entry(2,lcTime,":")).
      IF hh < 2 OR hh > 6 OR (hh = 6 AND mm = 0) THEN DO:
         ocError = "Time is outside time window!". 
         RETURN 2.
      END.
   END.

   RETURN 0.
END FUNCTION. 

FUNCTION fIsPenalty RETURNS LOG
(iiOrderer AS INT,
 iiMsSeq AS INT):

   DEF BUFFER bDCCLI FOR DCCLI.
   
   IF LOOKUP(STRING(iiOrderer),"3,5,9,10,11,12") > 0 THEN RETURN FALSE.
   
   FOR EACH bDCCLI NO-LOCK WHERE
            bDCCLI.MsSeq      = iiMsSeq AND
            bDCCLI.ValidFrom <= TODAY        AND
            bDCCLI.ValidTo   >= TODAY        AND
            bDCCLI.CreateFees = TRUE:
      IF CAN-FIND(DayCampaign WHERE
                  DayCampaign.Brand = Syst.Var:gcBrand AND
                  DayCampaign.DCEvent = bDCCLI.DCEvent AND
                  DayCampaign.TermFeeModel NE "" AND
                  DayCampaign.TermFeeCalc > 0) THEN DO:
         FIND DCCLI WHERE ROWID(DCCLI) = ROWID(bDCCLI) NO-LOCK.
         RETURN TRUE. 
      END.
   END.
   
   RETURN FALSE.

END.

FUNCTION fCheckOpCode RETURNS INT            
(piOpCode AS INT,
OUTPUT ocError AS CHAR):

   IF LENGTH(STRING(piOpCode)) NE 6 THEN DO: 
      ocError = "Operator code must be 6 digits!".
      RETURN 1.
   END.
   
   IF LOOKUP(SUBSTR(STRING(piOpCode),1,1),"7,9") = 0 THEN DO: 
      ocError = "Operator code must begin with digit 7 or 9!".
      RETURN 2.
   END.
   
   IF piOpCode >= 740000 AND
      piOpCode <= 744999 THEN DO: 
      ocError = "Operator code must be outside range 740000 - 744999!".
      RETURN 3.
   END.
  
   RETURN 0.
END.

FUNCTION fCheckOrderer RETURNS INT            
(INPUT  piOrderer     AS INT,
 INPUT  llYoigoCLI    AS LOG,
 INPUT  llMasmovilCLI AS LOG,
 OUTPUT ocError       AS CHAR):
 
 DEF VAR lcTenant         AS CHAR NO-UNDO.
 DEF VAR llYoigoTenant    AS LOGI NO-UNDO INIT FALSE.
 DEF VAR llMasmovilTenant AS LOGI NO-UNDO INIT FALSE.
 
   ASSIGN lcTenant = fGetCurrentTenant().
   IF lcTenant = "" THEN 
       ASSIGN lcTenant = {&TENANT_YOIGO}.

   ASSIGN 
       llYoigoTenant    = (IF lcTenant = {&TENANT_YOIGO}    THEN TRUE ELSE FALSE)  
       llMasmovilTenant = (IF lcTenant = {&TENANT_MASMOVIL} THEN TRUE ELSE FALSE).

   IF LOOKUP(STRING(piOrderer),"1,2,3,4,5,9,10,11,12") = 0 THEN DO:
      ocError = "Unknown Orderer". 
      RETURN 1.
   END.
   IF piOrderer = 3 AND ((NOT llYoigoCLI AND llYoigoTenant) OR (NOT llMasmovilCLI AND llMasmovilTenant)) THEN DO:
      ocError = "Cannot choose Order cancellation with MNP numbers!".
      RETURN 2.
   END.

   RETURN 0.
END.

/* use only if orderer = 5 */
FUNCTION fCheckMsisdnStat RETURNS INT
(iiMSISDNStat AS INTEGER,
 OUTPUT ocError AS CHARACTER):

   IF LOOKUP(STRING(iiMsisdnStat),"2,4,5") EQ 0 THEN DO:
     ocError = "Incorrect or missing MSISDN status value".
     RETURN 1.
   END.
   
   RETURN 0.
END.

/* use only if orderer = 5 */
FUNCTION fCheckSimStat RETURNS INT 
(iiSimStat AS INTEGER,
 OUTPUT ocError AS CHARACTER):

   IF LOOKUP(STRING(iiSimStat),"7,9") EQ 0 THEN DO:
      ocError = "Incorrect or missing ICC status value".
      RETURN 1.
   END.

   RETURN 0.

END FUNCTION. 

FUNCTION fInitialiseValues RETURNS INT            
(  INPUT iiOrderer     AS INT,
   INPUT ilYoigoCLI    AS LOG,
   INPUT ilMasmovilCLI AS LOG,
   OUTPUT piMSISDNStat AS INT,
   OUTPUT piSimStat    AS INT,
   OUTPUT piQuarTime   AS INT):

   DEF VAR lcTenant         AS CHAR NO-UNDO.
   DEF VAR llYoigoTenant    AS LOGI NO-UNDO INIT FALSE.
   DEF VAR llMasmovilTenant AS LOGI NO-UNDO INIT FALSE.

   ASSIGN lcTenant = fGetCurrentTenant().
   IF lcTenant = "" THEN 
      ASSIGN lcTenant = {&TENANT_YOIGO}.

   ASSIGN 
      llYoigoTenant    = (IF lcTenant = {&TENANT_YOIGO}    THEN TRUE ELSE FALSE)  
      llMasmovilTenant = (IF lcTenant = {&TENANT_MASMOVIL} THEN TRUE ELSE FALSE).

   CASE iiOrderer:
      WHEN 1 OR WHEN 4 OR WHEN 5 OR WHEN 9 OR WHEN 10 OR WHEN 13 OR WHEN 14 THEN
         ASSIGN
            piMSISDNStat   = (IF ((ilYoigoCLI AND llYoigoTenant) OR (ilMasmovilCLI AND llMasmovilTenant)) THEN 4  ELSE 11)
            piQuarTime     = (IF ((ilYoigoCLI AND llYoigoTenant) OR (ilMasmovilCLI AND llMasmovilTenant)) THEN 90 ELSE -1) 
            piSimStat      = 7.
      WHEN 6 OR WHEN 7 OR WHEN 8 THEN
         ASSIGN
            piMSISDNStat   = (IF ((ilYoigoCLI AND llYoigoTenant) OR (ilMasmovilCLI AND llMasmovilTenant)) THEN 4  ELSE 11)
            piQuarTime     = (IF ((ilYoigoCLI AND llYoigoTenant) OR (ilMasmovilCLI AND llMasmovilTenant)) THEN 30 ELSE -1) 
            piSimStat      = 7.
      WHEN 2 THEN
         ASSIGN
            piMSISDNStat   = (IF ((ilYoigoCLI AND llYoigoTenant) OR (ilMasmovilCLI AND llMasmovilTenant)) THEN 6  ELSE 13)
            piQuarTime     = (IF ((ilYoigoCLI AND llYoigoTenant) OR (ilMasmovilCLI AND llMasmovilTenant)) THEN -1 ELSE -1)
            piSimStat      = 7.
      WHEN 3 THEN 
         ASSIGN
            piMSISDNStat   = (IF ((ilYoigoCLI AND llYoigoTenant) OR (ilMasmovilCLI AND llMasmovilTenant)) THEN 4  ELSE 11)
            piQuarTime     = (IF ((ilYoigoCLI AND llYoigoTenant) OR (ilMasmovilCLI AND llMasmovilTenant)) THEN 1  ELSE -1)
            piSimStat      = 9.
      WHEN 11 OR WHEN 12 THEN 
         ASSIGN
            piMSISDNStat   = (IF ((ilYoigoCLI AND llYoigoTenant) OR (ilMasmovilCLI AND llMasmovilTenant)) THEN 1  ELSE 11)
            piQuarTime     = (IF ((ilYoigoCLI AND llYoigoTenant) OR (ilMasmovilCLI AND llMasmovilTenant)) THEN -1 ELSE -1)
            piSimStat      = 1.

   END.
END.



&ENDIF
