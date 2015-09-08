/* ----------------------------------------------------------------------
  MODULE .......: commission_run.p
  TASK .........: Calculate commission
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 27.10.08
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{cparam2.i}
{ftaxdata.i}
{fcpfat.i}
{ftopup.i}
{fsmsreq.i}
{create_eventlog.i}
{barrfunc.i}
{commission.i}

DEF OUTPUT PARAMETER oiChecked   AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiActivated AS INT  NO-UNDO.
 
DEF BUFFER bRelated    FOR CoTarg.
DEF BUFFER bPromotedSub FOR MobSub.
DEF BUFFER bRefereeSub  FOR MobSub.
DEF BUFFER bRule FOR CORule.

DEF VAR liRequest     AS INT  NO-UNDO.
DEF VAR lcResult      AS CHAR NO-UNDO.
DEF VAR lcError       AS CHAR NO-UNDO.
DEF VAR ldtDate       AS DATE NO-UNDO.
DEF VAR liTime        AS INT  NO-UNDO.
DEF VAR lcDebtBarring AS CHAR NO-UNDO.
 
DEF TEMP-TABLE ttAmount NO-UNDO
   FIELD ActStamp AS DEC
   FIELD Amount   AS DEC.

FUNCTION fMarkRelated RETURNS LOGIC
   (iiStatus   AS INT,
    iiReason   AS INT,
    icMessage  AS CHAR):

   /* also other commissions related to the same order will be marked */
   FOR EACH bRelated NO-LOCK USE-INDEX OrderID WHERE
            bRelated.Brand     = gcBrand AND
            bRelated.OrderID   = CoTarg.OrderID AND
            bRelated.CoTargID NE CoTarg.CoTargID AND
            bRelated.CommStatus = 1:
                        
      fCommStatus(bRelated.CoTargID,
                  iiStatus,
                  iiReason,
                  icMessage).
   END.
   
END FUNCTION.
 

/***** MAIN start *******/

/* barrings that are used for debt collection, should not be on */
lcDebtBarring = {&FRAUD_BARR_CODES}.
 
RUN pCalculateCommission (OUTPUT oiChecked,
                          OUTPUT oiActivated).

DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "CoTarg"  
      ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                               STRING(MONTH(TODAY),"99") + 
                               STRING(DAY(TODAY),"99")
      ActionLog.UserCode     = katun
      ActionLog.ActionID     = "COMMISSION"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
      ActionLog.ActionDec    = oiActivated
      ActionLog.ActionChar   = "Handled: " + STRING(oiChecked) + CHR(10) + 
                               " Activated: " + STRING(oiActivated)
      ActionLog.ActionStatus = 3.
      ActionLog.ActionTS     = fMakeTS().
END.

RETURN RETURN-VALUE.

/******MAIN end ***********/


PROCEDURE pCalculateCommission:

   DEF OUTPUT PARAMETER oiHandled    AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER oiCommission AS INT  NO-UNDO.
   
   DEF VAR ldPaid        AS DEC  NO-UNDO.
   DEF VAR llPayType     AS LOG  NO-UNDO.
   DEF VAR liCnt         AS INT  NO-UNDO.
   DEF VAR liCLIs        AS CHAR NO-UNDO. 
   DEF VAR ldtCreated    AS DATE NO-UNDO.
   DEF VAR ldAmount      AS DEC  NO-UNDO.
   DEF VAR ldTotAmount   AS DEC  NO-UNDO.
   DEF VAR ldStamp       AS DEC  NO-UNDO.
   DEF VAR liReason      AS INT  NO-UNDO.
   DEF VAR lcReason      AS CHAR NO-UNDO.
   DEF VAR llRelatedType AS LOG  NO-UNDO.
   
   FORM 
      oiHandled    AT 2 FORMAT ">>>>>>>9" LABEL "Handled .." SKIP
      oiCommission AT 2 FORMAT ">>>>>>>9" LABEL "Commission" SKIP
   WITH OVERLAY ROW 10 CENTERED SIDE-LABELS TITLE " COMMISSION QUEUE "
      FRAME fQty.

   /* RuleType:
      2 = for referee
      3 = for promoted
   */

   IF NOT SESSION:BATCH THEN DO:
      PAUSE 0.
      VIEW FRAME fQty.
   END.
   
   CommissionQueue:
   FOR EACH CoTarg NO-LOCK WHERE
            CoTarg.Brand      = gcBrand AND
            CoTarg.CommStatus = 1,
      FIRST CoRule NO-LOCK WHERE
            CoRule.Brand    = gcBrand AND
            CoRule.CoRuleID = CoTarg.CoRuleID:
            
      ASSIGN
         llPayType = (CoRule.PayType = 2)
         oiHandled = oiHandled + 1.

      IF NOT SESSION:BATCH AND 
         (oiHandled < 100 OR oiHandled MOD 100 = 0) 
      THEN DO:
         PAUSE 0.
         DISP oiHandled oiCommission WITH FRAME fQty.
      END.
      
      /* pending time has expired */
      IF CoRule.MaxPendingDays > 0 THEN DO:
         fSplitTS(CoTarg.CreatedTS,
                  OUTPUT ldtCreated,
                  OUTPUT liCnt).

         IF TODAY - ldtCreated > CoRule.MaxPendingDays THEN DO:
            fCommStatus(CoTarg.CoTargID,3,6,"").
            NEXT CommissionQueue.
         END.
      END.
      
      IF CoTarg.TargType = "M" THEN DO:      

         /* subscription must be available */
         FIND FIRST MobSub WHERE MobSub.MsSeq = INTEGER(CoTarg.CoTarg)
            NO-LOCK NO-ERROR.
         IF NOT AVAILABLE MobSub THEN DO:
         
            liReason =  IF CoRule.RuleType = 3 THEN 4 ELSE 5.
            
            fCommStatus(CoTarg.CoTargID,
                        3,
                        liReason,
                        "").

            /* also other commissions related to the same order will be
               rejected */
            FIND TermMobsub WHERE TermMobsub.MsSeq = INTEGER(CoTarg.CoTarg) 
               NO-LOCK NO-ERROR.
            fMarkRelated(3, 
                         liReason,
                         (IF AVAIL TermMobsub THEN 
                                   TermMobsub.CLI ELSE "Subscription")
                         + " terminated").
             
            NEXT CommissionQueue.
         END.
         
         /* payment type (subscription type) changed */
         IF MobSub.PayType NE llPayType THEN DO:
            fCommStatus(CoTarg.CoTargID,3,7,"").
               
            /* also other commissions related to the same order will be
               rejected */
            fMarkRelated(3,
                         7,
                         MobSub.CLI + " changed payment type").
               
            NEXT CommissionQueue. 
         END.
            
      END.

      ELSE DO:
         fCommStatus(CoTarg.CoTargID,3,8,"Unknown target type").
         NEXT CommissionQueue.
      END.

      /* promoted subscription must also be available */
      IF CoRule.RuleType = 2 AND CoTarg.PromotedID > 0 THEN DO:
         FIND FIRST bPromotedSub WHERE bPromotedSub.MsSeq = CoTarg.PromotedID
            NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bPromotedSub THEN DO:
            fCommStatus(CoTarg.CoTargID,3,4,"").

            /* also other commissions related to the same order will be
               rejected, but they will be handled on their own turn */
            NEXT CommissionQueue.   
         END.

         /* if promoted has done a payment type change then rejection */
         FOR FIRST bRelated NO-LOCK USE-INDEX OrderID WHERE
                   bRelated.Brand     = gcBrand AND
                   bRelated.OrderID   = CoTarg.OrderID AND
                   bRelated.CoTarg    = STRING(bPromotedSub.MsSeq),
             FIRST bRule NO-LOCK WHERE
                   bRule.Brand    = gcBrand AND
                   bRule.CoRuleID = bRelated.CoRuleID:
            
            llRelatedType = (bRule.PayType = 2).
            
            IF llRelatedType NE bPromotedSub.PayType THEN DO:
               fCommStatus(CoTarg.CoTargID,3,10,"").
               fMarkRelated(3,10,"").
               NEXT CommissionQueue.
            END.   

         END.          
          
      END.
      
      /* for ruletype 3 promoted = cotarg itself */
      ELSE IF CoRule.RuleType = 3 THEN DO:
         FIND FIRST bPromotedSub WHERE bPromotedSub.MsSeq = MobSub.MsSeq
            NO-LOCK.
            
         /* make sure that referee hasn't been terminated or done a stc */
         FOR FIRST bRelated NO-LOCK USE-INDEX OrderID WHERE
                   bRelated.Brand     = gcBrand AND
                   bRelated.OrderID   = CoTarg.OrderID AND
                   bRelated.CoTarg   NE CoTarg.CoTarg,
             FIRST bRule NO-LOCK WHERE
                   bRule.Brand    = gcBrand AND
                   bRule.CoRuleID = bRelated.CoRuleID:

            FIND FIRST bRefereeSub WHERE 
                       bRefereeSub.MsSeq = INTEGER(bRelated.CoTarg)
            NO-LOCK NO-ERROR.
            
            IF NOT AVAILABLE bRefereeSub THEN DO:    
               fCommStatus(CoTarg.CoTargID,3,5,"").
               NEXT CommissionQueue.
            END.   
            
            llRelatedType = (bRule.PayType = 2).
            
            IF llRelatedType NE bRefereeSub.PayType THEN DO:
               fCommStatus(CoTarg.CoTargID,3,7,"").
               NEXT CommissionQueue.
            END.   
             
         END.
      END.
      
      ELSE DO:
         lcReason = "Promoted subscription not defined".
         
         fCommStatus(CoTarg.CoTargID,
                     3,
                     8,
                     lcReason).

         fMarkRelated(3,
                      8,
                      lcReason).
         NEXT CommissionQueue.
      END.
            
      /* promoted subscription must have been open long enough */
      IF CoRule.OpenDays > 0 AND
         TODAY - bPromotedSub.ActivationDate < CoRule.OpenDays 
      THEN DO:
         fCommStatus(CoTarg.CoTargID,1,1,"").
         NEXT CommissionQueue.
      END.

      /* promoted must have paid more than defined limit */
      IF CoRule.AmtBilled > 0 THEN DO:

         ldPaid = 0.
             
         /* bills paid */
         IF bPromotedSub.PayType = FALSE THEN 
            RUN pBillsPaid(bPromotedSub.MsSeq,
                           lcDebtBarring,
                           OUTPUT ldPaid).
                
         /* topup recharged */    
         ELSE DO:
            FOR EACH PrepaidRequest NO-LOCK USE-INDEX MsSeq WHERE
                     PrepaidRequest.Brand   = gcBrand AND
                     PrepaidRequest.MsSeq   = bPromotedSub.MsSeq AND
                     LOOKUP(PrepaidRequest.Source,"ATM") > 0 AND
                     LOOKUP(PrepaidRequest.Request,"RCG,ANT") > 0 AND
                     PrePaidRequest.PPStatus = 2:
               /* without tax */      
               ldPaid = ldPaid + (PrepaidRequest.TopupAmt / 100).      
            END.
         END.
            
         IF ldPaid < CoRule.AmtBilled THEN DO:
            fCommStatus(CoTarg.CoTargID, 
                        1,
                        IF bPromotedSub.PayType = FALSE
                        THEN 2 
                        ELSE 3,
                        "").
            NEXT CommissionQueue.
         END.

      END. /* amtbilled */

      /* conditions have been filled, create commission */
      
      EMPTY TEMP-TABLE ttAmount.
      
      ASSIGN
         ldStamp     = fMakeTS()
         ldTotAmount = CoRule.CommAmount.

      /* divide amount to be paid into instalments */
      DO liCnt = 1 TO CoRule.CoNoInst:
         IF liCnt = CoRule.CoNoInst THEN
            ldAmount = ldTotAmount.
         ELSE ASSIGN
            ldAmount    = ROUND(CoRule.CommAmount / CoRule.CoNoInst,2)
            ldTotAmount = ldTotAmount - ldAmount.
            
         CREATE ttAmount.
         ASSIGN 
            ttAmount.Amount   = ldAmount
            ttAmount.ActStamp = ldStamp.
            
         /* first instalment is paid immediately, next ones on 1st of 
            each month */
         IF liCnt < CoRule.CoNoInst THEN DO:
            fSplitTS(ldStamp,
                     OUTPUT ldtDate,
                     OUTPUT liTime).
            IF MONTH(ldtDate) = 12 
            THEN ldtDate = DATE(1,1,YEAR(ldtDate) + 1).
            ELSE ldtDate = DATE(MONTH(ldtDate) + 1,1,YEAR(ldtDate)).
            ldStamp = fMake2DT(ldtDate,3600).
         END.
      END.

      /* postpaid */
      IF MobSub.PayType = FALSE THEN RUN pCreateFat.
      
      /* prepaid */
      ELSE RUN pCreateTopup.

      /* succesfully done, send sms */  
      IF CoTarg.CommStatus = 2 THEN DO:
         oiCommission = oiCommission + 1.
         
         IF CoRule.ActivationSMS > "" THEN RUN pSendSMS.
      END.   
        
   END.      

   IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
         
END PROCEDURE.

PROCEDURE pCreateFat:

   DEF VAR liFromPeriod  AS INT  NO-UNDO.
   

   /* nothing to do */
   IF CoRule.FtGrp = "" THEN DO:
      fCommStatus(CoTarg.CoTargID,
                  3,
                  8,
                  "Fatgroup not defined").
      RETURN.
   END.
         
   FIND FIRST FatGroup WHERE
              FatGroup.Brand = gcBrand AND
              FatGroup.FtGrp = CoRule.FtGrp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FatGroup THEN DO:
      fCommStatus(CoTarg.CoTargID,
                  3,
                  8,
                  "Fatgoup not found").
      RETURN.
   END.

   lcResult = "".

   /* create fatime for n months */
   DO TRANS: 
      FOR EACH ttAmount:      

         fSplitTS(ttAmount.ActStamp,
                  OUTPUT ldtDate,
                  OUTPUT liTime).
               
         liFromPeriod = YEAR(ldtDate) * 100 + MONTH(ldtDate).

         lcResult = fCreateFatRow(CoRule.FtGrp,
                                  MobSub.CustNum,
                                  MobSub.MsSeq,
                                  MobSub.Cli,
                                  "CoTarg",
                                  STRING(CoTarg.CoTargID),
                                  ttAmount.Amount,
                                  0,
                                  ?,
                                  liFromPeriod,
                                  999999,
                                  "").
 
         IF lcResult > "" THEN LEAVE.
      END.
      
      /* undo all if one fails */
      IF lcResult > "" THEN UNDO. 
   END.
   
   IF lcResult > "" THEN DO:
      fCommStatus(CoTarg.CoTargID,
                  3,
                  8,
                  "FAT creation failed: " + lcResult).
   END.

   ELSE DO:
      fCommStatus(CoTarg.CoTargID,2,9,"").
   END.
   
END PROCEDURE.

PROCEDURE pCreateTopup:

   DEF VAR lcTaxZone  AS CHAR NO-UNDO.

   IF CoRule.PPReqPrefix = "" OR CoRule.PPSource = "" THEN DO:
      fCommStatus(CoTarg.CoTargID,
                  3,
                  8,
                  "Topup prefix/source not defined").
      RETURN.
   END.

   IF NOT CAN-FIND(FIRST TMSCodes WHERE
                         TMSCodes.TableName = "PrepaidRequest" AND
                         TMSCodes.FieldName = "PPReqPrefix" AND
                         TMSCodes.CodeValue = CoRule.PPReqPrefix)
   THEN DO:
      fCommStatus(CoTarg.CoTargID,
                  3,
                  8,
                  "Unknown topup prefix").
      RETURN.
   END.

   IF NOT CAN-FIND(FIRST TMSCodes WHERE
                         TMSCodes.TableName = "PrepaidRequest" AND
                         TMSCodes.FieldName = "Source" AND
                         TMSCodes.CodeValue = CoRule.PPSource)
   THEN DO:
      fCommStatus(CoTarg.CoTargID,
                  3,
                  8,
                  "Unknown topup source").
      RETURN.
   END.
     
   FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK.
   lcTaxZone = fRegionTaxZone(Customer.Region).

   liRequest = 0.

   /* create topup for n months */
   DO TRANS: 
      FOR EACH ttAmount:      

         liRequest = 
             fCreateTopUpRequest(MobSub.MsSeq,
                                 MobSub.CLI,
                                 "RefillTRequest",
                                 CoRule.PPSource,
                                 "RefillTRequest",   
                                 CoRule.PPReqPrefix,
                                 STRING(CoTarg.CoTargID),  /* reference */
                                 lcTaxZone,
                                 ttAmount.ActStamp,
                                 ttAmount.Amount * 100,
                                 0).                 /* always without tax */


         IF liRequest = 0 THEN LEAVE.
      END.
      
      /* undo all if one fails */
      IF liRequest = 0 THEN UNDO. 
   END.
    
   IF liRequest = 0 THEN DO:
      fCommStatus(CoTarg.CoTargID,
                  3,
                  8,
                  "Topup creation failed").
   END.

   ELSE DO:
      fCommStatus(CoTarg.CoTargID,2,9,"").
   END.
 
END PROCEDURE.

PROCEDURE pSendSMS:
   
   DEF BUFFER bPromotedCust FOR Customer.
   
   DEF VAR lcCommAmount AS CHAR NO-UNDO.
   DEF VAR ldeActStamp AS DEC NO-UNDO. 

   IF CoRule.ActivationSMS = "" THEN RETURN.
   
   FIND Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK.
   
   lcSMSText = fGetSMSTxt(CORule.ActivationSMS,
                          TODAY,
                          Customer.Language,
                          OUTPUT ldeActStamp).

   IF NOT lcSMSText > "" THEN RETURN.
   
   lcCommAmount = STRING(INT(CORule.CommAmount / CoRule.coNoInst)).
   lcSMSText = REPLACE(lcSMSText, "#AMOUNT", lcCommAmount).
   
   IF CORule.RuleType = 2 THEN DO:
      FIND bPromotedCust NO-LOCK WHERE 
           bPromotedCust.Custnum = bPromotedSub.Custnum.
      lcSMSText = REPLACE(lcSMSText, "#FRIEND", bPromotedCust.FirstName).
   END.
   
   liRequest = fSMSRequest(MobSub.MsSeq,
                           9,                  /* type=info */
                           "Free",             /* source of message */
                           lcSMSText,
                           ldeActStamp,          
                           "8",
                           "",
                           "",
                           OUTPUT lcResult). 
         
   /* failing of sms doesn't affect the status of the actual commission */
   IF liRequest = 0 THEN DO:
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "CoTarg",
                       STRING(CoTarg.CoTargID),
                       0,
                       "COMMISSION",
                       "Activation SMS failed: " + lcResult).
   END.
   
END PROCEDURE.
         
PROCEDURE pBillsPaid:
         
   DEF INPUT  PARAMETER iiMsSeq         AS INT  NO-UNDO.
   DEF INPUT  PARAMETER icBarringPacket AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER odPaid          AS DEC  NO-UNDO.

   DEF VAR ldBilled   AS DEC  NO-UNDO. 
   DEF VAR lcBarring  AS CHAR NO-UNDO.
   DEF VAR llOngoing AS LOG NO-UNDO. 
   DEF VAR lrBarring  AS ROWID NO-UNDO.

   ldBilled = 0.
             
   FOR EACH SubInvoice NO-LOCK USE-INDEX MsSeq WHERE
            SubInvoice.MsSeq = iiMsSeq,
      FIRST Invoice OF SubInvoice NO-LOCK WHERE
            Invoice.InvType = 1 AND
            Invoice.PrintState > 0:
      /* without tax */      
      ldBilled = ldBilled + SubInvoice.AmtExclVat.      
   END.

   llOngoing = fCheckBarrStatus(MobSub.MsSeq,
                                OUTPUT lcBarring,
                                OUTPUT lrBarring).
   IF llOngoing THEN NEXT.

   /* if there is not an active barring then all billed is considered paid */
   IF fIsInList(lcBarring, icBarringPacket) EQ FALSE THEN odPaid = ldBilled.
   
END PROCEDURE.


