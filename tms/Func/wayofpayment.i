/* ----------------------------------------------------------------------
  MODULE .......: wayofpayment.i
  TASK .........: Functions for handling Way of Payment value YDR-2883 
                  and YOT-5729.               
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......:
  CREATED ......: Pasi Hautaniemi 24.4.2018
  CHANGED ......:
  ------------------------------------------------------------------------*/
&IF "{&WAYOFPAYMENT_I}" NE "YES"
&THEN
&GLOBAL-DEFINE WAYOFPAYMENT_I YES

{Func/fixedlinefunc.i}
{Func/extralinefunc.i}

/* Function returns convergent additional line 
   main line Way of Payment value */
FUNCTION fGetAddLinePayType RETURNS CHAR
   (INPUT iiCustNum AS INT):

   DEF VAR lcPayType AS CHAR NO-UNDO.
   DEF VAR lcCLIType AS CHAR NO-UNDO.
   DEF VAR ldaCreationDate AS DATE NO-UNDO INIT ?.
   DEFINE BUFFER bMobSub FOR MobSub.

   /* YDR-2883 Handle correctly Way of payment for 66 and 67 */
   FOR EACH bMobSub NO-LOCK WHERE 
            bMobSub.Brand   EQ Syst.Var:gcBrand AND
            bMobSub.CustNum EQ iiCustNum 
            BY CreationDate:
      /* Additional line convergent main line */
      IF fIsConvergentORFixedOnly(bMobSub.CLIType) THEN DO:
         /* two or more subscriptions with the same activation date */
         IF lcCLIType > "" AND
            ldaCreationDate NE ? AND
            bMobSub.CreationDate > ldaCreationDate THEN LEAVE.
         
         lcCLIType = bMobSub.CLIType.
         ldaCreationDate =  bMobSub.CreationDate.
         /* Fiber has priority over DSL. Correct found */
         IF INDEX(lcCLIType,"TFH") > 0 THEN LEAVE.
      END.
   END.

   IF INDEX(lcCLIType,"DSL") > 0 THEN
      lcPayType = "66".
   ELSE IF INDEX(lcCLIType,"TFH") > 0 THEN
      lcPayType = "67".
   ELSE lcPayType = "20".
    
   RETURN lcPayType.    
      
END FUNCTION.

/* Function returns extra line main line Way of Payment value */
FUNCTION fGetExtraPayType RETURNS CHAR
   (INPUT iiCustNum AS INT):

   DEF VAR lcPayType       AS CHAR NO-UNDO.
   DEF VAR lcCLIType       AS CHAR NO-UNDO.
   DEF VAR ldaCreationDate AS DATE NO-UNDO INIT ?.
   DEFINE BUFFER bMobSub FOR MobSub.

   /* YDR-2883 Handle correctly Way of payment for 66 and 67 */
   FOR EACH bMobSub NO-LOCK WHERE 
            bMobSub.Brand   EQ Syst.Var:gcBrand AND
            bMobSub.CustNum EQ iiCustNum 
            BY CreationDate:
      /* Search Extra Line Main line */
      IF fCLITypeIsMainLine(bMobSub.CLIType) THEN DO:
         /* two or more subscriptions with the same activation date */
         IF lcCLIType > "" AND
            ldaCreationDate NE ? AND
            bMobSub.CreationDate > ldaCreationDate THEN LEAVE.
         
         lcCLIType = bMobSub.CLIType.
         ldaCreationDate =  bMobSub.CreationDate.
         /* Fiber has priority over DSL. Correct found */
         IF INDEX(lcCLIType,"TFH") > 0 THEN LEAVE.
      END.
   END.

   IF INDEX(lcCLIType,"DSL") > 0 THEN
      lcPayType = "66".
   ELSE IF INDEX(lcCLIType,"TFH") > 0 THEN
      lcPayType = "67".
   ELSE lcPayType = "20".
    
   RETURN lcPayType.    
      
END FUNCTION.

/* Return true if tarif belongs to convergent additional line based on 
   MsOwner period */
FUNCTION fIsAddLineMsOwnerTariff RETURNS LOGICAL
   (INPUT icCli      AS CHAR,
    INPUT idToPeriod AS DEC,
    OUTPUT icPayType AS CHAR):

   DEF VAR LDaValidTo AS DATE NO-UNDO.
   DEFINE BUFFER bMobSub FOR MobSub.
   DEFINE BUFFER bDiscountPlan FOR DiscountPlan.
   DEFINE BUFFER bDPMember FOR DPMember.

   Func.Common:mTS2Date(idToPeriod, OUTPUT LDaValidTo).
   icPayType = "".
   FOR FIRST bMobSub NO-LOCK WHERE
             bMobSub.Brand EQ Syst.Var:gcBrand AND
             bMobSub.CLI   EQ icCli AND
      LOOKUP(bMobSub.CliType, {&ADDLINE_CLITYPES}) > 0,
         EACH bDiscountPlan NO-LOCK WHERE
              bDiscountPlan.Brand  EQ Syst.Var:gcBrand AND
       LOOKUP(bDiscountPlan.DPRuleID, {&ADDLINE_DISCOUNTS} + ","
                                   + {&ADDLINE_DISCOUNTS_20} + ","
                                   + {&ADDLINE_DISCOUNTS_HM}) > 0 AND
              bDiscountPlan.ValidTo >= LDaValidTo,
         FIRST bDPMember NO-LOCK WHERE
               bDPMember.DPID       EQ bDiscountPlan.DPID AND
               bDPMember.HostTable  EQ "MobSub" AND
               bDPMember.KeyValue   EQ STRING(bMobSub.MsSeq) AND
               bDPMember.ValidTo   >= LDaValidTo AND
               bDPMember.ValidFrom <= bDPMember.ValidTo:

         IF LOOKUP(bDiscountPlan.DPRuleID, {&ADDLINE_DISCOUNTS_HM}) > 0 
            THEN icPayType = "68".
         RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

/* Return Way of Payment value YDR-2883 and YOT-5729
   Different values 
   20 Postpaid and default value.
   30 Subscription Type CONTDR% or CONTD
   60 Subscription Type CONTDSL% and MSISDN active in the subscription
   61 Subscription Type CONTFH% and MSISDN active in the subscription
   62 Subscription Type CONTDSL% and MSISDN Not active in the subscription
   63 Subscription Type CONTFH% and MSISDN Not active in the subscription
   66 Convergent Extra- or Additional line. Mainline type CONTDSL%
   67 Convergent Extra- or Additional line. Mainline type CONTFH%
   68 Mobile Additional line. No need to check main line. 
*/

FUNCTION fIfsWayOfPayment RETURNS CHAR
   (INPUT icCli        AS CHAR,
    INPUT iiMsSeq      AS INT,
    INPUT idFromPeriod AS DEC,
    INPUT idToPeriod   AS DEC):

   DEF VAR lcPayType AS CHAR NO-UNDO INIT "".
   DEFINE BUFFER bMobSub  FOR MobSub.
   DEFINE BUFFER bOrder   FOR Order.
   DEFINE BUFFER bMsOwner FOR MsOwner.

   /* service invoices with MsOwner */
   FOR FIRST bMsOwner NO-LOCK USE-INDEX CLI_S WHERE
             bMsOwner.CLI   EQ icCli AND
             bMsOwner.MsSeq EQ iiMsSeq AND
             bMsOwner.TSEnd >= idFromPeriod AND
             bMsOwner.TsBeg <= idToPeriod AND
             bMsOwner.PayType EQ FALSE:
      /* MSISDN active in the subscription */
      FIND FIRST bMobSub NO-LOCK WHERE
                 bMobSub.Brand EQ Syst.Var:gcBrand AND
                 bMobSub.MsSeq EQ bMsOwner.MsSeq AND 
            NOT (bMobSub.msstatus EQ {&MSSTATUS_MOBILE_PROV_ONG} OR
                 bMobSub.msstatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) NO-ERROR.

      /* Subscription Type like CONTDSL% */
      IF bMsOwner.CliType BEGINS "CONTDSL" THEN DO:
         IF AVAIL bMobSub THEN ASSIGN lcPayType = "60".
         ELSE ASSIGN lcPayType = "62".
      END.
      /*  Subscription Type like CONTTFH% */
      ELSE IF bMsOwner.CliType BEGINS "CONTFH" THEN DO:
         IF AVAIL bMobSub THEN ASSIGN lcPayType = "61".
         ELSE ASSIGN lcPayType = "63".
      END.
      /* Subscription Type like CONTD or CONTRD% */
      ELSE IF bMsOwner.CliType BEGINS "CONTRD" OR
              bMsOwner.CliType EQ "CONTD" THEN ASSIGN lcPayType = "30".
      /* Subscription type In Extra Line (e.g. CONT28 or CONT29) */
      ELSE IF fCLITypeIsExtraLine(bMsOwner.CLIType) AND
              NOT bMsOwner.PayType THEN DO:
         lcPayType = fGetExtraPayType(bMsOwner.CustNum).
      END.
      /* Subscription discount type additional line  */
      ELSE IF fIsAddLineMsOwnerTariff(bMsOwner.CLI, idToPeriod, OUTPUT lcPayType) AND
              NOT bMsOwner.PayType THEN DO:
         IF lcPayType = "" THEN lcPayType = fGetAddLinePayType(bMsOwner.CustNum).
      END.
   END.
   /* Subscription type Like CONT% or terminated before period*/
   IF lcPayType = "" THEN ASSIGN lcPayType = "20". /* All other cases */

   RETURN lcPayType.

END FUNCTION.

&ENDIF
