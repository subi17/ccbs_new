/* ----------------------------------------------------------------------
  MODULE .......: addlinedisccron.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: 28.02.18
  Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Syst/tmsconst.i}
{Func/fdss.i} 
{Mc/dpmember.i}
{Func/ftransdir.i}
{Syst/eventlog.i}

DEFINE STREAM strout1.
DEFINE STREAM strout2.

DEFINE BUFFER bConvDiscountPlan FOR DiscountPlan.
DEFINE BUFFER bMODiscountPlan   FOR DiscountPlan.
DEFINE BUFFER bELDiscountPlan   FOR DiscountPlan.
DEFINE BUFFER bConvDPMember     FOR DPMember.
DEFINE BUFFER bMODPMember       FOR DPMember.
DEFINE BUFFER bELDPMember       FOR DPMember.
DEFINE BUFFER bALMobSub         FOR MobSub.
DEFINE BUFFER bELMobSub         FOR MobSub.
DEFINE BUFFER bAvELMobSub       FOR MobSub.
DEFINE BUFFER bMLMobSub         FOR MobSub.

DEF VAR lcConvDiscPlan     AS CHAR NO-UNDO. 
DEF VAR lcMODiscPlan       AS CHAR NO-UNDO. 
DEF VAR lcDiscError        AS CHAR NO-UNDO. 
DEF VAR lcOutputLine       AS CHAR NO-UNDO.
DEF VAR llgCreateDSS       AS LOG  NO-UNDO. 
DEF VAR liDSSPriMsSeq      AS INT  NO-UNDO. 
DEF VAR lcResult           AS CHAR NO-UNDO. 
DEF VAR liRequest          AS INT  NO-UNDO. 
DEF VAR lcBundleId         AS CHAR NO-UNDO. 
DEF VAR lcAddLineLogFile   AS CHAR NO-UNDO. 
DEF VAR lcExtraLineLogFile AS CHAR NO-UNDO. 
DEF VAR lcSpoolDir         AS CHAR NO-UNDO.
DEF VAR lcOutDir           AS CHAR NO-UNDO.

ASSIGN lcSpoolDir         = fCParam("AddLineCron","OutSpoolDir")
       lcOutDir           = fCParam("AddLineCron","OutDir")
       lcAddLineLogFile   = "addlinedisc "           + "_" + 
                            STRING(TODAY,"99999999") + "_" + 
                            REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".log"
       lcExtraLineLogFile = "extrainedisc"           + "_" + 
                            STRING(TODAY,"99999999") + "_" + 
                            REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".log".

OUTPUT STREAM strout1 TO VALUE(lcSpoolDir + lcExtraLineLogFile).
OUTPUT STREAM strout2 TO VALUE(lcSpoolDir + lcAddLineLogFile).

FUNCTION fExecuteMainLineOperations RETURNS LOGICAL
   (INPUT iiELMsSeq     AS INT,
    INPUT ilgCreateDisc AS LOG):

   DEF VAR llgLinkAvailable AS LOG NO-UNDO. 

   DEFINE BUFFER lMLMobSub  FOR MobSub.
   DEFINE BUFFER bfELMobSub FOR MobSub.

   FIND FIRST bfELMobSub EXCLUSIVE-LOCK WHERE 
              bfELMobSub.MsSeq EQ iiELMsSeq NO-ERROR.           

   llgLinkAvailable = FALSE.
  
   /* Check for Existing Convergent mainline */
   FOR EACH lMLMobSub EXCLUSIVE-LOCK USE-INDEX CustNum WHERE
            lMLMobSub.Brand    EQ Syst.Var:gcBrand      AND
            lMLMobSub.CustNum  EQ bfELMobSub.CustNum    AND
            lMLMobSub.PayType  EQ FALSE                 AND
           (lMLMobSub.MsStatus EQ {&MSSTATUS_ACTIVE} OR
            lMLMobSub.MsStatus EQ {&MSSTATUS_BARRED})
       BY lMLMobSub.ActivationTS:

       IF NOT fCLITypeAllowedForExtraLine(lMLMobSub.CLIType, bfELMobsub.CLIType) THEN NEXT.

       /* Check if this mainline is already associated with other Extraline */
       FIND FIRST bAvELMobSub NO-LOCK WHERE
                  bAvELMobSub.MsSeq        EQ lMLMobSub.MultiSimId      AND
                  bAvELMobSub.MultiSimId   EQ lMLMobSub.MsSeq           AND
                  bAvELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} NO-ERROR.

       IF NOT AVAIL bAvELMobSub THEN DO:
          ASSIGN bfELMobSub.MultiSimId   = lMLMobSub.MsSeq
                 bfELMobSub.MultiSimType = {&MULTISIMTYPE_EXTRALINE}
                 lMLMobSub.MultiSimId    = bfELMobSub.MsSeq
                 lMLMobSub.MultiSimType  = {&MULTISIMTYPE_PRIMARY}
                 llgLinkAvailable        = YES.

          IF ilgCreateDisc THEN DO: 
             fCreateExtraLineDiscount(bfELMobSub.MsSeq,
                                      bfELMobSub.CLIType + "DISC",
                                      TODAY).

             lcOutputLine = bfELMobSub.CLI                  + ";" +
                            bfELMobSub.CLIType              + ";" +
                            STRING(bfELMobSub.ActivationTS) + ";" + 
                            "ExtraLine Linking and Discount created".
          END.
          ELSE DO:
             lcOutputLine = bfELMobSub.CLI                  + ";" +
                            bfELMobSub.CLIType              + ";" +
                            STRING(bfELMobSub.ActivationTS) + ";" + 
                           "ExtraLine Linking done".
          END.

          LEAVE.  
       END.

   END.

   /* If Extraline discount is available and mainline
      link is not available then close the discount */
   IF NOT ilgCreateDisc    AND 
      NOT llgLinkAvailable THEN DO:
      fCloseExtraLineDiscount(bfELMobSub.MsSeq,
                              bfELMobSub.CLIType + "DISC",
                              TODAY).
      lcOutputLine = bfELMobSub.CLI                  + ";" +
                     bfELMobSub.CLIType              + ";" +
                     STRING(bfELMobSub.ActivationTS) + ";" + 
                     "Linking not available, Discount closed".
   END.

   RETURN llgLinkAvailable.

END FUNCTION.      

fBatchLog("START",lcSpoolDir + lcExtraLineLogFile).

/* Extra lines */
FOR EACH bELMobSub NO-LOCK WHERE
         bELMobSub.Brand   EQ Syst.Var:gcBrand AND
         bELMobSub.CLIType EQ "CONT28":

   ASSIGN lcOutputLine = ""
          lcBundleId   = "".

   FIND FIRST bELDiscountPlan NO-LOCK WHERE
              bELDiscountPlan.Brand    EQ Syst.Var:gcBrand           AND
              bELDiscountPlan.DPRuleID EQ bELMobSub.CLIType + "DISC" NO-ERROR.

   FIND FIRST bELDPMember NO-LOCK WHERE
              bELDPMember.DPId      EQ bELDiscountPlan.DPId    AND
              bELDPMember.HostTable EQ "MobSub"                AND
              bELDPMember.KeyValue  EQ STRING(bELMobSub.MsSeq) AND
              bELDPMember.ValidTo   >= TODAY                   AND
              bELDPMember.ValidFrom <= bELDPMember.ValidTo     NO-ERROR.

   FIND FIRST Customer NO-LOCK WHERE 
              Customer.Brand   EQ Syst.Var:gcBrand  AND
              Customer.CustNum EQ bELMobSub.CustNum NO-ERROR.
  
   IF AVAIL bELDPMember                                   AND 
      bELMobSub.MultiSimId   NE 0                         AND 
      bELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} THEN DO:

      FIND FIRST bMLMobSub NO-LOCK WHERE
                 bMLMobSub.MsSeq        EQ bELMobSub.MultiSimId    AND
                 bMLMobSub.MultiSimId   EQ bELMobSub.MsSeq         AND
                 bMLMobSub.MultiSimType EQ {&MULTISIMTYPE_PRIMARY} NO-ERROR.

      /* If Mainline - Extraline link is available and 
         discount is available and DSS is active   then skip */
      IF AVAIL bMLMobSub AND 
         fGetActiveDSSId(INPUT bMLMobSub.CustNum,INPUT Func.Common:mMakeTS()) > "" THEN 
         NEXT.

   END.

   IF NOT AVAIL bELDPMember THEN    
      llgCreateDSS = fExecuteMainLineOperations(bELMobsub.MsSeq,TRUE).
   ELSE DO: 
      IF bELMobSub.MultiSimId EQ 0 THEN 
         llgCreateDSS = fExecuteMainLineOperations(bELMobsub.MsSeq,FALSE).         
   END.

   IF NOT llgCreateDSS THEN DO: 
      PUT STREAM strout1 UNFORMATTED 
         lcOutputLine SKIP.
      NEXT.
   END.

   lcBundleId = fGetActiveDSSId(INPUT bELMobSub.CustNum,INPUT Func.Common:mMakeTS()).

   FIND FIRST bMLMobSub NO-LOCK WHERE 
              bMLMobSub.MsSeq        EQ bELMobSub.MultiSimId    AND 
              bMLMobSub.MultiSimId   EQ bELMobSub.MsSeq         AND 
              bMLMobSub.MultiSimType EQ {&MULTISIMTYPE_PRIMARY} NO-ERROR.

   IF NOT AVAIL bMLMobSub THEN NEXT.

   IF fIsDSS2Allowed(bMLMobSub.CustNum,
                     0,
                     Func.Common:mMakeTS(),
                     OUTPUT liDSSPriMsSeq,
                     OUTPUT lcResult) THEN DO:

      liRequest = fDSSRequest(bMLMobSub.MsSeq,
                              bMLMobSub.CustNum,
                              IF lcBundleId NE "" THEN "ADD"
                              ELSE "CREATE",
                              "",
                              "DSS2",
                              Func.Common:mMakeTS(),
                              {&REQUEST_SOURCE_SCRIPT},
                              "",
                              TRUE, /* create fees */
                              0,
                              FALSE,
                              OUTPUT lcResult).

      IF liRequest <> 0 THEN
         PUT STREAM strout1 UNFORMATTED
            lcOutputLine = lcOutputLine + ";" + "DSS2 Request created".
      ELSE
         PUT STREAM strout1 UNFORMATTED
            lcOutputLine = lcOutputLine + ";" + "Error in DSS2 Request Creation".

   END.  

   PUT STREAM strout1 UNFORMATTED
      lcOutputLine SKIP.

END.

OUTPUT STREAM strout1 CLOSE.

fMove2TransDir(lcExtraLineLogFile, "", lcOutDir).

fBatchLog("FINISH",lcOutDir + lcExtraLineLogFile).

fBatchLog("START",lcSpoolDir + lcAddLineLogFile).

/* Additional lines */
FOR EACH bALMobSub NO-LOCK WHERE 
         bALMobSub.Brand EQ Syst.Var:gcBrand  AND 
  LOOKUP(bALMobSub.CLIType,{&ADDLINE_CLITYPES}) > 0: 

   ASSIGN lcConvDiscPlan = "DISC" + bALMobSub.CLIType
          lcMODiscPlan   = "DISC" + bALMobSub.CLIType + "HM".  

   FIND FIRST bConvDiscountPlan NO-LOCK WHERE
              bConvDiscountPlan.Brand    EQ Syst.Var:gcBrand AND
              bConvDiscountPlan.DPRuleID EQ lcConvDiscPlan   NO-ERROR.

   FIND FIRST bMODiscountPlan NO-LOCK WHERE  
              bMODiscountPlan.Brand    EQ Syst.Var:gcBrand AND 
              bMODiscountPlan.DPRuleID EQ lcMODiscPlan     NO-ERROR.

   IF NOT AVAIL bConvDiscountPlan OR 
      NOT AVAIL bMODiscountPlan   THEN DO:
      PUT STREAM strout2 UNFORMATTED 
         "Additional line Convergent or MobileOnly DiscountPlan is not available" SKIP.
      LEAVE.   
   END.

   IF AVAIL bConvDiscountPlan THEN 
      FIND FIRST bConvDPMember NO-LOCK WHERE
                 bConvDPMember.DPId       EQ bConvDiscountPlan.DPId  AND
                 bConvDPMember.HostTable  EQ "MobSub"                AND
                 bConvDPMember.KeyValue   EQ STRING(bALMobSub.MsSeq) AND
                 bConvDPMember.ValidTo    >= TODAY                   AND
                 bConvDPMember.ValidFrom  <= bConvDPMember.ValidTo   NO-ERROR.

   IF AVAIL bMODiscountPlan THEN 
      FIND FIRST bMODPMember NO-LOCK WHERE
                 bMODPMember.DPId       EQ bMODiscountPlan.DPId    AND
                 bMODPMember.HostTable  EQ "MobSub"                AND
                 bMODPMember.KeyValue   EQ STRING(bALMobSub.MsSeq) AND
                 bMODPMember.ValidTo    >= TODAY                   AND
                 bMODPMember.ValidFrom  <= bMODPMember.ValidTo     NO-ERROR.
   
   IF AVAIL bConvDPMember OR 
      AVAIL bMODPMember   THEN NEXT.

   FIND FIRST Customer NO-LOCK WHERE 
              Customer.Brand   EQ Syst.Var:gcBrand  AND
              Customer.CustNum EQ bALMobSub.CustNum NO-ERROR.

   IF NOT AVAIL Customer THEN DO:
      PUT STREAM strout2 UNFORMATTED
         bALMobSub.CLI     ";"
         bALMobSub.CLIType ";"
         bALMobSub.CustNum ";"
         "Additional line Customer not available" SKIP.
      NEXT.
   END.

   /* Check for Existing 3P Convergent subscription for a customer */ 
   IF fCheckExistingConvergent(Customer.CustidType,
                               Customer.OrgId,
                               bALMobSub.CLIType) THEN 
      lcDiscError = fCreateAddLineDiscount(bALMobSub.MsSeq,
                                           bALMobSub.CLIType,
                                           TODAY,
                                           bConvDiscountPlan.DPRuleID).
   /* Check for Exisiting Mobile subscription for a customer */
   ELSE IF fCheckExistingMobileOnly(Customer.CustidType,
                                    Customer.OrgId,
                                    bALMobSub.CLIType) THEN
      lcDiscError = fCreateAddLineDiscount(bALMobSub.MsSeq,
                                           bALMobSub.CLIType,
                                           TODAY,
                                           bMODiscountPlan.DPRuleID).
                                           
   IF lcDiscError NE "" THEN DO:                                     
      PUT STREAM strout2 UNFORMATTED 
         bALMobSub.CLI          ";"
         bALMobSub.CLIType      ";"
         bALMobSub.ActivationTS ";"
         lcDiscError            SKIP.
   END.
   ELSE 
      PUT STREAM strout2 UNFORMATTED
         bALMobSub.CLI          ";"
         bALMobSub.CLIType      ";"
         bALMobSub.ActivationTS ";"
         "Additional Line Discount Created" SKIP.

END.

OUTPUT STREAM strout2 CLOSE.

fMove2TransDir(lcAddLineLogFile, "", lcOutDir).

fBatchLog("FINISH",lcOutDir + lcAddLineLogFile).
