/* ----------------------------------------------------------------------
  MODULE .......: reassign_orphan_extralines_to_mainline.p 
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: 12.05.18
  Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".

{Func/extralinefunc.i}
{Func/ftransdir.i}

DEF STREAM strout. 

DEFINE VARIABLE lcLogData  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liCount    AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcSpoolDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutDir   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcLogFile  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcToday    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeEndTS   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldtYesDate AS DATE      NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

DEFINE BUFFER bMLMobSub FOR MobSub.
DEFINE BUFFER bCustomer FOR Customer.
DEFINE BUFFER bMsOwner  FOR MsOwner.

ASSIGN 
   lcSpoolDir = fCParam("ExtralinesCron","OutSpoolDir")
   lcOutDir   = fCParam("ExtralinesCron","OutDir")
   lcToday    = STRING(YEAR(TODAY),"9999") + 
                STRING(MONTH(TODAY),"99")  +
                STRING(DAY(TODAY),"99")
   lcLogFile  = "reassign_orphan_extralines_to_mainline" + "_" + 
                lcToday                                  + "_" +
                STRING(TIME)                             + ".log"
   lcLogFile  = lcSpoolDir + lcLogFile
   ldeEndTS   = Func.Common:mDate2TS(TODAY + 1)
   ldtYesDate = TODAY - 1.

OUTPUT STREAM strout TO VALUE(lcLogFile). 

PUT STREAM strout UNFORMATTED 
   "CUSTID;MLSubId;MLMSISDN;MLCliType;ELSubId;ELMSISDN;ELCliType;ELMultiSimId;ELMultiSimType;STATUS" SKIP.

FOR EACH TMSRelation NO-LOCK WHERE
         TMSRelation.TableName     EQ {&ELTABLENAME} AND
         TMSRelation.KeyType       EQ {&ELKEYTYPE}   AND
         TMSRelation.ParentValue   NE ""             AND
         TMSRelation.RelationType  NE "MANDATORY"    BREAK BY TMSRelation.ParentValue:

   IF INT(TMSRelation.RelationType) < 0 THEN NEXT.

   IF LAST-OF(TMSRelation.ParentValue) THEN DO:

      FOR EACH bMLMobSub NO-LOCK WHERE
               bMLMobSub.Brand          EQ Syst.Var:gcBrand        AND
               bMLMobSub.clitype        EQ TMSRelation.ParentValue AND
              (bMLMobSub.MsStatus       EQ {&MSSTATUS_ACTIVE} OR
               bMLMobSub.MsStatus       EQ {&MSSTATUS_BARRED}):
         
         FIND FIRST bCustomer NO-LOCK WHERE
                    bCustomer.Brand   EQ Syst.Var:gcBrand  AND
                    bCustomer.CustNum EQ bMLMobSub.CustNum NO-ERROR.

         IF NOT AVAIL bCustomer THEN NEXT.

         FIND LAST bMsOwner NO-LOCK WHERE
                   bMsOwner.MsSeq   EQ bMLMobSub.MsSeq   AND
                   bMsOwner.CLI     EQ bMLMobSub.CLI     AND
                   bMsOwner.CLIType EQ bMLMobSub.CLIType AND
                   bMsOwner.TsEnd   GT ldeEndTS          NO-ERROR.

         IF NOT AVAIL bMsOwner THEN NEXT.

         IF Func.Common:mTSToDate(bMsOwner.TsBegin) NE ldtYesDate THEN NEXT.

         ASSIGN lcLogData = "" 
                lcLogData = STRING(bCustomer.OrgId).
      
         RUN pCheckAndAssignOrphanExtralineForMainline(bMLMobSub.MsSeq,
                                                       bMLMobSub.CLI,
                                                       bMLMobSub.CustNum,
                                                       bMLMobSub.CLIType).       
         IF NUM-ENTRIES(lcLogData,";") > 1 THEN  
            PUT STREAM strout UNFORMATTED 
               lcLogData SKIP.

      END. 

   END.
   
END.

OUTPUT STREAM strout CLOSE.

fMove2TransDir(lcLogFile, "", lcOutDir).

PROCEDURE pCheckAndAssignOrphanExtralineForMainline:
   DEFINE INPUT PARAMETER iiMLMsSeq   AS INT  NO-UNDO.
   DEFINE INPUT PARAMETER icMLCLI     AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER iiMLCustNum AS INT  NO-UNDO.
   DEFINE INPUT PARAMETER icMLCLIType AS CHAR NO-UNDO.

   DEFINE BUFFER bELMobSub  FOR MobSub.
   DEFINE BUFFER lbELMobSub FOR MobSub.

   DEF VAR liELCount AS INT NO-UNDO.
   DEF VAR liCount   AS INT NO-UNDO.

   ASSIGN liELCount = 0
          liCount   = 0.

   FOR EACH lbELMobSub EXCLUSIVE-LOCK USE-INDEX CustNum WHERE
            lbELMobSub.Brand    EQ Syst.Var:gcBrand      AND
            lbELMobSub.CustNum  EQ iiMLCustNum           AND
            lbELMobSub.PayType  EQ FALSE                 AND
           (lbELMobSub.MsStatus EQ {&MSSTATUS_ACTIVE} OR
            lbELMobSub.MsStatus EQ {&MSSTATUS_BARRED})   BY lbELMobSub.ActivationTS:

      IF NOT fCLITypeIsExtraLine(lbELMobSub.CLIType) THEN NEXT.

      IF lbELMobSub.MultiSimId   NE 0 AND
         lbELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} THEN NEXT.

      IF NOT fCLITypeAllowedForExtraLine(icMLCLIType, 
                                         lbELMobSub.CLIType,
                                         OUTPUT liELCount) THEN
         NEXT.

      FOR EACH bELMobSub NO-LOCK WHERE
               bELMobSub.Brand        EQ Syst.Var:gcBrand   AND
               bELMobSub.CustNum      EQ lbELMobSub.CustNum AND
               bELMobSub.PayType      EQ FALSE              AND
               bELMobSub.MultiSimId   EQ iiMLMsSeq          AND
               bELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}:
         liCount = liCount + 1.
      END.

      IF liCount EQ 0 THEN DO:
         IF NOT fCheckForMandatoryExtraLine(iiMLMsSeq,
                                            lbELMobSub.CustNum,
                                            icMLCLIType,
                                            lbELMobSub.CLIType,
                                            TRUE) THEN
            NEXT.
       END.

       IF liCount < liELCount THEN DO:

          ASSIGN lbELMobSub.MultiSimId   = iiMLMsSeq
                 lbELMobSub.MultiSimType = {&MULTISIMTYPE_EXTRALINE}.

          fCreateExtraLineDiscount(lbELMobSub.MsSeq,
                                   lbELMobSub.CLIType + "DISC",
                                   TODAY).

          lcLogData = lcLogData                       + ";" + 
                      STRING(iiMLMsSeq)               + ";" +
                      STRING(icMLCLI)                 + ";" +
                      STRING(icMLCLIType)             + ";" +
                      STRING(lbELMobSub.MsSeq)        + ";" +
                      STRING(lbELMobSub.CLI)          + ";" +
                      STRING(lbELMobSub.CLItype)      + ";" +
                      STRING(lbELMobSub.MultiSimId)   + ";" + 
                      STRING(lbELMobSub.MultiSimType). 

          liCount = liCount + 1.

          IF liCount = liELCount THEN
             LEAVE.

       END.

   END.

END PROCEDURE.
