/* read_discount_member.p    29.05.12/aam

   MSISDN;Discount Plan;From;To;Discount Amount
*/

{Syst/commali.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/timestamp.i}
{Syst/eventval.i}
{Mc/dpmember.i}


DEF INPUT  PARAMETER icFile   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiDone   AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER oiErrors AS INT  NO-UNDO.

DEF VAR lcLogFile    AS CHAR NO-UNDO.
DEF VAR lcPlainFile  AS CHAR NO-UNDO.
DEF VAR lcReadLine   AS CHAR NO-UNDO. 
DEF VAR lcTransDir   AS CHAR NO-UNDO.
DEF VAR liCheck      AS INT  NO-UNDO.
DEF VAR lcCLI        AS CHAR NO-UNDO.
DEF VAR lcCLIType    AS CHAR NO-UNDO.
DEF VAR lcDiscPlan   AS CHAR NO-UNDO.
DEF VAR ldDiscount   AS DEC  NO-UNDO.
DEF VAR lcValidFrom  AS CHAR NO-UNDO.
DEF VAR lcValidTo    AS CHAR NO-UNDO.
DEF VAR ldaValidFrom AS DATE NO-UNDO.
DEF VAR ldaValidTo   AS DATE NO-UNDO.
DEF VAR lcSep        AS CHAR NO-UNDO. 
DEF VAR liMsSeq      AS INT  NO-UNDO. 
DEF VAR ldaLimitedTo AS DATE NO-UNDO.
DEF VAR liLinesRead  AS INT NO-UNDO. 

DEF BUFFER bUpdMember FOR DPMember.

DEF STREAM sRead.
DEF STREAM sLog.

FORM 
   oiDone   AT 2 FORMAT ">>>>>>>>9" LABEL "Rows Read" SKIP
   oiErrors AT 2 FORMAT ">>>>>>>>9" LABEL "Errors .." SKIP
   WITH OVERLAY CENTERED ROW 10 SIDE-LABELS TITLE " IMPORT " FRAME fQty.


FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcReadLine  ";"
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).
   oiErrors = oiErrors + 1.
END FUNCTION.


/********* Main start ********/

IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".

ASSIGN
   lcLogFile  = fCParamC("ReadDPMemberLog")
   lcTransDir = fCParamC("ReadDPMemberArc")
   lcSep      = ";".

IF lcLogFile = ? OR lcLogFile = "" THEN 
   lcLogFile = "/tmp/read_dpmember_#DATE.log".
   
ASSIGN 
   lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(TODAY,"999999"))
   lcLogFile = REPLACE(lcLogFile,"#TIME",
                       REPLACE(STRING(TIME,"hh:mm:ss"),":","")).
                                        

/* file without the dir */
lcPlainFile = icFile.
IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
   lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").
 

INPUT STREAM sRead FROM VALUE(icFile).
OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

PUT STREAM sLog UNFORMATTED
   icFile  " "
   STRING(TODAY,"99.99.99") " "
   STRING(TIME,"hh:mm:ss") SKIP.

IF NOT SESSION:BATCH THEN DO:
   PAUSE 0.
   VIEW FRAME fQty.
END.


REPEAT TRANS:

   lcReadLine = "".
    
   IMPORT STREAM sRead UNFORMATTED lcReadLine.

   liLinesRead = liLinesRead + 1.

   IF TRIM(lcReadLine) EQ "" THEN NEXT. 
   
   IF NUM-ENTRIES(lcReadLine,lcSep) < 5 THEN DO:
      fError("Invalid line format").
      NEXT. 
   END.
   
   ASSIGN 
      lcCLI = ENTRY(1,lcReadLine,lcSep)
      lcDiscPlan = ENTRY(2,lcReadLine,lcSep)
      lcValidFrom = ENTRY(3,lcReadLine,lcSep)
      lcValidTo   = ENTRY(4,lcReadLine,lcSep)
      ldDiscount  = DEC(ENTRY(5,lcReadLine,lcSep))
      NO-ERROR.

   IF ERROR-STATUS:ERROR OR lcCLI = "" OR lcDiscPlan = "" THEN DO:
      fError("Invalid format").
      NEXT.
   END.

   ASSIGN 
      ldaValidFrom = DATE(lcValidFrom)
      ldaValidTo   = DATE(lcValidTo) NO-ERROR.
   IF ERROR-STATUS:ERROR OR ldaValidFrom = ? THEN DO:
      fError("Invalid period").
      NEXT.
   END.
   IF ldaValidTo = ? THEN ldaValidTo = 12/31/2049.

   IF ldaValidTo < ldaValidFrom THEN DO:
      fError("End date is earlier than begin date").
      NEXT.
   END.
   
   liMsSeq = 0.
   FIND FIRST MobSub WHERE MobSub.CLI = lcCLI NO-LOCK NO-ERROR.
   IF AVAILABLE MobSub THEN ASSIGN
      liMsSeq = MobSub.MsSeq
      lcCLIType = MobSub.CLIType.
   ELSE DO:
      FIND FIRST MsOwner WHERE
                 MsOwner.CLI = lcCLI NO-LOCK USE-INDEX CLI_s NO-ERROR.
      IF AVAILABLE MsOwner THEN ASSIGN
         liMsSeq = MsOwner.MsSeq
         lcCLIType = MsOwner.CLIType.
   END.
   IF liMsSeq = 0 THEN DO:
      fError("Invalid MSISDN").
      NEXT.
   END.
      
   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = gcBrand AND
              DiscountPlan.DPRuleID = lcDiscPlan NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DiscountPlan THEN DO:
      fError("Unknown discount plan").
      NEXT.
   END.

   IF ldaValidFrom < DiscountPlan.ValidFrom OR 
      ldaValidFrom > DiscountPlan.ValidTo THEN DO:
         fError("Plan is not valid").
         NEXT.
   END.
  
   /* period limit on plan overrides given validity period */
   IF DiscountPlan.ValidPeriods > 0 THEN DO:
      ldaLimitedTo = fCalcDPMemberValidTo(ldaValidFrom,
                                          DiscountPlan.ValidPeriods).
      IF ldaLimitedTo NE ? AND ldaLimitedTo < ldaValidTo THEN 
         ldaValidTo = ldaLimitedTo.
   END.

   IF DiscountPlan.Subject = "Contract Target" AND 
      DiscountPlan.SubjectType = "List"
   THEN DO:
      IF NOT CAN-FIND(FIRST DPSubject WHERE 
           DPSubject.DPId      = DiscountPlan.DPId AND
           DPSubject.DPSubject = lcCliType AND
           DPSubject.ValidFrom <= ldaValidTo AND
           DPSubject.ValidTo   >= ldaValidFrom) THEN DO:
              fError("Subscription type is not valid for discount plan").
              NEXT.
      END.
   END.
  
   FIND FIRST DPMember WHERE
              DPMember.DPId = DiscountPlan.DPId AND
              DPMember.HostTable = "MobSub" AND
              DPMember.KeyValue  = STRING(liMsSeq) AND
              DPMember.ValidTo >= ldaValidFrom AND
              DPMember.ValidFrom <= ldaValidTo NO-LOCK NO-ERROR.

   /* end the current one */
   IF AVAILABLE DPMember THEN DO:
      IF DPMember.DiscValue = ldDiscount AND 
         DPMember.ValidFrom = ldaValidFrom AND
         DPMember.ValidTo = ldaValidTo THEN DO:
            fLogLine("No changes").
            NEXT.
      END.
         
      FIND CURRENT DPMember EXCLUSIVE-LOCK.
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDPMember).
      DPMember.ValidTo = ldaValidFrom - 1.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDPMember).
   END.
   
   IF ldDiscount NE 0 THEN DO:
      CREATE DPMember.
      ASSIGN 
         DPMember.DPId      = DiscountPlan.DPId 
         DPMember.HostTable = "MobSub" 
         DPMember.KeyValue  = STRING(liMsSeq) 
         DPMember.ValidFrom = ldaValidFrom
         DPMember.ValidTo   = ldaValidTo          
         DPMember.DiscValue = ldDiscount
         DPMember.DPMemberID = NEXT-VALUE(DPMemberID)
         .
   
      /* dpmember creations not logged anymore YDR-1078 */
      /*
      IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDPMember).
      */
   
      fLogLine("OK"). 

      oiDone = oiDone + 1.
   END.
   ELSE DO:
      IF NOT AVAILABLE DPMember THEN DO:
         fError("Discount does not exist").
         NEXT.
      END.
      ELSE DO:
         fLogLine("OK"). 

         oiDone = oiDone + 1.
      END.
   END.
   
   IF NOT SESSION:BATCH AND 
      (oiDone < 100 OR oiDone MOD 100 = 0) THEN DO:
      PAUSE 0.
      DISP oiDone oiErrors WITH FRAME fQty.
   END.
END.

fCleanEventObjects().

DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "DPMember"  
      ActionLog.KeyValue     = lcPlainFile
      ActionLog.ActionID     = "DISCUPDATE"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                               MONTH(TODAY)
      ActionLog.ActionDec    = oiDone
      ActionLog.ActionChar   = STRING(oiDone) + 
                               " discounts were updated"
      ActionLog.ActionStatus = 3
      ActionLog.UserCode     = katun
      ActionLog.FromDate     = TODAY
      ActionLog.ToDate       = TODAY.
      ActionLog.ActionTS     = fMakeTS().
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

INPUT STREAM sRead CLOSE.
OUTPUT STREAM sLog CLOSE.

/* move to archive */
IF liLinesRead > 0 AND lcTransDir > "" THEN DO:   
   fTransDir(icFile,
             "",
             lcTransDir).
END.

/********* Main end ********/


