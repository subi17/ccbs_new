/* ---------------------------------------------------------------------------
  MODULE .......: DENYBILLING.P
  KUTSUVAMODULI : 
  FUNCTION .....: read billing denials from file         
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 11.02.08
  CHANGED ......: 
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhLimit AS HANDLE NO-UNDO.
   lhLimit = BUFFER Limit:HANDLE.
   RUN StarEventInitialize(lhLimit).
END.



DEF INPUT  PARAMETER icFile      AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icLogFile   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiRead      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiErrors    AS INT  NO-UNDO.

DEF VAR lcLine      AS CHAR NO-UNDO.
DEF VAR lcSep       AS CHAR NO-UNDO. 
DEF VAR lcCLI       AS CHAR NO-UNDO.
DEF VAR liMsSeq     AS INT  NO-UNDO.
DEF VAR liAgrCust   AS INT  NO-UNDO.
DEF VAR lcOrgID     AS CHAR NO-UNDO.
DEF VAR liCode      AS INT  NO-UNDO.
DEF VAR lcFrom      AS CHAR NO-UNDO.
DEF VAR lcTo        AS CHAR NO-UNDO.
DEF VAR ldaFrom     AS DATE NO-UNDO.
DEF VAR ldaTo       AS DATE NO-UNDO.
DEF VAR ldFrom      AS DEC  NO-UNDO.
DEF VAR ldTo        AS DEC  NO-UNDO.
DEF VAR lcError     AS CHAR NO-UNDO.
DEF VAR lcPlainFile AS CHAR NO-UNDO.
DEF VAR liCnt       AS INT  NO-UNDO.
DEF VAR ldCurrent   AS DEC  NO-UNDO.
DEF VAR llDone      AS LOG  NO-UNDO.
DEF VAR llExist     AS LOG  NO-UNDO.
DEF VAR lcTransDir  AS CHAR NO-UNDO.
DEF VAR liDenyCust  AS INT  NO-UNDO.
DEF VAR ldaFuture   AS DATE NO-UNDO. 

DEF STREAM sRead.
DEF STREAM sLog.

FUNCTION fWriteLog RETURNS LOGIC
   (icMessage AS CHAR):
   
   PUT STREAM sLog UNFORMATTED
      lcLine       lcSep
      icMessage    lcSep
      (IF liDenyCust > 0 
       THEN "Customer: " + STRING(liDenyCust)
       ELSE "") 
      SKIP.
   
END FUNCTION.


FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fWriteLog("ERROR:" + icMessage).
   
   oiErrors = oiErrors + 1.

   lcError = icMessage.
     
END FUNCTION.


IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".


INPUT STREAM sRead FROM VALUE(icFile).
OUTPUT STREAM sLog TO VALUE(icLogFile) APPEND.

ldCurrent = fMakeTS().

PUT STREAM sLog UNFORMATTED
   "File: " icFile
   SKIP
   "Started: " 
   fTS2HMS(ldCurrent)
   SKIP(1).

ASSIGN
   lcSep       = ";"
   liCnt       = R-INDEX(icFile,"/")
   lcPlainFile = icFile
   lcTransDir  = fCParamC("DenyBillArc")
   ldaFuture   = 12/31/2049.
   
IF liCnt > 1 THEN 
   lcPlainFile = SUBSTRING(lcPlainFile,liCnt + 1).


REPEAT:

   IMPORT STREAM sRead UNFORMATTED lcLine.

   IF lcLine = "" OR NUM-ENTRIES(lcLine,lcSep) < 4 OR
      ENTRY(2,lcLine,lcSep) = "msisdn" THEN NEXT.
    
   ASSIGN
      oiRead  = oiRead + 1
      lcError = ""
      liMsSeq = 0
      ldaFrom = ?
      ldaTo   = ?
      liDenyCust = 0.
    
   IF NOT SESSION:BATCH AND oiRead MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiRead LABEL "Rows" 
      WITH 1 DOWN SIDE-LABELS ROW 10 CENTERED OVERLAY FRAME fQty.
   END.

   ASSIGN
      liMsSeq    = INTEGER(ENTRY(1,lcLine,lcSep))
      lcCLI      = ENTRY(2,lcLine,lcSep)
      lcOrgID    = ENTRY(3,lcLine,lcSep)
      liCode     = INTEGER(ENTRY(4,lcLine,lcSep))
      NO-ERROR.
      
   IF ERROR-STATUS:ERROR OR liMsSeq = 0 THEN DO:
      fError("Invalid data").
      NEXT.
   END.
   
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "Limit" AND
              TMSCodes.FieldName = "Billing Permission" AND 
              TMSCodes.CodeValue = STRING(liCode) NO-LOCK NO-ERROR. 
   IF NOT AVAIL TMSCodes THEN DO:
      fError("Invalid Billing Permission Code").
      NEXT.  
   END.

   IF NUM-ENTRIES(lcLine,lcSep) > 4 THEN 
      lcFrom     = ENTRY(5,lcLine,lcSep).
   IF NUM-ENTRIES(lcLine,lcSep) > 5 THEN 
      lcTo       = ENTRY(6,lcLine,lcSep).
       
   IF lcFrom > "" THEN 
      ldaFrom = DATE(INTEGER(SUBSTRING(lcFrom,5,2)),
                     INTEGER(SUBSTRING(lcFrom,7,2)),
                     INTEGER(SUBSTRING(lcFrom,1,4))) NO-ERROR.
   ELSE ldaFrom = TODAY.
                     
   IF lcTo > "" THEN 
      ldaTo = DATE(INTEGER(SUBSTRING(lcTo,5,2)),
                   INTEGER(SUBSTRING(lcTo,7,2)),
                   INTEGER(SUBSTRING(lcTo,1,4))) NO-ERROR.
   ELSE ldaTo = ldaFuture.
                     
   IF ERROR-STATUS:ERROR OR ldaFrom = ? OR ldaTo = ? OR 
      ldaTo < ldaFrom
   THEN DO:
      fError("Invalid time period").
      NEXT.
   END.

   FIND FIRST MobSub USE-INDEX MsSeq WHERE
              MobSub.MsSeq   = liMsSeq AND
              MobSub.CLI     = lcCLI NO-LOCK NO-ERROR.

   IF NOT AVAILABLE MobSub THEN DO:
      fError("Subscription not found").
      NEXT.
   END.
   
   liDenyCust = MobSub.InvCust.
   
   /* release to billing */
   IF liCode = 0 THEN DO:
   
      FIND FIRST Limit USE-INDEX MsSeq WHERE
                 Limit.MsSeq     = MobSub.MsSeq   AND
                 Limit.LimitType = 3               AND
                 Limit.TMRuleSeq = 0               AND
                 Limit.ToDate   >= ldaFrom         AND
                 Limit.FromDate <= ldaTo           AND
                 Limit.LimitID   = 0               AND
                 Limit.CustNum   = MobSub.InvCust AND
                 Limit.LimitAmt > 0 NO-LOCK NO-ERROR.

       IF NOT AVAILABLE Limit THEN DO:
         fError("Active denial not found").
         NEXT.
      END.
      
      FIND CURRENT Limit EXCLUSIVE-LOCK.
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhLimit).
      Limit.ToDate = ldaFrom - 1.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhLimit).
      fWriteLog("OK").
   END.

   /* 1=suspend for determined period, 2=prohibit totally */
   ELSE DO:

      FIND FIRST Limit USE-INDEX MsSeq WHERE
                 Limit.MsSeq     = MobSub.MsSeq   AND
                 Limit.LimitType = 3               AND
                 Limit.TMRuleSeq = 0               AND
                 Limit.ToDate   >= ldaFrom         AND
                 Limit.FromDate <= ldaTo           AND
                 Limit.LimitID   = 0               AND
                 Limit.CustNum   = MobSub.InvCust NO-LOCK NO-ERROR.

      IF AVAILABLE Limit THEN DO:
         IF Limit.LimitAmt = liCode AND Limit.ToDate >= ldaFuture THEN DO:
            fError("Subscription already has a denial").
            NEXT.
         END.
      
         FIND CURRENT Limit EXCLUSIVE-LOCK.
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhLimit).
         Limit.ToDate = ldaFrom - 1.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhLimit).
      END.
      
      CREATE Limit.
      ASSIGN 
         Limit.Brand     = gcBrand
         Limit.MsSeq     = MobSub.MsSeq   
         Limit.LimitType = 3               
         Limit.TMRuleSeq = 0               
         Limit.FromDate  = ldaFrom         
         Limit.ToDate    = ldaTo           
         Limit.LimitID   = 0               
         Limit.CustNum   = MobSub.InvCust 
         Limit.LimitAmt  = liCode
         Limit.ValueType = 1.
         
      IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhLimit).   
      fWriteLog("OK").
   END.

END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE. 
   
PUT STREAM sLog UNFORMATTED
   SKIP(1)
   "Ended: " 
   STRING(TODAY,"99.99.9999") " " STRING(TIME,"hh:mm:ss")
   SKIP
   "Read: "
   oiRead 
   " Succesful: "
   oiRead - oiErrors
   " Errors: " 
   oiErrors
   SKIP.
   
/* move to archive */
IF oiRead > 0 AND lcTransDir > "" THEN DO:   
    fTransDir(icFile,
              "",
              lcTransDir).
END.
 
   
