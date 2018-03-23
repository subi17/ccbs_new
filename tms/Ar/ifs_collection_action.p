/* ----------------------------------------------------------------------
  MODULE .......: ifs_collection_action.p
  TASK .........: Read in collection actions from ifs
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 24.06.09
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/barrfunc.i}
{Func/transname.i}

DEF INPUT  PARAMETER icFile   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiRead   AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER oiErrors AS INT  NO-UNDO.

DEF VAR lcActionID     AS CHAR NO-UNDO.
DEF VAR liCustNum      AS INT  NO-UNDO.
DEF VAR liMsSeq        AS INT  NO-UNDO.
DEF VAR lcAction       AS CHAR NO-UNDO.
DEF VAR lcActionDate   AS CHAR NO-UNDO.
DEF VAR lcDebitDate    AS CHAR NO-UNDO.
DEF VAR lcAmount       AS CHAR NO-UNDO.
DEF VAR ldAmount       AS DEC  NO-UNDO.
DEF VAR lcValue        AS CHAR NO-UNDO.
DEF VAR lcLogFile      AS CHAR NO-UNDO.
DEF VAR lcPlainFile    AS CHAR NO-UNDO.
DEF VAR lcReadLine     AS CHAR NO-UNDO. 
DEF VAR lcArcDir       AS CHAR NO-UNDO.
DEF VAR lcTransDir     AS CHAR NO-UNDO.
DEF VAR lcBarrPacket   AS CHAR NO-UNDO.
DEF VAR lrActionID     AS RECID NO-UNDO.
DEF VAR llLogWritten   AS LOG  NO-UNDO.
DEF VAR lcDebitBarrings AS CHAR NO-UNDO. 
DEF VAR lcCompanyID    AS CHAR NO-UNDO.

DEF STREAM sRead.
DEF STREAM sLog.

FORM 
   oiRead   AT 2 FORMAT ">>>>>>>>9" LABEL "Rows Read" SKIP
   oiErrors AT 2 FORMAT ">>>>>>>>9" LABEL "Errors .." SKIP
   WITH OVERLAY CENTERED ROW 10 SIDE-LABELS TITLE " IMPORT " FRAME fQty.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcReadLine  ";"
      icMessage SKIP.
      
   llLogWritten = TRUE.
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).
   oiErrors = oiErrors + 1.

   DO TRANS:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
             ErrorLog.ActionID  = "COLLACTION"
             ErrorLog.TableName = "MobSub"
             ErrorLog.KeyValue  = STRING(liMsSeq)
             ErrorLog.ErrorChar = lcPlainFile
             ErrorLog.ErrorMsg  = lcReadLine + CHR(10) + icMessage
             ErrorLog.UserCode  = Syst.Var:katun.
             ErrorLog.ActionTS  = Func.Common:mMakeTS().
   END.
   
END FUNCTION.


/******* Main start *********/

IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".

RUN pInitialize.

/* check that there isn't already another run handling this file */
IF CAN-FIND(FIRST ActionLog USE-INDEX TableName WHERE
                  ActionLog.Brand        = Syst.Var:gcBrand      AND    
                  ActionLog.TableName    = "MobSub"     AND
                  ActionLog.KeyValue     = lcPlainFile  AND
                  ActionLog.ActionID     = "IFSCOLLECT" AND
                  ActionLog.ActionStatus = 0)
THEN RETURN.

DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = Syst.Var:gcBrand   
      ActionLog.TableName    = "MobSub"  
      ActionLog.KeyValue     = lcPlainFile
      ActionLog.UserCode     = Syst.Var:katun
      ActionLog.ActionID     = "IFSCOLLECT"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
      ActionLog.ActionStatus = 0.
      ActionLog.ActionTS     = Func.Common:mMakeTS().
      lrActionID             = RECID(ActionLog).
END.


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

RUN pReadEvents.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

INPUT STREAM sRead CLOSE.
OUTPUT STREAM sLog CLOSE.

IF oiRead > 0 AND lcArcDir > "" THEN DO:   
   fTransDir(icFile,
             "",
             lcArcDir).
END.

IF NOT llLogWritten THEN 
   OS-DELETE VALUE(lcLogFile).
ELSE IF lcTransDir > "" THEN DO:
   fTransDir(lcLogFile,
             "",
             lcTransDir).
END.
  
DO TRANS:
   FIND FIRST ActionLog WHERE RECID(ActionLog) = lrActionID EXCLUSIVE-LOCK.
   ASSIGN 
      ActionLog.ActionDec    = oiRead
      ActionLog.ActionChar   = lcPlainFile + CHR(10) + 
                               "Handled: " + STRING(oiRead) + CHR(10) + 
                               " Errors: " + STRING(oiErrors) 
      ActionLog.ActionStatus = 3.
END.

/******* Main end *********/


PROCEDURE pInitialize:    
   
   DEF VAR liSeq   AS INT NO-UNDO.
   DEF VAR ldToday AS DEC NO-UNDO.
   
   ASSIGN
      lcLogFile      = fCParamC("IFSCollActionLog")
      lcTransDir     = fCParamC("IFSCollActionLogTrans") 
      lcArcDir       = fCParamC("IFSCollActionArc")
      lcDebitBarrings = fGetBarringsInGroup("Collections")
      ldToday        = Func.Common:mMake2DT(TODAY,1)
      llLogWritten   = FALSE.

   IF lcLogFile = ? OR lcLogFile = "" THEN 
      lcLogFile = "/tmp/IFS_collaction_#DATE.log".
 
   liSeq = 1.
   FOR EACH ActionLog NO-LOCK WHERE
            ActionLog.Brand    = Syst.Var:gcBrand      AND
            ActionLog.ActionID = "IFSCOLLECT" AND
            ActionLog.ActionTS >= ldToday:
      liSeq = liSeq + 1.
   END.
 
   ASSIGN
      lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                            STRING(MONTH(TODAY),"99") +
                                            STRING(DAY(TODAY),"99"))
      lcLogFile = REPLACE(lcLogFile,"#SEQ",STRING(liSeq,"999")). 
  
   /* file without the dir */
   lcPlainFile = icFile.
   IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
      lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").

END PROCEDURE. /* pInitialize */

PROCEDURE pReadEvents:

   REPEAT:

      lcReadLine = "".
     
      IMPORT STREAM sRead UNFORMATTED lcReadLine.
   
      ASSIGN
         oiRead    = oiRead + 1
         liCustNum = 0
         liMsSeq   = 0
         lcAction  = "".
   
      IF NOT SESSION:BATCH AND 
         (oiRead < 100 OR oiRead MOD 100 = 0) THEN DO:
         PAUSE 0.
         DISP oiRead oiErrors WITH FRAME fQty.
      END.
   
      IF lcReadLine = "" THEN DO:
         fError("Empty line").
         NEXT.
      END.

      ASSIGN 
         lcActionID   = SUBSTRING(lcReadLine,1,12)           
         liCustNum    = INTEGER(SUBSTRING(lcReadLine,33,10)) 
         liMsSeq      = INTEGER(SUBSTRING(lcReadLine,43,12)) 
         lcAction     = SUBSTRING(lcReadLine,59,4)           
         lcActionDate = SUBSTRING(lcReadLine,63,8)           
         lcDebitDate  = SUBSTRING(lcReadLine,71,8)   
         lcAmount     = SUBSTRING(lcReadLine,79,16) 
         lcCompanyID  = SUBSTRING(lcReadLine,99,4) 
         lcBarrPacket = ""
         NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Invalid format").
         NEXT.
      END.

      FIND FIRST Customer WHERE Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Customer THEN DO:
         fError("Unknown customer").
         NEXT.
      END.
   
      FIND FIRST MsOwner WHERE 
                 MsOwner.InvCust = Customer.CustNum AND
                 MsOwner.MsSeq   = liMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MsOwner THEN DO:
         fError("Subscription not available").
         NEXT.
      END.
   
      ldAmount = 0.
 
      /* ifs uses dot as decimal point */
      IF SESSION:NUMERIC-FORMAT = "european" THEN
         lcAmount = REPLACE(lcAmount,".",",").
   
      ldAmount = DECIMAL(lcAmount) NO-ERROR.                        
      
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Invalid Amount").
         NEXT.
      END.
           
      CASE lcAction:
          WHEN "ZY13" THEN lcBarrPacket = "Debt_Restricted".
          WHEN "ZY14" THEN lcBarrPacket = "Debt_Hotl".
          WHEN "ZY17" THEN lcBarrPacket = "Debt_LP".
          WHEN "ZY18" THEN lcBarrPacket = "Debt_HOTLP". 
          WHEN "ZY99" THEN lcBarrPacket = "UN". 
          OTHERWISE lcBarrPacket = "".
      END CASE.
   
      IF lcBarrPacket > "" THEN DO:
         RUN pSetBarring(liMsSeq,
                         lcBarrPacket,
                         lcActionID).

         IF RETURN-VALUE > "" THEN DO:

            IF RETURN-VALUE BEGINS "ERROR:" THEN DO:
               fError(ENTRY(2,RETURN-VALUE,":")).
               NEXT.
            END.
         
            ELSE IF RETURN-VALUE NE "OK" THEN DO:
               fLogLine(RETURN-VALUE).
            END.
         END.
      END.
      ELSE DO:
         fError("Invalid action code").
         NEXT.
      END.   

   END.

END PROCEDURE.  /* pReadEvents */

PROCEDURE pSetBarring:

   DEF INPUT PARAMETER iiMsSeq      AS INT  NO-UNDO.
   DEF INPUT PARAMETER icSetBarring AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icActionID   AS CHAR NO-UNDO.

   DEF VAR lcResult  AS CHAR NO-UNDO.
   DEF VAR liRequest AS INT  NO-UNDO.
   DEF VAR llOngoing AS LOG  NO-UNDO.
   DEF VAR lrIdle    AS ROWID NO-UNDO.
   DEF VAR lcRemoveBarring AS CHAR NO-UNDO.
   DEF VAR liCount AS INT NO-UNDO.
   DEF VAR liAmt AS INT NO-UNDO.
   DEF VAR lcPayType AS CHAR NO-UNDO. 
   DEF VAR lcAllowedPayType AS CHAR NO-UNDO. 
   DEF VAR lcBarring AS CHAR NO-UNDO. 
   DEF VAR icBarrCommand AS CHAR NO-UNDO. 
   DEF VAR lcBarrTrans AS CHAR NO-UNDO. 
   
   /* subscription already terminated -> no action needed */
   FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN 
      RETURN "ERROR:Subscription not found".
   ELSE IF MobSub.msStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE} THEN
      RETURN "ERROR:Not allowed for fixed only".
   ASSIGN
      /* check current barring (or pending) */
      llOngoing  = fCheckBarrStatus(iiMsSeq, OUTPUT lcResult, OUTPUT lrIdle )
      liRequest = 0.
      
   /* pending exists */
   IF llOngoing EQ TRUE THEN RETURN "ERROR:Barring pending".
      
   IF icSetBarring = "UN" THEN DO:
    
      /* nothing to unbarr */  
      IF lcResult = "" THEN 
         RETURN "ERROR:No barrings active".
    
      /*Remove all Debit barrings*/
      DO liCount = 1 TO NUM-ENTRIES(lcResult):
         lcBarring = ENTRY(liCount,lcResult).
         IF LOOKUP(lcBarring, lcDebitBarrings) > 0 THEN DO:
            lcRemoveBarring = lcRemoveBarring + lcBarring + "=0,". 
            liAmt = liAmt + 1.
         END.
         icBarrCommand = RIGHT-TRIM(lcRemoveBarring,",").
      END.
      IF liAmt EQ 0 THEN RETURN "ERROR:Debt barring not active".
      ELSE DO:
         /*YDR-2843: Generate sms when barring is removed.*/
         IF Mm.MManMessage:mGetMessage("SMS", "ReconnSMS", 1) EQ TRUE THEN DO:
            Mm.MManMessage:mCreateMMLogSMS(MobSub.CLI).
         END.
      END.
   END.

   /*nbs: If there is already D level barring, IFS operation overwrites it*/
   ELSE DO: 
      
      /* already on */
      IF fIsInList(lcResult, icSetBarring) EQ TRUE THEN
         RETURN "INFORMATION: " + icSetBarring + " already on".
   
      ASSIGN
         lcAllowedPayType = fGetBarrAlowedPayment(icSetBarring)
         lcPayType = STRING(MobSub.PayType,"Prepaid/Postpaid").
      
      IF lcAllowedPayType NE "" AND
         LOOKUP(lcPayType,lcAllowedPayType) = 0 THEN
         RETURN "ERROR:" + icSetBarring + " is not allowed for " + lcPayType.
   
      icBarrCommand = icSetBarring + "=1".
     /*result: D_BARR1=1,D_BARR2=0*/
   END.
     
   /* create barring request */
   RUN Mm/barrengine.p(iiMsSeq,
                   icBarrCommand,
                   "9",                /* source  */
                   "Collection",       /* creator */
                   Func.Common:mMakeTS() + 0.0012, /* activate, 2min delay */
                   "",                 /* SMS */
                   OUTPUT lcResult).

   /* another barring request was created after last check */
   IF lcResult = "ONC" THEN RETURN "ERROR:Barring pending".
                               
   liRequest = INTEGER(lcResult) NO-ERROR. 
                               
   IF liRequest > 0 THEN DO:     
      FIND FIRST MsRequest WHERE MsRequest.MsRequest = liRequest
         EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE MsRequest THEN DO: 
         MsRequest.ReqCParam3 = icActionID.
         RELEASE MsRequest.   
      END.   

      lcBarring = ENTRY(1,icBarrCommand,"=").
      lcBarrTrans = fGetItemName(Syst.Var:gcBrand,
                                 "BarringCode",
                                 lcBarring,
                                 5,
                                 TODAY).
      IF lcBarrTrans > "" THEN 
         lcBarring = lcBarring + " (" + lcBarrTrans + ")".

      CREATE Memo.
      ASSIGN 
         Memo.Brand     = Syst.Var:gcBrand
         Memo.HostTable = "MobSub"
         Memo.KeyValue  = STRING(MobSub.MsSeq)
         Memo.CustNum   = MobSub.CustNum
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CreUser   = "IFS" 
         Memo.MemoType  = "service"
         Memo.MemoTitle = "Collection Action"
         Memo.MemoText  = lcBarring +
                         (IF icSetBarring EQ "UN"
                          THEN " released"
                          ELSE " applied")
         Memo.CreStamp  = Func.Common:mMakeTS().
      
      RETURN "OK".
   END.             
         
   ELSE RETURN "ERROR:Barring request creation failed".
      
END PROCEDURE. /* pSetBarring */
