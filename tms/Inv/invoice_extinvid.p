/* ----------------------------------------------------------------------
  MODULE .......: invoice_extinvid.p
  TASK .........: Check invoice numbering and renumber if needed
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 17.11.08
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{finvnum.i}
{funcrunprocess_update.i}

DEF INPUT  PARAMETER idtInvDate       AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiInvType        AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiAction         AS INT  NO-UNDO. 
DEF INPUT  PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdateInterval AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiHandled        AS INT  NO-UNDO.

/* iiAction:
   0=check 
   1=check and renumber
   2=number (no checks, handle only previously unnumbered)
*/   

DEF VAR liPrevious   AS INT  NO-UNDO.
DEF VAR lcPrevious   AS CHAR NO-UNDO.
DEF VAR liCheck      AS INT  NO-UNDO.
DEF VAR liRedo       AS INT  NO-UNDO.
DEF VAR llRenumber   AS LOG  NO-UNDO. 
DEF VAR liInvID      AS INT  NO-UNDO.
DEF VAR lcResult     AS CHAR NO-UNDO.
DEF VAR liOtherBatch AS INT  NO-UNDO.
DEF VAR lcOtherID    AS CHAR NO-UNDO.
DEF VAR liOtherID    AS INT  NO-UNDO.
DEF VAR lcStarted    AS CHAR NO-UNDO.
DEF VAR lcCurrent    AS CHAR NO-UNDO.
DEF VAR ldThisRun    AS DEC  NO-UNDO.
DEF VAR ldCheckRun   AS DEC  NO-UNDO.
DEF VAR liRenumbered AS INT  NO-UNDO.

DEF TEMP-TABLE ttGroup NO-UNDO
   FIELD InvGroup  AS CHAR
   FIELD InvPrefix AS CHAR
   FIELD InvNum    AS INT
   INDEX InvGroup InvGroup.
   
FORM
   liCheck COLON 10 LABEL "Check" SKIP
   liRedo  COLON 10 LABEL "Renumber"
WITH OVERLAY SIDE-LABELS ROW 10 CENTERED TITLE " Check IDs " FRAME fQty.

FORM
   SKIP(1) 
   "Waiting invoice runs to finish ..       " AT 10 SKIP(1) 
   lcStarted     COLON 15 FORMAT "X(20)"         LABEL "Started" 
   lcCurrent     COLON 15 FORMAT "X(20)"         LABEL "Latest Loop" 
   SKIP(1)
WITH CENTERED ROW 8 SIDE-LABELS OVERLAY
   TITLE " SET INVOICE IDS " FRAME fWait.


FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   llRenumber = TRUE.
   
   DO TRANS:
      CREATE ErrorLog.
      ASSIGN 
         ErrorLog.Brand     = gcBrand
         ErrorLog.ActionID  = "INVEXTID"
         ErrorLog.TableName = "Invoice"
         ErrorLog.KeyValue  = STRING(YEAR(idtInvDate),"9999") + 
                              STRING(MONTH(idtInvDate),"99") + 
                              STRING(DAY(idtInvDate),"99")
         ErrorLog.ErrorMsg  = icMessage + ": " +
                              STRING(Invoice.CustNum) +
                              " Nbr: " + STRING(Invoice.InvNum)
         ErrorLog.ErrorChar = Invoice.ExtInvID
         ErrorLog.UserCode  = katun.
         ErrorLog.ActionTS  = fMakeTS().
   END.
 
END FUNCTION.

FUNCTION fCreateActionLog RETURNS LOGIC
   (iiStatus AS INT,
    icText   AS CHAR):

   DO TRANS:
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "Invoice"  
         ActionLog.ActionID     = "ExtInvID"
         ActionLog.KeyValue     = STRING(YEAR(idtInvDate),"9999") + 
                                  STRING(MONTH(idtInvDate),"99") + 
                                  STRING(DAY(idtInvDate),"99")
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
         ActionLog.ActionDec    = iiInvType
         ActionLog.UserCode     = katun
         ActionLog.ActionStatus = iiStatus
         ActionLog.ActionChar   = icText
         ActionLog.ActionTS     = ldThisRun.
         
      RELEASE ActionLog.   
   END.
   
END FUNCTION.

/* get next external invoice id */
FUNCTION fLocalNextExtID RETURNS CHARACTER
   (icSeqPrefix AS CHAR,
    icExtInvID  AS CHAR):

   DEF VAR liSeqInvNum AS INT NO-UNDO.
   
   /* remove prefix (don't use replace) */
   IF icSeqPrefix > "" AND icExtInvID BEGINS icSeqPrefix THEN DO:
      IF LENGTH(icExtInvID) > LENGTH(icSeqPrefix)
      THEN icExtInvID = SUBSTRING(icExtInvID,LENGTH(icSeqPrefix) + 1).
      ELSE icExtInvID = "".
   END.
         
   liSeqInvNum = INTEGER(icExtInvID) NO-ERROR.
         
   /* invalid integer value */
   IF ERROR-STATUS:ERROR THEN RETURN "".

   RETURN icSeqPrefix + STRING(liSeqInvNum + 1,"99999999").
   
END FUNCTION.


/***** Main start *****/

ASSIGN
   llRenumber   = FALSE
   liRenumbered = 0
   ldThisRun    = fMakeTS().

/* if numbering of new invoices required, then make sure that there are not
   any billing runs active */
IF iiAction = 2 THEN DO:

   ASSIGN
      lcStarted  = fTS2HMS(ldThisRun)
      ldCheckRun = fOffSet(ldThisRun,-72).

   /* check that there isn't already another run for the same purpose */
   IF CAN-FIND(FIRST ActionLog USE-INDEX ActionID WHERE
                     ActionLog.Brand        = gcBrand     AND    
                     ActionLog.ActionID     = "ExtInvID"  AND
                     ActionLog.ActionTS    >= ldCheckRun  AND
                     ActionLog.ActionStatus = 0           AND
                     ActionLog.FromDate     = idtInvDate)
   THEN DO:
      lcResult = "ERROR:Another numbering run has already been started".
      IF NOT SESSION:BATCH THEN 
         MESSAGE lcResult VIEW-AS ALERT-BOX INFORMATION.
      RETURN lcResult.
   END.

   /* mark this run started */
   fCreateActionLog(0,"").

   /* check that all invoice runs are finished */
   DO WHILE TRUE 
   ON STOP UNDO, RETRY
   ON QUIT UNDO, RETRY:

      IF RETRY THEN DO:
         RUN pMarkFinished.
         RETURN "Billing run ongoing".
      END.
      
      IF NOT CAN-FIND(FIRST ActionLog USE-INDEX ActionID WHERE
                            ActionLog.Brand        = gcBrand    AND    
                            ActionLog.ActionID     = "BillRun"  AND
                            ActionLog.ActionTS    >= ldCheckRun AND
                            ActionLog.TableName    = "Invoice"  AND
                            ActionLog.ActionStatus = 0)
      THEN LEAVE.

      IF SESSION:BATCH THEN DO:
         RUN pMarkFinished.
         RETURN "ERROR:Billing run ongoing".
      END.
         
      lcCurrent = fTS2HMS(fMakeTS()).
   
      PAUSE 0.
      DISPLAY lcStarted lcCurrent WITH FRAME fWait.
   
      PUT SCREEN ROW 22 COL 2 "F8 TO QUIT".
 
      READKEY PAUSE 60.
    
      IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN DO:
         RUN pMarkFinished.
         RETURN "Billing run ongoing".
      END.
    
      PUT SCREEN ROW 22 COL 2 FILL(" ",20).
   END.

   HIDE FRAME fWait NO-PAUSE.
END.


IF NOT SESSION:BATCH THEN DO:
   PAUSE 0.
   VIEW FRAME fQty.
END.

IF iiAction = 2 THEN llRenumber = TRUE.

ELSE DO: 
   RUN pCheckNumbering(idtInvDate,
                       iiInvType).
   IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.

   /* only check */
   IF iiAction = 0 THEN DO:
      IF llRenumber THEN lcResult = "ERROR:Renumbering needed".
   
      llRenumber = FALSE.
   END.
END.

IF llRenumber THEN DO:
   RUN pRenumber(idtInvDate,
                 iiInvType,
                 (iiAction = 2)).
   IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.

   IF liRenumbered = 0 THEN lcResult = "ERROR:Renumbering failed".
   ELSE lcResult = STRING(liRenumbered) + " invoices renumbered".

   RUN pMarkFinished.
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

IF iiAction = 2 THEN oiHandled = liRenumbered.
ELSE oiHandled = liCheck.

RETURN lcResult.

/***** Main end ******/


PROCEDURE pCheckNumbering:

   DEF INPUT  PARAMETER idtInvDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiInvType    AS INT  NO-UNDO.

   FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
            Invoice.Brand   = gcBrand    AND
            Invoice.InvDate = idtInvDate AND
            Invoice.InvType = iiInvType
   BY Invoice.ExtInvID
   ON STOP UNDO, LEAVE:         

      liCheck = liCheck + 1.
      IF NOT SESSION:BATCH AND liCheck MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP liCheck WITH FRAME fQty.
      END.

      IF iiUpdateInterval > 0 AND liCheck MOD iiUpdateInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,liCheck) THEN
            RETURN "ERROR:Stopped".
      END.   
   
      IF Invoice.ExtInvID = "" THEN DO:
         fError("Empty ID").
         NEXT.
      END.
   
      liInvID = INTEGER(SUBSTRING(Invoice.ExtInvID,5)) NO-ERROR.
      IF ERROR-STATUS:ERROR OR liInvID = 0 THEN DO:
         fError("Invalid ID " + Invoice.ExtInvID).
         NEXT.
      END.

      IF liInvID NE liPrevious + 1 THEN DO:
   
         /* gap in this batch */
         IF liPrevious > 0 AND lcPrevious = SUBSTRING(Invoice.ExtInvID,1,4)
         THEN DO:
            fError("Gap before " + Invoice.ExtInvID).
         END.

         /* 1st with this prefix in this batch, previous one should be found 
            in the system also */
         ELSE IF lcPrevious NE SUBSTRING(Invoice.ExtInvID,1,4) AND 
            liInvId > 1 
         THEN DO:

            liOtherBatch = 0.

            /* check just 5 nbrs backwards, if gap is bigger than that then
               it is probably intentional */
            DO liOtherID = liInvID - 1 TO liInvID - 10 BY -1:

               lcOtherID = SUBSTRING(Invoice.ExtInvID,1,4) +
                           STRING(liOtherID,"99999999").
            
               IF CAN-FIND(FIRST Invoice WHERE 
                                 Invoice.Brand    = gcBrand AND
                                 Invoice.ExtInvID = lcOtherID)
               THEN DO:
                  liOtherBatch = liOtherID.
                  LEAVE.
               END.
            END.              

            IF liOtherBatch > 0 AND liInvID NE liOtherBatch + 1 THEN DO:
               fError("Gap before " + Invoice.ExtInvID + 
                      " (previous batches)").
            END.
         END.
      END.
   
      ASSIGN
         liPrevious = liInvID
         lcPrevious = SUBSTRING(Invoice.ExtInvID,1,4).
    
      IF NOT CAN-FIND(FIRST IgInvNum WHERE
                            IgInvNum.Brand   = gcBrand AND
                            IgInvNum.InvType = Invoice.InvType AND
                            IgInvNum.SeqPrefix = lcPrevious)
      THEN DO:
         fError("Invalid prefix " + Invoice.ExtInvID).
      END.

   END.
   
   RETURN "".

END PROCEDURE.

PROCEDURE pRenumber:

   DEF INPUT  PARAMETER idtInvDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiInvType    AS INT  NO-UNDO.
   DEF INPUT  PARAMETER ilFirstSet   AS LOG  NO-UNDO.

   DEF BUFFER bInv FOR Invoice.
   
   DEF VAR lcPrefix   AS CHAR NO-UNDO.
   DEF VAR liNumber   AS INT  NO-UNDO.
   DEF VAR lcExtInvID AS CHAR NO-UNDO.
   DEF VAR liCnt      AS INT  NO-UNDO.


   /* current sequences */
   FOR EACH InvGroup NO-LOCK WHERE
            InvGroup.Brand = gcBrand,
      FIRST IgInvNum NO-LOCK WHERE
            IgInvNum.Brand    = gcBrand AND
            IgInvNum.InvGroup = InvGroup.InvGroup AND
            IgInvNum.InvType  = iiInvType AND
            IgInvNum.FromDate <= idtInvDate:

      CREATE ttGroup.
      ASSIGN 
         ttGroup.InvGroup  = InvGroup.InvGroup
         ttGroup.InvPrefix = IgInvNum.SeqPrefix
         ttGroup.InvNum    = IgInvNum.InvNum.
   END.
   
   IF NOT ilFirstSet THEN DO:
   
      /* remove current numbers from invoices */
      FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
               Invoice.Brand      = gcBrand    AND
               Invoice.InvDate    = idtInvDate AND
               Invoice.InvType    = iiInvType  AND
               Invoice.PrintState = 0          AND
               Invoice.DDState    = 0,
         FIRST Customer NO-LOCK WHERE
               Customer.CustNum = Invoice.CustNum:

         IF Invoice.ExtInvID = "" THEN NEXT.
      
         ASSIGN 
            lcPrefix = SUBSTRING(Invoice.ExtInvID,1,4)
            liNumber = INTEGER(SUBSTRING(Invoice.ExtInvId,5)) NO-ERROR.

         IF NOT ERROR-STATUS:ERROR THEN DO:
            FIND FIRST ttGroup WHERE
                       ttGroup.InvGroup = Customer.InvGroup NO-ERROR.
            IF AVAILABLE ttGroup AND ttGroup.InvPrefix = lcPrefix THEN 
               ttGroup.InvNum = MIN(ttGroup.InvNum,liNumber - 1).
         END.
       
         DO FOR bInv TRANS:
            FIND bInv WHERE RECID(bInv) = RECID(Invoice) EXCLUSIVE-LOCK.
            bInv.ExtInvID = "".
         END.

      END.

      /* set the correct starting point for sequences */
      FOR EACH ttGroup:
   
         ASSIGN 
            ttGroup.InvNum = MAX(0,ttGroup.InvNum)
            liNumber       = ttGroup.InvNum.
      
         /* try to make sure that previous numbering started correctly */
         DO liCnt = 1 TO 20:

            lcExtInvID = ttGroup.InvPrefix + STRING(liNumber,"99999999").
         
            IF CAN-FIND(FIRST Invoice WHERE 
                              Invoice.Brand    = gcBrand AND
                              Invoice.ExtInvID = lcExtInvID)
            THEN DO:
               ttGroup.InvNum = liNumber.
               LEAVE.
            END.

            liNumber = MAX(0,liNumber - 1).
         END.              

         FOR FIRST IgInvNum EXCLUSIVE-LOCK WHERE
                   IgInvNum.Brand    = gcBrand AND
                   IgInvNum.InvGroup = ttGroup.InvGroup AND
                   IgInvNum.InvType  = iiInvType AND
                   IgInvNum.FromDate <= idtInvDate AND
                   IgInvNum.SeqPrefix = ttGroup.InvPrefix:
            IgInvNum.InvNum = ttGroup.InvNum.       
         END.           

      END.
   END.  /* first setting */
   
   /* set new numbers */
   FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
            Invoice.Brand      = gcBrand    AND
            Invoice.InvDate    = idtInvDate AND
            Invoice.ExtInvID   = ""         AND 
            Invoice.InvType    = iiInvType  AND
            Invoice.PrintState = 0          AND
            Invoice.DDState    = 0,
      FIRST Customer NO-LOCK WHERE
            Customer.CustNum = Invoice.CustNum
   BY Invoice.InvNum:

      lcExtInvID = fGetInvNum(Customer.InvGroup,
                              Invoice.Invtype,
                              Invoice.Invdate,
                              OUTPUT lcPrefix).

      REPEAT:
         IF NOT CAN-FIND(FIRST bInv WHERE 
                               bInv.Brand = gcBrand AND
                               bInv.ExtInvID = lcExtInvID) 
         THEN LEAVE.
         
         lcExtInvID = fLocalNextExtID(lcPrefix,
                                      lcExtInvId).
      END.

      DO FOR bInv TRANS:
         FIND bInv WHERE RECID(bInv) = RECID(Invoice) EXCLUSIVE-LOCK.
         ASSIGN
            bInv.ExtInvID = lcExtInvID
            bInv.RefNum   = lcExtInvID.
      END.
      
      fUpdateInvNum(Customer.InvGroup,
                    Invoice.InvType,
                    Invoice.InvDate,
                    lcExtInvID).

      /* payments may have been made during billing run */
      FOR EACH Payment EXCLUSIVE-LOCK WHERE
               Payment.InvNum = Invoice.InvNum:
         Payment.ExtInvID = lcExtInvID.
      END.
 
      liRenumbered = liRenumbered + 1.  

      liRedo = liRedo + 1.
      IF NOT SESSION:BATCH AND liRedo MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP liRedo WITH FRAME fQty.
      END.
 
      IF iiUpdateInterval > 0 AND liRenumbered MOD iiUpdateInterval = 0 
      THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,liRenumbered) THEN
            RETURN "ERROR:Stopped".
      END.   

   END.

   RETURN "".
   
END PROCEDURE.

PROCEDURE pMarkFinished:

   /* mark this run finished */
   FIND FIRST ActionLog USE-INDEX ActionID WHERE
              ActionLog.Brand        = gcBrand     AND    
              ActionLog.ActionID     = "ExtInvID"  AND
              ActionLog.ActionTS     = ldThisRun   AND
              ActionLog.ActionStatus = 0 NO-LOCK NO-ERROR.

   IF NOT AVAILABLE ActionLog THEN DO:
      fCreateActionLog(IF liRenumbered > 0 THEN 2 ELSE 1,
                       lcResult).
   END.
   ELSE DO TRANS:
      FIND CURRENT ActionLog EXCLUSIVE-LOCK.
      ASSIGN 
         ActionLog.ActionStatus = IF liRenumbered > 0 THEN 2 ELSE 1
         ActionLog.ActionChar   = lcResult.
   END.

END PROCEDURE.


