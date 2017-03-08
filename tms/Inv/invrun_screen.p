/* ----------------------------------------------------------------------
  MODULE .......: INVRUN_SCREEN
  TASK .........: RUN Inv/lamupers.p from customer split files
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 21.01.08
  CHANGED ......: 29.01.08 llTestRun from SESSION:PARAM
                  13.08.08/aam sequence to lcBillRun            
                  04.09.08/aam split files to archive after run
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Inv/billrund.i NEW}
{Func/ftransdir.i}

DEFINE VARIABLE lcInvRunFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLine       AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCustQty    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lhHandle     AS HANDLE    NO-UNDO.
DEFINE VARIABLE liQty        AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldeAmt       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldeVatAmt    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liLine       AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldaInvDate   AS DATE      NO-UNDO.
DEFINE VARIABLE ldaDueDate   AS DATE      NO-UNDO.
DEFINE VARIABLE ldaDateFrom  AS DATE      NO-UNDO.
DEFINE VARIABLE ldaDateTo    AS DATE      NO-UNDO.
DEFINE VARIABLE liFeePeriod  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcBillRun    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMessage    AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeBegTime   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldeEndTime   AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE liDurDays    AS INTEGER   NO-UNDO.
DEFINE VARIABLE liDurTime    AS INTEGER   NO-UNDO.
DEFINE VARIABLE llTestRun    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE liInvType    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTransDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCallQty    AS INTEGER   NO-UNDO.

ASSIGN
   gcBrand      = '1'
   katun        = 'cron'
   lcInvRunFile = SESSION:PARAM
   lcTransDir   = fCParamC("SplitInvRunArc")
   ldeBegTime   = fMakeTS()
   ldaDueDate   = ?.

RUN Inv/lamupers.p PERSISTENT SET lhHandle.

DEFINE STREAM sIn.

IF LOOKUP(lcInvRunFile,",?, ") = 0 THEN DO:
   
   INPUT STREAM sIn FROM VALUE (lcInvRunFile).
   
   REPEAT:
      
      IMPORT STREAM sIn UNFORMATTED lcLine.
      
      liLine = liLine + 1.
      
      IF liLine = 1 THEN DO:
         ASSIGN
            ldaInvDate  = DATE(ENTRY(1,lcLine))
            ldaDateFrom = DATE(ENTRY(2,lcLine))
            ldaDateTo   = DATE(ENTRY(3,lcLine))
            liFeePeriod = INTEGER(ENTRY(4,lcLine))
            llTestRun   = (LOOKUP(ENTRY(6,lcLine),"1,yes,true") > 0).
             
         IF NUM-ENTRIES(lcLine) >= 7 AND ENTRY(7,lcLine) > "" THEN DO:
            ldaDueDate = DATE(ENTRY(7,lcLine)).
          
            IF ldaDueDate < ldaInvDate THEN DO:
               MESSAGE "Due date cannot be earlier than invoice date"
               VIEW-AS ALERT-BOX ERROR.
               QUIT.
            END.
            
            IF ldaDueDate > ldaInvDate + 60 THEN DO:
               MESSAGE "Check the due date"
               VIEW-AS ALERT-BOX INFORMATION.
               QUIT.
            END.
         END.   
      END.
         
      ELSE DO:

         liCallQty = 0.
         liCallQty = INTEGER(ENTRY(2,lcLine,CHR(9))) NO-ERROR.
                        
         FIND FIRST Customer WHERE
                    Customer.Brand   = gcBrand AND 
                    Customer.CustNum = INT(ENTRY(1,lcLine,CHR(9)))
         NO-LOCK NO-ERROR.
      
         FIND FIRST InvGroup OF Customer NO-LOCK.
      
         CREATE ttInvCust.
         ASSIGN
            ttInvCust.CustNr  = Customer.CustNum
            ttInvCust.MinInv  = InvGroup.MinInvAmt
            ttInvCust.CallQty = liCallQty
            ttInvCust.LowVal  = FALSE
            liCustQty         = liCustQty + 1.

      END.

   END.
   
   INPUT STREAM sIn CLOSE.
   
   IF ldaInvDate = ? OR 
      ldaDateFrom = ? OR
      ldaDateTo = ? OR
      liFeePeriod = 0
   THEN DO:
      MESSAGE "Missing date / period definition"
      VIEW-AS ALERT-BOX ERROR.
      QUIT.
   END.
   
   IF llTestRun THEN ASSIGN
      lcBillRun = "TEST-BR"
      liInvType = 99.
   ELSE ASSIGN
      lcBillRun = "BR"
      liInvType = 1.

   /* use sequence InvFile to create a unique id for this run */
   lcBillRun = lcBillRun + 
               STRING(NEXT-VALUE(InvFile)) + "_" + 
               STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") +
               STRING(DAY(TODAY),"99") + STRING(TIME,"99999").
               
   /* make an entry to db that run has started */            
   IF liInvType NE 99 THEN DO FOR ActionLog TRANS:

      CREATE ActionLog.
   
      ASSIGN
         ActionLog.ActionTS     = fMakeTS()
         ActionLog.Brand        = gcBrand
         ActionLog.TableName    = "Invoice"
         ActionLog.KeyValue     = lcBillRun
         ActionLog.UserCode     = katun
         ActionLog.ActionID     = "BillRun"
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                  MONTH(TODAY)
         ActionLog.FromDate     = ldaDateFrom
         ActionLog.ToDate       = ldaDateTo
         ActionLog.ActionStatus = 0
         ActionLog.ActionChar   = lcInvRunFile.
   END.

   RUN pCreateInv IN lhHandle(ldaInvDate,
                              ldaDateFrom,
                              ldaDateTo,
                              liFeePeriod,
                              ldaDueDate,
                              ldaDueDate,
                              ?, /* Fusion due date support only in Funcrun */
                              TRUE,
                              TRUE,
                              liCustQty,
                              liInvType,
                              "*" + lcBillRun,
                              FALSE).

   ldeEndTime = fMakeTS().

   liDurDays = DYNAMIC-FUNCTION("fTSDuration" IN ghFunc1,
                                ldeBegTime,
                                ldeEndTime,
                                OUTPUT liDurTime).

   RUN pGetAmt IN lhHandle (OUTPUT liQty,
                            OUTPUT ldeAmt,
                            OUTPUT ldeVatAmt).

   RUN pUpdInvGroup in lhHandle.

   lcMessage = 
      STRING(liQty) +  
      " invoices were created." + CHR(10) + 
      "Duration for creation was " +
      (IF liDurDays > 0 
       THEN STRING(liDurDays) + " days and "
       ELSE "") +
      STRING(liDurTime,"hh:mm:ss") + CHR(10) + 
      "Total amount excluding VAT was " + 
      TRIM(STRING(ldeAmt,"->>>>>>>>9.99"))  + CHR(10) +
      "Total payable amount, including VAT and reductions was " +
      TRIM(STRING(ldeVatAmt,"->>>>>>>>9.99")) + CHR(10) +
      "Customer file: " + lcInvRunFile.

   /* mark run as finished */
   IF liInvType NE 99 THEN DO FOR ActionLog TRANS:

      FIND FIRST ActionLog WHERE
                 ActionLog.Brand        = gcBrand   AND
                 ActionLog.TableName    = "Invoice" AND
                 ActionLog.KeyValue     = lcBillRun AND
                 ActionLog.ActionID     = "BillRun" AND
                 ActionLog.ActionStatus = 0 NO-LOCK NO-ERROR.
                 
      IF NOT AVAILABLE ActionLog THEN DO:
         CREATE ActionLog.
   
         ASSIGN
            ActionLog.ActionTS     = fMakeTS()
            ActionLog.Brand        = gcBrand
            ActionLog.TableName    = "Invoice"
            ActionLog.KeyValue     = lcBillRun
            ActionLog.UserCode     = katun
            ActionLog.ActionID     = "BillRun"
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                     MONTH(TODAY)
            ActionLog.FromDate     = ldaDateFrom
            ActionLog.ToDate       = ldaDateTo.
      END.
      
      ELSE FIND Current ActionLog EXCLUSIVE-LOCK.
      
      ASSIGN
         ActionLog.ActionDec    = liQty
         ActionLog.ActionChar   = lcMessage
         ActionLog.ActionStatus = 2.

   END.

   /* move the split file to archive */
   IF lcTransDir > "" THEN DO:
      fTransDir(lcInvRunFile,
                "",
                lcTransDir).
   END.

END.

DELETE OBJECT lhHandle NO-ERROR.

QUIT.

