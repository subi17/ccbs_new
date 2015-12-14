/* -----------------------------------------------------
  MODULE .......: DEL_INV.P
  FUNCTION .....: Remove one invoice + linked data
  APPLICATION ..: TMPS
  AUTHOR .......: 
  CREATED ......: 09.10.01 kl
  MODIFIED .....: 05.02.02/aam Deposit returned TO customer correctly
                  06.02.02 kl: monthly invseqs created again (IF needed)
                  07.02.02 jp: MobCDR handling
                  08.02.02 jp: SingleFee 
                  27.03.02/aam DELETE OPLog
                  07.06.02/aam use Invoice.OverPaym FOR overpayment
                  26.09.02/aam customer balances in table CustBal
                  06.11.02 kl: found invseq for older calls must be UNBILLED
                  08.11.02/jr  Eventlog 
                  10.12.02/aam delete payments
                  11.09.03/aam brand,
                               invseq-update tried to use buffer mcdr (no-lock)
                  02.12.03/jp  xfatime             
                  30.03.04 kl  fixes after index changes
                  06.05.04/aam delete SingleFee if BillRun matches
                  11.01.05/aam check fees from event customers
                  04.02.05/aam use nnpcst,
                               eventlog from payment
                  08.08.05/aam clean objects made by eventlog routine
                  16.12.05/aam delete invasub and invccn here (not in nnpcst)
                  18.04.07/aam ActionLog
                  18.09.07/aam use msseq for keyvalue in actionlog
  Version ......: M15
  -------------------------------------------------------------------------- */

DISABLE TRIGGERS FOR LOAD OF FixedFee.
DISABLE TRIGGERS FOR LOAD OF SingleFee.

{commali.i}
{invseq.i}
{fcustbal.i}
{fcustcnt.i}
{eventval.i} 
{nnpcst.i}
{combine_invseq.i}

DEF INPUT PARAMETER pInvNo AS i NO-UNDO.

DEF BUFFER xxlasku    FOR Invoice.
DEF BUFFER xcall      FOR FixCDR.
DEF BUFFER xMobCDR    FOR MobCDR.
DEF BUFFER xFatime    FOR Fatime.
DEF BUFFER bEventCust FOR Customer.

DEF VAR lInvSeq AS i    NO-UNDO.
DEF VAR llDo    AS LOG  NO-UNDO.
DEF VAR liCDR   AS INT  NO-UNDO.
DEF VAR ldaFromDate AS DATE NO-UNDO.
DEF VAR ldaToDate   AS DATE NO-UNDO. 

IF llDoEvent THEN DO:

   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.
   RUN StarEventInitialize(lhInvoice).

   DEFINE VARIABLE lhPayment AS HANDLE NO-UNDO.
   lhPayment = BUFFER Payment:HANDLE.
   RUN StarEventInitialize(lhPayment).

END.


FIND FIRST Invoice WHERE Invoice.InvNum = pInvNo NO-LOCK NO-ERROR.
IF NOT AVAILABLE Invoice THEN RETURN "ERROR:Unknown invoice".

FIND FIRST Customer WHERE
         Customer.CustNum  = Invoice.CustNum NO-LOCK.

   IF CAN-FIND(FIRST Payment OF Invoice) THEN DO:
      llDo = FALSE.
      IF NOT SESSION:BATCH THEN DO:
         MESSAGE "Invoice has atleast one payment, delete anyway ?"
         VIEW-AS ALERT-BOX QUESTION
         BUTTONS YES-NO
         SET llDo.
      END.
      IF NOT llDo THEN DO:
         fCleanEventObjects().
         RETURN "ERROR:Payments on invoice".
      END.   
   END.
                                        
   /* invoice rows ... */
   FOR EACH InvRow of Invoice EXCLUSIVE-LOCK:
      DELETE InvRow.
   END.

   FOR EACH SubInvoice OF Invoice EXCLUSIVE-LOCK:

      /* release events */
      RUN nnpcst.p (Invoice.InvNum,
                    SubInvoice.SubInvNum,
                    FALSE,
                    INPUT table wMarked).

      /* advance payments  */
      IF SubInvoice.AdvPaym NE 0 THEN DO:

         fCustBal(Invoice.CustNum,
                  SubInvoice.CLI,             
                  "AP",
                  -1 * SubInvoice.AdvPaym). 

         /* log-file */
         FOR EACH OPLog EXCLUSIVE-LOCK where
                  OPLog.InvNum    = Invoice.InvNum AND
                  OPLog.SubInvNum = SubInvoice.SubInvNum:

            DELETE OPLog.
         END.
      END.

      /* overpayments  */
      IF SubInvoice.OverPaym NE 0 THEN DO:

         fCustBal(Invoice.CustNum,
                  SubInvoice.CLI,
                  "OP",
                  -1 * SubInvoice.OverPaym). 

         /* log-file */
         FOR EACH OPLog EXCLUSIVE-LOCK where
                  OPLog.InvNum = Invoice.InvNum AND
                  OPLog.SubInvNum = SubInvoice.SubInvNum:

            DELETE OPLog.
         END.   
      END.

      /* minimum consumption */
      FOR FIRST MinConsumption EXCLUSIVE-LOCK WHERE
                MinConsumption.MsSeq  = SubInvoice.MsSeq AND
                MinConsumption.InvNum = Invoice.InvNum:
         DELETE MinConsumption.
      END.
                                     
      /* combine invseqs (concerns mainly test invoices for current period) */
      IF SubInvoice.MsSeq > 0 THEN 
         RUN pCombineInvSeq(Invoice.CustNum,
                            SubInvoice.MsSeq,
                            Invoice.FromDate,
                            Invoice.ToDate,
                            OUTPUT liCDR).
                                       
      DELETE SubInvoice.
   END.

   /* make sure that nothing is left */
   RUN nnpcst.p (Invoice.InvNum,
                 0,
                 FALSE,
                 INPUT table wMarked).

   /* IF accounts have TO be updated */
   FIND FIRST InvGroup where 
              InvGroup.Brand    = Invoice.Brand AND
              InvGroup.InvGroup = Customer.InvGroup
   no-lock no-error.

   IF InvGroup.UpdCustBal THEN DO:

      fCustBal(Invoice.CustNum,
               "", 
               "ARBAL",
               -1 * Invoice.InvAmt). 

      fCustCount(Invoice.CustNum,
                 "UB",
                 Invoice.AmtExclVat). 
   END.

   /* payments */
   FOR EACH Payment OF Invoice EXCLUSIVE-LOCK:
      IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPayment).
      DELETE Payment.
   END. 

   /* counters */
   FOR EACH InvASub OF Invoice EXCLUSIVE-LOCK:
      DELETE InvASub.
   END.
   FOR EACH InvCCN OF Invoice EXCLUSIVE-LOCK:
      DELETE InvCCN.
   END.

   /* rerate customer */
   FIND FIRST TriggerConf WHERE
              TriggerConf.TriggerConfID = "Special_List"  AND
              TriggerConf.EventRule     > 0               AND
              TriggerConf.ValidTo       >= Today          AND
              TriggerConf.ValidFrom     <= Today NO-LOCK NO-ERROR.

   IF AVAIL TriggerConf THEN DO TRANS:
   
      CREATE TriggerEvent.
      ASSIGN
         TriggerEvent.TriggerEventID = NEXT-VALUE(TriggerEvent)
         TriggerEvent.TriggerConfID  = TriggerConf.TriggerConfID
         TriggerEvent.EventSource    = "MODIFY"
         TriggerEvent.Created        = NOW
         TriggerEvent.TableID        = 0
         TriggerEvent.TableName      = TriggerConf.TriggerConfID
         TriggerEvent.Keyvalue       = ""
         TriggerEvent.ChangedFields  = "Remove_Test_Invoice"
         TriggerEvent.ChangedValues  = STRING(Invoice.CustNum) + ",".
                    
      RELEASE TriggerEvent.
   END.
                                       
   DO TRANS:
      IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhInvoice).
      FIND CURRENT Invoice EXCLUSIVE-LOCK.
      DELETE Invoice.
   END.

   /* customer's invoicing InstDuePeriod */
   FIND FIRST xxlasku where
              xxlasku.Brand    = Customer.Brand AND
              xxlasku.CustNum  = Customer.Custnum AND
              xxlasku.CrInvNum = 0 AND
              xxlasku.InvAmt   > 0
   no-lock no-error.
   
   IF AVAIL xxlasku THEN
        fLatestInv(Customer.CustNum,
                   xxlasku.CLI,
                   xxlasku.InvDate). 

   ELSE fCustBal(Customer.CustNum,
                 "",
                 "INVP",
                 0.00). 


/* eventlog creates dynamic temp-table, but never cleans it out */
fCleanEventObjects().

RETURN "".


