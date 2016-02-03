/* -----------------------------------------------------------------
   MODULE .......: invoicedump.p
   TASK .........: dump invoice information each day to file
                   caution: with input value "ALL"
                            this program dumps all TF invoices
                            result file can be very large (>100MB)
   APPLICATION ..: TMS
   AUTHOR .......: mvi
   CREATED ......: 04.03.05
   CHANGED ......: 10.03.05
                   23.03.05  jl Debug removed + taken into cron
                   25.03.06 mvi now possible to dump only new invoices
                                using input parameter ilOnlyNew
                                and new TMSParam "LastInvoiceDumped" 
                   25.03.06 mvi Dumps now Customer.agrcust; new hierarchy      
                   21.04.06 mvi now uses ChgStamp to determine new invoices
                   19.03.07 kl  filename changed, Yoigo version
                   17.08.07 pa  When run, all invoices dated yesterday is dumped
                        
                   20.07.11     Moved the invoice dump to dump conf tool and added
                                functionality for daily delta dump
                   16.09.14 iv  delta dump includes invoices with eventlog  
                                records for payment changes
   Version ......: YOIGO
--------------------------------------------------- */               

{Syst/commali.i}
{Func/cparam2.i}
{Func/date.i}
{Func/timestamp.i}
{Func/coinv.i}
{Func/refcode.i}
{Func/frefnum.i}
{Syst/dumpfile_run.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR totalTime        AS DE   NO-UNDO.
DEF VAR amt              AS DE   NO-UNDO.
DEF VAR tsEnd            AS DEC  NO-UNDO.
DEF VAR liTmp            AS INT  NO-UNDO. 
DEF VAR ldtDate2         AS DATE NO-UNDO.
DEF VAR lcDelim          AS CHAR NO-UNDO.
DEF VAR ldMark           AS DEC  NO-UNDO.
DEF VAR lcNumeric        AS CHAR NO-UNDO.
DEF VAR lcOffsetMonths   AS CHAR NO-UNDO.
DEF VAR liOffsetMonths   AS INT  NO-UNDO.
DEF VAR liFromPeriod     AS INT  NO-UNDO.
DEF VAR ldFromDate       AS DATE NO-UNDO.
DEF VAR lcModifiedFields AS CHAR NO-UNDO.

DEF VAR ldaModified      AS DATE NO-UNDO.
DEF VAR lcLastDumpTime   AS CHAR NO-UNDO.
DEF VAR liTime           AS INT  NO-UNDO.

DEF STREAM excel.
DEF BUFFER bInv FOR Invoice.

DEF TEMP-TABLE ttInv NO-UNDO
   FIELD InvNum AS INT
   INDEX InvNum InvNum.

ASSIGN lcNumeric = SESSION:NUMERIC-FORMAT
       icFile    = REPLACE(icFile,".gz","")
       ldMark    = fMakeTS().

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   ASSIGN lcDelim  = fInitDelimiter(DumpFile.DumpDelimiter)
          lcOffsetMonths = DumpFile.ConfigParam.

   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END. /* IF AVAILABLE DumpFile THEN DO: */
ELSE DO:
   ASSIGN 
      lcDelim = "\":\""
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END. /* ELSE DO: */

OUTPUT STREAM excel TO VALUE(icFile).

/* DUMP HEADERS - Commented WITH # */
PUT STREAM excel UNFORMATTED
  "#Dump started: " + fTS2C(ldMark) SKIP.

/* FULL DUMP */
IF icDumpMode = "Full" THEN DO:
   IF lcOffsetMonths = "" OR lcOffsetMonths = ? THEN
      liOffsetMonths = 5.
   ELSE
      liOffsetMonths = INT(lcOffsetMonths).

   liFromPeriod = fOffsetMonthsToPeriod(INPUT liOffsetMonths).
   IF liFromPeriod = ? OR liFromPeriod = 0 THEN
      liFromPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).
   ldFromDate = fInt2Date(INT(liFromPeriod),1).

   /* Dump ALL invoices */
   FOR EACH invoice NO-LOCK USE-INDEX InvDate WHERE
            invoice.brand    = gcBrand    AND
            invoice.invdate >= ldFromDate AND
            Invoice.DeliveryState > 0
       ON QUIT UNDO, RETRY
       ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END. /* IF RETRY THEN DO: */
      
      FIND FIRST Customer WHERE
                 customer.custnum = invoice.custnum
      NO-LOCK NO-ERROR.
      
      IF NOT AVAIL Customer THEN NEXT.

      oiEvents = oiEvents + 1.

      IF NOT SESSION:BATCH AND oiEvents mod 100 = 0 THEN DO:
         DISP oiEvents LABEL "Invoices" FORMAT ">>>>>>>>>9" 
         WITH SIDE-LABELS OVERLAY ROW 10 CENTERED TITLE " Collecting " FRAME fQty.
         PAUSE 0.
      END. /* IF NOT SESSION:BATCH AND oiInvCount mod 100 = 0 THEN DO: */

      FOR EACH SubInvoice OF Invoice NO-LOCK:
         RUN local-Dump-Invoice.
      END.
        
   END.

END. /* IF icDumpMode = "Full" THEN DO: */
ELSE DO:

   IF idLastDump > 0 THEN DO:
      fSplitTS(idLastDump,ldtDate2,liTmp).
      ldFromDate = ldtDate2 - 4.
   END. /* IF idLastDump > 0 THEN DO: */
   ELSE
      ldFromDate = (TODAY - 5).

   EMPTY TEMP-TABLE ttInv.

   FOR EACH invoice NO-LOCK WHERE
            invoice.brand    = gcBrand    AND
            invoice.invdate >= ldFromDate AND
            Invoice.DeliveryState > 0
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END. /* IF RETRY THEN DO: */

      IF invoice.chgstamp < idLastDump AND Invoice.ExpStamp > 0 THEN NEXT.

      FIND FIRST Customer WHERE
                 Customer.custnum = Invoice.custnum
      NO-LOCK NO-ERROR.
      
      IF NOT AVAIL Customer THEN NEXT.

      oiEvents = oiEvents + 1.
      
      CREATE ttInv.
      ASSIGN ttInv.InvNum = Invoice.InvNum.

      FOR EACH SubInvoice OF Invoice NO-LOCK:
         RUN local-Dump-Invoice.
      END.
      
      DO FOR bInv TRANS:
         FIND bInv WHERE ROWID(bInv) = ROWID(Invoice) EXCLUSIVE-LOCK.
         bInv.ExpStamp = ldMark.
      END.
   END. /* FOR EACH Invoice NO-LOCK WHERE */

   /* collect invoices with eventlog records that have been modified since last dump */
   fSplitTS(idLastDump,
            OUTPUT ldaModified,
            OUTPUT liTime).

   lcLastDumpTime = STRING(liTime,"hh:mm:ss").

   FOR EACH EventLog NO-LOCK WHERE
            EventLog.eventdate >= ldaModified AND
            EventLog.tablename  = "Invoice" AND
            EventLog.action     = "Modify" 
            USE-INDEX eventdate:

      IF EventLog.EventDate EQ ldaModified AND
         EventLog.eventtime < lcLastDumpTime THEN NEXT.

      IF CAN-FIND(FIRST ttInv WHERE ttInv.InvNum = INT(EventLog.Key)) THEN NEXT.

      CREATE ttInv.
      ASSIGN ttInv.InvNum = INT(EventLog.Key).

      FIND FIRST Invoice NO-LOCK WHERE
                 Invoice.InvNum = INT(EventLog.Key) NO-ERROR.

      IF NOT AVAIL Invoice OR
         NOT Invoice.DeliveryState > 0 THEN NEXT.

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      FIND FIRST Customer WHERE
                 Customer.CustNum = Invoice.CustNum
                 NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE Customer THEN NEXT.

      oiEvents = oiEvents + 1.

      FOR EACH SubInvoice OF Invoice NO-LOCK:
         RUN local-Dump-Invoice.
      END.
      
      DO FOR bInv TRANS:
         FIND bInv WHERE ROWID(bInv) = ROWID(Invoice) EXCLUSIVE-LOCK.
         bInv.ExpStamp = ldMark.
      END.

   END. /* FOR EACH EventLog NO-LOCK WHERE */

END. /* ELSE DO: - Modified - */

ASSIGN
   tsEnd     = fMakeTS().
   totalTime = (tsEnd - ldMark) * 100000.

PUT STREAM excel UNFORMATTED
  "#Dump complete: " fTS2C(tsEnd) SKIP
  "#Process time : ". 
  
  IF totaltime / 3600 > 1 THEN DO:
     PUT STREAM excel UNFORMATTED totaltime / 3600 " h ".
  END.
  
  IF totaltime / 60 > 1 THEN DO:
     PUT STREAM excel UNFORMATTED totalTime / 60 " min ".
  END.

  PUT STREAM excel UNFORMATTED totaltime " sec" SKIP.

OUTPUT STREAM excel CLOSE.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

UNIX SILENT VALUE("gzip " + icFile).
icFile = icFile + ".gz".

SESSION:NUMERIC-FORMAT = lcNumeric.

/* ------------------------------- END MAIN ------------------------------- */
                          
PROCEDURE local-Dump-Invoice:

   PUT STREAM excel UNFORMATTED
      invoice.invnum   lcDelim
      invoice.custnum  lcDelim
      invoice.agrcust  lcDelim
      invoice.fromdate lcDelim
      invoice.todate   lcDelim.

   amt = 0.
   FOR EACH invrow NO-LOCK WHERE
            invrow.invnum = invoice.invnum AND
            InvRow.SubInvNum = SubInvoice.SubInvNum AND
            invrow.amt < 0.
   
      amt = amt + invrow.amt.
   END.   

   PUT STREAM excel UNFORMATTED
      amt                     lcDelim
      SubInvoice.invamt       lcDelim
      invoice.duedate         lcDelim
      SubInvoice.paidamt      lcDelim
      SubInvoice.paymstate    lcDelim
      fFormRefNum(invoice.custnum,
                  invoice.invnum,
                  invoice.invtype) lcDelim
      invoice.invtype         lcDelim 
      SubInvoice.AmtExclVAT   lcDelim
      SubInvoice.VATamount    lcDelim
      invoice.paymdate        lcDelim
      SubInvoice.OverPaym     lcDelim
      invoice.ARAccNum        lcDelim
      invoice.ChgStamp        lcDelim
      SubInvoice.VATPercent   lcDelim
      SubInvoice.VATAccount   lcDelim
      SubInvoice.VATBasis     lcDelim
      invoice.BillRun         lcDelim
      Invoice.ClaimStatus     lcDelim
      invoice.ExtInvID        lcDelim
      invoice.TaxZone         lcDelim
      invoice.Region          lcDelim
      SubInvoice.msseq        lcDelim
      SubInvoice.CLI          lcDelim         
      invoice.InvDate         lcDelim
      SubInvoice.VATAmt       lcDelim
      invoice.PrintState      lcDelim
      invoice.FirstCall       lcDelim
      SubInvoice.Rounding     lcDelim
      invoice.xxVATPerc       lcDelim
      invoice.CustName        lcDelim
      REPLACE(invoice.Address,CHR(10),"") lcDelim
      invoice.PostOffice      lcDelim
      invoice.ClaimPerm       lcDelim
      invoice.InterestPerm    lcDelim
      invoice.ITGroupID       lcDelim
      invoice.OPAccNum        lcDelim
      invoice.CashDisc        lcDelim
      invoice.ClaimDate       lcDelim
      invoice.EarliestRem     lcDelim
      invoice.CashDiscDate    lcDelim
      invoice.EndInvoice      lcDelim
      invoice.RoundAccNum     lcDelim
      SubInvoice.InterestAmt  lcDelim
      invoice.IntAccNum       lcDelim
      invoice.TransFile       lcDelim
      invoice.InvCfg          lcDelim
      invoice.xxMemo          lcDelim
      SubInvoice.CrInvNum     lcDelim
      invoice.WInvDisp        lcDelim
      invoice.Currency        lcDelim
      invoice.VATUsage        lcDelim
      invoice.ExchRate        lcDelim
      invoice.ExpStamp        lcDelim
      SubInvoice.InvAmt       lcDelim
      SubInvoice.AdvPaym      lcDelim
      invoice.APAccNum        lcDelim
      invoice.ClaimBatch      lcDelim
      SubInvoice.InvSeq       lcDelim 
      invoice.DiscPerc        lcDelim
      invoice.DirDisc         lcDelim
      invoice.ClaimStamp      lcDelim
      invoice.CancelStamp     lcDelim
      invoice.VATIncl         lcDelim
      invoice.ClaimCancel     lcDelim
      invoice.DDBankAcc       lcDelim
      invoice.Brand           lcDelim
      invoice.EPaymAmt        lcDelim
      invoice.ChargeType      lcDelim
      invoice.DelType         lcDelim
      invoice.DDState         lcDelim
      invoice.EPaymDate       lcDelim
      invoice.SpecDel         lcDelim
      invoice.IDelAddr        lcDelim
      invoice.IDelCountry     lcDelim
      invoice.IDelName        lcDelim
      invoice.IDelPost        lcDelim
      invoice.IDelZipCode     lcDelim
      invoice.IdelCOName      lcDelim
      invoice.CoName          lcDelim
      invoice.DDFile          lcDelim
      invoice.RefNum          lcDelim
      invoice.DeliveryState   lcDelim
      invoice.FirstName       lcDelim
      invoice.Surname2        lcDelim
      invoice.Surname1        lcDelim
      invoice.CreditReason    SKIP.

END PROCEDURE.
