/* highusagerep.p        2004/jp 

   changes:              29.11.04/aam use brand for invoice and memo,
                                      HighUsage before invseq in main loop
                         16.06.06/aam ClaimState instead of ClaimQty
                         05.07.07/fk  make modular, use accumulations
*/

/* Implement this function on the outside!
 * It will be called for each HighUsage record */
FUNCTION process_highspender_row RETURN LOGICAL
      ( BUFFER phInvCust FOR Customer,
        BUFFER phHighUsage FOR HighUsage,
        pcActiveDays AS CHAR,
        piCategoryLimit AS INT,
        pdTotalUnbilled AS DECIMAL,
        piInvoiceCount AS INT,
        pdInvoiceAverage AS DECIMAL,
        llHasOpenInv AS LOGICAL,
        llHasClaim AS LOGICAL,
        pcStatusName AS CHAR,
        BUFFER phMemo FOR Memo,
        piPeriod AS INT
      ) FORWARD.


FUNCTION loop_highspender_table RETURN LOGICAL
      ( pcBrand        AS CHAR,
        pdCreatedAfter AS DECIMAL,
        piStatus       AS INT ):

    DEF VAR lcActiveDays AS CHAR NO-UNDO.
    DEF VAR liUsageLimit AS INT INITIAL 0 NO-UNDO.
    DEF VAR liOpenInvCount AS INT NO-UNDO.
    DEF VAR liInvCount AS INT NO-UNDO.
    DEF VAR ldInvoiceAverage AS DECIMAL NO-UNDO.
    DEF VAR llHasOpenInv AS LOGICAL NO-UNDO.
    DEF VAR llHasClaim AS LOGICAL NO-UNDO.
    DEF VAR lcStatusName AS CHAR INITIAL "" NO-UNDO.
    DEF VAR ldTotalUnbilled AS DECIMAL NO-UNDO.
    DEF BUFFER lhInvSeq for Invseq.


    FOR EACH HighUsage NO-LOCK WHERE 
             HighUsage.HiUsageStatus EQ piStatus  AND
             HighUsage.crStamp GE pdCreatedAfter, 
       FIRST Invseq NO-LOCK WHERE 
             Invseq.invseq EQ HighUsage.Invseq  AND 
             Invseq.billed EQ FALSE,
       FIRST Customer NO-LOCK WHERE
             Customer.custnum EQ Invseq.custnum:


        FIND Mobsub NO-LOCK
        WHERE Mobsub.cli EQ HighUsage.cli NO-ERROR.
        IF AVAILABLE Mobsub THEN DO:
            lcActiveDays = STRING(TODAY - mobsub.activationdate).

            FOR EACH lhInvSeq NO-LOCK
            WHERE lhInvSeq.MSSeq   EQ Mobsub.msseq
              AND lhInvSeq.Custnum EQ Mobsub.Custnum
              AND lhInvSeq.Billed  EQ FALSE,
            EACH SaldoCounter NO-LOCK
            WHERE SaldoCounter.msseq EQ mobsub.msseq
              AND SaldoCounter.period EQ (YEAR(lhInvSeq.fromdate) * 100 +
                                          MONTH(lhInvSeq.fromdate)):

                ACCUMULATE SaldoCounter.amt (TOTAL).
            END.
            ldTotalUnbilled = (ACCUM TOTAL SaldoCounter.amt).

        END. ELSE ASSIGN
            lcActiveDays = "Terminated"
            ldTotalUnbilled = 0.


        FIND FIRST HiUsageLimit NO-LOCK
        WHERE HiUsageLimit.category EQ Highusage.category
          AND HiUsageLimit.billcode EQ HighUsage.Launch NO-ERROR.
        IF AVAILABLE HiUsageLimit THEN
            liUsageLimit = HiUsageLimit.limit.



        /* All open invoices */
        llHasClaim = FALSE.
        FOR EACH Invoice NO-LOCK
        WHERE Invoice.Brand    EQ pcBrand
          AND Invoice.Custnum  EQ Invseq.CustNum
          AND Invoice.CrInvNum EQ 0:
            ACCUMULATE Invoice.InvNum (COUNT).
            ACCUMULATE Invoice.PaymState (MINIMUM).
           IF Invoice.ClaimStatus > "" AND
              Invoice.ClaimStatus NE "0.0" THEN llHasClaim = TRUE.
        END.
        ASSIGN
            liOpenInvCount = (ACCUM COUNT Invoice.InvNum)
            llHasOpenInv = ((ACCUM MINIMUM Invoice.PaymState) EQ 0)
        .


        /* All Invoices of the last 90 days */
        DEF VAR ldSubAmount AS DECIMAL NO-UNDO.
        FOR EACH Invoice NO-LOCK WHERE 
                 Invoice.Brand    EQ pcBrand AND 
                 Invoice.Custnum  EQ Invseq.CustNum AND 
                 Invoice.InvDate  GE TODAY - 90,
           FIRST SubInvoice OF Invoice NO-LOCK WHERE 
                 SubInvoice.CLI = HighUsage.CLI:

            ACCUMULATE (SubInvoice.InvAmt) (AVERAGE).

        END.
        ldInvoiceAverage = (ACCUM AVERAGE SubInvoice.InvAmt).


        FIND FIRST TMSCodes NO-LOCK
        WHERE TMSCodes.TableName EQ "HighUsage"
          AND TMSCodes.FieldName EQ "HiUsageStatus"
          AND TMSCodes.CodeGroup EQ "HighUsage"
          AND TMSCodes.CodeValue EQ STRING(HighUsage.HiUsageStatus) NO-ERROR.
        IF AVAILABLE TMSCodes THEN
            lcStatusName = TMSCodes.CodeName.


        FIND FIRST memo NO-LOCK
        WHERE Memo.Brand     EQ pcBrand
          AND Memo.Custnum   EQ Invseq.Custnum
          AND Memo.Hosttable EQ "Highusage"
          AND Memo.keyvalue  EQ STRING(HighUsage.Invseq) + "|" +  
                                  STRING(Highusage.cli) NO-ERROR.
                 

        process_highspender_row(
            BUFFER Customer,
            BUFFER HighUsage,
            lcActiveDays,
            liUsageLimit,
            ldTotalUnbilled,
            liOpenInvCount,
            ldInvoiceAverage,
            llHasOpenInv,
            llHasClaim,
            lcStatusName,
            BUFFER Memo,
            (YEAR(Invseq.todate) * 100 + MONTH(Invseq.todate))
        ).

    END.
    RETURN TRUE.

END FUNCTION.



/**   This simplifies the current highusagerep.p to:


{highusage_report.i}

{commali.i}
{excel.i}
{timestamp.i}
{email.i}
{highusage.i}
{cparam2.i}

DEF input parameter   ideCreateTS  AS DE    NO-UNDO FORMAT "99999999.99999".
DEF  input parameter   iStatus       AS INT  NO-UNDO.

DEF VAR tiednimi AS c no-undo.
DEF VAR cfile              AS CHAR              NO-UNDO.
DEF VAR xConfDir           AS CHAR              NO-UNDO.
DEF VAR excelfile          AS CHAR              NO-UNDO.
DEF VAR xHighSpenderDir    AS CHAR              NO-UNDO.
DEF VAR lcOutPath          AS CHAR              NO-UNDO.

{cparam.i RepConfDir            return}.  xConfDir        = tmsparam.CharVal.
{cparam.i HighSpenderDirectory  return}.  xhighspenderDir = tmsparam.CharVal.
ASSIGN
      tiednimi    = fCParam("CRONSPOOL","highspendnew.p") + "highspender_" +

         REPLACE(STRING(YEAR(TODAY),"9999")  +
                 STRING(MONTH(TODAY),"99")   +
                 STRING(DAY(TODAY),"99")     +
                 STRING(time,"hh:mm:ss") + ".dump",":","").

lcOutPath   = fCParam("CRONOUTGOING","highspendnew.p").

OUTPUT STREAM excel TO  VALUE(tiednimi).

PUT STREAM excel UNFORMATTED
"Customer number"   tab
"Customer name"     TAB
"Customer ID"       TAB
"MSISDN no"         TAB
"Username"          TAB
"ActiveInDays"      TAB
"Customer birthday" TAB
"Reason"            TAB
"Reason Category"   TAB
"Category Limit"    TAB
"Period UnBilled"   TAB
"Total Unbilled"    TAB
"Date growth"       TAB
"date%"             TAB
"Previous date"     TAB
"avg.invoice"       TAB
"Qty of invoices"   TAB
"Open invoices"     TAB
"Claim invoices"    TAB
"Status"            TAB
"Memo text"         TAB
"Memo date"         TAB
"Period"            MY-nl.

FUNCTION process_highspender_row RETURN LOGICAL
      ( BUFFER phInvCust FOR Customer,
        BUFFER phHighUsage FOR HighUsage,
        pcActiveDays AS CHAR,
        piCategoryLimit AS INT,
        pdTotalUnbilled AS DECIMAL,
        piInvoiceCount AS INT,
        pdInvoiceAverage AS DECIMAL,
        llHasOpenInv AS LOGICAL,
        llHasClaim AS LOGICAL,
        pcStatusName AS CHAR,
        BUFFER phMemo FOR Memo,
        piPeriod AS INT
      ):

  DEF VAR lcCustName AS CHAR NO-UNDO.
  DEF VAR memotext AS CHAR NO-UNDO.
  DEF VAR memostamp AS CHAR NO-UNDO.

  lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                      BUFFER phInvCust).
  IF avail memo THEN  DO:
     ASSIGN memotext = REPLACE(memo.memotext,chr(10)," ") .
     IF memo.CreStamp > memo.ChgStamp THEN
        memostamp = fTS2HMS(memo.CreStamp) .
     ELSE memostamp = fTS2HMS(memo.ChgStamp).

  END.
  ELSE   ASSIGN memotext = "" memostamp = "".


  PUT STREAM excel UNFORMATTED
  phInvCust.Custnum    TAB
  lcCustName           TAB
  phInvCust.Orgid      TAB
  phHighUsage.cli      TAB
  lcCustName           TAB
  pcActiveDays         TAB
  phInvCust.birthday   TAB
  phHighUsage.Launch   TAB
  phHighUsage.Category TAB
  piCategoryLimit      TAB
  phHighUsage.Amount   TAB
  pdTotalUnbilled      TAB
  phHighUsage.Dategrow TAB
  phHighUsage.date%    TAB
  phHighUsage.date     TAB
  pdInvoiceAverage     TAB
  piInvoiceCount       TAB
  llHasOpenInv         TAB
  llHasClaim           TAB
  pcStatusName         TAB
  memotext             TAB
  memostamp            TAB
  piPeriod             MY-nl.

    RETURN TRUE.
END FUNCTION.

loop_highspender_table(gcBrand, ideCreateTS, iStatus).

OUTPUT STREAM excel CLOSE.

UNIX SILENT VALUE("mv " + tiednimi + " " + lcOutPath).

*/
