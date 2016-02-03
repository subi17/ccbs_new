{Syst/commpaa.i}
gcbrand = "1".
{Func/timestamp.i}
{Func/date.i}
{Func/cparam2.i}
{Func/istc.i}
{Inv/chk_cdr_invrowcounter.i &ttReference = "REFERENCE-ONLY"}
{Func/date.i}
{Func/timestamp.i}

def var i as int no-undo.
def var k as int no-undo.
def var ldatodate as date no-undo.
def var ldafromdate as date no-undo.
def var lccli as char no-undo.
def var llmatch as log no-undo.
def var lcline as char no-undo.
def var lcinputfile as char no-undo.
def var lcoutputfile as char no-undo.
def var liperiod as int no-undo.
DEFINE VARIABLE oiCounterQty AS INTEGER NO-UNDO. 

def temp-table ttcounter no-undo
   like invrowcounter.

def temp-table ttCLI no-undo
    FIELD CLI AS CHAR.

def stream sin.
def stream slog.

DEFINE VARIABLE l AS INTEGER NO-UNDO. 
pause 0.
update lcinputfile label "Enter Input File Path:" 
   FORMAT "X(256)" VIEW-AS FILL-IN SIZE 45 BY 1 skip
   with overlay row 10 centered title " CHECK COUNTER " 
        side-labels frame fCheck.
hide frame fCheck no-pause.

IF lcinputfile = "" THEN DO:
   MESSAGE "Please Enter Input File Path" VIEW-AS ALERT-BOX.
   RETURN.
END. /* IF lcinputfile = "" THEN DO: */

assign 
  ldafromdate = date(month(today),1,year(today))
  ldatodate = flastdayofmonth(ldafromdate)
  liperiod = year(today) * 100 + month(today)
  lcoutputfile = "/apps/yoigo/tms_support/billing/log/check_invrowcounter_" + string(fMakeTS()) + ".log".

input stream sin from value(lcinputfile).
output stream slog to value(lcoutputfile).

DEF VAR lcMatch   AS CHAR NO-UNDO.
DEF VAR liRepeat  AS INT  NO-UNDO.
DEF VAR liLoop    AS INT  NO-UNDO.
DEF VAR llHeader  AS LOG  NO-UNDO INIT TRUE.
DEF VAR llISTCChecked AS LOG NO-UNDO.
DEF VAR ldaISTCDate AS DATE NO-UNDO.
DEF VAR ldaCounterToDate AS DATE NO-UNDO.
DEF VAR llErrorFound AS LOGICAL NO-UNDO.
repeat:
   import stream sin unformatted lcline.

   if lcline = "" or lcline begins "MSISDN" then next.
   lccli = entry(1,lcline, chr(9)).

   if not can-find (first ttCLI WHERE ttCLI.CLI = lccli) then do:
      create ttCLI.
             ttCLI.CLI = lccli.
   end.
end.

FOR EACH ttCLI,
    first msowner no-lock use-index cli_s where
          msowner.cli = ttCLI.CLI:

   ASSIGN
      i = i + 1
      liLoop = 0
      llErrorFound = FALSE
      llISTCChecked = FALSE
      ldaISTCDate = ?.

   ChkCounter:
   REPEAT:

      liLoop = liLoop + 1.

      FOR EACH InvSeq NO-LOCK WHERE
               InvSeq.MsSeq = MsOwner.MsSeq AND
               InvSeq.CustNum = MsOwner.InvCust AND
               InvSeq.Billed = FALSE AND
               InvSeq.ToDate = ldaToDate:

         IF NOT llISTCChecked THEN DO:
            ldaISTCDate = fGetiSTCDate(Invseq.MsSeq,
                                       InvSeq.Custnum,
                                       InvSeq.ToDate).
            llISTCChecked = TRUE.
         END.

         EMPTY TEMP-TABLE ttCounter.

         FOR EACH MobCDR NO-LOCK USE-INDEX InvSeq WHERE
                  MobCDR.InvCust = InvSeq.CustNum AND
                  MobCDR.InvSeq = InvSeq.InvSeq:
            IF ldaISTCDate NE ? AND ldaISTCDate > MobCDR.DateSt THEN
               ldaCounterToDate = ldaISTCDate - 1.
            ELSE ldaCounterToDate = ldaToDate.

            FIND FIRST ttCounter WHERE
               ttCounter.InvCust     = MobCDR.InvCust AND
               ttCounter.InvSeq      = MobCDR.InvSeq AND
               ttCounter.BillCode    = MobCDR.BillCode AND
               ttCounter.CCN         = MobCDR.CCN AND
               ttCounter.MsSeq       = MobCDR.MsSeq AND
               ttCounter.CLI         = MobCDR.CLI AND
               ttCounter.TariffNum   = MobCDR.TariffNum AND
               ttCounter.VatIncl     = MobCDR.VatIncl AND
               ttCounter.ReportingID = "," AND
               ttCounter.DCEvent     = MobCDR.DCEvent AND
               ttCounter.ToDate      = ldaCounterToDate NO-ERROR.

            IF NOT AVAILABLE ttCounter THEN DO:
               CREATE ttCounter.
               ASSIGN
                  ttCounter.InvCust     = MobCDR.InvCust
                  ttCounter.InvSeq      = MobCDR.InvSeq
                  ttCounter.BillCode    = MobCDR.BillCode
                  ttCounter.CCN         = MobCDR.CCN
                  ttCounter.MsSeq       = MobCDR.MsSeq
                  ttCounter.CLI         = MobCDR.CLI
                  ttCounter.TariffNum   = MobCDR.TariffNum
                  ttCounter.VatIncl     = MobCDR.VatIncl
                  ttCounter.ReportingID = ","
                  ttCounter.DCEvent     = MobCDR.DCEvent
                  ttCounter.ToDate      = ldaCounterToDate.
            END.

            ASSIGN
               ttCounter.Quantity = ttCounter.Quantity + 1
               ttCounter.Duration = ttCounter.Duration + MobCDR.BillDur
               ttCounter.Amount   = ttCounter.Amount + MobCDR.Amount
               ttCounter.DataAmt  = ttCounter.DataAmt +
                                    MobCDR.DataIn + MobCDR.DataOut
               ttCounter.RefPrice = ttCounter.RefPrice + MobCDR.RefPrice.
         END.

         lcMatch = "".

         FOR EACH ttCounter
         BY ttCounter.billcode
         BY ttCounter.ccn:

            IF liLoop = 1 THEN oiCounterQty = oiCounterQty + 1.


            FIND FIRST InvRowCounter WHERE
               InvRowCounter.InvCust     = ttCounter.InvCust AND
               InvRowCounter.InvSeq      = ttCounter.InvSeq AND
               InvRowCounter.BillCode    = ttCounter.BillCode AND
               InvRowCounter.CCN         = ttCounter.CCN AND
               InvRowCounter.MsSeq       = ttCounter.MsSeq AND
               InvRowCounter.CLI         = ttCounter.CLI AND
               InvRowCounter.TariffNum   = ttCounter.TariffNum AND
               InvRowCounter.VatIncl     = ttCounter.VatIncl AND
               InvRowCounter.ReportingID = "," AND
               InvRowCounter.DCEvent     = ttCounter.DCEvent AND
               InvRowCounter.ToDate      = ttCounter.ToDate NO-LOCK NO-ERROR.
            IF NOT AVAILABLE InvRowCounter THEN
               lcMatch = "No counter found: " +
                         STRING(ttCounter.billcode) + "/" +
                         STRING(ttCounter.CCN) + "/" +
                         STRING(ttCounter.Quantity) + "/" +
                         STRING(ttCounter.DataAmt / 1024 * 1024) +
                         TRIM(STRING(ttCounter.Amount,"->>>>>9.99999")).

            ELSE IF
               InvRowCounter.Quantity NE ttCounter.Quantity OR
               InvRowCounter.Duration NE ttCounter.Duration OR
               InvRowCounter.Amount NE ttCounter.Amount OR
               InvRowCounter.DataAmt NE ttCounter.DataAmt THEN
                  lcMatch = "Values differ: " +
                            STRING(ttCounter.billcode) + "/" +
                            STRING(ttCounter.CCN).
            IF lcMatch > "" THEN LEAVE.
         END.

         IF lcMatch = "" THEN
         FOR EACH InvRowCounter NO-LOCK WHERE
            InvRowCounter.InvCust = InvSeq.CustNum AND
            InvRowCounter.InvSeq = InvSeq.InvSeq:

            IF InvRowCounter.Quantity = 0 AND InvRowCounter.Amount = 0 THEN
               NEXT.

            k = k + 1.

            FIND FIRST ttCounter WHERE
               ttCounter.InvCust     = InvRowCounter.InvCust AND
               ttCounter.InvSeq      = InvRowCounter.InvSeq AND
               ttCounter.BillCode    = InvRowCounter.BillCode AND
               ttCounter.CCN         = InvRowCounter.CCN AND
               ttCounter.MsSeq       = InvRowCounter.MsSeq AND
               ttCounter.CLI         = InvRowCounter.CLI AND
               ttCounter.TariffNum   = InvRowCounter.TariffNum AND
               ttCounter.VatIncl     = InvRowCounter.VatIncl AND
               ttCounter.ReportingID = "," AND
               ttCounter.DCEvent     = InvRowCounter.DCEvent AND
               ttCounter.ToDate      = InvRowCounter.ToDate NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttCounter THEN DO:
               lcMatch = "Counter without CDRs: " +
                         STRING(InvRowCounter.billcode) + "/" +
                         STRING(InvRowCounter.CCN) + "/" +
                         STRING(InvRowCounter.ToDate,"99-99-99").
               LEAVE.
            END.
         END.

         IF lcMatch > "" THEN DO:

            IF CAN-FIND(FIRST TMQueue WHERE TMQueue.InvSeq = InvSeq.InvSeq)
            THEN DO:
               PAUSE 0.
               liRepeat = liRepeat + 1.
               IF NOT SESSION:BATCH THEN DO:
                  DISP MsOwner.CLI MsOwner.MsSeq i oiCounterQty k l liRepeat
                      WITH 1 DOWN.
               END.
               PAUSE 5 NO-MESSAGE.
               NEXT ChkCounter.
            END.

            l = l + 1.
            llErrorFound = TRUE.

            IF llHeader THEN DO:
               PUT STREAM sLog UNFORMATTED
                  "MSISDN"  CHR(9)
                  "MsSeq"   CHR(9)
                  "InvSeq"  CHR(9)
                  "Period"  CHR(9)
                  "Reason"  SKIP.
               llHeader = FALSE.
            END.

            PUT STREAM sLog UNFORMATTED
               MsOwner.CLI CHR(9)
               MsOwner.MsSeq CHR(9)
               InvSeq.InvSeq CHR(9)
               InvSeq.ToDate CHR(9)
               lcMatch SKIP.
         END.

      END.

      LEAVE.
   END.

   IF NOT SESSION:BATCH THEN DO:
      IF i MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP MsOwner.CLI MsOwner.MsSeq i oiCounterQty k l liRepeat
            WITH 1 DOWN.
      END.
   END.

END.

OUTPUT STREAM sLog CLOSE.

IF NOT SESSION:BATCH THEN DO:
   DISP i oiCounterQty k l .
END.
