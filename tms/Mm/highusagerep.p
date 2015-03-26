/* highusagerep.p        2004/jp 

   changes:              29.11.04/aam use brand for invoice and memo,
                                      HighUsage before invseq in main loop
                         16.06.06/aam ClaimState instead of ClaimQty
*/
      
{commali.i}
{excel.i}
{timestamp.i}
{email.i}
{highusage.i}
{cparam2.i}

DEF input parameter   ideCreateTS  AS DE    NO-UNDO FORMAT "99999999.99999".
DEF  input parameter   iStatus       AS INT  NO-UNDO.

DEF VAR tiednimi AS c no-undo.
DEF VAR liAmountOfInvoice  AS INT               NO-UNDO.
DEF VAR ldeInvoiceAverage  AS DEC               NO-UNDO.
DEF VAR llOpenInvoice      AS LOG               NO-UNDO FORMAT "X/".
DEF VAR llClaim            AS LOG               NO-UNDO FORMAT "X/".
DEF VAR username           AS CHAR              NO-UNDO FORMAT "X(20)" .
DEF VAR lcCustName         AS CHAR              NO-UNDO FORMAT "X(20)" .
DEF VAR Statusname         AS CHAR              NO-UNDO FORMAT "X(20)" .
DEF VAR lcChanged          AS CHAR              NO-UNDO FORMAT "X(24)" .
DEF VAR lcCreated          AS CHAR              NO-UNDO FORMAT "X(24)" .
def var i                  AS INT               NO-UNDO.
DEF VAR lcperiod           AS CHAR              NO-UNDO.
DEF VAR liperiod           AS INT               NO-UNDO.
DEF VAR memotext           AS CHAR              NO-UNDO.
DEF VAR memoStamp          AS CHAR              NO-UNDO.
DEF VAR ldelimit           AS DE                NO-UNDO.
DEF VAR totalunbilled      AS DEC               No-UNDO.
DEF VAR xConfDir           AS CHAR              NO-UNDO.
DEF VAR activedays         AS CHAR              NO-UNDO.
DEF VAR xHighSpenderDir    AS CHAR              NO-UNDO.
DEF VAR lcOutPath          AS CHAR              NO-UNDO.

DEF BUFFER agrcustomer FOR customer.

DEF BUFFER xxhighusage for highusage .

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

FOR EACH HighUsage NO-LOCK WHERE 
         HighUsage.HiUsageStatus = iStatus  AND
         HighUsage.crStamp >= ideCreateTS, 
   FIRST invseq NO-LOCK WHERE 
         Invseq.invseq = HighUsage.Invseq  AND 
         invseq.billed = FALSE.
         
   FIND customer where 
        customer.custnum   = invseq.custnum NO-LOCK NO-ERROR.
        lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                              BUFFER Customer).

   FIND AgrCustomer WHERE 
        AgrCustomer.CustNum = Customer.AgrCust No-LOCK NO-ERROR.
        
   ASSIGN
      liAmountOfInvoice =  0
      llOpenInvoice     = FALSE
      llClaim           = FALSE.
   
   FOR EACH invoice WHERE 
            Invoice.Brand    = gcBrand          AND
            Invoice.Custnum  = Invseq.CustNum   AND 
            Invoice.CrInvNum = 0  NO-LOCK.

      ASSIGN 
         liAmountOfInvoice = liAmountOfInvoice + 1.
      IF Invoice.PaymState = 0 THEN llOpenInvoice    = TRUE.
      IF Invoice.ClaimStatus > "" AND
         Invoice.ClaimStatus NE "0.0" THEN llClaim      = TRUE.
   END.

   ASSIGN
      i                 = 0
      ldeInvoiceAverage = 0 .

   FOR EACH Invoice NO-LOCK WHERE 
            Invoice.Brand    = gcBrand AND 
            Invoice.Custnum  = Invseq.CustNum AND
            Invoice.InvDate >= today - 90,
      FIRST SubInvoice OF Invoice NO-LOCK WHERE
            SubInvoice.CLI = Highusage.cli:

      ASSIGN
         i = i + 1
         ldeInvoiceAverage = ldeInvoiceAverage + SubInvoice.InvAmt.
   END.        
   ldeInvoiceAverage = ldeInvoiceAverage / i.

   if ldeinvoiceaverage = ? THEN ldeinvoiceaverage = 0.
   
   username = "".
   
   FIND FIRST msowner where 
              msowner.cli = HighUsage.cli no-lock no-error.
   IF avail msowner then 
   Username = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                      BUFFER Customer).
   lcCreated = "(" + fTS2HMS(HighUsage.crstamp) + ")".     
   lcChanged = "(" + fTS2HMS(HighUsage.chstamp) + ")" .               

   FIND Mobsub where 
        Mobsub.cli = msowner.cli no-lock no-error.

   IF AVAIL mobsub THEN 
      ASSIGN activedays = STRING(today - mobsub.activationdate).
   ELSE      activedays = "Terminated".    
   
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "HighUsage"      AND
              TMSCodes.FieldName = "HiUsageStatus"  AND 
              TMSCodes.CodeGroup = "HighUsage"      AND
              TMSCodes.CodeValue = STRING(HighUsage.HiUsageStatus)
   NO-LOCK NO-ERROR.
   IF AVAIL tmscodes THEN Statusname = TMSCodes.CodeName.
   ELSE                   STATUSNAME = "".

   ASSIGN
      lcperiod = STRING(Invseq.FromDate) + " - " + STRING(Invseq.Todate)
      liperiod = YEAR(Invseq.todate) * 100 + MONTH(Invseq.todate).

  FIND FIRST memo WHERE 
             Memo.Brand     = gcBrand               AND
             memo.Custnum   = Invseq.Custnum        AND 
             Memo.Hosttable = "Highusage"           AND  
             memo.keyvalue  = STRING(HighUsage.Invseq) + "|" +  
                              STRING(Highusage.cli)
  NO-LOCK NO-ERROR.
             
             
  IF avail memo THEN  DO:
     ASSIGN memotext = REPLACE(memo.memotext,chr(10)," ") .
     IF memo.CreStamp > memo.ChgStamp THEN 
        memostamp = fTS2HMS(memo.CreStamp) .
     ELSE memostamp = fTS2HMS(memo.ChgStamp).   
     
  END.    
  ELSE   ASSIGN memotext = "" memostamp = "".


  FIND FIRST HiUsageLimit WHERE
             HiUsageLimit.category = Highusage.category AND 
             HiUsageLimit.billcode = HighUsage.Launch no-lock no-error.
             
  if avail hiusagelimit then ldelimit = hiusagelimit.limit.
  ELSE                       ldelimit = 0 .           
  
  fTotalHU(INPUT highusage.cli, OUTPUT totalunbilled).

  PUT STREAM excel UNFORMATTED
  Customer.Custnum   TAB
  lcCustName         TAB
  Customer.Orgid     TAB
  HighUsage.cli      TAB
  Username           TAB 
  Activedays         TAB 
  customer.birthday  TAB 
  HighUsage.Launch   TAB
  HighUsage.Category TAB 
  ldelimit           TAB
  HighUsage.Amount   TAB
  TotalUnbilled      TAB
  HighUsage.Dategrow TAB
  HighUsage.date%    TAB
  HighUsage.date     TAB
  ldeInvoiceAverage  TAB
  liAmountOfInvoice  TAB
  llOpenInvoice      TAB
  llClaim            TAB
  Statusname         TAB
  memotext           TAB
  memostamp          TAB
  liperiod           MY-nl.

END.

OUTPUT STREAM excel CLOSE.
                                  
UNIX SILENT VALUE("mv " + tiednimi + " " + lcOutPath).
