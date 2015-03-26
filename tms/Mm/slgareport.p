{commali.i}
{excel.i}
{timestamp.i}
{email.i}
{highusage.i}
{cparam2.i}


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
DEF VAR cfile              AS CHAR              NO-UNDO.
DEF VAR xConfDir           AS CHAR              NO-UNDO.
DEF VAR excelfile          AS CHAR              NO-UNDO.
DEF VAR activedays         AS CHAR              NO-UNDO.
DEF VAR xHighSpenderDir    AS CHAR              NO-UNDO.
DEF VAR lcOutPath          AS CHAR              NO-UNDO.
DEF VAR lcCCN              AS CHAR              NO-UNDO.
DEF VAR lcBdest            AS CHAR              NO-UNDO.
DEF VAR lcTypeName         AS CHAR              NO-UNDO.

ASSIGN 
   tiednimi    = "/tmp/slganalyse_list.txt".

OUTPUT STREAM excel TO  VALUE(tiednimi).

PUT STREAM excel UNFORMATTED
"Subs.Type"          TAB
"Subs.Type Name"     TAB
"Billing Item"       TAB
"Billing Item Name"  TAB
"Rating CCN"         TAB
"CCN Name"           TAB
"Bdest"              TAB
"Bdest Name"         TAB
"ValidFrom"          TAB
"ValidTo"            TAB
"SL Type"            TAB
"SL Name of type"    TAB
"SL Group"           TAB
"Priority"           SKIP.

FOR EACH SLGANalyse NO-LOCK.


   FIND FIRST CliType WHERE 
              CliType.Brand   = gcBrand   AND 
              Clitype.CliType = SLGAnalyse.CliType NO-LOCK.
              
                 
   FIND FIRST BillItem WHERE 
              BillItem.Brand    = gcBrand AND 
              BillItem.BillCode = slganalyse.BillCode NO-LOCK NO-ERROR.
              
   IF AVAIL CCN THEN lcCCN = CCN.CCNName.
   ELSE              lcCCN = "N/A"  . 
      
   FIND FIRST CCN WHERE 
              CCN.Brand = gcBrand AND 
              CCN.CCN   = slganalyse.CCN NO-LOCK NO-ERROR.
              
   IF AVAIL CCN THEN lcCCN = CCN.CCNName.
   ELSE              lcCCN = "N/A"  .
              
   FIND FIRST Bdest WHERE 
              Bdest.Brand = gcBrand AND 
              Bdest.Bdest = slganalyse.Bdest AND
              BDest.ToDate >= SLGAnalyse.ValidFrom AND
              BDest.FromDate <= SLGAnalyse.ValidTo NO-LOCK NO-ERROR.
              
   IF AVAIL Bdest THEN lcBdest = Bdest.BDName.
   ELSE                lcBdest = "N/A"  .             

   FIND FIRST TMSCodes WHERE
              TMSCodes.Tablename    = "SLGAanalyse"   AND
              TMSCodes.FieldName    = "SLGAType"      AND
              TMSCodes.CodeGroup    = "Servicelimit"  AND
              TMSCodes.CodeValue    = STRING(SLGAnalyse.slgaType)
   NO-LOCK NO-ERROR.
      
   IF AVAIL TMSCodes THEN lcTypeName = TMSCodes.CodeName.
   ELSE lcTypeName = "".
   
   PUT STREAM excel UNFORMATTED
   SLGAnalyse.CliType    TAB
   CliType.CliName       TAB
   SLGanalyse.BillCode   TAB
   BillItem.BiName       TAB
   SLGanalyse.CCN        TAB
   lcCCN                 TAB
   SLGAnalyse.Bdest      TAB
   LCBdest               TAB
   SLGAnalyse.ValidFrom  TAB
   SLGAnalyse.ValidTo    TAB
   SLGAnalyse.SLGAType   TAB
   lcTypeName            TAB
   SLGAnalyse.ServiceLimitGroup TAB
   SLGAnalyse.Prior  SKIP.

END.
OUTPUT STREAM excel CLOSE.

MESSAGE 
"File created:" tiednimi
VIEW-AS ALERT-BOX.

UNIX SILENT VALUE("mailx -s 'slganalyse_rows' risto.nupponen@starnet.fi < " + tiednimi).
