/* -----------------------------------------------
  MODULE .......: calldump_prepaid.p
  FUNCTION .....: daily/monthly summary prepaid call dump
  ------------------------------------------------------ */

{Syst/commali.i}
katun = "cron".
gcBrand = "1".

{Func/date.i}
{Func/excel.i}
{Func/coinv.i}
{Func/cparam2.i}
{Func/multitenantfunc.i}

DEF INPUT  PARAMETER icFilename        AS CHAR NO-UNDO.

DEFINE VARIABLE idaDate AS DATE NO-UNDO. 
DEFINE VARIABLE lcParam AS CHARACTER NO-UNDO.
lcParam = SESSION:PARAMETER.

IF lcParam NE "" THEN DO: 
  idaDate = DATE(lcParam) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO: 
     MESSAGE "Wrong date: " + lcParam.
     PAUSE 0.
     RETURN.
  END.
END.
ELSE idaDate = TODAY.

DEFINE VARIABLE ldate1     as da NO-UNDO.
DEFINE VARIABLE ldate2     as da NO-UNDO.
DEFINE VARIABLE i          as i  NO-UNDO.
DEFINE VARIABLE tmplaskuri as i  NO-UNDO.
DEFINE VARIABLE labels     as c  NO-UNDO.
DEFINE VARIABLE numform    as c  NO-UNDO.

DEFINE VARIABLE ldVatFactor AS DEC  NO-UNDO.
DEFINE VARIABLE ldAmt       AS DEC  NO-UNDO.
DEFINE VARIABLE ldServAmt   AS DEC  NO-UNDO.

DEFINE VARIABLE lcBiName    as char NO-UNDO.
DEFINE VARIABLE lcCCNName   as char NO-UNDO.
DEFINE VARIABLE liamt       AS INT  NO-UNDO.

assign
   ldate1     = idaDate - 1
   ldate2     = ldate1
   numform    = session:numeric-format
   session:numeric-format = "AMERICAN".

DEFINE TEMP-TABLE ttCalls
   FIELD calldate   AS DATE
   FIELD Mobtype    AS CHARACTER
   FIELD CLI        AS CHARACTER
   FIELD BillCode   AS CHARACTER
   FIELD ccn        AS INTEGER
   FIELD count      AS INTEGER
   FIELD sec        AS INTEGER
   FIELD dataAmt    AS DECIMAL
   FIELD amount     AS DECIMAL
   FIELD VatAmt     AS DECIMAL
   FIELD ServAmt    AS DECIMAL
   FIELD ServVatAmt AS DECIMAL
   index mobtype is primary unique calldate mobtype cli BillCode ccn.

FOR EACH PrepCDR NO-LOCK USE-INDEX ReadDate WHERE
         PrepCDR.ReadDate >= ldate1 AND
         PrepCDR.ReadDate <= ldate2 AND
         PrepCDR.ErrorCode = 0:
 
      find ttCalls where 
           ttCalls.calldate = PrepCDR.Datest    AND
           ttCalls.Mobtype  = PrepCDR.clitype   AND
           ttCalls.CLI      = PrepCDR.CLI       AND
           ttCalls.BillCode = PrepCDR.BillCode  AND
           ttCalls.ccn      = PrepCDR.ccn exclusive-lock no-error.
   
      if not avail ttCalls then do:
         create ttCalls.
         assign
            ttCalls.calldate = PrepCDR.Datest
            ttCalls.Mobtype  = PrepCDR.CLIType
            ttCalls.CLI      = PrepCDR.CLI
            ttCalls.BillCode = PrepCDR.BillCode
            ttCalls.ccn      = PrepCDR.ccn
            ttCalls.VatAmt     = 0
            ttCalls.ServAmt    = 0
            ttCalls.ServVatAmt = 0.
      end.

      ASSIGN
         ttCalls.count   = ttCalls.count   + 1
         ttCalls.sec     = ttCalls.sec     + PrepCDR.BillDur
         ttCalls.dataAmt = ttcalls.dataamt + PrepCDR.DataIn + PrepCDR.DataOut
         ttCalls.Amount  = ttCalls.Amount  + PrepCDR.Charge
         tmplaskuri      = tmplaskuri      + 1.
END.

output stream excel to value(icfilename).

for each ttCalls NO-LOCK:
   
   assign
     lcBiName = ""
     lcCCNName = "".

   find ccn where 
        CCN.Brand = gcBrand AND
        ccn.ccn = ttcalls.ccn no-lock no-error.
   find billitem where 
        BillItem.Brand    = gcBrand AND
        billitem.billcode = ttCalls.billcode no-lock no-error.
   assign
     lcBiName  = Billitem.biname when avail billitem
     lcCCNName = ccn.ccnname when avail ccn.
   
   put stream excel unformatted
      ttCalls.calldate tab
      ttCalls.Mobtype  tab 
      ttCalls.CLI      tab 
      ttCalls.BillCode tab 
      lcBiname         tab
      ttCalls.ccn      tab
      lcCCNName        tab
      ttCalls.count    tab 
      ttCalls.sec      tab 
      round((ttCalls.DataAmt / 1024),4)  tab
      round(ttCalls.amount,2)     tab 
      round(ttCalls.VatAmt,2)     tab
      round(ttCalls.ServAmt,2)    tab
      round(ttCalls.ServVatAmt,2) skip.
end.

output stream excel close.

session:numeric-format = numform.

