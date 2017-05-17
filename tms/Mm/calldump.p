/* -----------------------------------------------
  MODULE .......: calldump.p
  FUNCTION .....: daily/monthly call dump
  APPLICATION ..: tms
  AUTHOR .......: tk  
  CREATED ......: 16.03.04
  MODIFIED .....: 02.12.04/aam use-index Date,
                               use Brand
                  28.11.05/tk service part added (notInclVat and inclVat) 
                  29.11.05/tk MobCDR.VatIncl
                  19.02.08/jp new search rule
                  
  VERSION ......: SHARK
  ------------------------------------------------------ */

{Syst/commpaa.i}
katun = "cron".
gcBrand = "1".

{Func/date.i}
{Func/fvatfact.i}
{Func/excel.i}
{Func/coinv.i}
{Func/cparam2.i}
{Func/multitenantfunc.i}

DEF INPUT  PARAMETER icfilename      AS CHAR  NO-UNDO.

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

DEFINE VARIABLE ldeStartTS  AS DEC  NO-UNDO.
DEFINE VARIABLE ldeEndTS    AS DEC  NO-UNDO.
DEFINE VARIABLE liamt       AS INT  NO-UNDO.

DEF VAR lcTaxZone    AS CHAR NO-UNDO.
DEF VAR lcLogDir     AS CHAR NO-UNDO.
DEF VAR llLog        AS LOG  NO-UNDO.

DEF STREAM sLog.


assign
   ldate1     = idaDate - 1
   ldate2     = ldate1
   numform    = session:numeric-format
   session:numeric-format = "AMERICAN"
   lcLogDir   = fCParamC("DailyCallsLogDir").

IF lcLogDir = "" OR lcLogDir = ? THEN llLog = FALSE.
ELSE DO:
   llLog = TRUE.
   OUTPUT STREAM sLog TO VALUE
      (lcLogDir + "/" + 
       CAPS(fgetBrandNamebyTenantId(TENANT-ID(LDBNAME(1)))) +
       "_dailycalls_" +
       STRING(ldate1,"999999") + ".log") APPEND.
END.                               
                               
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

FOR EACH MobCDR NO-LOCK USE-INDEX ReadDate WHERE
         MobCDR.ReadDate >= ldate1 AND
         MobCDR.ReadDate <= ldate2 AND
         MobCDR.ErrorCode = 0:
            
      find ttCalls where 
           ttCalls.calldate = Mobcdr.Datest    AND
           ttCalls.Mobtype  = MobCDR.clitype   AND
           ttCalls.CLI      = MobCDR.CLI       AND
           ttCalls.BillCode = MobCDR.BillCode  AND
           ttCalls.ccn      = MobCDR.ccn exclusive-lock no-error.
   
      if not avail ttCalls then do:
         create ttCalls.
         assign
            ttCalls.calldate = Mobcdr.Datest
            ttCalls.Mobtype  = MobCDR.CLIType
            ttCalls.CLI      = MobCDR.CLI
            ttCalls.BillCode = MobCDR.BillCode
            ttCalls.ccn      = MobCDR.ccn.
      end.

      ASSIGN
         ttCalls.count   = ttCalls.count   + 1
         ttCalls.sec     = ttCalls.sec     + MobCDR.BillDur
         ttCalls.dataAmt = ttcalls.dataamt + mobcdr.datain + mobcdr.dataout.
         tmplaskuri      = tmplaskuri      + 1.

      FIND Customer WHERE Customer.CustNUm = MobCDR.InvCust NO-LOCK NO-ERROR.

      lcTaxZone = fRegionTaxZone(Customer.Region).
   
      ldVatFactor = fVatFactor(IF AVAILABLE Customer
                               THEN Customer.VATUsage
                               ELSE 1,
                               lcTaxZone,
                               MobCDR.BillCode,
                               TODAY - 1).

      IF MobCDR.MpmRid NE MobCDR.ServRid AND 
         MobCDR.MpmRid > "" AND MobCDR.ServRid > ""
      THEN ASSIGN ldAmt     = MobCDR.MPMAmt
                  ldServAmt = MobCDR.Amount - MobCDR.MPMAmt.
      ELSE ASSIGN ldAmt     = MobCDR.Amount
                  ldServAmt = 0.
   
      IF MobCDR.VatIncl THEN ASSIGN
         ttCalls.amount     = ttCalls.amount     + ldAmt / ldVatFactor
         ttCalls.VatAmt     = ttCalls.VatAmt     + ldAmt
         ttCalls.ServAmt    = ttCalls.ServAmt    + ldServAmt / ldVatFactor
         ttCalls.ServVatAmt = ttCalls.ServVatAmt + ldServAmt.
      ELSE ASSIGN 
         ttCalls.Amount     = ttCalls.Amount     + MobCDR.Amount
         ttCalls.VatAmt     = ttCalls.VatAmt     + MobCDR.Amount * ldVatFactor
         ttCalls.ServAmt    = ttCalls.ServAmt    + ldServAmt
         ttCalls.ServVatAmt = ttCalls.ServVatAmt + ldServAmt * ldVatFactor.
 
      IF llLog THEN 
         PUT STREAM sLog UNFORMATTED RECID(MobCDR) SKIP.
END.

IF llLog THEN OUTPUT STREAM sLog CLOSE.

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
      round((ttCalls.DataAmt / 1024 / 1024),4)  tab
      round(ttCalls.amount,2)     tab 
      round(ttCalls.VatAmt,2)     tab
      round(ttCalls.ServAmt,2)    tab
      round(ttCalls.ServVatAmt,2) skip.
end.

output stream excel close.

