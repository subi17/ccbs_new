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
                  
  VERSION ......: SHARK
  ------------------------------------------------------ */

{commali.i}
{date.i}
{fvatfact.i}
{excel.i}
{coinv.i}
{cparam.i2}

DEFINE INPUT PARAMETER iiper AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER idaDay AS DATE NO-UNDO.


DEFINE VARIABLE ldate1     as da NO-UNDO.
DEFINE VARIABLE ldate2     as da NO-UNDO.
DEFINE VARIABLE counter    as i  NO-UNDO.
DEFINE VARIABLE i          as i  NO-UNDO.
DEFINE VARIABLE tmplaskuri as i  NO-UNDO.
DEFINE VARIABLE labels     as c  NO-UNDO.
DEFINE VARIABLE filename   as c  NO-UNDO.
DEFINE VARIABLE dformat    as c  NO-UNDO.
DEFINE VARIABLE lcOdir     as c  NO-UNDO.
DEFINE VARIABLE lcSdir     as c  NO-UNDO.
DEFINE VARIABLE numform    as c  NO-UNDO.

DEFINE VARIABLE ldVatFactor AS DEC  NO-UNDO.
DEFINE VARIABLE ldAmt       AS DEC  NO-UNDO.
DEFINE VARIABLE ldServAmt   AS DEC  NO-UNDO.

DEFINE VARIABLE lcBiName as char NO-UNDO.
DEFINE VARIABLE lcCCNName as char NO-UNDO.

assign
   /*
   lcOdir   =  fCparam("dumpoutgoing","calldump.p")
   lcSdir   =  fCParam("dumpspool","calldump.p")
   */
   lcOdir   =  "/store/riftp/dumpfiles/calls/outgoing/" 
   lcSdir   =  "/store/riftp/dumpfiles/calls/spool/"
   ldate1   = idaDay 
   filename = "calls" + fDateFmt(ldate1,"yyyymmdd") + ".dump"
   ldate1   = idaDay - 1
   ldate2   = idaDay
   numform  = session:numeric-format
   session:numeric-format = "AMERICAN".

if iiper ne 0 then ASSIGN
   lDate1 = fper2date(iiPer,0) 
   lDate2 = fper2date(iiPer,1) - 1
   filename = "monthlycalls" + string(iiPer) + ".dump".

dformat = "yyyy-mm-dd".

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

for each MobCDR USE-INDEX Date where
         MobCDR.datest >= ldate1 and 
         MobCDR.datest <= ldate2 and 
         MobCDR.errorcode = 0
no-lock:

   find ttCalls where 
        ttCalls.calldate = ldate1 and
        ttCalls.Mobtype  = MobCDR.clitype and
        ttCalls.CLI      = MobCDR.CLI AND
        ttCalls.BillCode = MobCDR.BillCode  and
        ttCalls.ccn      = MobCDR.ccn
   exclusive-lock no-error.
   
   if not avail ttCalls then do:
      create ttCalls.
      assign
         ttCalls.calldate = ldate1
         ttCalls.Mobtype  = MobCDR.CLIType
         ttCalls.CLI      = MobCDR.CLI
         ttCalls.BillCode = MobCDR.BillCode
         ttCalls.ccn      = MobCDR.ccn.
   end.

   ASSIGN
      ttCalls.count  = ttCalls.count + 1
      ttCalls.sec    = ttCalls.sec + MobCDR.BillDur
      ttCalls.dataAmt = ttcalls.dataamt + mobcdr.datain + mobcdr.dataout.
      tmplaskuri     = tmplaskuri + 1.

   FIND Customer WHERE Customer.CustNUm = MobCDR.InvCust NO-LOCK NO-ERROR.
   
   ldVatFactor = fVatFactor(IF AVAILABLE Customer
                            THEN Customer.VATUsage
                            ELSE 1,
                            MobCDR.BillCode).

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
 
/*
   if tmplaskuri mod 1000 = 0 then put screen row 23 col 4 string(tmplaskuri).

   if tmplaskuri > 10000 then leave.
*/
end.

output stream excel to value(lcSdir + filename).

/*
put stream excel unformatted 
   "Date"     tab 
   "CLIType"  tab 
   "CLI"      tab 
   "BillCode" tab 
   "BillName" tab
   "CCN"      tab
   "CCN Name" tab
   "Count"    tab 
   "Sec"      tab 
   "Data kB"  tab
   "Amount"   tab 
   "VatAmt"   skip. 
*/

for each ttCalls NO-LOCK:
/*
break by ttCalls.calldate:
*/
   /*
   ACCUMULATE 
      ttCalls.Count   (TOTAL)
      ttCalls.Sec     (TOTAL)
      ttCalls.DataAmt (TOTAL)
      ttCalls.Amount  (TOTAL)
      ttCalls.VATAmt  (TOTAL).
   */
   
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
   
   
  /* if iiPer = 0 THEN*/ put stream excel unformatted
      ttCalls.calldate tab.
  /*
  else 
      put stream excel unformatted
      string(MONTH(ttCalls.calldate),"99") + "/" + 
      string(YEAR(ttCalls.calldate)) tab.*/

   put stream excel unformatted
      ttCalls.Mobtype  tab 
      ttCalls.CLI      tab 
      ttCalls.BillCode tab 
      lcBiname         tab
      ttCalls.ccn      tab
      lcCCNName        tab
      ttCalls.count    tab 
      ttCalls.sec      tab 
      ttCalls.DataAmt  tab
      round(ttCalls.amount,2)     tab 
      round(ttCalls.VatAmt,2)     tab
      round(ttCalls.ServAmt,2)    tab
      round(ttCalls.ServVatAmt,2) skip.
   /*
   if last-of(ttCalls.calldate) then do:
   
      put stream excel unformatted
         "TOTAL" tab 
         ""      tab 
         ""      tab 
         ""      tab 
         ""      tab
         ""      tab
         ""      tab
         (accum total ttCalls.count)    tab 
         (accum total ttCalls.sec)      tab 
         (accum total ttCalls.DataAmt)  tab
         round((accum total ttCalls.amount),2) tab 
         round((accum total ttCalls.VatAmt),2) skip.

   end.
   */
end.

output stream excel close.

unix silent value("mv " + lcSdir + filename + " " + lcODir).

session:numeric-format = numform.

