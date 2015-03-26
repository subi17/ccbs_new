/* -----------------------------------------------------------------
  MODULE .......: mobsubreppr.p
  TASK .........: print mobsub report with given criteria
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 30.10.03
  CHANGED ......: 26.03.04 tk brand, headers
  Version ......:
  ------------------------------------------------------------------ */

{commali.i}
{excel.i}
{utumaa.i "new"}

ASSIGN tuni1 = ""
       tuni2 = "".

DEF INPUT PARAMETER InvGroup  LIKE Customer.InvGroup NO-UNDO.
DEF INPUT PARAMETER CustNum1  AS I   no-undo format "zzzzzz9".
DEF INPUT PARAMETER CustNum2  AS I   no-undo format "zzzzzz9".
DEF INPUT PARAMETER CLIType   LIKE MobSub.CLIType NO-UNDO.
DEF INPUT PARAMETER MSStatus  LIKE MobSub.MSStatus NO-UNDO.
DEF INPUT PARAMETER actdate1  AS DA  NO-UNDO.
DEF INPUT PARAMETER actdate2  AS DA  NO-UNDO.
DEF INPUT PARAMETER details   AS LOG NO-UNDO.

DEF VAR lkm AS I NO-UNDO.
DEF VAR i   AS I NO-UNDO.

DEF TEMP-TABLE ttMobSub 
   FIELD MSStatus LIKE MobSub.MSStatus 
   FIELD CLIType  LIKE MobSub.CLIType
   FIELD CLI      LIKE MobSub.CLI
   FIELD IMSI     LIKE MobSub.IMSI
   FIELD ICC      LIKE MobSub.ICC
   FIELD ActDate  LIKE MobSub.ActivationDate
   INDEX MSStatus IS PRIMARY MSStatus CLIType CLI.

ASSIGN tila = true.
{utuloste.i "return"}

message "Printing in progress, wait ...".

FOR EACH Customer NO-LOCK WHERE 
         Customer.Brand     = gcBrand  AND
         Customer.CustNum  >= CustNum1 AND
         Customer.CustNum  <= CustNum2 AND
        (Customer.InvGroup  = InvGroup OR InvGroup = ""):

   FOR EACH MobSub NO-LOCK WHERE 
            MobSub.Brand    = gcBrand AND
            MobSub.CustNum  = Customer.CustNum AND
           (MobSub.MsStatus = MsStatus OR MsStatus = 0) AND
           (MobSub.CLIType  = CLIType OR CLIType = "") AND
           (MobSub.MSStatus = MSStatus OR MSStatus = 0) AND
            MobSub.ActivationDate >= actdate1 AND
            MobSub.ActivationDate <= actdate2:
      
      CREATE ttMobSub.
      ASSIGN
         ttMobSub.MSStatus = MobSub.MSStatus
         ttMobSub.CLIType  = MobSub.CLIType
         ttMobSub.CLI      = MobSub.CLI
         ttMobSub.IMSI     = MobSub.IMSI
         ttMobSub.ICC      = MobSub.ICC
         ttMobSub.ActDate  = MobSub.ActivationDate.

   END.
END.

PUT STREAM tul UNFORMATTED
   "Created subscriptions by status" SKIP(1)
   "InvGroup"  tab InvGroup skip
   "Customers" tab CustNum1 tab  CustNum2 SKIP 
   "Type"      tab CLIType  skip
   "Sstatus"   tab MSStatus skip
   "Period"    tab actdate1 tab  actdate2 SKIP(1).
   

PUT STREAM tul UNFORMATTED
   "Status" tab
   "CLIType" tab
   "CLIType name" tab
   "Amount" skip.


FOR EACH ttMobSub NO-LOCK
BREAK
   BY ttMobSub.MSStatus
   BY ttMobSub.CLIType:

   ACCUMULATE lkm (COUNT BY ttMobSub.CLIType).
   
   IF LAST-OF(ttMobSub.CLIType) THEN DO:
      FIND CLIType NO-LOCK WHERE
           clitype.Brand   = gcBrand AND 
           CLIType.CLIType = ttMobSub.CLIType.
      
   
      put stream tul unformatted
         ttMobSub.MSStatus tab
         ttMobSub.CLIType tab
         CLIType.CLIName tab
        (accum count by ttMobSub.CLIType lkm) skip.
   END.
   

END.

IF details then do:

   put stream tul unformatted
      skip(3)
      "Status" tab
      "CLIType" tab
      "CLIType name" tab
      "MSISDN" tab
      "IMSI" tab
      "ICC" tab
      "Act.Date" skip.
      

   FOR EACH ttMobSub NO-LOCK:
      FIND CLIType NO-LOCK WHERE
           clitype.Brand   = gcBrand AND 
           CLIType.CLIType = ttMobSub.CLIType.
      
      put stream tul unformatted
         ttMobSub.MsStatus tab
         ttMobSub.CLIType tab
         CLIType.CLIName tab
         ttMobSub.CLI tab
         ttMobSub.IMSI tab
         ttMobSub.ICC tab
         ttMobSub.ActDate skip.

   END.

END.

ASSIGN tila = false.
{utuloste.i}



