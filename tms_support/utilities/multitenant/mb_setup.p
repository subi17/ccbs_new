/* Setup scipt for multitenant table functionalities 
   Yoigo vs Masmovil */

{Func/multitenantfunc.i}
DEF STREAM sin.
DEF STREAM sFile.
DEF VAR lcLine AS CHAR NO-UNDO.
DEF VAR lcDFIdList AS CHAR NO-UNDO.
DEF VAR liId AS INT NO-UNDO.
DEF BUFFER MMInvText FOR InvText.

/* Copy Invtext to MM */
/*
fsetEffectiveTenantForAllDB("tMasMovil").
FIND FIRST InvText NO-ERROR.
IF NOT AVAIL InvText THEN DO:
   FOR EACH InvText TENANT-WHERE BUFFER-TENANT-ID(InvText) EQ 0:
      CREATE MMInvText.
      BUFFER-COPY InvText TO MMInvtext.
   END.
END.
*/

FUNCTION fAddTenant2Filename RETURNS LOGICAL (INPUT icdumpName AS CHAR):
   DEF VAR lcFilename AS CHAR NO-UNDO.
   FIND FIRST Dumpfile WHERE
              Dumpfile.brand EQ "1" AND
              Dumpfile.dumpname EQ icdumpname NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "No dumpfile: " + icDumpname VIEW-AS ALERT-BOX.
      NEXT.
   END.
   IF AVAIL DumpFile THEN DO:
      IF INDEX(dumpfile.filename, "#TENANT") > 0 THEN NEXT.
      IF dumpfile.filename BEGINS "#CAT" THEN
         lcFileName = REPLACE(dumpfile.filename,"#CAT","#CAT_#TENANT").
      ELSE IF dumpfile.filename BEGINS "DWH" THEN
         lcFileName = REPLACE(dumpfile.filename,"DWH","DWH_#TENANT").
      ELSE
         lcFilename = "#TENANT_" + dumpfile.filename.
      Dumpfile.filename = lcFilename.
      /*MESSAGE lcFileName VIEW-AS ALERT-BOX.*/
      IF lcDFIdList EQ "" THEN lcDFIdList = STRING(dumpfile.dumpID).
      ELSE  lcDFIdList = lcDFIdList + "," + STRING(dumpfile.dumpID).
   END.
END FUNCTION.




/* YOIGO and MasMovil setups */

DEFINE VARIABLE lcBrand AS CHARACTER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcTempID AS CHAR NO-UNDO.

do i = 1 to 2:

   if i eq 1 then do:
      fsetEffectiveTenantForAllDB("Default").
      lcBrand = "Yoigo".
   end.
   else do:
      fsetEffectiveTenantForAllDB("tMasmovil").
      lcBrand = "MasMovil".
   end.

   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "Report" AND
              TMSParam.ParamCode EQ "SIMStatistics" NO-ERROR.
   IF INDEX(TMSParam.charval,lcBrand) EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"SIMStatistics","SIMStatistics_" + lcBrand).
      
   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "Report" AND
              TMSParam.ParamCode EQ "ErrorFile" NO-ERROR.
   IF INDEX(TMSParam.charval,lcBrand) EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"errorfile","errorfile_" + lcBrand).

   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "Report" AND
              TMSParam.ParamCode EQ "MSISDNStatistics" NO-ERROR.
   IF INDEX(TMSParam.charval,lcBrand) EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"MSISDNStatistics","MSISDNStatistics_" + lcBrand).

   /*
   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "Report" AND
              TMSParam.ParamCode EQ "AddressFile" NO-ERROR.
   IF INDEX(TMSParam.charval,"Yoigo") EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"icc_msisdn_rep","icc_msisdn_rep_Yoigo").
   */

   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "Terminate" AND
              TMSParam.ParamCode EQ "SubsTermLog" NO-ERROR.
   IF INDEX(TMSParam.charval,"#TENANT") EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"spool/terminate","spool/#TENANT_terminate").


   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "IFS" AND
              TMSParam.ParamCode EQ "IFSPaymStatusFile" NO-ERROR.
   IF INDEX(TMSParam.charval,"#TENANT") EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"/IFS_PAY","/#TENANT_IFS_PAY").

   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "IFS" AND
              TMSParam.ParamCode EQ "IFSCollActionFile" NO-ERROR.
   IF INDEX(TMSParam.charval,"#TENANT") EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"/IFS_BAR","/#TENANT_IFS_BAR").

   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "BillReport" AND
              TMSParam.ParamCode EQ "CCNReportFile" NO-ERROR.
   IF INDEX(TMSParam.charval,"#TENANT") EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"/billing","/#TENANT_billing").
   
   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "FuncRun" AND
              TMSParam.ParamCode EQ "FRDaemonLockFile" NO-ERROR.
   IF INDEX(TMSParam.charval,lcBrand) EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"daemon.lock","daemon." + lower(lcBrand) + ".lock").

   FOR EACH BankAccount.
      IF i eq 1 THEN
         ASSIGN
            bankaccount.presenterID = bankAccount.creditorid
            lctempId = bankAccount.creditorid.
      ELSE
         bankaccount.presenterID = lctempId.
   END.
END.

/* ----------------------------------------------------------------------
   MASMOVIL setups start here */

fsetEffectiveTenantForAllDB("tMasmovil").


FOR EACH TMSParam WHERE
         TMSParam.brand EQ "1" AND
         TMSParam.Paramgroup EQ "CustCare" AND
         TMSParam.ParamCode BEGINS "DefCust":

   TMSParam.ParamCode = REPLACE(TMSParam.ParamCode,"1","2").
END.

FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "DirDebit" AND
              TMSParam.ParamCode EQ "DDebitFileName" NO-ERROR.
   IF INDEX(TMSParam.charval,"MASMOVIL") EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"YOIGO","MASMOVIL").

FOR EACH BankAccount:
   IF BankAccount.bankoffice EQ "BBVA" THEN DO:
      ASSIGN
         bankaccount.bankaccount = "ES0701824572400201566648"
         bankaccount.bankdata = "ES0701824572400201566648"
         bankaccount.bic = "BBVAESMMXXX"
         bankaccount.creditorid = "ES22000A84633643".
         bankaccount.ddallocation = 100.0.
   END.
   ELSE 
      DELETE BankAccount.

END.

FIND FIRST Company.
Company.compname = "MASMOVIL TELECOM 3.0, SAU".


/* Common tables */

/* MB-142 */
FIND FIRST Dumpfile WHERE 
           DumpFile.dumpid EQ 73 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#TENANT" THEN
   dumpfile.filename = "#TENANT_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 30 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#TENANT" THEN
   dumpfile.filename = "#TENANT_" + dumpfile.filename.
RELEASE Dumpfile. 

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 94 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#TENANT" THEN
   dumpfile.filename = "#TENANT_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 62 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#TENANT" THEN
   dumpfile.filename = "#TENANT_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 32 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#TENANT" THEN
   dumpfile.filename = "#TENANT_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 91 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#TENANT" THEN
   dumpfile.filename = "#TENANT_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 54 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#TENANT" THEN
   dumpfile.filename = "#TENANT_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 101 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#TENANT" THEN
   dumpfile.filename = "#TENANT_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 55 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#TENANT" THEN
   dumpfile.filename = "#TENANT_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 31 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#TENANT" THEN
   dumpfile.filename = "#TENANT_" + dumpfile.filename.
RELEASE Dumpfile.

/* MB-631 DWH dumps and MB-675 Track dumps */
FIND FIRST Dumpfile WHERE dumpfile.dumpname EQ "Barrings" NO-ERROR.
IF NOT AVAIL DumpFile THEN DO:
   FIND LAST DumpFile USE-INDEX DumpID NO-LOCK NO-ERROR.
   IF AVAILABLE DumpFile
   THEN liid = DumpFile.DumpID + 1.
   ELSE liid = 1.
   FIND FIRST DumpFile WHERE Dumpfile.dumpid EQ 101 NO-ERROR.
   IF NOT AVAIL DumpFile THEN liid = 101. /* Same as in prod */
   CREATE DumpFile .
   ASSIGN
      dumpfile.Active          = TRUE
      dumpfile.AllowReplica    = NO
      dumpfile.AveDurFull      = 1770
      dumpfile.AveDurMod       = 2
      dumpfile.BatchID         = 1
      dumpfile.Brand           = "1"
      dumpfile.ConfigParam     = ""
      dumpfile.DecimalPoint    = "."
      dumpfile.Description     = "Subscription barrings"
      dumpfile.DumpCharSet     = ""
      dumpfile.DumpDelimiter   = "|"
      dumpfile.DumpFormat      = "ASCII"
      dumpfile.DumpID          = liid
      dumpfile.DumpLineFeed    = ""
      dumpfile.DumpName        = "Barrings"
      dumpfile.EmptyFile       = TRUE
      dumpfile.EventLogFields  = ""
      dumpfile.FileCategory    = "DWH"
      dumpfile.FileName        = "DWH_barrings_#MODE_#DATE_#TIME.txt"
      dumpfile.FullCollModule  = ""
      dumpfile.LinkKey         = ""
      dumpfile.LogFile         = ""
      dumpfile.LogicModule     = "Mm/barring_dump.p"
      dumpfile.MainTable       = "Barring"
      dumpfile.ModCollModule   = ""
      dumpfile.ModFromEventLog = TRUE
      dumpfile.ModFromField    = "EventTS"
      dumpfile.QueryClause     = ""
      dumpfile.SideTables      = ""
      dumpfile.SpoolDir        = "/store/riftp/dumpfiles/dwh/spool"
      dumpfile.TransDir        = "/store/riftp/dumpfiles/dwh/outgoing"
      dumpfile.UseIndex        = "".
END.

FIND FIRST Dumpfile WHERE dumpfile.dumpname EQ "SingleFixFeeCollect" NO-ERROR.
IF NOT AVAIL DumpFile THEN DO:
   FIND LAST DumpFile USE-INDEX DumpID NO-LOCK NO-ERROR.
   IF AVAILABLE DumpFile
   THEN liid = DumpFile.DumpID + 1.
   ELSE liid = 1.
   FIND FIRST DumpFile WHERE Dumpfile.dumpid EQ 97 NO-ERROR.   
   IF NOT AVAIL DumpFile THEN liid = 97. /* same as in prod */
   CREATE DumpFile .
   ASSIGN
      dumpfile.Active          = TRUE
      dumpfile.AllowReplica    = NO
      dumpfile.AveDurFull      = 13
      dumpfile.AveDurMod       = 5
      dumpfile.BatchID         = 1
      dumpfile.Brand           = "1"
      dumpfile.ConfigParam     = ""
      dumpfile.DecimalPoint    = "."
      dumpfile.Description     = "Dump for collecting info about Single Fees and Fixed Fees"
      dumpfile.DumpCharSet     = ""
      dumpfile.DumpDelimiter   = "|"
      dumpfile.DumpFormat      = "ASCII"
      dumpfile.DumpID          = liid
      dumpfile.DumpLineFeed    = ""
      dumpfile.DumpName        = "SingleFixFeeCollect"
      dumpfile.EmptyFile       = NO
      dumpfile.EventLogFields  = ""
      dumpfile.FileCategory    = "DWH"
      dumpfile.FileName        = "DWH_Modified_Single_Fix_Fees_#DATE.txt"
      dumpfile.FullCollModule  = ""
      dumpfile.LinkKey         = ""
      dumpfile.LogFile         = ""
      dumpfile.LogicModule     = "Mm/fixedsinglefee_collecdump.p"
      dumpfile.MainTable       = "FixedFee"
      dumpfile.ModCollModule   = ""
      dumpfile.ModFromEventLog = TRUE
      dumpfile.ModFromField    = ""
      dumpfile.QueryClause     = ""
      dumpfile.SideTables      = "SingleFee"
      dumpfile.SpoolDir        = "/store/riftp/dumpfiles/dwh/spool"
      dumpfile.TransDir        = "/store/riftp/dumpfiles/dwh/outgoing"
      dumpfile.UseIndex        = "".
END.


INPUT STREAM sin FROM VALUE("../tms_support/utilities/multitenant/dumpfiles_phase1.txt").

REPEAT:
   IMPORT STREAM sin UNFORMATTED lcLine.
   IF TRIM(lcLine) EQ "" THEN NEXT.
   fAddTenant2Filename(lcLine).
END.

/* Remove extra dumps from DFTimeTable that are not suported in phase 1 */

FOR EACH DFTimeTable WHERE
         lookup(STRING(DFTimeTable.dumpid),lcDFIdList) EQ 0:
   /*MESSAGE DFTimeTable.dumpid VIEW-AS ALERT-BOX.*/
   /*DELETE DFTimeTable */

END.
