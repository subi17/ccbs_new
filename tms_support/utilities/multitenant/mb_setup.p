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
      TMSParam.charval = REPLACE(TMSParam.charval,"SIMStatistics",lcBrand + "_SIMStatistics_").
      
   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "Report" AND
              TMSParam.ParamCode EQ "ErrorFile" NO-ERROR.
   IF INDEX(TMSParam.charval,lcBrand) EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"icc_msisdn",lcBrand + "_icc_msisdn").

   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "Report" AND
              TMSParam.ParamCode EQ "MSISDNStatistics" NO-ERROR.
   IF INDEX(TMSParam.charval,lcBrand) EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"MSISDNStatistics",lcBrand + "_MSISDNStatistics").

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
              TMSParam.Paramgroup EQ "BillReport" AND
              TMSParam.ParamCode EQ "BillQualityFileName" NO-ERROR.
   IF INDEX(TMSParam.charval,"#TENANT") EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"/billing","/#TENANT_billing").

   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "FuncRun" AND
              TMSParam.ParamCode EQ "FRDaemonLockFile" NO-ERROR.
   IF INDEX(TMSParam.charval,lcBrand) EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"daemon.lock","daemon." + lower(lcBrand) + ".lock").

   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "Reports" AND
              TMSParam.ParamCode EQ "ErrorLogRepFile" NO-ERROR.
   IF INDEX(TMSParam.charval,lcbrand) EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"log/errorlog","log/" + CAPS(lcbrand) + "_errorlog").   

   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "CDR" AND
              TMSParam.ParamCode EQ "DoubleCallLog" NO-ERROR.
   IF INDEX(TMSParam.charval,lcbrand) EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"mobcdr",CAPS(lcbrand) + "_mobcdr").
   
   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "BillRun" AND
              TMSParam.ParamCode EQ "BillRunStatFile" NO-ERROR.
   IF INDEX(TMSParam.charval,lcbrand) EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"billrun_statistic",
                         CAPS(lcbrand) + "_billrun_statistic").

   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "IFS" AND
              TMSParam.ParamCode EQ "IFSPaymStatusLog" NO-ERROR.
   IF INDEX(TMSParam.charval,lcbrand) EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"spool/IFS","spool/" + CAPS(lcbrand) + "_IFS").

   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "Pentaho" AND
              TMSParam.ParamCode EQ "PentahoBITotals" NO-ERROR.
   IF INDEX(TMSParam.charval,"#TENANT") EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"/billing","/#TENANT_billing").
   
   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramgroup EQ "Pentaho" AND
              TMSParam.ParamCode EQ "InvGrainFile" NO-ERROR.
   IF INDEX(TMSParam.charval,"#TENANT") EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"invoice_row_dump",
                                 "#TENANT_invoice_row_dump").
   FOR EACH BankAccount.
      IF i eq 1 THEN
         ASSIGN
            bankaccount.presenterID = bankAccount.creditorid
            lctempId = bankAccount.creditorid.
      ELSE
         bankaccount.presenterID = lctempId.
   END.

   FIND FIRST Company.
   IF i eq 1 then assign
      company.CreditorName = "YOIGO".
   else assign
      company.CreditorName = "MASMOVIL TELECOM"
      Company.compname = "MASMOVIL TELECOM 3.0, SAU".

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

   CREATE DFField.
   ASSIGN
      DFField.brand = "1"
      DFField.DFField = "MsSeq"
      DFField.DFLabel = "SubSeq"
      DFField.DFTable = "Barring"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 1
      DFField.ToDate = 12/31/49.

   CREATE DFField.
   ASSIGN
      DFField.brand = "1"
      DFField.DFField = "BarringCode"
      DFField.DFLabel = "Barring"
      DFField.DFTable = "Barring"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 2
      DFField.ToDate = 12/31/49.
   CREATE DFField.
   ASSIGN
      DFField.brand = "1"
      DFField.DFField = "BarringStatus"
      DFField.DFLabel = "Barring Status"
      DFField.DFTable = "Barring"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 3
      DFField.ToDate = 12/31/49.

   CREATE DFField.
   ASSIGN
      DFField.brand = "1"
      DFField.DFField = "UserCode"
      DFField.DFLabel = "User ID"
      DFField.DFTable = "Barring"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 4
      DFField.ToDate = 12/31/49.

   CREATE DFField.
   ASSIGN
      DFField.brand = "1"
      DFField.DFField = "EventTS"
      DFField.DFLabel = "Time Stamp"
      DFField.DFTable = "Barring"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 5
      DFField.ToDate = 12/31/49.

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

FIND FIRST Dumpfile WHERE dumpfile.dumpname EQ "ClitypeDump" NO-ERROR.
IF NOT AVAIL DumpFile THEN DO:
   FIND LAST DumpFile USE-INDEX DumpID NO-LOCK NO-ERROR.
   IF AVAILABLE DumpFile
   THEN liid = DumpFile.DumpID + 1.
   ELSE liid = 1.
   FIND FIRST DumpFile WHERE Dumpfile.dumpid EQ 503 NO-ERROR.
   IF NOT AVAIL DumpFile THEN liid = 503. /* same as in prod */
   CREATE DumpFile .
   ASSIGN
      dumpfile.Active          = TRUE
      dumpfile.AllowReplica    = NO
      dumpfile.AveDurFull      = 1
      dumpfile.AveDurMod       = 1
      dumpfile.BatchID         = 1
      dumpfile.Brand           = "1"
      dumpfile.ConfigParam     = ""
      dumpfile.DecimalPoint    = "."
      dumpfile.Description     = "Tariff dump for Track"
      dumpfile.DumpCharSet     = ""
      dumpfile.DumpDelimiter   = "|"
      dumpfile.DumpFormat      = "ASCII"
      dumpfile.DumpID          = liid
      dumpfile.DumpLineFeed    = ""
      dumpfile.DumpName        = "CliTypeDump"
      dumpfile.EmptyFile       = NO
      dumpfile.EventLogFields  = ""
      dumpfile.FileCategory    = "TRACK"
      dumpfile.FileName        = "#TENANT_tariff_dump_#DATE_#TIME.csv"
      dumpfile.FullCollModule  = ""
      dumpfile.LinkKey         = ""
      dumpfile.LogFile         = ""
      dumpfile.LogicModule     = ""
      dumpfile.MainTable       = "CliType"
      dumpfile.ModCollModule   = ""
      dumpfile.ModFromEventLog = NO
      dumpfile.ModFromField    = ""
      dumpfile.QueryClause     = ""
      dumpfile.SideTables      = ""
      dumpfile.SpoolDir        = "/mnt/qss/spool"
      dumpfile.TransDir        = "/mnt/qss/subscription_types"
      dumpfile.UseIndex        = "".

   CREATE DFField.
   ASSIGN 
      DFField.brand = "1"
      DFField.DFField = "Clitype"
      DFField.DFLabel = "CLI Type"
      DFField.DFTable = "CLIType"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 1
      DFField.ToDate = 12/31/49.

   CREATE DFField.
   ASSIGN
      DFField.brand = "1"
      DFField.DFField = "Paytype"
      DFField.DFLabel = "Payment Type"
      DFField.DFTable = "CLIType"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 2
      DFField.ToDate = 12/31/49.
   CREATE DFField.
   ASSIGN
      DFField.brand = "1"
      DFField.DFField = "Bundletype"
      DFField.DFLabel = "Bundle Based Type"
      DFField.DFTable = "CLIType"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 3
      DFField.ToDate = 12/31/49.

   CREATE DFField.
   ASSIGN
      DFField.brand = "1"
      DFField.DFField = "BaseBundle"
      DFField.DFLabel = "BaseBundle"
      DFField.DFTable = "CLIType"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 4
      DFField.ToDate = 12/31/49.

   CREATE DFField.
   ASSIGN
      DFField.brand = "1"
      DFField.DFField = "MiniAmt"
      DFField.DFLabel = "MinimPrice"
      DFField.DFTable = "CLIType"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 5
      DFField.ToDate = 12/31/49.

   CREATE DFField.
   ASSIGN
      DFField.brand = "1"
      DFField.DFField = "CommercialFee"
      DFField.DFLabel = "Commercial Fee"
      DFField.DFTable = "CLIType"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 6
      DFField.ToDate = 12/31/49.

   CREATE DFField.
   ASSIGN
      DFField.brand = "1"
      DFField.DFField = "CompareFee"
      DFField.DFLabel = "Comparison Fee"
      DFField.DFTable = "CLIType"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 7
      DFField.ToDate = 12/31/49.

   CREATE DFField.
   ASSIGN
      DFField.brand = "1"
      DFField.DFField = "UsageType"
      DFField.DFLabel = "Usage Type"
      DFField.DFTable = "CLIType"
      DFField.DumpId = dumpfile.dumpid
      DFField.fromdate = TODAY
      DFField.OrderNbr = 8
      DFField.ToDate = 12/31/49.


END.

/* MB-631 DWH dumps and MB-675 Track dumps */
FIND FIRST Dumpfile WHERE dumpfile.dumpname EQ "PrepaidCompReport" NO-ERROR.
IF NOT AVAIL DumpFile THEN DO:
   FIND LAST DumpFile USE-INDEX DumpID NO-LOCK NO-ERROR.
   IF AVAILABLE DumpFile
   THEN liid = DumpFile.DumpID + 1.
   ELSE liid = 1.
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
      dumpfile.Description     = ""
      dumpfile.DumpCharSet     = ""
      dumpfile.DumpDelimiter   = "|"
      dumpfile.DumpFormat      = "ASCII"
      dumpfile.DumpID          = liid
      dumpfile.DumpLineFeed    = ""
      dumpfile.DumpName        = "PrepaidCompReport"
      dumpfile.EmptyFile       = TRUE
      dumpfile.EventLogFields  = ""
      dumpfile.FileCategory    = "DWH"
      dumpfile.FileName        = "#TENANT_cc_prepaid_comp_#DATE_#TIME.txt"
      dumpfile.FullCollModule  = ""
      dumpfile.LinkKey         = ""
      dumpfile.LogFile         = ""
      dumpfile.LogicModule     = "Gwy/ppcomprep.p"
      dumpfile.MainTable       = "PrepaidRequest"
      dumpfile.ModCollModule   = ""
      dumpfile.ModFromEventLog = TRUE
      dumpfile.ModFromField    = "EventTS"
      dumpfile.QueryClause     = ""
      dumpfile.SideTables      = ""
      dumpfile.SpoolDir        = "/store/riftp/dumpfiles/prepaidcomp/spool/"
      dumpfile.TransDir        = "/store/riftp/dumpfiles/prepaidcomp/outgoing/"
      dumpfile.UseIndex        = "".
END.

FIND FIRST Dumpfile WHERE dumpfile.dumpname EQ "HighUsageReport" NO-ERROR.
IF NOT AVAIL DumpFile THEN DO:
   FIND LAST DumpFile USE-INDEX DumpID NO-LOCK NO-ERROR.
   IF AVAILABLE DumpFile
   THEN liid = DumpFile.DumpID + 1.
   ELSE liid = 1.
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
      dumpfile.Description     = ""
      dumpfile.DumpCharSet     = ""
      dumpfile.DumpDelimiter   = "|"
      dumpfile.DumpFormat      = "ASCII"
      dumpfile.DumpID          = liid
      dumpfile.DumpLineFeed    = ""
      dumpfile.DumpName        = "HighUsageReport"
      dumpfile.EmptyFile       = TRUE
      dumpfile.EventLogFields  = ""
      dumpfile.FileCategory    = "DWH"
      dumpfile.FileName        = "#TENANT_highspender_#DATE_#TIME.txt"
      dumpfile.FullCollModule  = ""
      dumpfile.LinkKey         = ""
      dumpfile.LogFile         = ""
      dumpfile.LogicModule     = "Gwy/highusagerep.p"
      dumpfile.MainTable       = "HighUsage"
      dumpfile.ModCollModule   = ""
      dumpfile.ModFromEventLog = TRUE
      dumpfile.ModFromField    = "EventTS"
      dumpfile.QueryClause     = ""
      dumpfile.SideTables      = ""
      dumpfile.SpoolDir        = "/store/riftp/dumpfiles/highspender/spool/"
      dumpfile.TransDir        = "/store/riftp/dumpfiles/highspender/outgoing/"
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
