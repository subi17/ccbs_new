/* Setup scipt for multitenant table functionalities 
   Yoigo vs Masmovil */

{Func/multitenantfunc.i}
DEF STREAM sin.
DEF STREAM sFile.
DEF VAR lcLine AS CHAR NO-UNDO.

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
   END.
END FUNCTION.




/* YOIGO and MasMovil setups */

DEFINE VARIABLE lcBrand AS CHARACTER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

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
              TMSParam.Paramgroup EQ "FuncRun" AND
              TMSParam.ParamCode EQ "FRDaemonLockFile" NO-ERROR.
   IF INDEX(TMSParam.charval,lcBrand) EQ 0 THEN
      TMSParam.charval = REPLACE(TMSParam.charval,"daemon.lock","daemon." + lower(lcBrand) + ".lock").

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

/* MB-631 */
INPUT STREAM sin FROM VALUE("../tms_support/utilities/multitenant/dumpfiles_phase1.txt").

REPEAT:
   IMPORT STREAM sin UNFORMATTED lcLine.
   IF TRIM(lcLine) EQ "" THEN NEXT.
   fAddTenant2Filename(lcLine).
END.

