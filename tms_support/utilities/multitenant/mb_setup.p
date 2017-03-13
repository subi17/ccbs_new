/* Setup scipt for multitenant table functionalities 
   Yoigo vs Masmovil */

{Func/multitenantfunc.i}

/* YOIGO setups */

fsetEffectiveTenantForAllDB("Default").
FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.Paramgroup EQ "Report" AND
           TMSParam.ParamCode EQ "SIMStatistics" NO-ERROR.
IF INDEX(TMSParam.charval,"Yoigo") EQ 0 THEN
   TMSParam.charval = REPLACE(TMSParam.charval,"SIMStatistics","SIMStatistics_Yoigo").
   
FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.Paramgroup EQ "Report" AND
           TMSParam.ParamCode EQ "ErrorFile" NO-ERROR.
IF INDEX(TMSParam.charval,"Yoigo") EQ 0 THEN
   TMSParam.charval = REPLACE(TMSParam.charval,"errorfile","errorfile_Yoigo").

FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.Paramgroup EQ "Report" AND
           TMSParam.ParamCode EQ "MSISDNStatistics" NO-ERROR.
IF INDEX(TMSParam.charval,"Yoigo") EQ 0 THEN
   TMSParam.charval = REPLACE(TMSParam.charval,"MSISDNStatistics","MSISDNStatistics_Yoigo").

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
IF INDEX(TMSParam.charval,"#COMPANY") EQ 0 THEN
   TMSParam.charval = REPLACE(TMSParam.charval,"/IFS_PAY","/#COMPANY_IFS_PAY").

FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.Paramgroup EQ "IFS" AND
           TMSParam.ParamCode EQ "IFSCollActionFile" NO-ERROR.
IF INDEX(TMSParam.charval,"#COMPANY") EQ 0 THEN
   TMSParam.charval = REPLACE(TMSParam.charval,"/IFS_BAR","/#COMPANY_IFS_BAR").

/* ----------------------------------------------------------------------
   MASMOVIL setups start here */

fsetEffectiveTenantForAllDB("tMasmovil").
FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.Paramgroup EQ "Report" AND
           TMSParam.ParamCode EQ "SIMStatistics" NO-ERROR.
IF INDEX(TMSParam.charval,"MasMovil") EQ 0 THEN
   TMSParam.charval = REPLACE(TMSParam.charval,"SIMStatistics","SIMStatistics_MasMovil").

FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.Paramgroup EQ "Report" AND
           TMSParam.ParamCode EQ "ErrorFile" NO-ERROR.
IF INDEX(TMSParam.charval,"MasMovil") EQ 0 
THEN
   TMSParam.charval = REPLACE(TMSParam.charval,"errorfile","errorfile_MasMovil").

FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.Paramgroup EQ "Report" AND
           TMSParam.ParamCode EQ "MSISDNStatistics" NO-ERROR.
IF INDEX(TMSParam.charval,"MasMovil") EQ 0 THEN
   TMSParam.charval = REPLACE(TMSParam.charval,"MSISDNStatistics","MSISDNStatistics_MasMovil").

/*
FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.Paramgroup EQ "Report" AND
           TMSParam.ParamCode EQ "AddressFile" NO-ERROR.
IF INDEX(TMSParam.charval,"MasMovil") EQ 0 THEN
   TMSParam.charval = REPLACE(TMSParam.charval,"icc_msisdn_rep","icc_msisdn_rep_MasMovil").
*/

FOR EACH TMSParam WHERE
         TMSParam.brand EQ "1" AND
         TMSParam.Paramgroup EQ "CustCare" AND
         TMSParam.ParamCode BEGINS "DefCust":

   TMSParam.ParamCode = REPLACE(TMSParam.ParamCode,"1","2").
END.

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
IF INDEX(TMSParam.charval,"#COMPANY") EQ 0 THEN
   TMSParam.charval = REPLACE(TMSParam.charval,"/IFS_PAY","/#COMPANY_IFS_PAY").

FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.Paramgroup EQ "IFS" AND
           TMSParam.ParamCode EQ "IFSCollActionFile" NO-ERROR.
IF INDEX(TMSParam.charval,"#COMPANY") EQ 0 THEN
   TMSParam.charval = REPLACE(TMSParam.charval,"/IFS_BAR","/#COMPANY_IFS_BAR").

/* MB-142 */
FIND FIRST Dumpfile WHERE 
           DumpFile.dumpid EQ 73 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#COMPANY" THEN
   dumpfile.filename = "#COMPANY_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 30 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#COMPANY" THEN
   dumpfile.filename = "#COMPANY_" + dumpfile.filename.
RELEASE Dumpfile. 

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 94 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#COMPANY" THEN
   dumpfile.filename = "#COMPANY_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 62 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#COMPANY" THEN
   dumpfile.filename = "#COMPANY_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 32 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#COMPANY" THEN
   dumpfile.filename = "#COMPANY_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 91 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#COMPANY" THEN
   dumpfile.filename = "#COMPANY_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 54 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#COMPANY" THEN
   dumpfile.filename = "#COMPANY_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 101 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#COMPANY" THEN
   dumpfile.filename = "#COMPANY_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 55 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#COMPANY" THEN
   dumpfile.filename = "#COMPANY_" + dumpfile.filename.
RELEASE Dumpfile.

FIND FIRST Dumpfile WHERE
           DumpFile.dumpid EQ 31 NO-ERROR.
IF AVAIL dumpfile AND NOT dumpfile.filename BEGINS "#COMPANY" THEN
   dumpfile.filename = "#COMPANY_" + dumpfile.filename.
RELEASE Dumpfile.

