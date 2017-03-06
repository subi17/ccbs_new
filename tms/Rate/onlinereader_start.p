/* ---------------------------------------------------------------------------
  MODULE .......: onlinereader_start.p
  FUNCTION .....: For starting the module that reads in CDRs
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 27.12.2010
  VERSION ......: Yoigo
  -------------------------------------------------------------------------- */

{Syst/commpaa.i}
{Func/direct_dbconnect.i}

DEF VAR llDateLog       AS LOG  NO-UNDO INIT TRUE.
DEF VAR llDispErrors    AS LOG  NO-UNDO INIT TRUE.
DEF VAR liPortNum       AS INT  NO-UNDO.
DEF VAR lcTableNames    AS CHAR NO-UNDO.
DEF VAR lcOldTableNames AS CHAR NO-UNDO.

gcBrand = "1".

liPortNum = INTEGER(SESSION:PARAMETER) NO-ERROR.
IF ERROR-STATUS:ERROR OR liPortNum = 0 THEN QUIT.

/* roaming in */
IF liPortNum = 2230 THEN lcTableNames = "RoamCDR,McdrDtl2".
ELSE IF liPortNum = 2260 THEN lcTableNames = "PrepEDR,EDRDtl,McdrDtl2".
/* postpaid and prepaid */
ELSE lcTableNames = "MobCDR,McdrDtl2,PrepCDR,ErrorCDR".


DO WHILE TRUE:

   fInitializeConnectTables(lcTableNames,"").
   RUN pDirectConnect2Dbs(gcBrand,
                          "",
                          TODAY,
                          TODAY).
   
   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      MESSAGE RETURN-VALUE
      VIEW-AS ALERT-BOX ERROR.
      QUIT.
   END.

   /* if new dbs have been taken into use today then connect also to previous
      dbs in order to save cdrs older than today that come in late to old dbs 
      (done on 1. day of new dbs only)
   */
   lcOldTableNames = "".
   
   FOR EACH ttDB,
      FIRST DBConfig NO-LOCK WHERE
            DBConfig.DBConfigID = ttDB.DBConfigID AND
            DBConfig.FromDate = TODAY:
      lcOldTableNames = lcOldTableNames + 
                        (IF lcOldTableNames > "" THEN "," ELSE "") +
                        DBConfig.TableName.
   END.

   IF lcOldTableNames = "" THEN 
      RUN pDirectDisconnect("old").
      
   fInitializeConnectTables(lcOldTableNames,"old").

   IF lcOldTableNames > "" THEN 
      RUN pDirectConnect2Dbs(gcBrand,
                             "old",
                             TODAY - 1,
                             TODAY - 1).
   /* no error check; connections to old dbs are not essential, if fails then 
      old cdrs can be written to new dbs */
 
   IF liPortNum = 2230 THEN 
      RUN roamcdr.p (liPortNum).
   ELSE IF liPortNum = 2260 THEN  DO:
      RUN Rate/edr_reader.p ("EDR_OnLine",
			               liPortNum).
   END.
      
   ELSE 
      RUN Rate/onlinereader.p(llDateLog,
                         "Mobile_OnLine",
                         llDispErrors,
                         liPortNum).
                      
   IF RETURN-VALUE NE "RESET" THEN LEAVE.
END.

QUIT.

