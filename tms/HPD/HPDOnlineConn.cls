/*------------------------------------------------------------------------
    File        : HPDOnlineConn
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Notes       : Supports currenty only instances
                  MobCDR, MobCDROld, PrepCDR, PrepCDROld,
                  PrepEDR, PrepEDROld, Mcdr and McdrOld
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING HPD.HPDCommon.

{HPD/HPDEnvironment.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.HPDOnlineConn INHERITS HPD.HPDOnline:  

   DEFINE PRIVATE VARIABLE lcLastDB            AS CHARACTER       NO-UNDO.
   DEFINE PRIVATE VARIABLE llCurrent           AS LOGICAL         NO-UNDO.
   
   DEFINE PRIVATE VARIABLE lhDBConfig          AS HANDLE          NO-UNDO.
   DEFINE PRIVATE VARIABLE lhDBConfQuery       AS HANDLE          NO-UNDO.


   METHOD PRIVATE VOID mCheckTableAndPeriod():

      CASE lcInstance:
         WHEN "MobCDR" OR WHEN "PrepCDR" OR WHEN "PrepEDR" OR WHEN "Mcdr"
         THEN llCurrent = YES.
         WHEN "MobCDROld" OR WHEN "PrepCDROld" OR WHEN "PrepEDROld" OR WHEN "McdrOld"
         THEN ASSIGN
                 lcInstance  = SUBSTRING(lcInstance,1,LENGTH(lcInstance) - 3)
                 llCurrent   = NO
                 .
         OTHERWISE RETURN ERROR NEW Progress.Lang.AppError("Instance value is not supported", 1).

      END CASE.
      
   END METHOD.
   
   
   METHOD PRIVATE VOID mSetDBConfigQuery():

      DEFINE VARIABLE lcDBConfigTable AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcQuery         AS CHARACTER NO-UNDO.
      
      &IF "{&HPD_ENVIRONMENT}" = "Salt"
      &THEN
      ASSIGN
         lcDBConfigTable = "DBConf"
         lcQuery = "FOR EACH &1 NO-LOCK WHERE " +
                   "&1.Name      = &2  AND " +
                   "&1.State     = 1 " +
                   "BY &1.ToDate DESC"
         .
      &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN
      ASSIGN
         lcDBConfigTable = "DBConfig"
         lcQuery = "FOR EACH &1 NO-LOCK WHERE " +
                   "&1.Brand     = '1' AND " +
                   "&1.TableName = &2  AND " +
                   "&1.DBState   < 2 " +
                   "BY &1.ToDate DESC"
         .
      &ELSE
      RETURN ERROR NEW Progress.Lang.AppError("Unknown HPD environment", 1).
      &ENDIF

      CREATE BUFFER lhDBConfig FOR TABLE lcDBConfigTable.      
      CREATE QUERY lhDBConfQuery.
      lhDBConfQuery:SET-BUFFERS(lhDBConfig).
      lhDBConfQuery:QUERY-PREPARE(SUBSTITUTE(lcQuery, lcDBConfigTable, QUOTER(lcInstance))).
      
   END METHOD.
   
  
   METHOD PRIVATE VOID mGetDBInfo
      ( OUTPUT ocDBPath        AS CHARACTER,
        OUTPUT ocDBLogicalName AS CHARACTER ):

      DEFINE VARIABLE liPickNumber AS INTEGER NO-UNDO.
      DEFINE VARIABLE lii          AS INTEGER NO-UNDO.
      DEFINE VARIABLE llOK         AS LOGICAL NO-UNDO.
      
      ASSIGN
         ocDBPath        = ""
         ocDBLogicalName = ""
         .
      
      IF llCurrent = YES
      THEN liPickNumber = 1.
      ELSE liPickNumber = 2.      
      
      lhDBConfQuery:QUERY-OPEN().
      
      DO WHILE TRUE:
         
         llOK = lhDBConfQuery:GET-NEXT(NO-LOCK).

         IF llOK = ?
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process dbconfig query", 1).
         
         /* No more records, or query is not open */
         IF NOT llOK
         THEN LEAVE.
         
         lii = lii + 1.

         IF lii = liPickNumber 
         THEN DO:
            &IF "{&HPD_ENVIRONMENT}" = "Salt"
            &THEN
            ASSIGN
               ocDBPath        = lhDBConfig::Path
               ocDBLogicalName = lhDBConfig::Name
               .            
            &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
            &THEN              
            ASSIGN
               ocDBPath        = lhDBConfig::DirectConnect
               ocDBLogicalName = lhDBConfig::DBConnName
               .
            &ELSE
            RETURN ERROR NEW Progress.Lang.AppError("Unknown HPD environment", 1).
            &ENDIF               
            
            RETURN.
         END. 

      END.

      FINALLY:
         lhDBConfQuery:QUERY-CLOSE().
      END FINALLY.

   END METHOD.      
      

   METHOD PRIVATE LOGICAL mCheckConnection():
      
      DEFINE VARIABLE lcDBPath        AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcDBLogicalName AS CHARACTER NO-UNDO.
      
      mGetDBInfo(OUTPUT lcDBPath,
                 OUTPUT lcDBLogicalName).
      
      IF lcDBLogicalName = ""
      THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot find a database for the daemon", 1).      

      IF lcLastDB <> lcDBLogicalName
      THEN DO:

         IF lcLastDB <> ""
         THEN shutDown().

         IF CONNECTED(lcDBLogicalName)
         THEN DO:
            DISCONNECT VALUE(lcDBLogicalName).
            IF LOG-MANAGER:LOGFILE-NAME <> ?
            THEN LOG-MANAGER:WRITE-MESSAGE("Disconnected database " + lcDBLogicalName, "INFO").
         END.

         &IF "{&HPD_ENVIRONMENT}" = "Salt"
         &THEN
         CONNECT VALUE(lcDBPath + " -ld " + lcDBLogicalName) NO-ERROR.
         &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
         &THEN
         /* In test and stage environment we cannot use DBConfig values
            as the databases are located differently. This code
            allows to connect to the test databases */
         IF NOT HPDCommon:mDBExist(lcDBPath + "/" + lcDBLogicalName)
         THEN DO:
            IF llCurrent
            THEN DO:
               CASE lcInstance:

                  WHEN "MobCDR"
                  THEN ASSIGN
                          lcDBPath        = "/db1/mcdr"
                          lcDBLogicalName = "mcdr"
                          .
                  WHEN "PrepCDR"
                  THEN ASSIGN
                          lcDBPath        = "/db1/prepcdr"
                          lcDBLogicalName = "prepcdr"
                          .
                  WHEN "PrepEDR"
                  THEN ASSIGN
                          lcDBPath        = "/db1/prepedr"
                          lcDBLogicalName = "prepedr"
                          .
                  OTHERWISE RETURN ERROR NEW Progress.Lang.AppError("Instance " + lcInstance + " is not supported", 1).

               END CASE.

               IF NOT HPDCommon:mDBExist(lcDBPath + "/" + lcDBLogicalName)
               THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot find " + lcDBLogicalName + " database located in " + lcDBPath, 1).
            END.
            ELSE RETURN ERROR NEW Progress.Lang.AppError("Cannot find " + lcDBLogicalName + " database located in " + lcDBPath, 1).
         END.

         CONNECT VALUE(lcDBPath + "/" + lcDBLogicalName) NO-ERROR.
         &ELSE
         RETURN ERROR NEW Progress.Lang.AppError("Unknown HPD environment", 1).
         &ENDIF

         IF ERROR-STATUS:ERROR = TRUE
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot connect to " + lcDBLogicalName + " database located in " + lcDBPath, 1).

         IF LOG-MANAGER:LOGFILE-NAME <> ?
         THEN LOG-MANAGER:WRITE-MESSAGE("Connected database " + lcDBLogicalName, "INFO").

         lcLastDB = lcDBLogicalName.

         objRepLogHandler = NEW HPD.RepLogHandler(lcDBLogicalName, lcInstance).

         RETURN TRUE.
      END.

      RETURN FALSE.

   END METHOD.            


   METHOD OVERRIDE PROTECTED VOID mReload():

      IF NOT mCheckConnection()
      THEN objRepLogHandler:mReload().

      ldaLastCheckingDate = TODAY.

   END METHOD.


   METHOD OVERRIDE PUBLIC VOID setUp ():

      lcInstance = REPLACE(lDaemonName, THIS-OBJECT:GetClass():TypeName, "").

      mCheckTableAndPeriod().
      
      mSetDBConfigQuery().

      mCheckConnection().
      
   END METHOD.


   METHOD OVERRIDE PUBLIC VOID shutDown ():
      
      SUPER:shutDown().

      IF CONNECTED(lcLastDB)
      THEN DO:
         DISCONNECT VALUE(lcLastDB).
         IF LOG-MANAGER:LOGFILE-NAME <> ?
         THEN LOG-MANAGER:WRITE-MESSAGE("Disconnected database " + lcLastDB, "INFO").
      END.
      
   END METHOD.
   
   
   DESTRUCTOR PUBLIC HPDOnlineConn( ):

      shutDown().

      IF VALID-HANDLE(lhDBConfQuery)
      THEN DO:
         lhDBConfQuery:QUERY-CLOSE().
         DELETE OBJECT lhDBConfQuery.
      END.

      IF VALID-HANDLE(lhDBConfig)
      THEN DELETE OBJECT lhDBConfig.

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon stopped", "INFO").

   END DESTRUCTOR.

END CLASS.
