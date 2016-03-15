/*------------------------------------------------------------------------
    File        : DumpTimeBasedConnect
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 20:43:52 EET 2014
    Notes       : Add database connection routines
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING HPD.DumpTimeBased.
USING HPD.DumpHandler.
USING HPD.HPDTime.

{HPD/HPDEnvironment.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DumpTimeBasedConnect INHERITS DumpTimeBased:
   
   DEFINE PRIVATE VARIABLE ldaConnectFromDate AS DATE      NO-UNDO.
   DEFINE PRIVATE VARIABLE ldaConnectToDate   AS DATE      NO-UNDO.
   DEFINE PRIVATE VARIABLE lcDBLogicalName    AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE lhDBConfig         AS HANDLE    NO-UNDO.
   DEFINE PRIVATE VARIABLE lhDBConfQuery      AS HANDLE    NO-UNDO.


   CONSTRUCTOR PUBLIC DumpTimeBasedConnect
      ( iiDumpID AS INTEGER,
        iobjHandler AS CLASS DumpHandler ):
      THIS-OBJECT(iiDumpID, "", iobjHandler).   
   END CONSTRUCTOR.
   

   CONSTRUCTOR PUBLIC DumpTimeBasedConnect
      ( iiDumpID AS INTEGER,
        icFile   AS CHARACTER,
        iobjHandler AS CLASS DumpHandler ):
           
      SUPER(iiDumpID, icFile, iobjHandler).
     
      ASSIGN
         ldaConnectFromDate = HPDTime:mMake2Date(ldeFromTS)
         ldaConnectToDate   = HPDTime:mMake2Date(ldeToTS)
         .

      mSetDBConfigQuery().
        
   END CONSTRUCTOR.


   DESTRUCTOR PUBLIC DumpTimeBasedConnect( ):

      IF CONNECTED(lcDBLogicalName)
      THEN DISCONNECT VALUE(lcDBLogicalName).

      IF VALID-HANDLE(lhDBConfQuery)
      THEN DO:
         lhDBConfQuery:QUERY-CLOSE().
         DELETE OBJECT lhDBConfQuery.
      END.

      IF VALID-HANDLE(lhDBConfig)
      THEN DELETE OBJECT lhDBConfig.

   END DESTRUCTOR.


   METHOD PRIVATE VOID mSetDBConfigQuery():

      DEFINE VARIABLE lcDBConfigTable AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcQuery         AS CHARACTER NO-UNDO.
      
      &IF "{&HPD_ENVIRONMENT}" = "Salt"
      &THEN
      ASSIGN
         lcDBConfigTable = "DBConf"
         lcQuery = "FOR EACH &1 NO-LOCK WHERE " +
                   "&1.TableName = &2  AND " +
                   "&1.State     = 1 " +
                   "BY &1.ToDate"
         .
      &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN
      ASSIGN
         lcDBConfigTable = "DBConfig"
         lcQuery = "FOR EACH &1 NO-LOCK WHERE " +
                   "&1.Brand     = '1' AND " +
                   "&1.TableName = &2  AND " +
                   "&1.DBState   < 2 " +
                   "BY &1.ToDate"
         .
      &ELSE
      RETURN ERROR NEW Progress.Lang.AppError("Unknown HPD environment", 1).
      &ENDIF

      CREATE BUFFER lhDBConfig FOR TABLE lcDBConfigTable.
      CREATE QUERY lhDBConfQuery.
      lhDBConfQuery:SET-BUFFERS(lhDBConfig).
      lhDBConfQuery:QUERY-PREPARE(SUBSTITUTE(lcQuery, lcDBConfigTable, QUOTER(lbDumpFile.MainTable))).
      lhDBConfQuery:QUERY-OPEN().
      
   END METHOD.


   METHOD PUBLIC LOGICAL mConnectMore():

      DEFINE VARIABLE llOK     AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lcDBPath AS CHARACTER NO-UNDO.

      DO WHILE TRUE:

         llOK = lhDBConfQuery:GET-NEXT(NO-LOCK).

         IF llOK = ?
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process dbconfig query", 1).

         /* No more records, or query is not open */
         IF NOT llOK
         THEN RETURN FALSE.

         IF lhDBConfig::FromDate > ldaConnectToDate + 1
         THEN NEXT.
   
         IF lhDBConfig::ToDate <  ldaConnectFromDate - 1
         THEN NEXT.

         &IF "{&HPD_ENVIRONMENT}" = "Salt"
         &THEN
         ASSIGN
            lcDBPath        = lhDBConfig::Path
            lcDBLogicalName = lhDBConfig::Name
            .
         &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
         &THEN
         ASSIGN
            lcDBPath        = lhDBConfig::DirectConnect
            lcDBLogicalName = lhDBConfig::DBConnName
            .
         &ELSE
         RETURN ERROR NEW Progress.Lang.AppError("Unknown HPD environment", 1).
         &ENDIF

         IF CONNECTED(lcDBLogicalName)
         THEN DISCONNECT VALUE(lcDBLogicalName).

         &IF "{&HPD_ENVIRONMENT}" = "Salt"
         &THEN
         CONNECT VALUE(lcDBPath + " -ld " + lcDBLogicalName) NO-ERROR.
         &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
         &THEN
         CONNECT VALUE(lcDBPath + "/" + lcDBLogicalName) NO-ERROR.
         &ELSE
         RETURN ERROR NEW Progress.Lang.AppError("Unknown HPD environment", 1).
         &ENDIF
         IF ERROR-STATUS:ERROR = TRUE
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot connect to " + lcDBLogicalName + " database located in " + lcDBPath, 1).

         RETURN TRUE.

      END.

   END METHOD.

END CLASS.