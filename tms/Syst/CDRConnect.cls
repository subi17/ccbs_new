 
 /*------------------------------------------------------------------------
    File        : CDRConnect
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Fri Aug 12 15:15:47 EEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS Syst.CDRConnect:
   
   DEFINE PRIVATE VARIABLE lcCDRTableName         AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE ldaLastFromDate        AS DATE      NO-UNDO.
   DEFINE PRIVATE VARIABLE ldaLastToDate          AS DATE      NO-UNDO.

   DEFINE PUBLIC VARIABLE  lhCurrentQueryHandle      AS HANDLE EXTENT NO-UNDO.
   DEFINE PUBLIC VARIABLE  liCurrentQueryHandleCount AS INTEGER       NO-UNDO.

   DEFINE PRIVATE TEMP-TABLE ttDBConfig NO-UNDO
      FIELD DBConnected       AS LOGICAL INITIAL FALSE
      FIELD DBLogicalName     AS CHARACTER /* DBConnName */
      FIELD DBPathName        AS CHARACTER /* DirectConnect */
      FIELD DisconnectAllowed AS LOGICAL INITIAL TRUE
      FIELD FromDate          AS DATE
      FIELD ToDate            AS DATE
      FIELD QueryHandle       AS HANDLE
      INDEX ToDate IS PRIMARY UNIQUE ToDate DESC
      .

   CONSTRUCTOR CDRConnect
      (icCDRTableName AS CHARACTER):
      
      DEFINE VARIABLE lii      AS INTEGER NO-UNDO.
      DEFINE VARIABLE lhBuffer AS HANDLE  NO-UNDO.
      DEFINE VARIABLE lhQuery  AS HANDLE  NO-UNDO.
         
      lcCDRTableName = icCDRTableName.

      IF lcCDRTableName = "ErrorCDR" OR lcCDRTableName = "ErrorDtl"
      THEN RETURN ERROR NEW Progress.Lang.AppError("ErrorCDR and ErrorDtl tables are using roamcdr database. Please initialize object using RoamCDR parameter", 1).
      
      FOR EACH DBConfig NO-LOCK WHERE
         DBConfig.Brand     = "1"            AND
         DBConfig.TableName = lcCDRTableName AND
         DBConfig.DBState   < 2:

         IF mDBExist(DBConfig.DirectConnect + "/" + DBConfig.DBConnName)
         THEN DO:
            CREATE ttDBConfig.
                     
            ASSIGN
               lii                      = lii + 1                      
               ttDBConfig.DBLogicalName = DBConfig.DBConnName
               ttDBConfig.DBPathName    = DBConfig.DirectConnect
               ttDBConfig.FromDate      = DBConfig.FromDate
               ttDBConfig.ToDate        = DBConfig.ToDate
               .
            /* There can be an existing connection which we should use */
            IF DBConfig.DBState = 0 AND CONNECTED(DBConfig.LogicalName)
            THEN DO:
               CREATE BUFFER lhBuffer FOR TABLE DBConfig.LogicalName + "." + lcCDRTableName.
               CREATE QUERY lhQuery.
               lhQuery:PRIVATE-DATA = DBConfig.LogicalName.
               lhQuery:SET-BUFFERS(lhBuffer).              
               
               ASSIGN
                  ttDBConfig.QueryHandle       = lhQuery
                  ttDBConfig.DBConnected       = TRUE
                  ttDBConfig.DisconnectAllowed = FALSE
                  .
            END.
         END.
      END.
      
      IF lii = 0
      THEN RETURN ERROR NEW Progress.Lang.AppError("No CDR database configuration available for CDR table " + lcCDRTableName, 1).
      
      EXTENT(lhCurrentQueryHandle) = lii.
      
   END CONSTRUCTOR.
   
   
   DESTRUCTOR CDRConnect():

      EXTENT(lhCurrentQueryHandle)  = ?.
      
      DEFINE VARIABLE lcBufferHandles AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lii AS INTEGER NO-UNDO.

      FOR EACH ttDBConfig:
         
         IF VALID-HANDLE(ttDBConfig.QueryHandle)
         THEN DO:
            
            ttDBConfig.QueryHandle:QUERY-CLOSE().
            
            lcBufferHandles = "".
            DO lii = 1 TO ttDBConfig.QueryHandle:NUM-BUFFERS:
               lcBufferHandles = lcBufferHandles + 
                                 ( IF lcBufferHandles = "" THEN "" ELSE ",") +
                                 STRING(ttDBConfig.QueryHandle:GET-BUFFER-HANDLE(lii)).
            END.
            
             DO lii = 1 TO ttDBConfig.QueryHandle:NUM-BUFFERS:
                 DELETE OBJECT HANDLE(ENTRY(lii,lcBufferHandles)).
             END.
             
             DELETE OBJECT ttDBConfig.QueryHandle.            
         
         END.

         IF ttDBConfig.DisconnectAllowed
         THEN mDisconnectDB(ttDBConfig.DBLogicalName).
         
      END.

      EMPTY TEMP-TABLE ttDBConfig.

   END DESTRUCTOR.


   METHOD PRIVATE VOID mDisconnectDB
      (icLogicalName AS CHARACTER):
      
      IF CONNECTED(icLogicalName)
      THEN DISCONNECT VALUE(icLogicalName).

   END METHOD.


   METHOD PRIVATE LOGICAL mDBExist
      (icDB AS CHARACTER):

      FILE-INFO:FILE-NAME = icDB + ".db".

      IF FILE-INFO:FILE-TYPE BEGINS "F"
      THEN RETURN TRUE.

      RETURN FALSE.

   END METHOD.


   METHOD PRIVATE HANDLE mConnectDBAndGiveQueryHandle
      ( icDBPath        AS CHARACTER,
        icDBName        AS CHARACTER ):

      DEFINE VARIABLE lhBuffer AS HANDLE NO-UNDO.
      DEFINE VARIABLE lhQuery  AS HANDLE NO-UNDO.

      IF NOT CONNECTED(icDBName)
      THEN DO:
         CONNECT VALUE(icDBPath + "/" + icDBName) NO-ERROR.
         IF ERROR-STATUS:ERROR = TRUE
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot connect to database " + icDBName + " located on directory " + icDBPath, 1).
      END.
      
      CREATE BUFFER lhBuffer FOR TABLE icDBName + "." + lcCDRTableName.
      CREATE QUERY lhQuery.
      lhQuery:PRIVATE-DATA = icDBName.
      lhQuery:SET-BUFFERS(lhBuffer).              

      RETURN lhQuery.

   END METHOD.


   METHOD PUBLIC VOID mSetQueryHandlesConnectIfNeeded
      ( idaFromDate AS DATE,
        idaToDate   AS DATE ):

      IF idaFromDate = ldaLastFromDate AND
         idaToDate   = ldaLastToDate
      THEN RETURN.
      
      DEFINE VARIABLE lii AS INTEGER NO-UNDO.
      
      FOR EACH ttDBConfig WHERE
         ttDBConfig.ToDate >= idaFromDate:
      
         IF ttDBConfig.FromDate > idaToDate
         THEN NEXT.

         IF NOT ttDBConfig.DBConnected
         THEN DO:
            ASSIGN
               ttDBConfig.QueryHandle = mConnectDBAndGiveQueryHandle(ttDBConfig.DBPathName,
                                                                     ttDBConfig.DBLogicalName)
               ttDBConfig.DBConnected  = TRUE
               .
            
            IF NOT VALID-HANDLE(ttDBConfig.QueryHandle)
            THEN RETURN ERROR NEW Progress.Lang.AppError("Invalid buffer handle for database " +
                                                         ttDBConfig.DBPathName + "/" + ttDBConfig.DBLogicalName +
                                                         " and tablename " + lcCDRTableName, 1).
         END.
         
         ASSIGN
            lii = lii + 1
            lhCurrentQueryHandle[lii] = ttDBConfig.QueryHandle
            .

      END.

      ASSIGN
         liCurrentQueryHandleCount  = lii
         ldaLastFromDate            = idaFromDate
         ldaLastToDate              = idaToDate
         .

   END METHOD.

END CLASS.