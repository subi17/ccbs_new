/* direct_dbconnect.i     03.01.11/aam 

   make a direct (shared memory) connection to dbs according to 
   configuration in DbConfig
*/

DEF TEMP-TABLE ttDB NO-UNDO
   FIELD ConnName    AS CHAR
   FIELD TableName   AS CHAR 
   FIELD LogicalName AS CHAR
   FIELD DBConfigID  AS INT
   FIELD DBConnected AS LOG
   INDEX ConnName ConnName.


FUNCTION fInitializeConnectTables RETURNS LOGIC
   (icTableNameList AS CHAR,
    icConnName AS CHAR):
   
   DEF VAR liCount AS INT  NO-UNDO.
   
   FOR EACH ttDB WHERE ttDB.ConnName = icConnName:
      DELETE ttDB.
   END.
      
   DO liCount = 1 TO NUM-ENTRIES(icTableNameList):
      CREATE ttDB.
      ASSIGN 
         ttDB.ConnName    = icConnName
         ttDB.TableName   = ENTRY(liCount,icTableNameList)
         ttDB.DBConnected = FALSE.
   END.
END.

FUNCTION fGetCurrentDB RETURNS INT
   (icBrand AS CHAR,
    icTableName AS CHAR,
    OUTPUT odaValidFrom AS DATE,
    OUTPUT odaValidTo AS DATE):
   
   DEF VAR liCurrentDB AS INT  NO-UNDO.
   
   FOR FIRST DBConfig NO-LOCK WHERE
             DBConfig.Brand = icBrand AND
             DBConfig.TableName = icTableName AND
             DBConfig.DBState = 0:
      ASSIGN 
         liCurrentDB = DBConfig.DBConfigID
         odaValidFrom = DBConfig.FromDate
         odaValidTo  = DBConfig.ToDate.
   END.   

   RETURN liCurrentDB.
   
END FUNCTION.

PROCEDURE pGetDBPeriods:
                                            
   DEF INPUT  PARAMETER icBrand       AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icTableName   AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER idaFromDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaToDate     AS DATE NO-UNDO.
   DEF OUTPUT PARAMETER oiPeriod1From AS DATE NO-UNDO.
   DEF OUTPUT PARAMETER oiPeriod1To   AS DATE NO-UNDO.
   DEF OUTPUT PARAMETER oiPeriod2From AS DATE NO-UNDO.
   DEF OUTPUT PARAMETER oiPeriod2To   AS DATE NO-UNDO.

   DEF VAR liState AS INT  NO-UNDO.
   
   ASSIGN
      oiPeriod1From = idaFromDate
      oiPeriod1To   = idaToDate
      oiPeriod2From = ?
      oiPeriod2To   = ?.
      
   DO liState = 0 TO 1:

      FOR FIRST DBConfig NO-LOCK WHERE
                DBConfig.Brand = icBrand AND
                DBConfig.TableName = icTableName AND
                DBConfig.DBState = liState AND
                DBConfig.ToDate >= idaFromDate AND
                DBConfig.FromDate <= idaToDate AND
                DBConfig.DirectConnect > "":

         IF DBConfig.FromDate > idaFromDate THEN ASSIGN 
            oiPeriod1To   = DBConfig.FromDate - 1  
            oiPeriod2From = DBConfig.FromDate
            oiPeriod2To   = idaToDate.
         ELSE IF DBConfig.ToDate < idaToDate THEN ASSIGN 
            oiPeriod1To   = DBConfig.ToDate  
            oiPeriod2From = DBConfig.ToDate + 1
            oiPeriod2To   = idaToDate.
      END.
      
      IF oiPeriod2To NE ? THEN LEAVE.
   END.   
 
END PROCEDURE.

PROCEDURE pDirectConnect2Dbs:

   DEF INPUT PARAMETER icBrand     AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icConnName  AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT PARAMETER idaToDate   AS DATE NO-UNDO.
   
   DEF VAR liState     AS INT  NO-UNDO.
   DEF VAR lcLogicName AS CHAR NO-UNDO.
   

   FOR EACH ttDB WHERE
            ttDB.ConnName = icConnName:

      DO liState = 0 TO 1:
      
         /* connect db that has been active on desired dates */
         FOR FIRST DBConfig NO-LOCK WHERE
                   DBConfig.Brand = icBrand AND
                   DBConfig.TableName = ttDB.TableName AND
                   DBConfig.DBState = liState AND
                   DBConfig.ToDate >= idaFromDate AND
                   DBConfig.FromDate <= idaToDate AND
                   DBConfig.DirectConnect > "":
        
            /* no changes */
            IF DBConfig.DBConfigID = ttDB.DBConfigID AND ttDB.DBConnected THEN 
               LEAVE.
            
            ASSIGN
               ttDB.DBConnected = FALSE
               lcLogicName      = (IF ttDB.ConnName > ""
                                   THEN ttDB.ConnName
                                   ELSE "") + DBConfig.LogicalName.
         
            IF CONNECTED (lcLogicName) THEN 
               DISCONNECT VALUE(lcLogicName) NO-ERROR.
      
            CONNECT VALUE(DBConfig.DirectConnect + "/" +
                          DBConfig.DBConnName + 
                          " -ld " + lcLogicName) NO-ERROR.
    
            IF ERROR-STATUS:ERROR THEN LEAVE.
         
            ASSIGN 
               ttDB.DBConfigID  = DBConfig.DBConfigID
               ttDB.LogicalName = lcLogicName
               ttDB.DBConnected = TRUE.
          END.
          
          IF ttDB.DBConnected THEN LEAVE. 
      END.
            
      IF NOT ttDB.DBConnected THEN 
         RETURN "ERROR:Connection to " +
                 (IF ttDB.LogicalName > "" 
                  THEN ttDB.LogicalName 
                  ELSE ttDB.TableName) +
                 " failed".
   END.

   RETURN "".
   
END PROCEDURE.

PROCEDURE pDirectDisconnect:

   DEF INPUT PARAMETER icConnName AS CHAR NO-UNDO.
   
   FOR EACH ttDB WHERE
            ttDB.ConnName = icConnName:

      IF CONNECTED (ttDB.LogicalName) THEN 
         DISCONNECT VALUE(ttDB.LogicalName) NO-ERROR.
   END.

   RETURN "".
   
END PROCEDURE.

PROCEDURE pGetCurrentDbtt:

   DEF INPUT PARAMETER icBrand     AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icConnName  AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT PARAMETER idaToDate   AS DATE NO-UNDO.
   
   DEF VAR liState     AS INT  NO-UNDO.
   DEF VAR lcLogicName AS CHAR NO-UNDO.
   

   FOR EACH ttDB WHERE
            ttDB.ConnName = icConnName:

      DO liState = 0 TO 1:
      
         FOR FIRST DBConfig NO-LOCK WHERE
                   DBConfig.Brand = icBrand AND
                   DBConfig.TableName = ttDB.TableName AND
                   DBConfig.DBState = liState AND
                   DBConfig.ToDate >= idaFromDate AND
                   DBConfig.FromDate <= idaToDate AND
                   DBConfig.DirectConnect > "":
        
            ASSIGN
               ttDB.DBConnected = FALSE
               lcLogicName      = (IF ttDB.ConnName > ""
                                   THEN ttDB.ConnName
                                   ELSE "") + DBConfig.LogicalName.
         
            IF CONNECTED (lcLogicName) THEN
              ASSIGN ttDB.DBConfigID  = DBConfig.DBConfigID
                     ttDB.LogicalName = lcLogicName
                     ttDB.DBConnected = TRUE.
          END.
          
          IF ttDB.DBConnected THEN LEAVE. 
      END.

      IF NOT ttDB.DBConnected THEN 
         RETURN "ERROR:Connection to " +
                 (IF ttDB.LogicalName > "" 
                  THEN ttDB.LogicalName 
                  ELSE ttDB.TableName) +
                 " failed".         
   END.

   RETURN "".
   
END PROCEDURE.

