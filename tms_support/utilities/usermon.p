DEFINE TEMP-TABLE ttUserIDs NO-UNDO
   FIELD tcDB      AS CHARACTER
   FIELD tiPID     AS INTEGER
   FIELD tiUserNum AS INTEGER
   FIELD tcName    AS CHARACTER
   FIELD tcLogin   AS CHARACTER
   FIELD tcDevice  AS CHARACTER
   INDEX idxDB AS PRIMARY
      tcDB.

DEFINE TEMP-TABLE ttTableStat NO-UNDO
   FIELD tcDB      AS CHARACTER
   FIELD titable   AS INTEGER
   FIELD tctable   AS CHARACTER
   FIELD tiRead    AS INTEGER
   FIELD tiUpdate  AS INTEGER
   FIELD tiDelete  AS INTEGER
   FIELD tiCreate  AS INTEGER
   INDEX idxDB AS PRIMARY
      tcDB.

DEFINE STREAM sReport.

PROCEDURE pFindUserIDs:

   DEFINE INPUT PARAMETER iiPID  AS INTEGER   NO-UNDO.
   
   DEFINE VARIABLE liLoop1       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liLoop2       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcDB          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lhTable       AS HANDLE    NO-UNDO.    
   DEFINE VARIABLE lhField       AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE lhQuery       AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE lcQuery       AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcField       AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liPID         AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE liUserNum     AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE lcUserName    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcLoginTime   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcDevice      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liiPID        AS INTEGER   NO-UNDO.

   EMPTY TEMP-TABLE ttUserIDs.
   EMPTY TEMP-TABLE ttTableStat.
   liiPID = iiPID.
   IF ( iiPID = 0 ) THEN
   DO:
      FIND FIRST _MyConnection NO-LOCK NO-ERROR.
      IF NOT AVAILABLE _MyConnection THEN LEAVE.
      liiPID = _MyConnection._MyConn-PID.
   END.

   DO liLoop1 = 1 to NUM-DBS:

      lcDB = LDBNAME(liLoop1).
      CREATE QUERY lhQuery.
      CREATE BUFFER lhTable FOR TABLE lcDB + "._Connect".
      lcQuery = "FOR EACH " + lcDB + "._Connect NO-LOCK WHERE " +
                "_Connect._Connect-Pid = " + STRING(liiPID).

      lhQuery:SET-BUFFERS(lhTable).
      lhQuery:QUERY-PREPARE(lcQuery).
      lhQuery:QUERY-OPEN.
      
      REPEAT:
      
         lhQuery:GET-NEXT(NO-LOCK).
      
         IF lhTable:AVAILABLE THEN DO liLoop2 = 1 TO lhTable:NUM-FIELDS:

            lhField = lhTable:BUFFER-FIELD(liLoop2).
            CASE lhField:NAME:
               WHEN "_Connect-Pid"    THEN
                  liPID       = lhField:BUFFER-VALUE.
               WHEN "_Connect-Usr"  THEN
                  liUserNum   = lhField:BUFFER-VALUE.
               WHEN "_Connect-Name" THEN
                  lcUserName  = lhField:BUFFER-VALUE.
               WHEN "_Connect-Time" THEN
                  lcLoginTime = lhField:BUFFER-VALUE.
               WHEN "_Connect-Device" THEN
                  lcDevice    = lhField:BUFFER-VALUE.
            END.
         
         END.
    
         IF NOT lhTable:AVAILABLE THEN LEAVE.
         CREATE ttUserIDs.
         ASSIGN 
            ttUserIDs.tcDB      = lcDB
            ttUserIDs.tiPID     = liPID
            ttUserIDs.tiUserNum = liUserNum
            ttUserIDs.tcName    = lcUserName
            ttUserIDs.tcLogin   = lcLoginTime
            ttUserIDs.tcDevice  = lcDevice.
         RELEASE ttUserIDs.
           
      END.
      
      IF VALID-HANDLE(lhTable)  THEN DELETE OBJECT lhTable.
      IF VALID-HANDLE(lhField)  THEN DELETE OBJECT lhField.
      IF VALID-HANDLE(lhQuery)  THEN DELETE OBJECT lhQuery.
   
   END.
   
END PROCEDURE.


PROCEDURE pFindUserStats:

   DEFINE VARIABLE liLoop1       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liLoop2       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcDB          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lhTable       AS HANDLE    NO-UNDO.    
   DEFINE VARIABLE lhField       AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE lhQuery       AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE lcQuery       AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcField       AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lhFileTable   AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhFileField   AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhFileQuery   AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lcFileQuery   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcFileField   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE litable       AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE lctable       AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liRead        AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE liUpdate      AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE liDelete      AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE liCreate      AS INTEGER   NO-UNDO. 
    
   EMPTY TEMP-TABLE ttTableStat.
   DO liLoop1 = 1 to NUM-DBS:

      lcDB = LDBNAME(liLoop1).
      FIND FIRST ttUserIDs  WHERE ttUserIDs.tcDB = lcDB NO-LOCK NO-ERROR.
      IF NOT AVAIL ttUserIDs THEN NEXT.
      CREATE QUERY lhQuery.
      CREATE BUFFER lhTable FOR TABLE lcDB + "._UserTableStat".

      lcQuery = "FOR EACH " + lcDB + "._UserTableStat NO-LOCK WHERE" + 
                " _UserTableStat._UserTableStat-Conn = " + 
                 STRING(ttUserIDs.tiUserNum).
      lhQuery:SET-BUFFERS(lhTable).
      lhQuery:QUERY-PREPARE(lcQuery).
      lhQuery:QUERY-OPEN.
      
      REPEAT:
      
         lhQuery:GET-NEXT(NO-LOCK).
      
         IF lhTable:AVAILABLE THEN DO liLoop2 = 1 TO lhTable:NUM-FIELDS:

            lhField = lhTable:BUFFER-FIELD(liLoop2).

            CASE lhField:NAME:
               WHEN "_UserTableStat-Num"    THEN
                  litable   = lhField:BUFFER-VALUE.
               WHEN "_UserTableStat-read"  THEN
                  liRead    = lhField:BUFFER-VALUE.
               WHEN "_UserTableStat-update"  THEN
                  liUpdate  = lhField:BUFFER-VALUE.
               WHEN "_UserTableStat-create"  THEN
                  liCreate  = lhField:BUFFER-VALUE.
               WHEN "_UserTableStat-delete"  THEN
                  liDelete  = lhField:BUFFER-VALUE.
            END.
         
         END.
    
         IF NOT lhTable:AVAILABLE THEN LEAVE.
         IF 
           liRead > 0 OR liUpdate > 0 OR liCreate > 0 OR
           liDelete > 0 THEN 
           DO:
              CREATE QUERY lhFileQuery.
              CREATE BUFFER lhFileTable FOR TABLE lcDB + "._File".

              lcFileQuery = "FOR EACH " + lcDB + "._File NO-LOCK WHERE" + 
                                " _File._File-Number = " + 
                                 STRING(liTable).
              lhFileQuery:SET-BUFFERS(lhFileTable).
              lhFileQuery:QUERY-PREPARE(lcFileQuery).
              lhFileQuery:QUERY-OPEN.
              
              REPEAT:
              
                 lhFileQuery:GET-NEXT(NO-LOCK).
              
                 IF lhFileTable:AVAILABLE THEN 
                   DO liLoop2 = 1 TO lhFileTable:NUM-FIELDS:
                      lhFileField = lhFileTable:BUFFER-FIELD(liLoop2).

                        CASE lhFileField:NAME:
                          WHEN "_File-Name"    THEN
                              lcTable   = lhFileField:BUFFER-VALUE.
                          END.
                 
                   END.
    
                 IF NOT lhFileTable:AVAILABLE THEN LEAVE.
               END.

              IF VALID-HANDLE(lhFileTable)  THEN DELETE OBJECT lhFileTable.
              IF VALID-HANDLE(lhFileField)  THEN DELETE OBJECT lhFileField.
              IF VALID-HANDLE(lhFileQuery)  THEN DELETE OBJECT lhFileQuery.

              CREATE ttTableStat.
              ASSIGN 
                 ttTableStat.tcDB = lcDB
                 ttTableStat.titable  = litable
                 ttTableStat.tctable  = lctable
                 ttTableStat.tiRead   = liRead 
                 ttTableStat.tiUpdate = liUpdate
                 ttTableStat.tiDelete = liDelete
                 ttTableStat.tiCreate = liCreate.
             RELEASE ttTableStat.
           END.
           
      END.
      
      IF VALID-HANDLE(lhTable)  THEN DELETE OBJECT lhTable.
      IF VALID-HANDLE(lhField)  THEN DELETE OBJECT lhField.
      IF VALID-HANDLE(lhQuery)  THEN DELETE OBJECT lhQuery.
   
   END.
   
END PROCEDURE.

PROCEDURE pPrintStatsToFile:
   DEFINE INPUT PARAMETER lcOutputDir AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSpoolPath   AS CHARACTER NO-UNDO.
   
   IF lcOutputDir NE "" THEN 
     lcSpoolPath = lcOutputDir.
    ELSE
     lcSpoolPath = "/tmp".
     
   FIND FIRST ttUserIDs NO-LOCK NO-ERROR.
   IF NOT AVAIL ttUserIDs THEN LEAVE.
   
   OUTPUT STREAM sReport TO VALUE (lcSpoolPath + 
                                   "/usertablestats_" + 
                                   STRING(ttUserIDs.tiPID)).
   PUT STREAM sReport UNFORMATTED
       "User: "  
       ttUserIDs.tcName                   AT 7
       "Device: " + ttUserIDs.tcDevice    AT 30
       " (PID: " STRING(ttUserIDs.tiPID,">>>>9") + ")  ".
   PUT STREAM sReport UNFORMATTED   
       "Login time :"                     AT 1
       ttUserIDs.tcLogin                  AT 15
       "Report time:"                     AT 1
       NOW                                AT 15
       CHR(10).
       
   PUT STREAM sReport UNFORMATTED
       "DATABASE"    AT 1
       "TBL#"        AT 12
       "TABLE NAME"  AT 19
       "READ"        AT 36
       "CREATE"      AT 47
       "UPDATE"      AT 58
       "DELETE"      AT 69.
       
   FOR EACH ttTableStat NO-LOCK.
     PUT STREAM sReport UNFORMATTED
              ttTableStat.tcDB       AT 1
              ttTableStat.tiTable    AT 12
              ttTableStat.tcTable    AT 19
              ttTableStat.tiRead     AT 36         
              ttTableStat.tiCreate   AT 47
              ttTableStat.tiUpdate   AT 58
              ttTableStat.tiDelete   AT 69.
   END.
    PUT STREAM sReport UNFORMATTED 
       SKIP.      
   OUTPUT STREAM sReport CLOSE.
END PROCEDURE.


for each _connect where _connect-pid <> ? and _connect-type = "SELF" no-lock.
run pFindUserIDs(_connect._connect-pid).
run pFindUserStats.
run pPrintStatsToFile("/home/anttis/tablestat").
end.
