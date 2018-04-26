/* ----------------------------------------------------------------------
  MODULE .......: dumpfile_run.p
  TASK .........: Create a dump file from a table
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 21.10.08
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/transname.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Syst/eventlog.i}
{Syst/dumpfile_run.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO. /* full/modified */
DEF INPUT  PARAMETER icRunNameTag  AS CHAR NO-UNDO.
DEF INPUT  PARAMETER ilReplication AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER oiEventCount  AS INT  NO-UNDO.


DEF VAR liCnt         AS INT    NO-UNDO.
DEF VAR lcField       AS CHAR   NO-UNDO.
DEF VAR lhDefTemp     AS HANDLE NO-UNDO.
DEF VAR lhTempFind    AS HANDLE NO-UNDO.
DEF VAR lhCollect     AS HANDLE NO-UNDO.
DEF VAR lhFind        AS HANDLE NO-UNDO.
DEF VAR lhTable       AS HANDLE NO-UNDO.
DEF VAR lhField       AS HANDLE NO-UNDO.
DEF VAR lhXML         AS HANDLE NO-UNDO.
DEF VAR lhDoc         AS HANDLE NO-UNDO.
DEF VAR lhRoot        AS HANDLE NO-UNDO.
DEF VAR lhFieldLevel  AS HANDLE NO-UNDO.
DEF VAR lhRecLevel    AS HANDLE NO-UNDO.
DEF VAR lhValue       AS HANDLE NO-UNDO.
DEF VAR llSax         AS LOG    NO-UNDO INIT TRUE.
DEF VAR liTable       AS INT    NO-UNDO.
DEF VAR lcFind        AS CHAR   NO-UNDO.
DEF VAR lcPlainFile   AS CHAR   NO-UNDO.
DEF VAR lcFile        AS CHAR   NO-UNDO.
DEF VAR lcExtension   AS CHAR   NO-UNDO.
DEF VAR lcValue       AS CHAR   NO-UNDO.
DEF VAR lcReplace     AS CHAR   NO-UNDO.
DEF VAR ldLastDump    AS DEC    NO-UNDO.
DEF VAR ldtLastDump   AS DATETIME NO-UNDO.
DEF VAR lcEventSource AS CHAR   NO-UNDO. 
DEF VAR lcEventFields AS CHAR   NO-UNDO.
DEF VAR ldStarted     AS DEC    NO-UNDO. 
DEF VAR lcSkip        AS CHAR   NO-UNDO.
DEF VAR lcFinalFile   AS CHAR   NO-UNDO.
DEF VAR lcError       AS CHAR   NO-UNDO. 
DEF VAR llInterrupted AS LOG    NO-UNDO.
DEF VAR lcLogFile     AS CHAR   NO-UNDO.
DEF VAR lcActionKey   AS CHAR   NO-UNDO.
DEF VAR lcFinished    AS CHAR   NO-UNDO.
DEF VAR lcDelimiter   AS CHAR   NO-UNDO. 
DEF VAR llTestRun     AS LOG    NO-UNDO.
DEF VAR liTestQty     AS INT    NO-UNDO.
DEF VAR lcModFields   AS CHAR   NO-UNDO.
DEF VAR liDumpLogID   AS INT    NO-UNDO. 
DEF VAR os-file       AS CHAR   NO-UNDO.
DEF VAR ldeToday      AS DEC    NO-UNDO.
DEF VAR ldeTomorrow   AS DEC    NO-UNDO. 

DEF STREAM sFile.
DEF STREAM sLog.


/* convert decimal value to use the configured decimal point */
FUNCTION fDispDecimal RETURNS CHARACTER
   (idAmt      AS DEC,
    iiDecimals AS INT):
   
   DEF VAR lcAmt AS CHAR NO-UNDO.
   
   IF lcReplace = "" THEN RETURN STRING(idAmt). 
   
   /* no decimal point at all */
   IF lcReplace = "R" THEN 
      RETURN STRING(EXP(10,iiDecimals) * idAmt).
   
   IF idAmt > -1 AND idAmt < 1 AND idAmt NE 0 THEN 
      lcAmt = TRIM(STRING(idAmt,"->>>>>9.9<<<<<")).
   ELSE lcAmt = STRING(idAmt).   
      
   RETURN REPLACE(lcAmt,lcReplace,DumpFile.DecimalPoint).
      
END FUNCTION.

FUNCTION fErrorLog RETURNS LOGIC
   (icError AS CHAR):
   
   DO TRANS:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
             ErrorLog.ActionID  = "DUMP" + STRING(iiDumpID)
             ErrorLog.TableName = IF AVAILABLE DumpFile 
                                  THEN DumpFile.DumpName
                                  ELSE STRING(iiDumpID)
             ErrorLog.KeyValue  = STRING(YEAR(TODAY),"9999") + 
                                  STRING(MONTH(TODAY),"99") + 
                                  STRING(DAY(TODAY),"99")
             ErrorLog.ErrorChar = icDumpMode
             ErrorLog.ErrorMsg  = icError
             ErrorLog.UserCode  = Syst.Var:katun.
             ErrorLog.ActionTS  = Func.Common:mMakeTS().
   END.
   
END FUNCTION.

FUNCTION fWriteLog RETURNS LOGIC
   (icAction  AS CHAR,
    icMessage AS CHAR):
   
   IF lcLogFile = "" THEN RETURN FALSE.
   
   OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.
   
   PUT STREAM sLog UNFORMATTED 
      Func.Common:mISOTimeZone(TODAY,TIME)   " "
      icAction                   " " 
      DumpFile.FileCategory      " "
      icMessage SKIP.

   OUTPUT STREAM sLog CLOSE.
   
END FUNCTION.


/****** MAIN start ******************/
      
FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF NOT AVAILABLE DumpFile THEN DO:
   lcError = "Dump configuration not available".
   fErrorLog(lcError).
   RETURN "ERROR:" + lcError.
END.

IF DumpFile.FileName = "" OR DumpFile.SpoolDir = "" OR
   DumpFile.MainTable = "" OR 
   (DumpFile.LogicModule = "" AND 
    NOT CAN-FIND(FIRST DFField OF DumpFile)) 
THEN DO:
   lcError = "Dump configuration is incomplete".
   fErrorLog(lcError).
   RETURN "ERROR:" + lcError.
END.

IF icDumpMode = "test" THEN DO:
   ASSIGN
      llTestRun  = TRUE
      icDumpMode = "Full"
      liTestQty  = fCParamI("DumpTestQty").
   IF liTestQty = ? THEN liTestQty = 500.
END.
ELSE llTestRun = FALSE.


IF NOT DumpFile.Active AND NOT llTestRun THEN DO:
   lcError = "Dump configuration is inactive".
   fErrorLog(lcError).
   RETURN "INFORMATION:" + lcError.
END.

ldeToday = Func.Common:mHMS2TS(TODAY,'').
ldeTomorrow = Func.Common:mHMS2TS(TODAY + 1,'').

lcActionKey = STRING(YEAR(TODAY),"9999") + 
              STRING(MONTH(TODAY),"99") + 
              STRING(DAY(TODAY),"99") +
              CAPS(SUBSTRING(icDumpMode,1,1)).

/* already running */
IF CAN-FIND(FIRST DumpLog WHERE
                  DumpLog.DumpId = iiDumpID AND
                  DumpLog.DumpLogStatus = 0)
THEN DO:
   lcError = "Already running".
   fErrorLog(lcError).
   RETURN "INFORMATION:" + lcError.
END.


ASSIGN
   ldStarted     = Func.Common:mMakeTS()
   llInterrupted = FALSE
   lcFinished    = "FINISH"
   lcLogFile     = DumpFile.LogFile.

IF lcLogFile = "" THEN lcLogFile = fCParamC("DumpLogFile").    
IF lcLogFile = ? THEN lcLogFile = "".

IF llTestRun AND lcLogFile NE "" THEN DO:
   liCnt = R-INDEX(lcLogFile,".").
   IF liCnt > 0 THEN 
      lcLogFile = SUBSTRING(lcLogFile,1,liCnt - 1) + "_TEST" + 
                  SUBSTRING(lcLogFile,liCnt).
   ELSE lcLogFile = lcLogFile + "_TEST".
END.
   
/* all dynamic objects to this */
CREATE WIDGET-POOL "DumpFile".

/* define temp-table and other needed */
RUN pInitialize.    

/* something went wrong */
IF RETURN-VALUE BEGINS "ERROR:" THEN DO:
   fErrorLog(ENTRY(2,RETURN-VALUE,":")).
   DELETE WIDGET-POOL "DumpFile".
   RETURN RETURN-VALUE.
END.


/* mark as started */

DO FOR DumpLog TRANS:
   CREATE DumpLog.
   ASSIGN
      DumpLog.DumpLogId     = NEXT-VALUE(DumpLog)
      DumpLog.CreateStart   = ldStarted
      DumpLog.DumpLogStatus = 0
      liDumpLogID = DumpLog.DumpLogId
      DumpLog.DumpId        = iiDumpID
      DumpLog.FileName      = lcPlainFile
      DumpLog.DumpType      = icDumpMode
      DumpLog.CreateEnd     = 0
      DumpLog.Filesize      = 0
      DumpLog.CreationDB    = (IF ilReplication THEN "Replica" ELSE "Master").
END. /* DO FOR DumpLog TRANS: */

/* notify monitoring */
fWriteLog("START",lcFile).

/* a separate module to handle data (collect and dump) */
IF DumpFile.LogicModule > "" THEN DO:
   RUN VALUE(DumpFile.LogicModule) (DumpFile.DumpID,
                                    lcFile,
                                    icDumpMode,
                                    ldLastDump,
                                    lcEventSource,
                                    lcEventFields,
                                    OUTPUT oiEventCount,
                                    OUTPUT llInterrupted).

   IF RETURN-VALUE BEGINS "ERROR:" THEN 
      lcError = RETURN-VALUE.
END.
   
/* collect and dump data here locally */   
ELSE DO:   

   /* collection for modified ones done in a separate module */ 
   IF icDumpMode = "modified" AND DumpFile.ModCollModule > "" THEN 
      RUN VALUE(DumpFile.ModCollModule) (INPUT-OUTPUT TABLE-HANDLE lhDefTemp,
                                         ldLastDump,
                                         lcEventSource,
                                         lcEventFields,
                                         lcModFields).

   /* collection for full dump done in a separate module */ 
   ELSE IF icDumpMode = "full" AND DumpFile.FullCollModule > "" THEN 
      RUN VALUE(DumpFile.FullCollModule) (INPUT-OUTPUT TABLE-HANDLE lhDefTemp,
                                          0,
                                          "",
                                          "",
                                          "").

   /* collection done here; get records from defined table and save them 
      into a temp-table */
   ELSE RUN pGetRecords.

   IF RETURN-VALUE BEGINS "ERROR:" THEN 
      lcError = RETURN-VALUE.

   ELSE IF NOT llInterrupted THEN DO:
      /* dump temp-table rows to file */
      RUN pWriteDumpFile.
      IF RETURN-VALUE BEGINS "ERROR:" THEN 
         lcError = RETURN-VALUE.
   END.
END.

DELETE WIDGET-POOL "DumpFile".

lcFinalFile = lcFile.

IF llInterrupted THEN DO:
   lcError = "ERROR:Interrupted".
END.

/* remove the file if an empty file should not be handled */
ELSE IF oiEventCount = 0 AND DumpFile.EmptyFile = FALSE THEN DO:
   OS-DELETE VALUE(lcFile).
   ASSIGN
      lcPlainFile = "Not created"
      lcFinished  = "CANCEL".
END.

/* move the file to the transfer directory */
ELSE IF DumpFile.TransDir > "" THEN DO:

   lcExtension = "".
   IF NUM-ENTRIES(lcFile,".") > 1 THEN 
      lcExtension = "." + ENTRY(NUM-ENTRIES(lcFile,"."),lcFile,".").
      
      
   lcFinalFile = fMove2TransDir(lcFile,
                                lcExtension,
                                DumpFile.TransDir).
                                
   IF lcFinalFile = "" THEN DO:
      lcError = "Move to transfer directory failed".
      fErrorLog(lcError).
   END.   
END.            

/* Save dump status data into Dumplog */
DO FOR DumpLog TRANS:
   FIND FIRST DumpLog WHERE
              DumpLog.DumpLogId   = liDumpLogID AND
              DumpLog.DumpLogStatus = 0
      EXCLUSIVE-LOCK NO-ERROR.

   IF NOT AVAILABLE DumpLog THEN DO:
      CREATE DumpLog.
      ASSIGN
         DumpLog.DumpLogId     = NEXT-VALUE(DumpLog)
         DumpLog.CreateStart   = ldStarted
         DumpLog.DumpId        = iiDumpID
         DumpLog.FileName      = lcPlainFile.
   END.

   IF oiEventCount <> 0 OR DumpFile.EmptyFile = TRUE THEN DO:
      IF DumpFile.TransDir > "" THEN
         SET os-file = DumpFile.TransDir + "/" + DumpLog.FileName.
      ELSE
         SET os-file = DumpFile.SpoolDir + "/" + DumpLog.FileName.

      FILE-INFO:FILE-NAME = os-file.
      ASSIGN
         DumpLog.Filesize      = DECIMAL(FILE-INFO:FILE-SIZE).
   END.
   DumpLog.CreateEnd     = Func.Common:mMakeTS().

   IF llInterrupted THEN ASSIGN
      DumpLog.DumpLogStatus = 5.
   ELSE DO:
      DumpLog.DumpLogStatus = 3.
      RUN Syst/dumpfile_calcavedur.p(DumpFile.DumpId).
   END.
   /* TODO - quick solution (otherwise requires a new logic module param.) */
   IF oiEventCount EQ {&DUMPLOG_ERROR_NOTIFICATION} THEN ASSIGN
      DumpLog.DumpLogStatus = 1
      oiEventCount = 0.
END. /* DO FOR DumpLog TRANS: */

/* notify monitoring */
IF llInterrupted OR lcFinalFile = "" 
THEN fWriteLog("INTERRUPT",lcFile).
ELSE fWriteLog(lcFinished,lcFinalFile).

RETURN lcError.

/******* MAIN end ****************/


PROCEDURE pInitialize:

   DEF VAR liSeq    AS INT  NO-UNDO.
    
   DEF BUFFER bDumpLog FOR DumpLog.

   /* handling of decimal point */
   CASE DumpFile.DecimalPoint:
   WHEN "." THEN lcReplace = ",".
   WHEN "," THEN lcReplace = ".".
   WHEN "remove" THEN lcReplace = "R".
   OTHERWISE lcReplace = "".
   END CASE. 

   /* delimiter */
   lcDelimiter = fInitDelimiter(DumpFile.DumpDelimiter).
   
   /* linefeed between rows */
   IF DumpFile.DumpLineFeed > "" THEN 
   DO liCnt = 1 TO NUM-ENTRIES(DumpFile.DumpLineFeed):
      lcSkip = lcSkip + CHR(INTEGER(ENTRY(liCnt,DumpFile.DumpLineFeed))).
   END.
 
   /* output file name */
   ASSIGN
      lcFile = DumpFile.FileName
      lcFile = REPLACE(lcFile,"#DATE",STRING(YEAR(TODAY),"9999") + 
                                      STRING(MONTH(TODAY),"99") + 
                                      STRING(DAY(TODAY),"99"))
      lcFile = REPLACE(lcFile,"#TIME",REPLACE(STRING(TIME,"hh:mm:ss"),":",""))
      lcFile = REPLACE(lcFile,"#YYMM",SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2) + STRING(MONTH(TODAY),"99"))
      lcFile = REPLACE(lcFile,"#MODE",icDumpMode)
      lcFile = REPLACE(lcFile,"#CAT",DumpFile.FileCategory)
      lcFile = REPLACE(lcFile,"#RUN",icRunNameTag)
      lcFile = REPLACE(lcFile,"#TENANT",
                       CAPS(multitenancy.TenantInformation:mGetEffectiveBrand())).
   
   /* sequential nbr for the same day */
   IF INDEX(lcFile,"#DSEQ") > 0 THEN DO:
      liSeq = 1.
       
      FOR EACH bDumpLog NO-LOCK WHERE
               bDumpLog.DumpId = iiDumpID AND
               bDumpLog.CreateStart > ldeToday AND
               bDumpLog.CreateStart < ldeTomorrow AND
               bDumpLog.DumpLogStatus = 3:
         liSeq = liSeq + 1.
      END.

      lcFile = REPLACE(lcFile,"#DSEQ",STRING(liSeq,"999")).
   END.

   ASSIGN   
      lcPlainFile = lcFile
      lcFile = DumpFile.SpoolDir + "/" + lcFile.
   
   IF DumpFile.LogicModule = "" THEN DO:
      /* create a temp-table, fields according to dumpfile definitions */ 
      IF CAN-FIND(FIRST DFField OF DumpFile) THEN DO:

         CREATE TEMP-TABLE lhDefTemp IN WIDGET-POOL "DumpFile".
       
         FOR EACH DFField OF DumpFile NO-LOCK:
            IF DFField.DFField BEGINS "#" THEN
               lhDefTemp:ADD-NEW-FIELD(REPLACE(DFField.DFField,"#","DF_"),
                                       "character",
                                       ?,
                                       ?,
                                       "",
                                       DFField.DFLabel).
            
            ELSE lhDefTemp:ADD-LIKE-FIELD(DFField.DFField,
                                          DFField.DFTable + "." + 
                                             DFField.DFField).
         END.

         lhDefTemp:TEMP-TABLE-PREPARE("ttCollect").
         lhCollect = lhDefTemp:DEFAULT-BUFFER-HANDLE.

         /* set labels here, so that fields to temp-table don't need to be 
            added with add-new-field */
         DO liCnt = 1 TO lhCollect:NUM-FIELDS:
            lhField = lhCollect:BUFFER-FIELD(liCnt).
            FIND FIRST DFField OF DumpFile WHERE
                       DFField.DFField = lhField:NAME NO-LOCK NO-ERROR.
            IF AVAILABLE DFField AND DFField.DFLabel > "" THEN 
               lhField:LABEL = DFField.DFLabel. 
         END.
      END.
   END.

   IF DumpFile.FileCategory = "HPD"
   THEN RETURN "".

   /* table to be dumped */
   CREATE BUFFER lhTable FOR TABLE DumpFile.MainTable 
      IN WIDGET-POOL "DumpFile".
   
   /* is this a full dump or just from modified ones */
   IF icDumpMode = "" THEN 
      icDumpMode = "full".
   ELSE IF icDumpMode = "modified" THEN DO:
   
      /* check modifications from eventlog */
      IF DumpFile.ModFromEventLog THEN ASSIGN
         lcEventSource = "EventLog"
         lcEventFields = fEventKeyFields(lhTable)
         lcModFields   = DumpFile.EventLogFields.
     
      /* and/or from named fields */
      IF DumpFile.ModFromField > "" THEN 
      DO liCnt = 1 TO NUM-ENTRIES(DumpFile.ModFromField):
         ASSIGN 
            lcEventSource = lcEventSource + 
                            (IF lcEventSource > "" THEN "|" ELSE "") + 
                            "Field"
            lcEventFields = lcEventFields + 
                            (IF lcEventFields > "" THEN "|" ELSE "") +
                            ENTRY(liCnt,DumpFile.ModFromField).
      END.
                            
      ldLastDump = 0.
   
      /* When was this dump last done; primarily from last modified dump, if
         not found then from last full dump */   
      /* DO FIRST THIS */

      FIND FIRST bDumpLog NO-LOCK WHERE
                 bDumpLog.DumpId = iiDumpID AND
                 bDumpLog.DumpLogStatus NE 5 USE-INDEX DumpId NO-ERROR.
      IF AVAIL bDumpLog THEN DO:
         IF NOT bDumpLog.CreateStart > Func.Common:mHMS2TS(TODAY - 65,'') THEN ldLastDump = 0.
         ELSE ldLastDump = bDumpLog.CreateStart.
      END.

      IF ldLastDump = 0 THEN RETURN 
         "ERROR:Log for previous dump was not found within last 2 months".
         
      ldtLastDump = Func.Common:mTimeStamp2DateTime(ldLastDump).   
   END.

   RETURN "".

END PROCEDURE.


PROCEDURE pGetRecords:

   DEF VAR llPick        AS LOG  NO-UNDO.
   DEF VAR ldEventTime   AS DEC  NO-UNDO.
   DEF VAR ldaModified   AS DATE NO-UNDO.
   DEF VAR liModTime     AS INT  NO-UNDO. 
   DEF VAR liCollect     AS INT  NO-UNDO.
   DEF VAR liPicked      AS INT  NO-UNDO.
   DEF VAR lhAddTable    AS HANDLE NO-UNDO EXTENT 10.
   DEF VAR lcTable       AS CHAR NO-UNDO.
   DEF VAR lcKeyField    AS CHAR NO-UNDO.
   DEF VAR liKeyFields   AS INT  NO-UNDO.
   DEF VAR liAddTableQty AS INT  NO-UNDO.
      
   IF NOT VALID-HANDLE(lhCollect) THEN 
      RETURN "ERROR:Collection temp-table not initialized".
   
   Func.Common:mSplitTS(ldLastDump,
            OUTPUT ldaModified,
            OUTPUT liModTime).
    
   /* create a query to get records from the table to be dumped */
   CREATE QUERY lhFind IN WIDGET-POOL "DumpFile".
   lhFind:SET-BUFFERS(lhTable).

   /* additional tables */ 
   IF DumpFile.SideTables > "" THEN 
   DO liCnt = 1 TO MIN(10,NUM-ENTRIES(DumpFile.SideTables)):
      CREATE BUFFER lhAddTable[liCnt] 
         FOR TABLE ENTRY(liCnt,DumpFile.SideTables) IN WIDGET-POOL "DumpFile".
      lhFind:ADD-BUFFER(lhAddTable[liCnt]).
      liAddTableQty = liCnt. 
   END.
      
   /* find clause predefined */
   IF DumpFile.QueryClause > "" THEN
      lcFind = DumpFile.QueryClause.
            
   /* general find */
   ELSE DO:
      /* main table */
      lcFind = "FOR EACH " + DumpFile.MainTable.
      IF DumpFile.UseIndex > "" THEN 
         lcFind = lcFind + " USE-INDEX " + DumpFile.UseIndex.
      lcFind = lcFind + " NO-LOCK".
      
      DO liCnt = 1 TO lhTable:NUM-FIELDS:
         lhField = lhTABLE:BUFFER-FIELD(liCnt).
         IF lhField:NAME = "Brand" THEN DO:
            lcFind = lcFind + " WHERE " + DumpFile.MainTable + 
                              '.Brand = "' + Syst.Var:gcBrand + '"'.
            LEAVE.
         END.
      END.

      /* additional tables */  
      IF DumpFile.SideTables > "" AND DumpFile.LinkKey > "" THEN 
      DO liCnt = 1 TO NUM-ENTRIES(DumpFile.SideTables):
         ASSIGN
            lcTable = ENTRY(liCnt,DumpFile.SideTables)
            lcFind = lcFind + "," +
                     " EACH " + lcTable + " NO-LOCK WHERE ".
                     
         DO liKeyFields = 1 TO NUM-ENTRIES(DumpFile.LinkKey):
            ASSIGN 
               lcKeyField = ENTRY(liKeyFields,DumpFile.LinkKey)
               lcFind = lcFind + 
                        (IF liKeyFields > 1 THEN " AND " ELSE "") + 
                        lcTable + "." + lcKeyField + " = " +
                        DumpFile.MainTable + "." + lcKeyField.
         END.
      END.   
       
   END.

   lhFind:QUERY-PREPARE(lcFind).
   lhFind:QUERY-OPEN.

   REPEAT ON QUIT UNDO, RETRY
          ON STOP UNDO, RETRY:

      /* make sure that ctrl-c doesn't quit -> widget-pool gets deleted */
      IF RETRY THEN DO:
         llInterrupted = TRUE.
         LEAVE.
      END.
 
      lhFind:GET-NEXT.
   
      IF lhFind:QUERY-OFF-END THEN LEAVE.

      liCollect = liCollect + 1.
      IF NOT SESSION:BATCH AND liCollect MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP
            DumpFile.MainTable LABEL "Table  " SKIP
            liCollect          LABEL "Collect" FORMAT ">>>>>>>9" SKIP
            liPicked           LABEL "Picked " FORMAT ">>>>>>>9"
         WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY TITLE " Collecting "
            FRAME fColl.
      END.

      /* dump only modified ones */
      IF icDumpMode = "modified" THEN DO:

         IF NOT fWasRecordModified(lhTable,
                                   lcEventSource,
                                   lcEventFields,
                                   ldLastDump,
                                   ldaModified,
                                   ldtLastDump,
                                   lcModFields)
         THEN NEXT. 
         
         /* check also side tables? */
      END.
 
      /* save to temp-table */
      lhCollect:BUFFER-CREATE.
      /* main table */
      lhCollect:BUFFER-COPY(lhTable).
      /* additional tables */
      IF liAddTableQty > 0 THEN DO liCnt = 1 TO liAddTableQty:
         lhCollect:BUFFER-COPY(lhAddTable[liCnt]).
      END. 

      liPicked = liPicked + 1.

      IF llTestRun AND liPicked >= liTestQty THEN LEAVE.  
   END.

   IF NOT SESSION:BATCH THEN 
      HIDE FRAME fColl NO-PAUSE.

   lhFind:QUERY-CLOSE.

   RETURN "".
   
END PROCEDURE. 

PROCEDURE pWriteDumpFile:

   DEF VAR liField   AS INT NO-UNDO.
   DEF VAR liExtFrom AS INT NO-UNDO.
   DEF VAR llXML     AS LOG NO-UNDO.
   
   llXML = (DumpFile.DumpFormat = "xml").
   
   IF llXML THEN DO:
   
      IF llSax THEN DO:
         CREATE SAX-WRITER lhXML IN WIDGET-POOL "DumpFile".
         lhXML:FORMATTED = TRUE.
         lhXML:ENCODING = IF DumpFile.DumpCharSet > ""
                          THEN DumpFile.DumpCharSet
                          ELSE 'ISO-8859-15'.
         lhXML:SET-OUTPUT-DESTINATION("FILE",lcFile).                 
         lhXML:START-DOCUMENT().
         lhXML:START-ELEMENT(DumpFile.DumpName).
      END.
         
      ELSE DO:
         CREATE X-DOCUMENT lhDoc  IN WIDGET-POOL "DumpFile".
         CREATE X-NODEREF lhRoot  IN WIDGET-POOL "DumpFile".
         CREATE X-NODEREF lhRecLevel IN WIDGET-POOL "DumpFile".
         CREATE X-NODEREF lhFieldLevel IN WIDGET-POOL "DumpFile".
         CREATE X-NODEREF lhValue IN WIDGET-POOL "DumpFile".

         lhDoc:CREATE-NODE(lhRoot,
                           DumpFile.DumpName,
                           "ELEMENT").
         lhDoc:APPEND-CHILD(lhRoot).                       
      
         lhDoc:ENCODING = IF DumpFile.DumpCharSet > "" 
                          THEN DumpFile.DumpCharSet
                          ELSE 'ISO-8859-15'.
      END.
   END.
   
   ELSE DO: 
      /* open output file */
      IF DumpFile.DumpCharSet > "" THEN 
         OUTPUT STREAM sFile TO VALUE(lcFile) 
            CONVERT TARGET DumpFile.DumpCharSet.
      ELSE OUTPUT STREAM sFile TO VALUE(lcFile).
   END.
   
   /* query for temp-table */ 
   CREATE QUERY lhTempFind IN WIDGET-POOL "DumpFile".
   lhTempFind:SET-BUFFERS(lhCollect).
   lhTempFind:QUERY-PREPARE("FOR EACH ttCollect NO-LOCK").
   lhTempFind:QUERY-OPEN.

   REPEAT ON QUIT UNDO, RETRY
          ON STOP UNDO, RETRY:

      /* make sure that ctrl-c doesn't quit -> widget-pool gets deleted */
      IF RETRY THEN DO:
         llInterrupted = TRUE.
         LEAVE.
      END.
   
      lhTempFind:GET-NEXT.
   
      IF lhTempFind:QUERY-OFF-END THEN LEAVE.

      IF llXML THEN DO:
         IF llSax THEN DO:
            lhXML:START-ELEMENT(DumpFile.MainTable).
         END.
         
         ELSE DO:
            lhDoc:CREATE-NODE(lhRecLevel,
                              DumpFile.MainTable,
                              "ELEMENT").
            lhRoot:APPEND-CHILD(lhRecLevel).
         END.
      END.
      
      DO liField = 1 TO lhCollect:NUM-FIELDS:
   
         lhField = lhCollect:BUFFER-FIELD(liField).

         lcValue = "".
         
         /* normal field */  
         IF lhField:EXTENT = 0 
         THEN liExtFrom = 0.
         /* has extents */
         ELSE liExtFrom = 1.
         
         DO liCnt = liExtFrom TO lhField:EXTENT:
            IF lhField:DATA-TYPE = "DECIMAL" THEN
               lcValue = fDispDecimal(lhField:BUFFER-VALUE(liCnt),
                                      lhField:DECIMALS).
            ELSE lcValue = STRING(lhField:BUFFER-VALUE(liCnt)).
         
            IF llXML THEN DO:
               IF llSax THEN DO:
                  lhXML:WRITE-DATA-ELEMENT(lhFIELD:LABEL,
                                           lcValue).
               END.
               ELSE DO:
                  lhDoc:CREATE-NODE(lhFieldLevel,
                                    lhFIELD:LABEL,
                                    "ELEMENT").
                  lhRecLevel:APPEND-CHILD(lhFieldLevel).
                  lhDoc:CREATE-NODE(lhValue,
                                    "",
                                    "Text").
                  lhFieldLevel:APPEND-CHILD(lhValue).
                  lhValue:NODE-VALUE = lcValue.
               END.
            END.
            
            ELSE DO:
               /* value to file */
               IF LOOKUP(DumpFile.DumpName, "CustDumpTXT") > 0 THEN  
                   lcvalue = REPLACE(lcValue, lcDelimiter , " ").

               PUT STREAM sFile UNFORMATTED lcValue.
         
               /* delimiter between fields */
               IF liCnt < lhField:EXTENT THEN 
                  PUT STREAM sFile UNFORMATTED lcDelimiter.
            END.
         END.

         IF NOT llXML THEN DO:
            /* delimiter between fields */
            IF liField < lhCollect:NUM-FIELDS THEN 
               PUT STREAM sFile UNFORMATTED lcDelimiter.
         END.
      END.

      IF llXML THEN DO:
         IF llSax THEN DO:
            lhXML:END-ELEMENT(DumpFile.MainTable).
         END.
      END.
      
      ELSE DO:
         /* linefeed between rows */
         IF lcSkip > "" THEN 
            PUT STREAM sFile UNFORMATTED lcSkip.
         
         ELSE PUT STREAM sFile SKIP. 
      END.
      
      oiEventCount = oiEventCount + 1.

      IF NOT SESSION:BATCH AND oiEventCount MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP
            DumpFile.MainTable LABEL "Table" SKIP
            oiEventCount       LABEL "Rows " FORMAT ">>>>>>>9"
         WITH SIDE-LABELS 1 DOWN ROW 15 CENTERED OVERLAY TITLE " Dumping "
            FRAME fQty.
      END.

   END.

   lhTempFind:QUERY-CLOSE.

   IF llXML THEN DO:
      IF llSax THEN DO:
         lhXML:END-ELEMENT(DumpFile.DumpName).
         lhXML:END-DOCUMENT().
      END.
      ELSE DO:
         lhDoc:SAVE("FILE",lcFile).
      END.
   END.
   ELSE DO:
      OUTPUT STREAM sFile CLOSE.
   END.
   
   IF NOT SESSION:BATCH THEN 
      HIDE FRAME fQty NO-PAUSE.

   RETURN "".
   
END PROCEDURE.


