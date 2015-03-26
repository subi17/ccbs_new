
/*------------------------------------------------------------------------
    File        : fatime.p
    Purpose     : 

    Syntax      :

    Description : 	

    Author(s)   : Subhash Sanjeevi
    Created     : Fri Aug 08 14:54:04 EEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand      AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.
DEFINE VARIABLE llgDate      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ldtDate      AS DATE      NO-UNDO.
DEFINE VARIABLE ldtTime      AS DECIMAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{cparam2.i}
{timestamp.i}
{ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/fatime_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH FATime NO-LOCK:

    FIND FIRST MobSub WHERE
               MobSub.MsSeq = FATime.MsSeq NO-LOCK NO-ERROR.

    IF AVAILABLE MobSub THEN RUN pDumpRecord.
    ELSE DO:
        FIND FIRST TermMobSub WHERE
                   TermMobSub.MsSeq = FATime.MsSeq NO-LOCK NO-ERROR.

        IF AVAILABLE TermMobSub THEN 
        DO:
            FIND FIRST MSOwner USE-INDEX MsSeq WHERE
                       MSOwner.MsSeq   = TermMobSub.MsSeq NO-LOCK NO-ERROR.

            IF AVAILABLE MSOwner THEN 
            DO:
                llgDate = fSplitTS(INPUT MSOwner.TsEnd,
                                   OUTPUT ldtDate,
                                   OUTPUT ldtTime).
                                   
                IF ldtDate > TODAY - 60 THEN RUN pDumpRecord.
            END.
        END.
    END.

END.

PROCEDURE pDumpRecord:

   liEvents = liEvents + 1.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "FATime" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   PUT STREAM slog UNFORMATTED
       "FATime"                            lcDel
       "CREATE"                            lcDel
       fNotNull(STRING(RECID(Fatime)))     lcDel
       fNotNull(STRING(Fatime.Fatnum))     lcDel
       fNotNull(STRING(ldtTimeStamp))      lcDel
       fNotNull(STRING(Fatime.FATNum))  lcDel 
       fNotNull(Fatime.FTGrp)           lcDel 
       fNotNull(STRING(Fatime.Period))  lcDel 
       fNotNull(STRING(Fatime.Amt))     lcDel 
       fNotNull(Fatime.CLI)             lcDel 
       fNotNull(STRING(Fatime.MsSeq))   lcDel 
       fNotNull(STRING(Fatime.CustNum)) lcDel  
       fNotNull(STRING(Fatime.Used))    lcDel  
       fNotNull(STRING(Fatime.InvNum))  SKIP.
       
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).

