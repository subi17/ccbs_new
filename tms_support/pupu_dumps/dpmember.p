
/*------------------------------------------------------------------------
    File        : dpmember.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Subhash Sanjeevi
    Created     : Thu Aug 07 16:04:26 EEST 2014
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

ASSIGN lcLogFile = lcSpoolDir              + "/dpmember_"              +
                   STRING(YEAR(TODAY))     + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_"                       +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","")             + ".txt"
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH DPMember WHERE
         DPMember.HostTable = "MobSub" NO-LOCK:

    FIND FIRST MobSub WHERE
               MobSub.MsSeq = INTEGER(DPMember.KeyValue) NO-LOCK NO-ERROR.

    IF AVAILABLE MobSub THEN RUN pDumpRecord.
    ELSE DO:
        FIND FIRST TermMobSub WHERE
                   TermMobSub.MsSeq = INTEGER(DPMember.KeyValue) NO-LOCK NO-ERROR.

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

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
   DO:
       PAUSE 0.
       DISP liEvents LABEL "DPMember" 
       WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
            TITLE " Collecting " FRAME fQty.
   END. 

   PUT STREAM slog UNFORMATTED
       "DPMember"                           lcDel
       "CREATE"                             lcDel
       fNotNull(STRING(RECID(DPMember)))        lcDel
       fNotNull(STRING(DPMember.DPId) + CHR(255) +
                DPMember.HostTable + CHR(255) +
                DPMember.KeyValue + CHR(255) +
                STRING(DPMember.ValidFrom))   lcDel
       fNotNull(STRING(ldtTimeStamp))       lcDel
       fNotNull(STRING(DPMember.DPId))      lcDel
       fNotNull(DPMember.HostTable)         lcDel
       fNotNull(DPMember.KeyValue)          lcDel 
       fNotNull(STRING(DPMember.DiscValue)) lcDel 
       fNotNull(STRING(DPMember.ValidFrom)) lcDel 
       fNotNull(STRING(DPMember.ValidTo))   SKIP.
    
END.        
    
OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).
