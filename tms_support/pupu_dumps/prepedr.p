/* ----------------------------------------------------------------------
  MODULE .......: prepedr.p
  TASK .........: Create a PrepEDR table dump file for HPD
  APPLICATION ..: tms
  AUTHOR .......: anttis 
  CREATED ......: 7.8.2014
  Version ......: yoigo
---------------------------------------------------------------------- */

DEF INPUT PARAMETER idaFromDate AS DATE NO-UNDO.
DEF INPUT PARAMETER idaToDate   AS DATE NO-UNDO.
DEF INPUT PARAMETER iiSeq       AS INT  NO-UNDO.

DEF VAR lcDel         AS CHAR NO-UNDO INIT "|".
DEF VAR liEvents      AS INT  NO-UNDO.
DEF VAR lcLogFile     AS CHAR NO-UNDO.
DEF VAR lcSpoolDir    AS CHAR NO-UNDO.
DEF VAR lcOutDir      AS CHAR NO-UNDO.
DEF VAR lcKeyValue    AS CHAR NO-UNDO.
DEF VAR lcDel2        AS CHAR NO-UNDO.
DEF VAR gcBrand       AS CHAR NO-UNDO INIT "1".
DEF VAR ldtTimeStamp  AS DATETIME  NO-UNDO.

{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/date.i}
{Func/cparam2.i}
{Func/ftransdir.i}

DEF STREAM sFile.

ASSIGN ldtTimeStamp = DATETIME(TODAY,MTIME)
       lcDel2       = CHR(255)
       lcSpoolDir   = fCParam("HPD","DumpSpoolDir")
       lcOutDir     = fCParam("HPD","DumpOutDir")
       lcLogFile    = lcSpoolDir + "/prepedr_full_" + STRING(iiSeq) + "_" +
                      STRING(YEAR(TODAY)) +
                      STRING(MONTH(TODAY),"99") +
                      STRING(DAY(TODAY),"99") + "_" +
                      REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM sFile TO VALUE(lcLogFile).

FOR EACH PrepEDR NO-LOCK USE-INDEX ReadDate WHERE
         PrepEDR.ReadDate   >= idaFromDate AND
         PrepEDR.ReadDate   <= idaToDate AND
         PrepEDR.ErrorCode   = 0          
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN LEAVE.

   RUN pWrite2File.
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).

PROCEDURE pWrite2File:

   ASSIGN lcKeyValue = PrepEDR.CLI + lcDel2 + STRING(PrepEDR.DtlSeq) + lcDel2 +
                       STRING(PrepEDR.DateSt).
          liEvents   = liEvents + 1.

   PUT STREAM sFile UNFORMATTED
      "PrepEDR"                            lcDel
      "CREATE"                             lcDel
      STRING(RECID(PrepEDR))               lcDel
      lcKeyValue                           lcDel
      STRING(ldtTimeStamp)                 lcDel
      PrepEDR.MsSeq                        lcDel
      PrepEDR.CLI                          lcDel
      PrepEDR.CustNum                      lcDel
      STRING(PrepEDR.DateSt,"99.99.9999")  lcDel
      STRING(PrepEDR.TimeStart,"HH:MM:SS") lcDel
      PrepEDR.ReadInTS                     lcDel
      PrepEDR.SubscriberFee                lcDel
      PrepEDR.SuccessCode                  lcDel
      PrepEDR.BalanceAfter SKIP.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "PrepEDRs" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END PROCEDURE.               


