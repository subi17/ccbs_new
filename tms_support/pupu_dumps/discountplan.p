
/*------------------------------------------------------------------------
    File        : discountplan.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Subhash Sanjeevi
    Created     : Fri Aug 08 13:33:18 EEST 2014
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

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

{cparam2.i}
{timestamp.i}
{ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/discountplan_" +
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

FOR EACH DiscountPlan NO-LOCK:

   liEvents = liEvents + 1.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "BillItem" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   PUT STREAM slog UNFORMATTED
       "DiscountPlan"                              lcDel
       "CREATE"                                    lcDel
       fNotNull(STRING(RECID(DiscountPlan)))       lcDel
       fNotNull(STRING(DiscountPlan.DPId))         lcDel
       fNotNull(STRING(ldtTimeStamp))              lcDel
       fNotNull(STRING(DiscountPlan.DPId))         lcDel 
       fNotNull(STRING(DiscountPlan.ValidFrom))    lcDel  
       fNotNull(STRING(DiscountPlan.ValidTo))      lcDel  
       fNotNull(DiscountPlan.DPRuleID)             lcDel 
       fNotNull(DiscountPlan.DPName)               lcDel 
       fNotNull(STRING(DiscountPlan.DPUnit))       lcDel  
       fNotNull(DiscountPlan.BillCode)             lcDel 
       fNotNull(STRING(DiscountPlan.ValidPeriods)) SKIP .
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).
