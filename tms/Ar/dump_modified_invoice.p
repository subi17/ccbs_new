/* ----------------------------------------------------------------------
  MODULE .......: dump_modified_invoice.p
  TASK .........: Collect rows to a dump file for modified invoices
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 09.02.09
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}

DEF INPUT-OUTPUT PARAMETER TABLE-HANDLE ihTempTable.
DEF INPUT  PARAMETER idLastDump       AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icModifiedFields AS CHAR NO-UNDO.

DEF VAR lhTable      AS HANDLE NO-UNDO.
DEF VAR lhCollect    AS HANDLE NO-UNDO.
DEF VAR lhReqField   AS HANDLE NO-UNDO.
DEF VAR liPicked     AS INT    NO-UNDO.
DEF VAR liCnt        AS INT    NO-UNDO.
DEF VAR lcTableName  AS CHAR   NO-UNDO.
DEF VAR ldaModified  AS DATE   NO-UNDO.
DEF VAR liTime       AS INT    NO-UNDO.
DEF VAR ldtLastDump  AS DATETIME NO-UNDO.

DEF TEMP-TABLE ttPicked NO-UNDO
   FIELD InvNum AS INT
   INDEX InvNum InvNum.

FORM
    lcTableName AT 2  LABEL "Table  " FORMAT "X(15)"    SKIP
    liPicked    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY 
    TITLE " Collecting " FRAME fColl.


FUNCTION fCollect RETURNS LOGIC:

   lhReqField = lhTable:BUFFER-FIELD("InvNum").
   IF CAN-FIND(FIRST ttPicked WHERE 
                     ttPicked.InvNum = lhReqField:BUFFER-VALUE)
   THEN RETURN FALSE.
 
   CREATE ttPicked.
   ttPicked.InvNum = lhReqField:BUFFER-VALUE.
   
   lhCollect:BUFFER-CREATE.
   lhCollect:BUFFER-COPY(lhTable).

   liPicked = liPicked + 1.
   IF NOT SESSION:BATCH AND liPicked MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP lcTableName liPicked WITH FRAME fColl.
   END.

END FUNCTION.


/***** MAIN start ********/

ASSIGN
   lhCollect   = ihTempTable:DEFAULT-BUFFER-HANDLE
   lhTable     = BUFFER Invoice:HANDLE
   lcTableName = lhTable:NAME.

fSplitTS(idLastDump,
         OUTPUT ldaModified,
         OUTPUT liTime).

ldtLastDump = fTimeStamp2DateTime(idLastDump).


/* collect only those that have been modified since last dump */

FOR EACH Invoice NO-LOCK WHERE
         Invoice.Brand = gcBrand:
         
   IF fWasRecordModified(lhTable,
                         icEventSource,
                         icEventFields,
                         idLastDump,
                         ldaModified,
                         ldtLastDump,
                         icModifiedFields)
   THEN DO:
      fCollect().
   END.

   /* printed */
   ELSE 
   FOR EACH ITSendLog NO-LOCK USE-INDEX InvNum WHERE 
            ITSendLog.InvNum     = Invoice.InvNum AND
            ITSendLog.SendStamp >= idLastDump:
      fCollect().
      LEAVE.
   END.

   /* no need to check from ClaimHist, Invoice.ClaimStamp can be used */
END.

IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE. 

/****  MAIN end ********/



