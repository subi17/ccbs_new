/* ----------------------------------------------------------------------
  MODULE .......: dump_modified_msrequest.p
  TASK .........: Collect rows to a dump file for modified msrequests
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 28.11.08
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{dumpfile_run.i}

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

DEF TEMP-TABLE ttPicked NO-UNDO
   FIELD MsRequest AS INT
   INDEX MsRequest MsRequest.

FORM
    lcTableName AT 2  LABEL "Table  " FORMAT "X(15)"    SKIP
    liPicked    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY 
    TITLE " Collecting " FRAME fColl.


FUNCTION fCollect RETURNS LOGIC:

   lhReqField = lhTable:BUFFER-FIELD("MsRequest").
   IF CAN-FIND(FIRST ttPicked WHERE 
                     ttPicked.MsRequest = lhReqField:BUFFER-VALUE)
   THEN RETURN FALSE.
 
   CREATE ttPicked.
   ttPicked.MsRequest = lhReqField:BUFFER-VALUE.
   
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
   lhTable     = BUFFER MsRequest:HANDLE
   lcTableName = lhTable:NAME.

/* collect only those that have been modified since last dump */

DO liCnt = 1 TO NUM-ENTRIES(icEventSource,"|"):
   
   /* check modification from eventlog */
   IF ENTRY(liCnt,icEventSource,"|") = "EventLog" THEN 
      RUN pFindFromEventLog(lhTable,
                            ENTRY(liCnt,icEventFields,"|"),
                            "",
                            idLastDump,
                            "fCollect").
      
   /* check modification from a timestamp field */
   ELSE IF ENTRY(liCnt,icEventSource,"|") = "field" AND
           ENTRY(liCnt,icEventFields,"|") = "UpdateStamp" 
   THEN DO:
      
      FOR EACH TMSCodes NO-LOCK WHERE
               TMSCodes.TableName = "MsRequest" AND
               TMSCodes.FieldName = "ReqStatus",
          EACH MsRequest NO-LOCK USE-INDEX UpdateStamp WHERE
               MsRequest.Brand       = gcBrand AND
               MsRequest.ReqStatus   = INTEGER(TMSCodes.CodeValue) AND
               MsRequest.UpdateStamp >= idLastDump:
                 
         fCollect().         
      END.
   END.
END.

IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE. 

/****  MAIN end ********/







