/*------------------------------------------------------------------------
  MODULE .......: dms_casefile_reader.p
  TASK .........: Case file update DMS -> TMS
  APPLICATION ..: TMS
  AUTHOR .......: ivekov
  CREATED ......: 26.08.15
  CHANGED ......: 
  Version ......: yoigo
-------------------------------------------------------------------------- */

{commpaa.i}
ASSIGN
   katun   = "Cron"
   gcBrand = "1".
{tmsconst.i}
{ftransdir.i}
{cparam2.i}
{eventlog.i}
{dms.i}

DEF VAR lcIncDir        AS CHAR NO-UNDO.
DEF VAR lcProcDir       AS CHAR NO-UNDO.
DEF VAR lcSpoolDir      AS CHAR NO-UNDO.
DEF VAR lcLogDir        AS CHAR NO-UNDO.
DEF VAR lcFileName      AS CHAR NO-UNDO.
DEF VAR lcInputFile     AS CHAR NO-UNDO.
DEF VAR lcLogFileOut    AS CHAR NO-UNDO.
DEF VAR lcProcessedFile AS CHAR NO-UNDO.
DEF VAR lcLogFile       AS CHAR NO-UNDO.
DEF VAR lcLine          AS CHAR NO-UNDO.
DEF VAR lcSep           AS CHAR NO-UNDO.
DEF VAR ldaReadDate     AS DATE NO-UNDO.

DEF BUFFER bDMS FOR DMS.

ASSIGN
   lcIncDir   = fCParam("DMS","TMS_IncDir")
   lcProcDir  = fCParam("DMS","TMS_ProcDir")
   lcSpoolDir = fCParam("DMS","TMS_SpoolDir")
   lcLogDir   = fCParam("DMS","TMS_LogDir")
   lcSep      = "|".

DEF STREAM sIn.
DEF STREAM sFile.
DEF STREAM sLog.

FUNCTION fLogLine RETURNS LOGICAL
   (icMessage AS CHAR):
   PUT STREAM sLog UNFORMATTED
      lcLine "#"
      icMessage "#"
      "DMS" SKIP.
END FUNCTION.

/*Is feature active:*/
IF fDMSOnOff() NE TRUE THEN RETURN.


INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
   lcInputFile = lcIncDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN
      INPUT STREAM sIn FROM VALUE(lcInputFile).
   ELSE NEXT.

   ldaReadDate  = TODAY.
   lcLogFile = lcSpoolDir + 
               "dms_to_tms_" +
               STRING(YEAR(ldaReadDate)) +
               STRING(MONTH(ldaReadDate),"99") +
               STRING(DAY(ldaReadDate),"99") + ".log".
   OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

   LINE_LOOP:
   REPEAT:

      IMPORT STREAM sIn UNFORMATTED lcLine.

      IF lcLine EQ "" THEN NEXT.
      
      lcLine = CODEPAGE-CONVERT(lcLine, SESSION:CHARSET, "UTF-8").

      RUN pUpdateDMS (lcLine).

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         fLogLine(ENTRY(2,RETURN-VALUE,":")).
      END.
      ELSE DO:
         fLogLine(RETURN-VALUE).
      END.
   END.

   ASSIGN
      lcLogFileOut    = fMove2TransDir(lcLogFile, "", lcLogDir)
      lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir).

   INPUT STREAM sIn CLOSE.
   OUTPUT STREAM sLog CLOSE.

END.

INPUT STREAM sFile CLOSE.

PROCEDURE pUpdateDMS:

   DEF INPUT PARAMETER pcLine AS CHAR NO-UNDO.

   DEF VAR liOrderId       AS INT  NO-UNDO.
   DEF VAR lcDmsExternalID AS CHAR NO-UNDO.
   DEF VAR lcCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcContractID    AS CHAR NO-UNDO.
   DEF VAR lcStatusCode    AS CHAR NO-UNDO.
   DEF VAR lcStatusDesc    AS CHAR NO-UNDO.
   DEF VAR ldStatusTS      AS DEC  NO-UNDO.
   DEF VAR lcDocList       AS CHAR NO-UNDO.
   DEF VAR lcUpdateDMS     AS CHAR NO-UNDO.
 
   DEF BUFFER Order FOR Order.

   ASSIGN
      lcDmsExternalID = ENTRY(1,pcLine,lcSep)
      lcCaseTypeID    = ENTRY(2,pcLine,lcSep)
      lcContractID    = ENTRY(3,pcLine,lcSep)
      liOrderId       = INTEGER(ENTRY(4,pcLine,lcSep))
      lcStatusCode    = ENTRY(5,pcLine,lcSep)
      lcStatusDesc    = ENTRY(6,pcLine,lcSep)
      ldStatusTS      = DECIMAL(ENTRY(7,pcLine,lcSep))
      lcDocList       = ENTRY(8,pcLine,lcSep).

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand EQ gcBrand AND
              Order.OrderID EQ liOrderId NO-ERROR.
   IF NOT AVAIL Order THEN 
      RETURN "ERROR:ORDER NOT AVAILABLE:" + STRING(liOrderId).

   lcUpdateDMS = fUpdateDMS(lcDmsExternalID,
                            lcCaseTypeID,
                            lcContractID,
                            "Order",
                            liOrderId,
                            lcStatusCode,
                            lcStatusDesc,
                            "",
                            ldStatusTS,
                            lcDocList,
                            ";").
   
   IF lcUpdateDMS <> "OK" THEN RETURN "ERROR:" + lcUpdateDMS + ":UPDATE".
   ELSE IF (lcCaseTypeID = {&DMS_CASE_TYPE_ID_ORDER_RESTUDY} OR
            lcCaseTypeID = {&DMS_CASE_TYPE_ID_COMPANY}) THEN DO:
      CASE lcStatusCode:
         WHEN "E" THEN DO:
            IF NOT ((Order.StatusCode = "20" OR Order.StatusCode = "21") AND
                     Order.PayType = FALSE) THEN
               RUN orderhold.p(liOrderId, "RELEASE_BATCH").
         END.
         WHEN "J" THEN RUN closeorder.p(liOrderId, TRUE).
         WHEN "F" THEN RUN orderbyfraud.p(liOrderId, TRUE,
                                           {&ORDER_STATUS_CLOSED_BY_FRAUD}).
         WHEN "N" OR
         WHEN "G" THEN RUN orderbyfraud.p(liOrderId, TRUE,
                                           {&ORDER_STATUS_AUTO_CLOSED}).
      END CASE.
   END.

   IF RETURN-VALUE > "" THEN RETURN "ERROR:" + RETURN-VALUE.

   RETURN "OK".

END PROCEDURE.
