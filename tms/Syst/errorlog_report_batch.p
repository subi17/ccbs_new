 /* ------------------------------------------------------
  MODULE .......: errorlog_report_batch.p
  FUNCTION .....: errorlog report as batch
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 16.04.09 
  VERSION ......: yoigo
  ------------------------------------------------------ */


{Syst/commpaa.i}
gcBrand = "1".
{Func/cparam2.i}
{Func/email.i}

DEF VAR lcConfDir    AS CHAR NO-UNDO.
DEF VAR lcFile       AS CHAR NO-UNDO.
DEF VAR lcActionID   AS CHAR NO-UNDO.
DEF VAR lcTableName  AS CHAR NO-UNDO.
DEF VAR liReported   AS INT  NO-UNDO.
DEF VAR lcMsgFile    AS CHAR NO-UNDO.
DEF VAR ldtEventDate AS DATE NO-UNDO.

DEF STREAM sLog.


ASSIGN
   lcActionID   = SESSION:PARAMETER
   ldtEventDate = TODAY - 1.

IF lcActionID = "" THEN QUIT.

IF NUM-ENTRIES(lcActionID) > 1 THEN ASSIGN
   lcTableName = ENTRY(2,lcActionID)
   lcActionID  = ENTRY(1,lcActionID).
   
RUN Syst/errorlog_report(ldtEventDate,
                    ldtEventDate,
                    lcActionID,
                    lcTableName,
                    OUTPUT liReported,
                    OUTPUT lcFile).

IF liReported > 0 THEN DO:

   lcConfDir = fCParamC("RepConfDir").

   GetRecipients(lcConfDir + "errorlog_" + LC(lcActionID) + ".email").

   IF xMailAddr > "" THEN DO:

      lcMsgFile = "/tmp/errorlog_" + LC(lcActionID) + ".txt".

      OUTPUT STREAM sLog TO VALUE(lcMsgFile).
      PUT STREAM sLog UNFORMATTED
         "ErrorLog report for " + 
         lcActionID + (IF lcTableName > "" THEN "/" + lcTableName ELSE "") +
         " events on " + STRING(ldtEventDate,"99.99.9999") + 
         " was printed to file " + CHR(10) + 
         lcFile
         SKIP.
      OUTPUT STREAM sLog CLOSE.

      SendMail(lcMsgFile,"").
   END.
      
END.

QUIT.
