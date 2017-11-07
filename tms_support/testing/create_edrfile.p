/* ----------------------------------------------------------------------
MODULE .......: create_edrfile.p
TASK .........: Creates a test EDR file for edr_reader.p
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 18.2.2013
CHANGED ......:
Version ......: Yoigo
Modified......: ilkkasav & kariaika
----------------------------------------------------------------------- */
{Func/detailvalue.i}

/*ilkkasav&kariaika: remember to change the directories before releasing*/
/*{/apps/yoigo/tms_support/testing/cdrreader.i}*/
{testing/cdrreader.i}


DEF INPUT PARAM icCLI AS CHAR NO-UNDO.
DEF INPUT PARAM idtCallDateTime AS DATETIME NO-UNDO. 
DEF INPUT PARAM icFile AS CHAR NO-UNDO.
DEF INPUT PARAM icOutPutFile AS CHAR NO-UNDO.

DEF VAR lcLine AS CHAR NO-UNDO. 
DEF VAR lcCallDate AS CHAR NO-UNDO. 
DEF VAR lcCallTime AS CHAR NO-UNDO. 
DEF VAR lcCSVVersion AS CHAR NO-UNDO INIT "0101YD". 


/*CDR creation is available only in internal environments.
Check that we are not in production*/
IF ( fISInInternalEnv() <> TRUE ) THEN DO:

 MESSAGE "NOT IN TEST" VIEW-AS ALERT-BOX ERROR.
 RETURN.

END.



FILE-INFO:FILE-NAME = icFile.
IF FILE-INFO:FILE-TYPE = ? THEN DO:
   MESSAGE SUBST("ERROR:File &1 not found", icFile) VIEW-AS ALERT-BOX.
   RETURN.
END.

IF idtCallDateTime NE ? THEN DO:
   lcCallDate = STRING(YEAR(idtCallDateTime)) +
                STRING(MONTH(idtCallDateTime),"99") +
                STRING(DAY(idtCallDateTime),"99").
   lcCallTime = fMTimetoString(MTIME(idtCallDateTime)).


END.


FUNCTION fReplaceValue RETURNS LOGICAL
   (input-output icLine AS CHARACTER,
    iiPos AS INTEGER,
    icValue AS CHARACTER):
      
    icLine = replace(icLine,"|" + entry(iiPos,icLine,"|") + "|","|" + 
      icValue + "|") no-error. 

    return (not error-status:error).

END FUNCTION. 

def stream sinfile.
input stream sinfile from value(icFile).

def stream soutfile.
output stream soutfile to value(icOutPutFile).

repeat:
   import stream sinfile unformatted lcLine.
   
   if not fReplaceValue(input-output lcLine,
                        fGetPosition(lcCsvVersion,"Subscriber number"),
                        icCli) then next.
   
   if not fReplaceValue(input-output lcLine,
                        fGetPosition(lcCsvVersion,"Account number"),
                        icCli) then next.
   

   IF lcCallDate > "" THEN DO:
      IF NOT fReplaceValue(input-output lcLine,
                           fGetPosition(lcCsvVersion,"Event date"),
                           lcCallDate) THEN NEXT.
   END.
   
   IF lcCallTime > "" THEN DO:
      IF NOT fReplaceValue(input-output lcLine,
                           fGetPosition(lcCsvVersion,"Event time"),
                           lcCallTime) THEN NEXT.
   END.
   
   put stream soutfile unformatted lcLine skip.
end.

output stream soutfile close.
input stream sinfile close.

RETURN "".
