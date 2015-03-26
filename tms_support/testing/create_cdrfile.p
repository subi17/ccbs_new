/* ----------------------------------------------------------------------
MODULE .......: create_cdrfile.p
TASK .........: Creates a test CDR file for onlinereader
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 2.8.2012
CHANGED ......: 30.1.2015 kariaika & ilkkasav
Version ......: Yoigo
----------------------------------------------------------------------- */

DEF INPUT PARAM icCLI AS CHAR NO-UNDO.
DEF INPUT PARAM icIMSI AS CHARACTER NO-UNDO. 
DEF INPUT PARAM idaCallDate AS DATE NO-UNDO. 
DEF INPUT PARAM icCallTime AS CHAR NO-UNDO. 
DEF INPUT PARAM iiDuration AS INT NO-UNDO. 
DEF INPUT PARAM icFile AS CHAR NO-UNDO.
DEF INPUT PARAM icOutPutFile AS CHAR NO-UNDO.
DEF INPUT PARAM iiLoops AS INT NO-UNDO.

DEF VAR lcLine AS CHAR NO-UNDO. 
DEF VAR lcCallDate AS CHAR NO-UNDO. 
DEF VAR lcCallTime AS CHAR NO-UNDO. 

DEFINE VARIABLE liCallDatePos AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCallTimePos AS INTEGER NO-UNDO. 

FILE-INFO:FILE-NAME = icFile.
IF FILE-INFO:FILE-TYPE = ? THEN DO:
   MESSAGE SUBST("ERROR:File &1 not found", icFile) VIEW-AS ALERT-BOX.
   RETURN.
END.

IF idaCallDate NE ? THEN
   lcCallDate = STRING(YEAR(idaCallDate)) +
                STRING(MONTH(idaCallDate),"99") +
                STRING(DAY(idaCallDate),"99").

IF icCallTime > "" THEN
   lcCallTime = SUBSTR(icCallTime,1,2) +
             SUBSTR(icCallTime,4,2) +
             "00".

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
   
   if trim(entry(4,lcLine,"|")) EQ "GE" THEN DO:
      
      ASSIGN
         liCallDatePos = 19
         liCallTimePos = 20.
      
      IF NOT fReplaceValue(lcLine,34,icImsi) THEN NEXT.

   END.
   ELSE IF trim(entry(4,lcLine,"|")) EQ "YC" THEN DO:
      
      ASSIGN
         liCallDatePos = 16
         liCallTimePos = 17.
   
      if lookup(trim(entry(14,lcLine,"|")),"7,33,96,106") > 0 then do:
         if not fReplaceValue(input-output lcLine,25,icCli) then next.
      end.
      else do:
         if not fReplaceValue(input-output lcLine,28,icCli) then next.
      end.
 
   END.
   ELSE DO:
      
      ASSIGN
         liCallDatePos = 16
         liCallTimePos = 17.
   
      if lookup(trim(entry(14,lcLine,"|")),"7,33,96,106") > 0 then do:
         if not fReplaceValue(input-output lcLine,26,icCli) then next.
      end.
      else do:
         if not fReplaceValue(input-output lcLine,22,icCli) then next.
      end.
 
   END.
   
   if error-status:error then next.

   IF lcCallDate > "" THEN DO:
      IF NOT fReplaceValue(input-output lcLine,liCallDatePos,lcCallDate)
         THEN NEXT.
   END.
   
   IF lcCallTime > "" THEN DO:
      IF NOT fReplaceValue(input-output lcLine,liCallTimePos,lcCallTime)
         THEN NEXT.
   END.
/*
   if iiDuration ne 0 then do:
      lcLine = replace(lcLine, "|" + entry(19,lcLine,"|") + "|", "|" + STRING(iiDuration) + "|") no-error.   
      if error-status:error then next.
   end.
  */ 
   put stream soutfile unformatted lcLine skip.
end.

output stream soutfile close.
input stream sinfile close.

RETURN "".
