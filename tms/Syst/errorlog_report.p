/* ----------------------------------------------------------------------
  MODULE .......: errorlog_report.p
  TASK .........: Print a report from ErrorLog
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 17.04.09
  Version ......: yoigo
---------------------------------------------------------------------- */


{Syst/commali.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/timestamp.i}

DEF INPUT  PARAMETER idaEventDate1 AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaEventDate2 AS DATE NO-UNDO.
DEF INPUT  PARAMETER icActionID    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icTableName   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocFile        AS CHAR NO-UNDO.

DEF VAR lcTransDir AS CHAR NO-UNDO.
DEF VAR ldBeginTS  AS DEC  NO-UNDO.
DEF VAR ldEndTS    AS DEC  NO-UNDO.
DEF VAR liCnt      AS INT  NO-UNDO.
   
DEF STREAM sFile.


ASSIGN 
   ocFile     = fCParamC("ErrorLogRepFile")
   lcTransDir = fCParamC("ErrorLogRepTransDir").

IF ocFile = ? OR ocFile = "" THEN RETURN "ERROR:File not defined".   
IF lcTransDir = ? THEN lcTransDir = "".

ASSIGN
   ocFile    = REPLACE(ocFile,"#DATE",STRING(YEAR(idaEventDate2),"9999") + 
                                      STRING(MONTH(idaEventDate2),"99") +
                                      STRING(DAY(idaEventDate2),"99"))
   ocFile    = REPLACE(ocFile,"#ACTION",icActionID)
   ldBeginTS = fMake2DT(idaEventDate1,0)
   ldEndTS   = fMake2DT(idaEventDate2,86399).
   
ocFile = fUniqueFileName(ocFile,".txt").

OUTPUT STREAM sFile TO VALUE(ocFile).

PUT STREAM sFile UNFORMATTED
   "ActionID"    CHR(9)
   "Table Name"  CHR(9)
   "Key Value"   CHR(9)
   "Time"        CHR(9)
   "User"        CHR(9)
   "Info"        CHR(9)
   "Error"       SKIP.

FOR EACH ErrorLog NO-LOCK WHERE
         ErrorLog.Brand     = gcBrand    AND
         ErrorLog.ActionID  = icActionID AND
         ErrorLog.ActionTS >= ldBeginTS  AND
         ErrorLog.ActionTS <= ldEndTS:
       
   IF icTableName > "" AND ErrorLog.TableName NE icTableName THEN NEXT.

   PUT STREAM sFile UNFORMATTED
      ErrorLog.ActionID   CHR(9)
      ErrorLog.TableName  CHR(9)
      ErrorLog.KeyValue   CHR(9)
      fTS2HMS(ErrorLog.ActionTS)  CHR(9)
      ErrorLog.UserCode   CHR(9)
      ErrorLog.ErrorChar  CHR(9)
      ENTRY(1,ErrorLog.ErrorMsg,CHR(10)) SKIP.
      
   IF NUM-ENTRIES(ErrorLog.ErrorMsg,CHR(10)) > 1 THEN 
   DO liCnt = 2 TO NUM-ENTRIES(ErrorLog.ErrorMsg,CHR(10)):
      PUT STREAM sFile UNFORMATTED
         FILL(CHR(9),6)
         ENTRY(liCnt,ErrorLog.ErrorMsg,CHR(10))
         SKIP.
   END.
      
   oiEvents = oiEvents + 1.
   
   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
              
      PAUSE 0.
      DISP oiEvents 
         LABEL "Event Qty" 
         FORMAT ">>>>>>>9"
      WITH SIDE-LABELS 1 DOWN ROW 15 COL 55 OVERLAY TITLE " Collecting "
           FRAME fQty.
   END.
END.
 
OUTPUT STREAM sFile CLOSE.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

IF oiEvents = 0 THEN OS-DELETE VALUE(ocFile).
 
/* move the file to the transfer directory */
ELSE IF lcTransDir > "" THEN DO:
   ocFile = fMove2TransDir(ocFile,
                           ".txt",
                           lcTransDir).
END.            


