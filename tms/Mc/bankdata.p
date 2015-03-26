/* ---------------------------------------------------------------------------
  MODULE .......: bankdata.p
  KUTSUVAMODULI : 
  FUNCTION .....: read bank data from file         
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 06.03.08
  CHANGED ......: 
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhBank AS HANDLE NO-UNDO.
   lhBank = BUFFER Bank:HANDLE.
   RUN StarEventInitialize(lhBank).
END.

DEF INPUT  PARAMETER icFile      AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icLogFile   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiRead      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiNew       AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiUpdated   AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER oiErrors    AS INT  NO-UNDO.

DEF VAR lcLine      AS CHAR NO-UNDO.
DEF VAR lcSep       AS CHAR NO-UNDO.
DEF VAR lcOffice    AS CHAR NO-UNDO.
DEF VAR lcBankID    AS CHAR NO-UNDO.
DEF VAR lcName      AS CHAR NO-UNDO.
DEF VAR lcName2     AS CHAR NO-UNDO.
DEF VAR lcZipCode   AS CHAR NO-UNDO.
DEF VAR lcAddress   AS CHAR NO-UNDO.
DEF VAR lcCity      AS CHAR NO-UNDO.
DEF VAR ldtFileDate AS DATE NO-UNDO.
DEF VAR liCnt       AS INT  NO-UNDO.
DEF VAR ldCurrent   AS DEC  NO-UNDO.
DEF VAR llDone      AS LOG  NO-UNDO.
DEF VAR llExist     AS LOG  NO-UNDO.
DEF VAR lcValue     AS CHAR NO-UNDO.
DEF VAR liLineQty   AS INT  NO-UNDO.
DEF VAR liEmpty     AS INT  NO-UNDO.

DEF STREAM sRead.
DEF STREAM sLog.


FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):
   
   PUT STREAM sLog UNFORMATTED
      lcLine 
      lcSep
      "ERROR:" 
      icMessage
      SKIP.
   
   oiErrors = oiErrors + 1.

END FUNCTION.


IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".


INPUT STREAM sRead FROM VALUE(icFile).
OUTPUT STREAM sLog TO VALUE(icLogFile) APPEND.

ldCurrent = fMakeTS().

PUT STREAM sLog UNFORMATTED
   "File: " icFile
   SKIP
   "Started: " 
   fTS2HMS(ldCurrent)
   SKIP.

lcSep = "|".
   
PAUSE 0.
DISPLAY oiRead LABEL "Lines Read" 
WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED ROW 15 FRAME fLines.

ReadFile:
REPEAT:

   IMPORT STREAM sRead UNFORMATTED lcLine.
   
   IF NOT SESSION:BATCH THEN DO:
      PAUSE 0.
      DISPLAY oiRead WITH FRAME fLines.
   END.
         
   IF lcLine = "" THEN DO:
      liEmpty = liEmpty + 1.
      NEXT.
   END.   
   
   CASE SUBSTRING(lcLine,1,2):
   /* header */
   WHEN "00" THEN DO:

        
      lcValue = SUBSTRING(lcLine,259,8).

      ldtFileDate = DATE(INTEGER(SUBSTRING(lcValue,3,2)),
                         INTEGER(SUBSTRING(lcValue,1,2)),
                         INTEGER(SUBSTRING(lcValue,5,2)) + 2000) NO-ERROR.
      
      IF ERROR-STATUS:ERROR OR ldtFileDate = ? THEN DO:
         fError("Invalid file date").
         LEAVE ReadFile.
      END.

   END.
   
   /* bank line */
   WHEN "01" THEN DO:
   
      IF ldtFileDate = ? THEN NEXT.

      ASSIGN
         oiRead    = oiRead + 1
         lcBankID  = SUBSTRING(lcLine,3,4)
         lcOffice  = SUBSTRING(lcLine,7,4)
         lcName    = SUBSTRING(lcLine,15,36)
         lcName2   = SUBSTRING(lcLine,51,36)
         lcAddress = SUBSTRING(lcLine,87,42)
         lcCity    = SUBSTRING(lcLine,129,42)
         lcZipCode = SUBSTRING(lcLine,243,8).
        
      FIND FIRST Bank WHERE
                 Bank.Brand      = gcBrand  AND
                 Bank.BankId     = lcBankID AND
                 Bank.BankOffice = lcOffice EXCLUSIVE-LOCK NO-ERROR.
                  
      IF NOT AVAILABLE Bank THEN DO:

         CREATE Bank.
         ASSIGN 
            Bank.Brand      = gcBrand
            Bank.BankId     = lcBankID
            Bank.BankOffice = lcOffice
            oiNew           = oiNew + 1.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhBank).
      END.

      ELSE DO:
         IF Bank.FileDate NE ? AND Bank.FileDate > ldtFileDate THEN DO:
            fError("File date newer in TMS").
            NEXT.
         END.

         oiUpdated = oiUpdated + 1.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhBank).
      END.
      
      ASSIGN 
         Bank.Name     = RIGHT-TRIM(lcName)
         Bank.Name2    = RIGHT-TRIM(lcName2)
         Bank.Address  = RIGHT-TRIM(lcAddress)
         Bank.ZipCode  = RIGHT-TRIM(lcZipCode)
         Bank.City     = RIGHT-TRIM(lcCity)
         Bank.FileDate = ldtFileDate.

      IF NOT NEW Bank AND llDoEvent THEN 
         RUN StarEventMakeModifyEvent(lhBank).
      
   END.
   
   /* tailer */
   WHEN "99" THEN DO:
      liLineQty = INTEGER(SUBSTRING(lcLine,259,10)) NO-ERROR.
   END.
   
   END CASE.
  
END.

HIDE FRAME fLines NO-PAUSE.

/* compare total qty of lines (+2 = header + tailer) */
IF liLineQty NE oiRead + 2 AND 
   liLineQty NE oiRead + 2 + liEmpty
THEN DO:
   lcLine = "Total". 
   fError("Lines read: " + STRING(oiRead + 2) + 
          ", qty in file: " + STRING(liLineQty)).
END.

/* list all banks that were not in the file */
FOR EACH Bank NO-LOCK WHERE
         Bank.Brand = gcBrand AND
         Bank.FileDate NE ldtFileDate:

   IF Bank.FileDate > ldtFileDate THEN NEXT. 
    
   lcLine = Bank.BankID     + lcSep +
            Bank.BankOffice + lcSep +
            Bank.Name       + lcSep +
            Bank.Address    + lcSep + 
            Bank.City.
            
   fError("In TMS but not in file").
END.         
   

PUT STREAM sLog UNFORMATTED
   "Ended: " 
   STRING(TODAY,"99.99.9999") " " STRING(TIME,"hh:mm:ss")
   SKIP
   "Bank Rows: "
   oiRead 
   " New: "
   oiNew
   " Updated: "
   oiUpdated
   " Empty: " 
   liEmpty 
   " Errors: " 
   oiErrors
   SKIP.
   
fCleanEventObjects().
   

