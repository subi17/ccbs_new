/* ---------------------------------------------------------------------------
  MODULE .......: READORDERB.P
  KUTSUVAMODULI : 
  FUNCTION .....: batch process for reading orders from files            
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 07.11.07
  CHANGED ......: 
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "Cron".
       
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/lib/eventlog.i}
{Func/timestamp.i}

DEF INPUT PARAMETER icFileType AS CHAR NO-UNDO.

DEF VAR liCnt       AS INT  NO-UNDO.
DEF VAR lcOrderFile AS CHAR NO-UNDO.
DEF VAR liRead      AS INT  NO-UNDO. 
DEF VAR liError     AS INT  NO-UNDO.
DEF VAR liFiles     AS INT  NO-UNDO.
DEF VAR lcPlainFile AS CHAR NO-UNDO.
DEF VAR lcTransDir  AS CHAR NO-UNDO.
DEF VAR lcReadDir   AS CHAR NO-UNDO.
DEF VAR lcLogFile   AS CHAR NO-UNDO. 

DEF TEMP-TABLE ttFiles NO-UNDO
   FIELD OrderFile AS CHAR
   INDEX OrderFile OrderFile.

DEF STREAM sRead.

FUNCTION fCollTemp RETURNS LOGIC
   (icOrderFile AS CHAR).

   /* file not found */
   IF icOrderFile = "" OR SEARCH(icOrderFile) = ? THEN NEXT.
   
   IF CAN-FIND(FIRST ttFiles WHERE ttFiles.OrderFile = icOrderFile) THEN NEXT.
   
   CREATE ttFiles.
   ASSIGN ttFiles.OrderFile = icOrderFile.
   
END FUNCTION.


FIND FIRST Company WHERE
           Company.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE Company THEN ynimi = Company.CompName.

CASE icFileType:
WHEN "gift" THEN ASSIGN 
   lcReadDir  = fCParamC("GiftOrderFiles")
   lcTransDir = fCParamC("GiftOrderArc")
   lcLogFile  = fCParamC("GiftOrderLog").
WHEN "preact" THEN ASSIGN
   lcReadDir  = fCParamC("PreActOrderFiles")
   lcTransDir = fCParamC("PreActOrderArc")
   lcLogFile  = fCParamC("PreActOrderLog").
OTHERWISE RETURN "ERROR:Unknown file type".
END CASE. 
   
   
IF lcReadDir = "" OR lcReadDir = ? THEN RETURN "ERROR:Definitions missing".
   
lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                      STRING(MONTH(TODAY),"99") + 
                                      STRING(DAY(TODAY),"99")). 
                                      
fELog("READORDER",CAPS(icFileType) + "started").

RUN pFindFiles (lcReadDir,
                OUTPUT lcOrderFile).
                      
IF lcOrderFile > "" THEN 
DO liCnt = 1 TO NUM-ENTRIES(lcOrderFile,"¤"):
   fCollTemp(ENTRY(liCnt,lcOrderFile,"¤")).
END.

      
FOR EACH ttFiles:

   liFiles = liFiles + 1.
   
   RUN Mm/readorderfile (icFileType,
                      ttFiles.OrderFile,
                      lcLogFile,
                      OUTPUT liRead,
                      OUTPUT liError).
                        
   /* no events found from file -> move file anyway to processed */
   IF liRead = 0 THEN DO:
      RUN pTransferFile(ttFiles.OrderFile).
      NEXT. 
   END.

   DO TRANS:
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "Cron"  
         ActionLog.KeyValue     = "" 
         ActionLog.ActionID     = icFileType + "Order"
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                  MONTH(TODAY)
         ActionLog.ActionDec    = liRead
         ActionLog.ActionChar   = "Read: " + STRING(liRead) + 
                                  " Errors: " + STRING(liError) + 
                                  " Succesful: " + STRING(liRead - liError)
         ActionLog.ActionStatus = 3.
         ActionLog.ActionTS     = fMakeTS().

      /* file without the dir */
      lcPlainFile = ttFiles.OrderFile.
      IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
         lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").
         
      ActionLog.KeyValue = lcPlainFile.
   
   END.

   /* delete or move the payment file to archive */
   IF liRead > 0 THEN RUN pTransferFile(ttFiles.OrderFile). 

END.

fELog("READORDER",CAPS(icFileType) + "stopped,Files:" + STRING(liFiles)).


PROCEDURE pFindFiles:

   DEF INPUT  PARAMETER icFilter AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER ocFiles  AS CHAR NO-UNDO. 
       
   DEF VAR lcFileName AS CHAR NO-UNDO.
   
   INPUT STREAM sRead THROUGH VALUE("ls -1 " + icFilter).

   REPEAT:
      IMPORT STREAM sRead UNFORMATTED lcFileName.
      
      IF lcFileName = "" OR 
         lcFileName MATCHES ("*o such file or*") 
      THEN NEXT.
      
      ocFiles = ocFiles + 
                (IF ocFiles > "" THEN "¤" ELSE "") + 
                lcFileName.
   END.
                        
   INPUT STREAM sRead CLOSE.

END PROCEDURE.

PROCEDURE pTransferFile:

   DEF INPUT PARAMETER icFile AS CHAR NO-UNDO.
   
   IF lcTransDir = "" THEN RETURN.
   
   fTransDir(icFile,
             "",
             lcTransDir).
END.



