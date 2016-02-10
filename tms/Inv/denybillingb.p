/* ---------------------------------------------------------------------------
  MODULE .......: DENYBILLINGB.P
  KUTSUVAMODULI : 
  FUNCTION .....: batch process for reading billing denials from files         
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 11.02.08
  CHANGED ......: 
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "Cron".
       
{Func/cparam2.i}
{Func/ftransdir.i}
{Syst/eventlog.i}
{Func/timestamp.i}

DEF VAR liCnt       AS INT  NO-UNDO.
DEF VAR lcDenyFile  AS CHAR NO-UNDO.
DEF VAR liRead      AS INT  NO-UNDO. 
DEF VAR liExist     AS INT  NO-UNDO.
DEF VAR liError     AS INT  NO-UNDO.
DEF VAR liFiles     AS INT  NO-UNDO.
DEF VAR lcPlainFile AS CHAR NO-UNDO.
DEF VAR lcTransDir  AS CHAR NO-UNDO.
DEF VAR lcReadDir   AS CHAR NO-UNDO.
DEF VAR lcLogFile   AS CHAR NO-UNDO. 

DEF TEMP-TABLE ttFiles NO-UNDO
   FIELD DenyFile AS CHAR
   INDEX DenyFile DenyFile.

DEF STREAM sRead.

FUNCTION fCollTemp RETURNS LOGIC
   (icDenyFile AS CHAR).

   /* file not found */
   IF icDenyFile = "" OR SEARCH(icDenyFile) = ? THEN NEXT.
   
   IF CAN-FIND(FIRST ttFiles WHERE ttFiles.DenyFile = icDenyFile) THEN NEXT.
   
   CREATE ttFiles.
   ASSIGN ttFiles.DenyFile = icDenyFile.
   
END FUNCTION.


FIND FIRST Company WHERE
           Company.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE Company THEN ynimi = Company.CompName.

ASSIGN 
   lcReadDir  = fCParamC("DenyBillFiles")
   lcTransDir = fCParamC("DenyBillArc")
   lcLogFile  = fCParamC("DenyBillLog").
   
IF lcReadDir = "" OR lcReadDir = ? THEN RETURN "ERROR:Definitions missing".
   
lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                      STRING(MONTH(TODAY),"99") + 
                                      STRING(DAY(TODAY),"99")). 
                                      
fELog("READ_DENY_BILLING","Started").

RUN pFindFiles (lcReadDir,
                OUTPUT lcDenyFile).
                      
IF lcDenyFile > "" THEN 
DO liCnt = 1 TO NUM-ENTRIES(lcDenyFile,"¤"):
   fCollTemp(ENTRY(liCnt,lcDenyFile,"¤")).
END.

      
FOR EACH ttFiles:

   liFiles = liFiles + 1.
   
   RUN Inv/denybilling (ttFiles.DenyFile,
                    lcLogFile,
                    OUTPUT liRead,
                    OUTPUT liExist,
                    OUTPUT liError).
                        
   /* no events found from file -> move file anyway to processed */
   IF liRead = 0 THEN DO:
      RUN pTransferFile(ttFiles.DenyFile).
      NEXT. 
   END.

   DO TRANS:
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "Cron"  
         ActionLog.KeyValue     = "" 
         ActionLog.ActionID     = "DENYBILL"
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                  MONTH(TODAY)
         ActionLog.ActionDec    = liRead
         ActionLog.ActionChar   = "Read: " + STRING(liRead) + 
                                  " Errors: " + STRING(liError) + 
                                  " Existed: " + STRING(liExist) + 
                                  " Succesful: " + 
                                  STRING(liRead - liError - liExist)
         ActionLog.ActionStatus = 3.
         ActionLog.ActionTS     = fMakeTS().

      /* file without the dir */
      lcPlainFile = ttFiles.DenyFile.
      IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
         lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").
         
      ActionLog.KeyValue = lcPlainFile.
   
   END.

   /* delete or move the payment file to archive */
   IF liRead > 0 THEN RUN pTransferFile(ttFiles.DenyFile). 

END.

fELog("READ_DENY_BILL","Stopped,Files:" + STRING(liFiles)).


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



