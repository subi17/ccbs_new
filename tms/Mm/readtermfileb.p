/* ---------------------------------------------------------------------------
  MODULE .......: READTERMFILEB.P
  KUTSUVAMODULI : 
  FUNCTION .....: batch process for reading subscription terminations 
                  from files            
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 25.03.08
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
{Func/multitenantfunc.i}

DEF VAR liCnt       AS INT  NO-UNDO.
DEF VAR lcTermFile AS CHAR NO-UNDO.
DEF VAR liRead      AS INT  NO-UNDO. 
DEF VAR liError     AS INT  NO-UNDO.
DEF VAR liFiles     AS INT  NO-UNDO.
DEF VAR lcPlainFile AS CHAR NO-UNDO.
DEF VAR lcTransDir  AS CHAR NO-UNDO.
DEF VAR lcReadDir   AS CHAR NO-UNDO.
DEF VAR lcLogFile   AS CHAR NO-UNDO. 
DEF VAR lcLogTrans  AS CHAR NO-UNDO.
DEF VAR lcFoundTenants AS CHAR NO-UNDO.
DEF VAR liCounter   AS INT NO-UNDO.
DEF VAR lcTenant    AS CHAR NO-UNDO.

DEF TEMP-TABLE ttFiles NO-UNDO
   FIELD TermFile AS CHAR
   INDEX TermFile TermFile.

DEF STREAM sRead.

FUNCTION fCollTemp RETURNS LOGIC
   (icTermFile AS CHAR).

   /* file not found */
   IF icTermFile = "" OR SEARCH(icTermFile) = ? THEN NEXT.
   
   IF CAN-FIND(FIRST ttFiles WHERE ttFiles.TermFile = icTermFile) THEN NEXT.
   
   CREATE ttFiles.
   ASSIGN ttFiles.TermFile = icTermFile.
   
END FUNCTION.

ASSIGN
   lcReadDir  = fCParamC("SubsTermFiles")
   lcTransDir = fCParamC("SubsTermArc")
   lcLogFile  = fCParamC("SubsTermLog")
   lcLogTrans = fCParamC("SubsTermLogTrans").
   
IF lcReadDir = "" OR lcReadDir = ? THEN RETURN "ERROR:Definitions missing".
   
ASSIGN
   lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                         STRING(MONTH(TODAY),"99") + 
                                         STRING(DAY(TODAY),"99"))
   lcLogFile = REPLACE(lcLogFile,"#TIME",STRING(TIME)).
                                       
fELog("READTERM","started").

RUN pFindFiles (lcReadDir,
                OUTPUT lcTermFile).
                      
IF lcTermFile > "" THEN 
DO liCnt = 1 TO NUM-ENTRIES(lcTermFile,"¤"):
   fCollTemp(ENTRY(liCnt,lcTermFile,"¤")).
END.

      
FOR EACH ttFiles:

   liFiles = liFiles + 1.
      
   lcPlainFile = ttFiles.TermFile.
   IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
      lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").

   /* Set effective tenant based on file name. If not regocniced go next file
   */
   lcTenant  = ENTRY(1,lcPlainFile,"_").
   IF NOT fsetEffectiveTenantForAllDB(
      fConvertBrandToTenant(lcTenant)) THEN NEXT.

   IF CAN-FIND (FIRST ActionLog NO-LOCK WHERE
                      ActionLog.Brand = gcBrand AND
                      ActionLog.TableName = "Cron" AND
                      ActionLog.KeyValue = lcPlainFile AND
                      ActionLog.ActionID = "SubsTerm" AND
                      ActionLog.ActionStatus = 0) THEN NEXT.

   DO TRANS:
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "Cron"  
         ActionLog.KeyValue     = lcPlainFile
         ActionLog.ActionID     = "SubsTerm"
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                  MONTH(TODAY)
         ActionLog.ActionStatus = 0
         ActionLog.ActionTS     = fMakeTS().
   END.
   
   RUN Mm/readtermfile.p (ttFiles.TermFile,
                     REPLACE(lcLogFile,"#TENANT",lcTenant),
                     OUTPUT liRead,
                     OUTPUT liError).
   IF lcFoundTenants EQ "" THEN
      lcFoundTenants = lcTenant.
   ELSE IF INDEX(lcFoundTenants,lcTenant) EQ 0 THEN
      lcFoundTenants = lcFoundTenants + "," + lcTenant.
     
   DO TRANS:
      ASSIGN 
         ActionLog.ActionDec    = liRead
         ActionLog.ActionChar   = "Read: " + STRING(liRead) + 
                                  " Errors: " + STRING(liError) + 
                                  " Succesful: " + STRING(liRead - liError) + 
                                  CHR(10) + "Finished: " + fTS2HMS(fMakeTS())
         ActionLog.ActionStatus = 3.
   END.
   
   /* delete or move the files to archive, even if no lines were read */
   RUN pTransferFile(ttFiles.TermFile,
                     lcTransDir). 
                     
END.

fELog("READTERM","stopped,Files:" + STRING(liFiles)).

DO liCounter = 1 TO NUM-ENTRIES(lcFoundTenants):

   /* move the log file to transfer directory */
   RUN pTransferFile(REPLACE(lcLogFile,"#TENANT",
                             ENTRY(liCounter,lcFoundTenants)),
                     lcLogTrans).
END.

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

   DEF INPUT PARAMETER icFile     AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icTransDir AS CHAR NO-UNDO.
   
   IF icTransDir = "" THEN RETURN.
   
   fTransDir(icFile,
             "",
             icTransDir).
END.



