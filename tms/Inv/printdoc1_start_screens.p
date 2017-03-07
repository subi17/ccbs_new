/* ---------------------------------------------------------------------------
  MODULE .......: printdoc1_start_screens.p
  KUTSUVAMODULI : 
  FUNCTION .....: batch process for creating screens to print doc1 files       
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 02.09.08
  CHANGED ......: 
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

DEF INPUT PARAMETER idtInvDate AS DATE NO-UNDO.
DEF INPUT PARAMETER icFileList AS CHAR NO-UNDO.
DEF INPUT PARAMETER icFileType AS CHAR NO-UNDO.
DEF INPUT PARAMETER ilDBWrite  AS LOG  NO-UNDO. 

DEF VAR lcReadDir    AS CHAR NO-UNDO.
DEF VAR lcCommand    AS CHAR NO-UNDO.
DEF VAR lcPlainFile  AS CHAR NO-UNDO.
DEF VAR lcRun        AS CHAR NO-UNDO.
DEF VAR liFile       AS INT  NO-UNDO.
DEF VAR lcFilePref   AS CHAR NO-UNDO.
DEF VAR lcRunParam   AS CHAR NO-UNDO.

DEF TEMP-TABLE ttFile
   FIELD PrintFile AS CHAR.
   
DEF STREAM sRead.
DEF STREAM sLog.

IF icFileType BEGINS "XML" THEN ASSIGN 
   lcCommand  = fCParamC("InvXMLScreenCommand")
   lcReadDir  = fCParamC("SplitInvXMLDir")
   lcFilePref = "invxml_*".

ELSE ASSIGN
   lcCommand  = fCParamC("Doc1ScreenCommand")
   lcReadDir  = fCParamC("SplitDoc1Dir")
   lcFilePref = "doc1_*".

IF lcReadDir = "" OR lcReadDir = ? THEN lcReadDir = "/tmp".

IF lcCommand = "" THEN RETURN "ERROR:Command not specified".

/* try to find files if list was not given */
IF icFileList = "" THEN DO:

   lcReadDir = lcReadDir + "/" + lcFilePref + STRING(idtInvDate,"999999") + 
               "*.txt".

   RUN pFindFiles(lcReadDir).
END.

ELSE DO liFile = 1 TO NUM-ENTRIES(icFileList):
   CREATE ttFile.
   ttFile.PrintFile = ENTRY(liFile,icFileList).
END.

IF NOT CAN-FIND(FIRST ttFile) THEN RETURN "ERROR:No files".

OUTPUT STREAM sLog TO VALUE(lcReadDir + "/screen_start.log") APPEND.

/* create screen for each file */
FOR EACH ttFile:

   /* file without the dir */
   lcPlainFile = ttFile.PrintFile.
   IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
      lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").

   lcRunParam = ttFile.PrintFile + "," + STRING(ilDBWrite).
   IF icFileType BEGINS "XML" THEN
      lcRunParam = lcRunParam + "," + icFileType.
      
   ASSIGN
      lcRun = REPLACE(lcCommand,"#PARAM",lcRunParam)
      lcRun = REPLACE(lcRun,"#NAME","-t " + lcPlainFile).

   PUT STREAM sLog UNFORMATTED
      STRING(TODAY,"99.99.99") " " 
      STRING(TIME,"hh:mm:ss") " "
      lcRun SKIP.

   PAUSE 2 NO-MESSAGE.    
   UNIX SILENT VALUE(lcRun).
END.

OUTPUT STREAM sLog CLOSE.

RETURN "".


PROCEDURE pFindFiles:

   DEF INPUT  PARAMETER icFilter AS CHAR NO-UNDO.
       
   DEF VAR lcFileName AS CHAR NO-UNDO.

   
   INPUT STREAM sRead THROUGH VALUE("ls -1 " + icFilter).

   REPEAT:
      IMPORT STREAM sRead UNFORMATTED lcFileName.
      
      IF lcFileName = "" OR 
         lcFileName MATCHES ("*o such file or*") 
      THEN NEXT.
      
      CREATE ttFile.
      ttFile.PrintFile = lcFileName.
   END.
                        
   INPUT STREAM sRead CLOSE.

END PROCEDURE.

