/* ----------------------------------------------------------------------
  MODULE .......: combine_doc1_files.p
  TASK .........: Combine doc1 work files into the final one
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 07.07.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{ftransdir.i}
{funcrunprocess_update.i}

DEF INPUT  PARAMETER idaInvDate       AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdateInterval AS INT  NO-UNDO.
DEF INPUT  PARAMETER icRunMode        AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiHandled        AS INT  NO-UNDO.

DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR ldaDate       AS DATE NO-UNDO.
DEF VAR lcDoc1File    AS CHAR NO-UNDO.
DEF VAR lcTargetFile  AS CHAR NO-UNDO.
DEF VAR lcTransDir    AS CHAR NO-UNDO.
DEF VAR liPHousePos   AS INT  NO-UNDO.
DEF VAR liCnt         AS INT  NO-UNDO.
DEF VAR lcFileDate    AS CHAR NO-UNDO.
DEF VAR lcAppend      AS CHAR NO-UNDO.
DEF VAR lcFinalFile   AS CHAR NO-UNDO.
DEF VAR liResultOrder AS INT  NO-UNDO.
DEF VAR lcArchiveDir  AS CHAR NO-UNDO.

DEF TEMP-TABLE ttFile NO-UNDO
   FIELD FileName AS CHAR
   FIELD PrintHouse AS CHAR
   INDEX FileName PrintHouse FileName.
   
DEF STREAM sRead.


ASSIGN
   lcDoc1File   = fCParamC("SplitDoc1Print")
   lcTargetFile = fCParamC("SplitDoc1FinalFile")
   lcArchiveDir = fCParamC("SplitDoc1PartialArc")
   lcFileDate   = STRING(YEAR(idaInvDate),"9999") +
                  STRING(MONTH(idaInvDate),"99") +
                  STRING(DAY(idaInvDate),"99").

IF icRunMode = "test" THEN lcTransDir = fCParamC("FRTestRunDir").
ELSE lcTransDir = fCParamC("SplitDoc1Trans").
IF lcTransDir = ? THEN lcTransDir = "".   

IF lcDoc1File = ? OR lcDoc1File = "" THEN 
   RETURN "ERROR:Source file name not defined".
IF lcTargetFile = ? OR lcTargetFile = "" THEN 
   RETURN "ERROR:Target file name not defined".

DO liCnt = 1 TO NUM-ENTRIES(lcDoc1File,"_"):
   IF ENTRY(liCnt,lcDoc1File,"_") = "#PHOUSE" THEN liPHousePos = liCnt.
END.

ASSIGN
   lcDoc1File = REPLACE(lcDoc1File,"#PHOUSE","*")
   lcDoc1File = REPLACE(lcDoc1File,"#IGRP","*")
   lcDoc1File = REPLACE(lcDoc1File,"#IDATE",lcFileDate)
   lcDoc1File = lcDoc1File + "*".                                          
                                            
INPUT STREAM sRead THROUGH VALUE("ls -1 " + lcDoc1File).
REPEAT:
   IMPORT STREAM sRead lcFile.
   IF lcFile = "" THEN NEXT.
   
   FILE-INFO:FILE-NAME = lcFile.
   IF FILE-INFO:FILE-TYPE BEGINS "F" THEN DO:
      CREATE ttFile.
      ttFile.FileName = lcFile.
   END.

   IF liPHousePos > 0 THEN 
      ttFile.PrintHouse = ENTRY(liPHousePos,ttFile.FileName,"_").
END.

INPUT STREAM sRead CLOSE.


FOR EACH ttFile 
BREAK BY ttFile.PrintHouse
      BY ttFile.FileName:

   IF FIRST-OF(ttFile.PrintHouse) THEN DO:
      ASSIGN 
         lcAppend = ""
         lcFile   = REPLACE(lcTargetFile,"#PHOUSE",ttFile.PrintHouse)
         lcFile   = REPLACE(lcFile,"#IDATE",lcFileDate)
         lcFile   = fUniqueFileName(lcFile,"").   
   END.
   ELSE lcAppend = ">".

   UNIX SILENT VALUE("cat " + ttFile.FileName + " >" + lcAppend +
                     " " + lcFile).

   oiHandled = oiHandled + 1.

   IF LAST-OF(ttFile.PrintHouse) THEN DO:
      ASSIGN 
         lcFinalFile   = ""
         liResultOrder = liResultOrder + 1.
         
      IF lcTransDir > "" THEN DO:
         lcFinalFile = fMove2TransDir(lcFile,
                                      "",
                                      lcTransDir).
      END.
      IF lcFinalFile = "" THEN lcFinalFile = lcFile.
      
      IF iiFRProcessID > 0 THEN DO TRANS:
         CREATE FuncRunResult.
         ASSIGN 
            FuncRunResult.FRProcessID = iiFRProcessID
            FuncRunResult.FRResultSeq = 1
            FuncRunResult.ResultOrder = liResultOrder
            FuncRunResult.CharParam   = lcFinalFile.
      END.
   END.
 
   IF iiUpdateInterval > 0 AND oiHandled MOD iiUpdateInterval = 0 THEN DO:
      IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiHandled) THEN
         RETURN "ERROR:Stopped".
   END.   
    
END.

IF oiHandled > 0 THEN DO:
 
   /* move handled (partial) files to archive */
   IF lcArchiveDir > "" THEN 
   FOR EACH ttFile:
      fTransDir(ttFile.FileName,
                "",
                lcArchiveDir).
   END.

   lcDoc1File = lcArchiveDir + "/" + 
                ENTRY(NUM-ENTRIES(lcDoc1File,"/"),lcDoc1File,"/").
                
   /* do this here separately instead of for each file in the previous loop, 
      so that there will be only one process in os */
   UNIX SILENT VALUE("gzip " + lcDoc1File + "&").
END.

RETURN "".


