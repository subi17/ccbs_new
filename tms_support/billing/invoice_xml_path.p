{commpaa.i}
ASSIGN
   gcBrand = "1"
   katun = "cron".

{cparam2.i}
{timestamp.i}

DEF VAR lcFile      AS CHAR NO-UNDO.
DEF VAR lcDir       AS CHAR NO-UNDO.
DEF VAR ldaRunDate  AS DATE NO-UNDO.
DEF VAR liInv       AS INT  NO-UNDO.
DEF VAR ldBegin     AS DEC  NO-UNDO.
DEF VAR ldEnd       AS DEC  NO-UNDO.
DEF VAR lcResultDir AS CHAR NO-UNDO.
DEF VAR liLoop      AS INT  NO-UNDO.
DEF VAR lcFileDir   AS CHAR NO-UNDO.
DEF VAR lcCheckDir  AS CHAR NO-UNDO.
DEF VAR llCanCheck  AS LOG  NO-UNDO.
DEF VAR liQueueID   AS INT  NO-UNDO INIT 3.

DEF TEMP-TABLE ttPrintHouse NO-UNDO
   FIELD PrintHouse AS CHAR
   FIELD InvNum AS INT
   FIELD XMLPath AS CHAR
   INDEX PrintHouse PrintHouse InvNum.

DEF STREAM sFile.


FUNCTION fTargetDir RETURNS CHAR
   (INPUT icDir      AS CHAR,
    INPUT idaInvDate AS DATE,
    INPUT iiInvNum   AS INT):

   DEF VAR lcDirName AS CHAR NO-UNDO.
   DEF VAR lcInvNum  AS CHAR NO-UNDO.
   DEF VAR lcFirst   AS CHAR NO-UNDO.
   DEF VAR lcSecond  AS CHAR NO-UNDO.
   DEF VAR liDir     AS INT  NO-UNDO.
   
   ASSIGN 
      lcInvNum  = STRING(iiInvNum)                      
      lcFirst   = SUBSTRING(lcInvNum,LENGTH(lcInvNum) - 1,2).
      
   IF LENGTH(lcInvNum) >= 4 THEN 
      lcSecond = SUBSTRING(lcInvNum,LENGTH(lcInvNum) - 3,2).
   ELSE lcSecond = "XX".
 
   ASSIGN 
      icDir = REPLACE(icDir,"#BILLRUN",STRING(YEAR(idaInvDate)) +
                                       STRING(MONTH(idaInvDate),"99") +
                                       STRING(DAY(idaInvDate),"99"))
      icDir = REPLACE(icDir,"#FF",lcFirst)
      icDir = REPLACE(icDir,"#SS",lcSecond).
      
   RETURN icDir.
   
END FUNCTION.
 

ASSIGN 
   lcFile = fCParamC("InvXMLFile")
   lcFile = ENTRY(NUM-ENTRIES(lcFile,"/"),lcFile,"/").
   lcDir  = fCParamC("InvXMLTransDir").


IF lcFile = "" OR lcDir = "" THEN DO:
   MESSAGE "Invalid file and directory definition"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

ASSIGN 
   lcFileDir = lcDir
   liLoop = INDEX(lcFileDir,"#BILLRUN").
IF liLoop > 0 THEN 
   lcFileDir = "./" + SUBSTRING(lcFileDir,liLoop).

DO liLoop = 2 TO NUM-ENTRIES(lcDir,"/"):
   lcCheckDir = lcCheckDir + "/" + ENTRY(liLoop,lcDir,"/").
   IF ENTRY(liLoop,lcDir,"/") = "#BILLRUN" THEN LEAVE.
END.


PAUSE 0.
UPDATE 
   ldaRunDate FORMAT "99-99-9999" COLON 18
      LABEL "Run Date" HELP "Date of billing process" SKIP
   liQueueID FORMAT ">>9" COLON 18
      LABEL "Queue ID" HELP "Funcrun queue ID" SKIP
   lcResultDir FORMAT "X(40)" COLON 18
      LABEL "Result Directory" HELP "Directory for result files"
   WITH SIDE-LABELS OVERLAY ROW 10 CENTERED TITLE " Invoice XML Paths "
   FRAME fInv.
HIDE FRAME fInv NO-PAUSE.

IF ldaRunDate = ? OR lcResultDir = "" OR liQueueID = 0 THEN RETURN.

lcCheckDir = REPLACE(lcCheckDIr,"#BILLRUN",STRING(YEAR(ldaRunDate)) +
                                           STRING(MONTH(ldaRunDate),"99") +
                                           STRING(DAY(ldaRunDate),"99")).
FILE-INFO:FILE-NAME = lcCheckDir + "/01".
llCanCheck = (FILE-INFO:FILE-TYPE NE ?).

ASSIGN 
   ldBegin = fMake2DT(ldaRunDate,0)
   ldEnd   = fMake2DT(ldaRunDate,86399).
  
FOR FIRST FuncRunQSchedule NO-LOCK WHERE
          FuncRunQSchedule.FRQueueID = liQueueID AND
          FuncRunQSchedule.StartTS >= ldBegin AND
          FuncRunQSchedule.StartTS < ldEnd AND
          FuncRunQSchedule.RunMode = "Production",
     EACH FuncRunExec USE-INDEX FRQScheduleID NO-LOCK WHERE
          FuncRunExec.FRQScheduleID = FuncRunQSchedule.FRQScheduleID AND
          FuncRunExec.FRConfigID = 5,
     EACH FuncRunResult NO-LOCK WHERE
          FuncRunResult.FRExecID = FuncRunExec.FRExecID,
    FIRST Invoice NO-LOCK WHERE
          Invoice.InvNum = FuncRunResult.IntParam:
          
   CREATE ttPrintHouse.
   ASSIGN 
      ttPrintHouse.PrintHouse = FuncRunResult.CharParam
      ttPrintHouse.InvNum     = Invoice.InvNum
      ttPrintHouse.XMLPath    = fTargetDir(lcFileDir,
                                           Invoice.InvDate,
                                           Invoice.InvNum) + "/" +
                                REPLACE(lcFile,
                                        "#INVNUM",
                                        STRING(Invoice.InvNum)).

   IF llCanCheck THEN DO:
      lcCheckDir = fTargetDir(lcDir,
                              Invoice.InvDate,
                              Invoice.InvNum) + "/" +
                   REPLACE(lcFile,
                           "#INVNUM",
                           STRING(Invoice.InvNum)).
      FILE-INFO:FILE-NAME = lcCheckDir.
      IF FILE-INFO:FILE-TYPE = ? THEN DO:
         DELETE ttPrintHouse.
         NEXT.
      END.
   END.

   liInv = liInv + 1.
   IF liInv MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liInv LABEL "Qty" FORMAT ">>>>>>>>9"
        WITH SIDE-LABELS OVERLAY ROW 10 CENTERED TITLE " Invoices " FRAME fGet.
   END.
        
   /*
   DISPLAY 
      Invoice.InvDate
      Invoice.InvNum
      Invoice.PrintState
      FuncRunResult.CharParam FORMAT "x(20)".
   disp ttPrintHouse.XMLPath format "x(50)"
        llcancheck.
   */
END.

HIDE FRAME fGet NO-PAUSE.

FOR EACH ttPrintHouse
BREAK BY ttPrintHouse.PrintHouse:

   IF FIRST-OF(ttPrintHouse.PrintHouse) THEN DO:
      OUTPUT STREAM sFile TO 
         VALUE(lcResultDir + "/" + ttPrintHouse.PrintHouse + "_xmlpath.txt").
   END.

   PUT STREAM sFile UNFORMATTED
      ttPrintHouse.XMLPath SKIP.
      
   IF LAST-OF(ttPrintHouse.PrintHouse) THEN 
      OUTPUT STREAM sFile CLOSE.
END.

          
