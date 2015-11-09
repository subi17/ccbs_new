/* ----------------------------------------------------------------------
  MODULE .......: FUNCRUN_INVOICE_XML_TAR_REPORT.p
  TASK .........: Print invoices to xml (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: sanjeevi
  CREATED ......: 08.01.2015
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".
   
{timestamp.i}
{cparam2.i}
{funcrunprocess_run.i}

DEF VAR lcSep            AS CHAR NO-UNDO.
DEF VAR lcFile           AS CHAR NO-UNDO.
DEF VAR ldaInvDate       AS DATE NO-UNDO.
DEF VAR lcPHList         AS CHAR NO-UNDO.
DEF VAR ldFrom           AS DEC  NO-UNDO.
DEF VAR ldTo             AS DEC  NO-UNDO.
DEF VAR lcDate           AS CHAR NO-UNDO.
DEF VAR liFRProcessID    AS INT  NO-UNDO.
DEF VAR liFRExecID       AS INT  NO-UNDO.
DEF VAR lcRunMode        AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT  NO-UNDO.
DEF VAR liPrinted        AS INT  NO-UNDO.
DEF VAR liCount          AS INT  NO-UNDO.
DEF VAR liQueueID        AS INT  NO-UNDO.   
DEF VAR liFRCount        AS INT  NO-UNDO. 

DEFINE TEMP-TABLE ttFuncRunResult NO-UNDO 
    FIELD fname AS CHARACTER 
    FIELD fsize AS DECIMAL 
    FIELD fQty  AS INTEGER 
    FIELD fMD5  AS CHARACTER.

DEFINE BUFFER bFuncRunResult FOR FuncRunResult.

DEFINE STREAM slog.

/************* Main Start ***************/

RUN pInitializeFuncRunProcess(OUTPUT liFRProcessID,
                              OUTPUT liFRExecID,   
                              OUTPUT lcRunMode,
                              OUTPUT liUpdateInterval).
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pGetFuncRunProcessParameters(liFRProcessID).

ASSIGN lcPHList   = fSetFuncRunCharParameter(1)
       ldaInvDate = fSetFuncRunDateParameter(2)
       liQueueID  = fSetFuncRunIntParameter(3).

ASSIGN
   lcDate    = STRING(YEAR(TODAY))       +
               STRING(MONTH(TODAY),"99") +
               STRING(DAY(TODAY),"99")
   ldFrom    = fmake2dt(ldaInvDate,0)
   ldTo      = fmake2dt(TODAY,86399) 
   liFRCount = 0.
   
DO liCount = 1 TO NUM-ENTRIES(lcPHList):
   
   EMPTY TEMP-TABLE ttFuncRunResult.

   ASSIGN
      lcFile = fCParamC("InvXMLSpoolDir")
      lcFile = REPLACE(lcFile,"#PHOUSE", ENTRY(liCount,lcPHList))
      lcFile = REPLACE(lcFile,"#YYYYMMDD",lcDate).
   
   FOR EACH  FuncRunQSchedule NO-LOCK WHERE
             FuncRunQSchedule.FRQueueid = liQueueID     AND
             FuncRunQSchedule.StartTS  >= ldFrom        AND
             FuncRunQSchedule.StartTS  <= ldTo          AND
             FuncRunQSchedule.RunState NE "Initialized",
        EACH FuncRunExec NO-LOCK WHERE
             FuncRunExec.Brand         EQ gcBrand AND
             FuncRunExec.FRQScheduleID EQ FuncRunQSchedule.FRQScheduleID AND
             FuncRunExec.RunState      EQ "Finished",
        EACH FuncRunConfig NO-LOCK WHERE
             FuncRunConfig.FRConfigID EQ FuncRunExec.FRConfigID AND
             FuncRunConfig.ConfName   EQ "PrintXML",
        EACH FuncRunQSParam NO-LOCK WHERE
             FuncRunQSParam.FRQScheduleID EQ FuncRunExec.FRQScheduleID AND
             FuncRunQSParam.FRQRowSeq EQ FuncRunExec.FRQRowSeq AND
             FuncRunQSParam.ParamSeq  EQ 1 AND /* First parameter for FR is invoice date */
             FuncRunQSParam.DateParam EQ ldaInvDate, /* Value of that parameter */
        EACH FuncRunProcess NO-LOCK WHERE
             FuncRunProcess.FRConfigID EQ FuncRunExec.FRConfigID AND
             FuncRunProcess.FRExecID   EQ FuncRunExec.FRExecID   AND
             FuncRunProcess.RunState   EQ "Finished",
        EACH FuncRunResult NO-LOCK WHERE
             FuncRunResult.FRProcessID EQ FuncRunProcess.FRProcessID AND
             FuncRunResult.FRExecID    EQ FuncRunProcess.FRExecID    AND
             FuncRunResult.CharParam BEGINS "YOIGOBR_" AND
             INDEX (FuncRunResult.CharParam, (ENTRY(liCount,lcPHList))) > 0
    BREAK BY FuncRunExec.FRQScheduleID
          BY FuncRunProcess.FRProcessID
          BY FuncRunResult.FRResultSeq
          BY FuncRunResult.ResultOrder:


      /* FRQScheduleID cannot be used here but commented only if some other needed
      IF NOT FIRST-OF(FuncRunExec.FRQScheduleID) THEN NEXT. */

      CREATE ttFuncRunResult.
      ASSIGN
         ttFuncRunResult.fname = ENTRY(1,funcrunresult.charparam," ")
         ttFuncRunResult.fsize = funcrunresult.decparam
         ttFuncRunResult.fQty  = funcrunresult.intparam
         ttFuncRunResult.fMD5  = ENTRY(2,ENTRY(2,funcrunresult.charparam," "),":").

   END.
   
   IF CAN-FIND(FIRST ttFuncRunResult NO-LOCK) THEN DO:
      OUTPUT STREAM slog TO VALUE(lcFile).
   
      liFRCount = liFRCount + 1. 
      
      lcSep = ";".
   
      PUT STREAM slog UNFORMATTED
          "File"    lcSep
          "Size"    lcSep
          "XML Qty" lcSep
          "MD5"     SKIP.    
       
      FOR EACH ttFuncRunResult NO-LOCK:
          PUT STREAM slog UNFORMATTED
              ttFuncRunResult.fname  lcSep
              ttFuncRunResult.fsize  lcSep
              ttFuncRunResult.fQty   lcSep
              ttFuncRunResult.fMD5   SKIP.
      END.

      CREATE bFuncRunResult.
      ASSIGN bFuncRunResult.FRProcessID = liFRProcessID
             bFuncRunResult.FRExecID    = liFRExecID 
             bFuncRunResult.FRResultSeq = liFRCount
             bFuncRunResult.CharParam   = lcFile.     
      
      liPrinted = liPrinted + 1. 
     
     OUTPUT STREAM slog CLOSE.
   END.
    
END. /* DO liCount = 1 TO NUM-ENTRIES(lcPHList) */

RUN pFinalizeFuncRunProcess(liFRProcessID,liPrinted).

QUIT.
