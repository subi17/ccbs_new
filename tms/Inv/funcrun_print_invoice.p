/* ----------------------------------------------------------------------
  MODULE .......: FUNCRUN_PRINT_INVOICE.p
  TASK .........: Print invoices to doc1 or xml (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 29.04.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".
   
{Func/timestamp.i}
{Func/cparam2.i}
{Inv/printdoc1tt.i}
{Func/email.i}
{Syst/funcrunprocess_run.i}
{Syst/host.i}

DEF VAR ldaInvDate       AS DATE NO-UNDO.
DEF VAR liInvType        AS INT  NO-UNDO.
DEF VAR lcFileType       AS CHAR NO-UNDO.
DEF VAR liFRProcessID    AS INT  NO-UNDO.
DEF VAR liFRExecID       AS INT  NO-UNDO.
DEF VAR liFRConfigID     AS INT  NO-UNDO.
DEF VAR lcRunMode        AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT  NO-UNDO.
DEF VAR liOrder          AS INT  NO-UNDO.
DEF VAR lcPrintFile      AS CHAR NO-UNDO.
DEF VAR liInvCount       AS INT  NO-UNDO.
DEF VAR liPrinted        AS INT  NO-UNDO.
DEF VAR ldaNameDate      AS DATE NO-UNDO.
DEF VAR llLast           AS LOG  NO-UNDO.
DEF VAR lcPrintHouse     AS CHAR NO-UNDO.
DEF VAR ldStarted        AS DEC  NO-UNDO.
DEF VAR llSeparate       AS LOG  NO-UNDO.
DEF VAR lcActionID       AS CHAR NO-UNDO.
DEF VAR lcTestDir        AS CHAR NO-UNDO.
DEF VAR llTarFile        AS LOG  NO-UNDO.
DEF VAR llReplica        AS LOG  NO-UNDO.
DEF VAR llgFuncRunPDF    AS LOG  NO-UNDO.

DEF STREAM sLog.

DEFINE BUFFER bFRProcess FOR FuncRunProcess.

FUNCTION fErrorLog RETURNS LOGIC
   (iiFRExecID AS INT,
    icError    AS CHAR):
   
   DO TRANS:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = gcBrand
             ErrorLog.ActionID  = "FRINVPRINT" + STRING(iiFRExecID)
             ErrorLog.TableName = "FuncRunExec"
             ErrorLog.KeyValue  = STRING(iiFRExecID)
             ErrorLog.ErrorMsg  = icError
             ErrorLog.UserCode  = katun.
             ErrorLog.ActionTS  = fMakeTS().
   END.
   
END FUNCTION.


FUNCTION fMakeTemp RETURNS LOGICAL
   (OUTPUT ocError AS CHAR):

    IF Invoice.ExtInvID = "" THEN DO:
       ocError = "Invoice ID not set".
       RETURN FALSE.
    END.
    
    IF Invoice.InvCfg[1] = TRUE THEN DO:
       ocError = "Printing denied".
       RETURN FALSE.
    END.

    CREATE ttInvoice.
    ASSIGN ttInvoice.InvNum = Invoice.InvNum
           liInvCount       = liInvCount + 1. 

    IF Customer.IDelName > "" 
    THEN ttInvoice.ZipCode = Customer.IDelZipCode.
    ELSE ttInvoice.ZipCode = Customer.ZipCode.

    IF ldaNameDate = ? THEN ldaNameDate = Invoice.InvDate.

    RETURN TRUE.
    
END FUNCTION.


/****** Main start ********/

RUN pInitializeFuncRunProcess(OUTPUT liFRProcessID,
                              OUTPUT liFRExecID,    
                              OUTPUT lcRunMode,
                              OUTPUT liUpdateInterval).
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pGetFuncRunProcessParameters(liFRProcessID).

ASSIGN 
   ldaInvDate    = fSetFuncRunDateParameter(1)
   liInvType     = fSetFuncRunIntParameter(2)
   lcFileType    = fSetFuncRunCharParameter(3)
   llTarFile     = fSetFuncRunLogParameter(4)
   llgFuncRunPDF = fSetFuncRunLogParameter(5).
   
IF ldaInvDate = ? OR liInvType = ? OR lcFileType = ? THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,"ERROR:Invalid parameters").
   QUIT.
END.   
IF llTarFile = ? THEN llTarFile = FALSE.

RUN pInitialize.

RUN pGetFeeds.

RUN pPrintInvoices.

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pFinalizeFuncRunProcess(liFRProcessID,liPrinted).

QUIT.


/****** Main end *******/


PROCEDURE pInitialize:

   FIND FIRST Company WHERE
              Company.Brand = gcBrand NO-LOCK NO-ERROR.
   IF AVAILABLE Company THEN ynimi = Company.CompName.

   ASSIGN 
      llReplica = fIsThisReplica()
      lcPrintFile = "".

   IF lcFileType BEGINS "XML" THEN DO:
      llSeparate = (lcFileType = "XMLSEP").
      IF llReplica THEN DO:
         lcPrintFile = fCParamC("SplitInvXMLReplica").
         IF lcPrintFile = ? THEN lcPrintFile = "".
      END.
      IF lcPrintFile = "" THEN lcPrintFile = fCParamC("SplitInvXMLPrint").
   END.
   ELSE lcPrintFile = fCParamC("SplitDoc1Print").

   ASSIGN 
      llLast    = FALSE
      ldStarted = fMakeTS().

   FOR FIRST FuncRunProcess NO-LOCK WHERE
             FuncRunProcess.FRProcessID = liFRProcessID,
       FIRST FuncRunExec NO-LOCK WHERE
             FuncRunExec.FRExecID = liFRExecID,
       FIRST FuncRunResult NO-LOCK WHERE
             FuncRunResult.FRExecID    = FuncRunExec.FeedFromExecSeq AND
             FuncRunResult.FRResultSeq = FuncRunProcess.ProcSeq:

      ASSIGN 
         lcPrintHouse = FuncRunResult.CharParam
         llLast       = (FuncRunResult.DecParam = 1)
         liOrder      = FuncRunProcess.ProcSeq
         liFRConfigID = FuncRunProcess.FRConfigID.
   END.      
             
   /* no paper invoice */
   IF LOOKUP(lcPrintHouse,"NOPAPER,DELTYPE10") > 0 AND 
      NOT lcFileType BEGINS "XML" THEN
         lcPrintFile = fCParamC("SplitDoc1Del10Print").

   IF lcPrintFile = "" OR lcPrintFile = ? THEN 
      lcPrintFile = "/tmp/" + lcFileType + "_BR.txt".

   IF lcRunMode = "test" THEN lcTestDir = fCParamC("FRTestRunDir").
   
   RETURN "".
   
END PROCEDURE.

PROCEDURE pGetFeeds:

   DEF VAR lcError  AS CHAR NO-UNDO.

   FIND FIRST FuncRunProcess WHERE FuncRunProcess.FRProcessID = liFRProcessID 
      NO-LOCK.
   FIND FIRST FuncRunExec WHERE FuncRunExec.FRExecID = liFRExecID 
      NO-LOCK.
 
   FOR EACH FuncRunResult NO-LOCK WHERE
            FuncRunResult.FRExecID    = FuncRunExec.FeedFromExecSeq AND
            FuncRunResult.FRResultSeq = FuncRunProcess.ProcSeq:

      FIND Invoice WHERE Invoice.InvNum = FuncRunResult.IntParam 
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Invoice THEN NEXT.

      FIND Customer OF Invoice NO-LOCK.
    
      IF NOT fMakeTemp(OUTPUT lcError) THEN DO:
         fErrorLog(liFRExecID,
                   "ERROR:" + Invoice.ExtInvID + " " + lcError + " (" +
                   STRING(Invoice.InvNum) + ")").
      END.             
   END. 

END PROCEDURE.

PROCEDURE pPrintInvoices:

   DEF VAR lcDate       AS CHAR NO-UNDO.
   DEF VAR liDurDays    AS INT  NO-UNDO.
   DEF VAR liDurTime    AS INT  NO-UNDO.
   DEF VAR ldFinished   AS DEC  NO-UNDO.
   DEF VAR lcActionMsg  AS CHAR NO-UNDO.

   ASSIGN
      /* invgroup to file name */
      lcPrintFile = REPLACE(lcPrintFile,"#IGRP","ALL")
      /* and printing house */
      lcPrintFile = REPLACE(lcPrintFile,"#PHOUSE",lcPrintHouse).
   
   /* invoice date to file name */   
   IF ldaNameDate NE ? THEN DO:
   
      lcDate = DYNAMIC-FUNCTION("fDateFmt" IN ghFunc1,
                                ldaNameDate,
                                "yyyymmdd").
      lcPrintFile = REPLACE(lcPrintFile,"#IDATE",lcDate).
   END.
   ELSE lcPrintFile = REPLACE(lcPrintFile,"#IDATE","").

   /* order nbr */
   IF NOT lcFileType BEGINS "XML" THEN 
      lcPrintFile = lcPrintFile + "_" + STRING(liOrder,"99").

   /* print */
   IF lcFileType BEGINS "XML" THEN DO:
      IF lcRunMode = "test" AND lcTestDir > "" THEN 
         lcPrintFile = lcTestDir + "*" + lcPrintFile.

      /* tar file is not needed for no paper invoices */
      IF LOOKUP(lcPrintHouse,"NOPAPER,DELTYPE10") > 0 THEN llTarFile = FALSE.
 
      RUN Inv/invoice_xml.p (INPUT-OUTPUT TABLE ttInvoice,
                         ldaInvDate,
                         liInvCount,
                         llSeparate,
                         lcPrintFile,
                         lcPrintHouse,
                         INTEGER(llTarFile),
                         TRUE,
                         liFRProcessID,
                         liUpdateInterval,
                         OUTPUT liPrinted).
   END.

   ELSE
      RUN printdoc1.p (INPUT-OUTPUT TABLE ttInvoice,  
                       liInvCount,
                       "*" + lcPrintFile,  /* *= transfer directory as empty */
                       lcPrintHouse,
                       INTEGER(NOT llLast),/* 0=print footer to file */
                       liFRProcessID,
                       liUpdateInterval,
                       OUTPUT liPrinted). 

   ASSIGN 
      ldFinished  = fMakeTS()
      liDurDays   = DYNAMIC-FUNCTION("fTSDuration" IN ghFunc1,
                                     ldStarted,
                                     ldFinished,
                                     OUTPUT liDurTime)
      lcActionID  = IF lcFileType BEGINS "XML"
                    THEN "PRINTINVXML"
                    ELSE "PRINTDOC1"
      lcActionMsg = "Read: " + STRING(liInvCount) + 
                    " Printed: " + STRING(liPrinted) + CHR(10) +
                    (IF RETURN-VALUE BEGINS "ERROR:" 
                     THEN RETURN-VALUE + CHR(10)
                     ELSE "") +
                    "Duration was " +
                    (IF liDurDays > 0 
                     THEN STRING(liDurDays) + " days and "
                     ELSE "") +
                    STRING(liDurTime,"hh:mm:ss") + CHR(10) + 
                    "FR process: " + STRING(liFRProcessID) +
                             " (" + TRIM(lcRunMode) + ")".

   DO TRANS:
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "Invoice"  
         ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                                  STRING(MONTH(TODAY),"99") + 
                                  STRING(DAY(TODAY),"99")
         ActionLog.UserCode     = katun
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
         ActionLog.ActionDec    = liPrinted
         ActionLog.ActionChar   = lcActionMsg
         ActionLog.ActionStatus = IF RETURN-VALUE BEGINS "ERROR:"
                                  THEN 1
                                  ELSE 2.
         ActionLog.ActionTS     = fMakeTS().
   END.

   /*YTS-9144 changes done to send message to activemq only if it is a last process of the execution*/
   IF lcRunMode = "test" AND llgFuncRunPDF THEN DO:
      FIND LAST bFRProcess NO-LOCK WHERE
                bFRProcess.FRConfigID = liFRConfigID AND
                bFRProcess.FRExecID   = liFRExecID   AND
                LOOKUP(bFRProcess.RunState,"Initialized,Running") > 0 AND
                bFRProcess.FRProcessID <> liFRProcessID NO-ERROR.
      IF NOT AVAILABLE bFRProcess THEN
         RUN Inv/funcrun_invpdf_creation.p (INPUT liFRExecID) NO-ERROR.
   END.

   IF RETURN-VALUE BEGINS "ERROR:" THEN DO TRANS:
      /* send also mail if printing was interrupted */
      RUN pSendErrorMail(RETURN-VALUE).
      
      RETURN RETURN-VALUE.
   END.

   RETURN "".
   
END PROCEDURE.

PROCEDURE pSendErrorMail:

   DEF INPUT PARAMETER icError AS CHAR NO-UNDO.
   
   DEF VAR lcErrFile AS CHAR NO-UNDO.
   DEF VAR lcNewLine AS CHAR NO-UNDO.
   DEF VAR lcConfDir AS CHAR NO-UNDO.
   
   ASSIGN
      lcErrFile = "/tmp/inv" + lcFileType + "b_errmsg.txt"
      lcNewLine = CHR(13) + CHR(10).

   OUTPUT STREAM sLog TO VALUE(lcErrFile).
   PUT STREAM sLog UNFORMATTED
       "Errors from creating an invoice " + lcFileType + "-file as batch " + 
       STRING(TODAY,"99.99.9999") + "." + lcNewLine + lcNewLine +
       icError + lcNewLine. 
   OUTPUT STREAM sLog CLOSE.

   lcConfDir = fCParamC("RepConfDir").
    
   /* mail recipients AND actual sending */
   GetRecipients(lcConfDir + "inv" + 
                 (IF lcFileType BEGINS "XML" THEN "xml" ELSE "doc1") +
                 "_error.email").
   SendMail(lcErrFile,"").
    
END PROCEDURE.


