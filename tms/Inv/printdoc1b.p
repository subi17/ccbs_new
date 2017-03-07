/* ---------------------------------------------------------------------------
  MODULE .......: printdoc1b.p
  KUTSUVAMODULI : 
  FUNCTION .....: batch process for printing invoices to doc1 files        
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 12.03.08
  CHANGED ......: 
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "Cron".
       
{Func/timestamp.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Syst/eventlog.i}
{Inv/printdoc1tt.i}
{Func/email.i}

DEF VAR liCnt        AS INT  NO-UNDO.
DEF VAR liRead       AS INT  NO-UNDO. 
DEF VAR liError      AS INT  NO-UNDO.
DEF VAR liFiles      AS INT  NO-UNDO.
DEF VAR lcPlainFile  AS CHAR NO-UNDO.
DEF VAR lcTransDir   AS CHAR NO-UNDO.
DEF VAR lcLine       AS CHAR NO-UNDO.
DEF VAR liInvNum     AS INT  NO-UNDO.
DEF VAR liOrder      AS INT  NO-UNDO.
DEF VAR lcPrintFile  AS CHAR NO-UNDO.
DEF VAR liInvCount   AS INT  NO-UNDO.
DEF VAR liPrinted    AS INT  NO-UNDO.
DEF VAR ldtNameDate  AS DATE NO-UNDO.
DEF VAR lcFile       AS CHAR NO-UNDO.
DEF VAR lcError      AS CHAR NO-UNDO.
DEF VAR lcDate       AS CHAR NO-UNDO.
DEF VAR llLast       AS LOG  NO-UNDO.
DEF VAR lcPrintHouse AS CHAR NO-UNDO.
DEF VAR ldStarted    AS DEC  NO-UNDO.
DEF VAR ldFinished   AS DEC  NO-UNDO.
DEF VAR liDurDays    AS INT  NO-UNDO.
DEF VAR liDurTime    AS INT  NO-UNDO.
DEF VAR lcFileType   AS CHAR NO-UNDO.
DEF VAR llSeparate   AS LOG  NO-UNDO.
DEF VAR lcLogName    AS CHAR NO-UNDO.
DEF VAR llDBWrite    AS LOG  NO-UNDO.
DEF VAR lcParam      AS CHAR NO-UNDO.
DEF VAR lcActionDir  AS CHAR NO-UNDO.
DEF VAR lcActionFile AS CHAR NO-UNDO.
DEF VAR lcActionID   AS CHAR NO-UNDO.
DEF VAR lcActionMsg  AS CHAR NO-UNDO.

DEF TEMP-TABLE ttFiles NO-UNDO
   FIELD DenyFile AS CHAR
   INDEX DenyFile DenyFile.

DEF STREAM sRead.
DEF STREAM sLog.
DEF STREAM sAction. 

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

    IF ldtNameDate = ? THEN ldtNameDate = Invoice.InvDate.

    RETURN TRUE.
    
END FUNCTION.


FIND FIRST Company WHERE
           Company.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE Company THEN ynimi = Company.CompName.

lcFile = SESSION:PARAMETER.

ASSIGN 
   lcParam   = SESSION:PARAMETER
   lcFile    = ENTRY(1,lcParam)
   llDBWrite = (ENTRY(2,lcParam) NE "no").
   
IF NUM-ENTRIES(lcParam) > 2 AND ENTRY(3,lcParam) BEGINS "XML" THEN ASSIGN
   lcFileType  = ENTRY(3,lcParam)
   llSeparate  = (lcFileType = "XMLSEP")
   lcTransDir  = fCParamC("SplitInvXMLArc")
   lcPrintFile = fCParamC("SplitInvXMLPrint")
   lcLogName   = "PrintInvXML_Batch".

ELSE ASSIGN 
   lcFileType = "Doc1"
   lcTransDir  = fCParamC("SplitDoc1Arc")
   lcPrintFile = fCParamC("SplitDoc1Print")
   lcLogName   = "PrintDoc1_Batch".

IF NOT llDBWrite THEN DO:
   lcActionDir = fCParamC("SplitActionDir").
   IF lcActionDir = ? OR lcActionDir = "" THEN lcActionDir = "/tmp".
   lcActionFile = lcActionDir + "/" + lcFileType + "_print_" +
                  STRING(YEAR(TODAY),"9999") +
                  STRING(MONTH(TODAY),"99") +
                  STRING(DAY(TODAY),"99") + "_" + 
                  REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".txt".
   OUTPUT STREAM sAction TO VALUE(lcActionFile).
END.

ASSIGN 
   lcPrintHouse = ENTRY(2,lcFile,"_")
   llLast       = FALSE
   ldStarted    = fMakeTS()
   liOrder      = INTEGER(ENTRY(1,ENTRY(4,lcFile,"_"),".")) 
   NO-ERROR.

IF liOrder = 0 THEN RETURN "ERROR:No file order nbr available".

/* no paper invoice */
IF LOOKUP(lcPrintHouse,"NOPAPER,DELTYPE10") > 0 THEN ASSIGN
   lcPrintFile = fCParamC("SplitDoc1Del10Print")
   lcLogName   = "PrintDoc1_Del10_Batch".

IF lcPrintFile = "" OR lcPrintFile = ? THEN 
   lcPrintFile = "/tmp/" + lcFileType + "_BR.txt".

IF SEARCH(lcFile) = ? THEN DO:
   fELog(lcLogName,"FileNotFound:" + lcFile).
   RETURN "ERROR:File not found".
END.

INPUT STREAM sRead FROM VALUE(lcFile).

REPEAT:
   
   IMPORT STREAM sRead UNFORMATTED lcLine.
   
   liInvNum = INTEGER(ENTRY(1,lcLine,"|")) NO-ERROR.
   
   IF ERROR-STATUS:ERROR OR liInvNum = 0 THEN DO:
      /* last file */
      IF liInvNum = 0 AND NUM-ENTRIES(lcLine,"|") >= 3 AND
         ENTRY(3,lcLine,"|") = "end" 
      THEN llLast = TRUE.
      
      NEXT.
   END.
   
   FIND Invoice WHERE Invoice.InvNum = liInvNum NO-LOCK NO-ERROR.

   IF NOT AVAILABLE Invoice THEN DO:
      NEXT.
   END.

   FIND Customer OF Invoice NO-LOCK.
    
   fMakeTemp(OUTPUT lcError).
END. 

ASSIGN
   /* invgroup to file name */
   lcPrintFile = REPLACE(lcPrintFile,"#IGRP","ALL")
   /* and printing house */
   lcPrintFile = REPLACE(lcPrintFile,"#PHOUSE",lcPrintHouse).
   
/* invoice date to file name */   
IF ldtNameDate NE ? THEN DO:
   
   lcDate = DYNAMIC-FUNCTION("fDateFmt" IN ghFunc1,
                             ldtNameDate,
                             "yyyymmdd").
   lcPrintFile = REPLACE(lcPrintFile,"#IDATE",lcDate).
END.
ELSE lcPrintFile = REPLACE(lcPrintFile,"#IDATE","").

/* order nbr */
IF NOT lcFileType BEGINS "XML" THEN 
   lcPrintFile = lcPrintFile + "_" + STRING(liOrder,"99").

/* print */
IF lcFileType BEGINS "XML" THEN 
   RUN Inv/invoice_xml.p (INPUT-OUTPUT TABLE ttInvoice,
                    ldtNameDate,
                    liInvCount,
                    llSeparate,
                    lcPrintFile,
                    lcPrintHouse,
                    0,
                    llDbWrite,
                    0,
                    0,
                    OUTPUT liPrinted).


ELSE
   RUN printdoc1 (INPUT-OUTPUT TABLE ttInvoice,  
                  liInvCount,
                  "*" + lcPrintFile,   /* *=mark transfer directory as empty */
                  lcPrintHouse,
                  INTEGER(NOT llLast), /* 0=print footer to file */
                  0,
                  0,
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
                 STRING(liDurTime,"hh:mm:ss").

/* file without the dir */
lcPlainFile = lcFile.
IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
   lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").
 
IF llDBWrite THEN DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "Invoice"  
      ActionLog.KeyValue     = "" 
      ActionLog.UserCode     = katun
      ActionLog.ActionID     = lcActionID
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
      ActionLog.ActionDec    = liPrinted
      ActionLog.ActionChar   = lcActionMsg
      ActionLog.ActionStatus = IF RETURN-VALUE BEGINS "ERROR:"
                               THEN 1
                               ELSE 2.
      ActionLog.ActionTS     = fMakeTS().
      ActionLog.KeyValue     = lcPlainFile.
END.

ELSE DO:
   PUT STREAM sAction UNFORMATTED
      lcFileType  "|"
      katun       "|"
      lcActionID  "|"
      TODAY       "|"
      "Finished"  "|"
      STRING(TIME,"hh:mm:ss") "|"
      lcPlainFile "|"
      REPLACE(lcActionMsg,CHR(10)," ")
      SKIP.

   OUTPUT STREAM sAction CLOSE.
END.
   
/* send also mail if printing was interrupted */
IF RETURN-VALUE BEGINS "ERROR:" THEN 
   RUN pSendErrorMail(RETURN-VALUE).

/* move the split file to archive */
IF liPrinted > 0 THEN RUN pTransferFile(lcFile). 

fELog(lcLogName + "_" + STRING(liOrder),"Stopped," + 
      STRING(liInvCount) + "InFile," +
      STRING(liPrinted) + "Printed").

QUIT. 

PROCEDURE pTransferFile:

   DEF INPUT PARAMETER icFile AS CHAR NO-UNDO.
   
   IF lcTransDir = "" THEN RETURN.
   
   fTransDir(icFile,
             "",
             lcTransDir).
END.

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


