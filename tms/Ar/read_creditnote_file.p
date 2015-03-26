/* ----------------------------------------------------------------------
  MODULE .......: read_creditnote_file.p
  TASK .........: Creates credit notes through batch files (YOT-1058)
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 14.01.11
  CHANGED ......: 
  Version ......: yoigo
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{tmsconst.i}
{ftransdir.i}
{cparam2.i}
{eventlog.i}
{timestamp.i}
{fcreditreq.i}

/* files and dirs */
DEF VAR lcLine AS CHAR NO-UNDO.
DEF VAR lcLogFile AS CHAR NO-UNDO. 
DEF VAR lcFileName AS CHAR NO-UNDO. 
DEF VAR lcIncDir  AS CHAR NO-UNDO. 
DEF VAR lcInputFile AS CHAR NO-UNDO. 
DEF VAR lcProcDir AS CHAR NO-UNDO. 
DEF VAR lcProcessedFile AS CHAR NO-UNDO. 
DEF VAR lcSpoolDir AS CHAR NO-UNDO. 
DEF VAR lcReportFileOut AS CHAR NO-UNDO. 
DEF VAR lcOutDir AS CHAR NO-UNDO. 
DEF VAR lcSep AS CHAR NO-UNDO INIT "|".

ASSIGN
   lcIncDir    = fCParam("CreditNote","IncDir") 
   lcProcDir   = fCParam("CreditNote","IncProcDir")
   lcSpoolDir  = fCParam("CreditNote","OutSpoolDir")
   lcOutDir    = fCParam("CreditNote","OutDir").

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine lcSep 
      icMessage SKIP.    
END FUNCTION.

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName. 
   lcInputFile = lcIncDir + lcFileName.
   
   IF SEARCH(lcInputFile) NE ? THEN 
      INPUT STREAM sin FROM VALUE(lcInputFile).
   ELSE NEXT.

   /* extract date from filename */
   fBatchLog("START", lcInputFile).
   lcLogFile = lcSpoolDir + lcFileName + ".log".
   OUTPUT STREAM sLog TO VALUE(lcLogFile) append.

   LINE_LOOP:
   REPEAT:
      
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine EQ "" THEN NEXT.

      RUN pCreateCreditNote (lcLine).
      
      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         fLogLine(ENTRY(2,RETURN-VALUE,":")).
      END.
      ELSE DO:
         fLogLine(RETURN-VALUE).
      END.
   END.
  
   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir). 
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

END.

INPUT STREAM sFile CLOSE.

PROCEDURE pCreateCreditNote : 

   DEFINE INPUT PARAMETER pcLine AS CHARACTER NO-UNDO. 

   /* local variables */
   DEF VAR lcExtInvID AS CHAR NO-UNDO.
   DEF VAR ldeAmount AS DEC NO-UNDO. 
   DEF VAR liInvType AS INT NO-UNDO. 
   DEF VAR lcReasonCode AS CHAR NO-UNDO. 
   DEF VAR lcMemoTitle AS CHAR NO-UNDO. 
   DEF VAR lcMemoContent AS CHAR NO-UNDO. 

   DEF VAR lcError AS CHAR NO-UNDO.  
   DEF VAR liReq AS INT NO-UNDO. 
   DEF VAR lcSubInvNums AS CHAR NO-UNDO. 
   DEF VAR ldeInvRowVATAmt  AS DEC NO-UNDO.
   DEF VAR lcInvRowDetails  AS CHAR NO-UNDO.
   DEF VAR ldeTotalUncreditedAmt AS DEC NO-UNDO. 

   ASSIGN
      lcExtInvID = ENTRY(1,pcLine,lcSep)
      ldeAmount = DECIMAL(REPLACE(ENTRY(2,pcLine,lcSep),",","."))
      liInvType = INT(ENTRY(3,pcLine,lcSep))
      lcReasonCode = ENTRY(4,pcLine,lcSep)
      lcMemoTitle= ENTRY(5,pcLine,lcSep)
      lcMemoContent= ENTRY(6,pcLine,lcSep)
      ldeTotalUncreditedAmt = 0 NO-ERROR.

   IF ERROR-STATUS:ERROR THEN RETURN "ERROR:Wrong file format".

   /* check invoice */
   FIND Invoice WHERE 
        Invoice.Brand = gcBrand AND
        Invoice.ExtInvId = lcExtInvID NO-LOCK NO-ERROR.
   IF NOT AVAIL Invoice THEN RETURN "ERROR:Invalid invoice number".

   IF lcMemoContent > "" AND NOT lcMemoTitle > "" THEN
      RETURN "ERROR:Memo title is missing".

   /* Invoice already credited */
   IF Invoice.CrInvNum > 0  THEN DO:
      FOR EACH SubInvoice OF Invoice NO-LOCK:
         IF SubInvoice.CrInvNum = 0 THEN
            ASSIGN lcSubInvNums     = lcSubInvNums + "," +
                                      STRING(SubInvoice.SubInvNum)
                   ldeTotalUncreditedAmt = ldeTotalUncreditedAmt + SubInvoice.InvAmt.
         /* Also consider subinvoice(s) which has been partially credited */
         /* but we will create partial credit note for rest subinvoice    */
         ELSE
            FOR EACH InvRow OF SubInvoice NO-LOCK:
               IF InvRow.CreditInvNum = 0 THEN ASSIGN
                   lcSubInvNums     = lcSubInvNums + "," + STRING(SubInvoice.SubInvNum) WHEN
                                      LOOKUP(STRING(SubInvoice.SubInvNum), lcSubInvNums) = 0    
                   lcInvRowDetails = lcInvRowDetails + "," +
                                     "InvRow="       + STRING(InvRow.InvRowNum) + "|" +
                                     "InvRowAmt="    + STRING(InvRow.Amt)
                   ldeInvRowVATAmt       = ROUND(((InvRow.Amt * InvRow.VATPerc) / 100),2)                  
                   ldeTotalUncreditedAmt = ldeTotalUncreditedAmt + InvRow.Amt + ldeInvRowVATAmt.

            END. /* FOR EACH lbInvRow OF chkBInvoice WHERE */
      END. /* FOR EACH SubInvoice OF Invoice NO-LOCK: */
      
      ASSIGN lcSubInvNums    = TRIM(lcSubInvNums,",")
             lcInvRowDetails = TRIM(lcInvRowDetails,",").

      IF lcSubInvNums = "" THEN RETURN "ERROR:Invoice is already credited".
      
      IF NOT ABS(ldeAmount - ldeTotalUncreditedAmt) <= 0.1 THEN
      RETURN "ERROR:The specified amount does not match with actual " +
             "uncredited amount: " + STRING(ldeTotalUncreditedAmt). 
   END.
   ELSE IF Invoice.InvAmt NE ldeAmount THEN 
      RETURN "ERROR:The specified amount does not match with actual " +
             "Invoice amount: " + STRING(Invoice.InvAmt).
   
   liReq = fFullCreditNote(
      Invoice.InvNum,
      lcSubInvNums, /* sub invoice list */
      lcInvRowDetails,
      "", /* reason group */
      lcReasonCode,
      "",
      OUTPUT lcError).

   if lireq = 0 then RETURN "ERROR:" + lcError.

   create Memo.
   assign Memo.Brand     = gcBrand
          Memo.HostTable = "Invoice"
          Memo.KeyValue  = string(Invoice.Invnum)
          Memo.CustNum   = Invoice.Custnum 
          Memo.MemoSeq   = next-value(memoseq)
          Memo.CreUser   = katun 
          Memo.MemoTitle = lcMemoTitle
          Memo.MemoText  = lcMemoContent
          Memo.CreStamp  = fmakets().

   RETURN "OK".

END PROCEDURE.  
