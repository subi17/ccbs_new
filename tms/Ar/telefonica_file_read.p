/* ----------------------------------------------------------------------
  MODULE .......: telefonica_file_read.p
  TASK .........: Process fusion invoice fixed data from Telefonica.
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 25.11.2013
  Version ......: yoigo
---------------------------------------------------------------------- */
{commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{cparam2.i}
{timestamp.i}
{tmsconst.i}
{ftransdir.i}
{email.i}
{eventlog.i}
{fusioninvoice.i}

DEF VAR lcProcessedFile AS CHAR NO-UNDO.
DEF VAR lcIncDir AS CHAR NO-UNDO. 
DEF VAR lcOutDir AS CHAR NO-UNDO. 
DEF VAR lcOutSpoolDir AS CHAR NO-UNDO. 
DEF VAR lcIncProcDir AS CHAR NO-UNDO. 
DEF VAR lcNotProcDir AS CHAR NO-UNDO. 
DEF VAR lcAddrConfDir AS CHAR NO-UNDO. 
DEF VAR lcLogDir AS CHARACTER NO-UNDO. 
DEF VAR lcRootDir AS CHAR NO-UNDO. 
DEF VAR lcLine AS CHAR NO-UNDO. 
DEF VAR lcFileName AS CHAR NO-UNDO. 
DEF VAR lcInputFile AS CHAR NO-UNDO. 
DEF VAR lcYoigoLog AS CHAR NO-UNDO. 
DEF VAR lcTFLog AS CHAR NO-UNDO. 
DEF VAR liLineNum AS INT NO-UNDO. 
DEF VAR ldaInvDate AS DATE NO-UNDO. 
DEF VAR lcDelTypes AS CHAR NO-UNDO. 
DEF VAR liDelType AS INT NO-UNDO. 
DEF VAR liLoop AS INT NO-UNDO. 
DEF VAR lcLogFile AS CHAR NO-UNDO. 
DEF VAR ldaToday AS DATE NO-UNDO.
DEF VAR liTime AS INT NO-UNDO.

DEF VAR liFiles AS INTEGER NO-UNDO. 
DEF VAR liNumYoigoTF AS INT NO-UNDO. 
DEF VAR liNumYoigo AS INT NO-UNDO. 
DEF VAR liNumTF AS INT NO-UNDO. 
DEF VAR liNumErr AS INT NO-UNDO. 

lcDelTypes = SUBST("&1,&2",{&INV_DEL_TYPE_FUSION_EMAIL_PENDING},{&INV_DEL_TYPE_FUSION_EMAIL}).

DEF VAR ldThisRun AS DEC NO-UNDO. 

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sYoigoLog.
DEF STREAM sTFLog.
DEF STREAM sLog.

DEFINE TEMP-TABLE ttTF LIKE FusionInvoice
   FIELD LinePos as int
   FIELD LineContent AS CHAR
   FIELD InvoiceSeq AS INT
   FIELD MonthlyFees AS DEC DECIMALS 4
   FIELD ProductMF AS DEC DECIMALS 4
   FIELD FiberMF AS DEC DECIMALS 4
   FIELD ValidationEnvio AS CHAR
   FIELD ValidationProceso AS CHAR
   FIELD ValidatedEmail AS LOG

INDEX LinePos IS PRIMARY LinePos 
INDEX InvoiceNum InvoiceNum InvoiceSeq.  

DEFINE TEMP-TABLE ttTFDB LIKE FusionInvoice.

lcRootDir = fCParam("EIF","TFFileRoot").
IF NOT lcRootDir > "" THEN RETURN.

ASSIGN
   lcAddrConfDir = fCParamC("RepConfDir")
   lcOutDir = lcRootDir + "outgoing/outgoing/"
   lcOutSpoolDir = lcRootDir + "outgoing/spool/"
   lcIncDir = lcRootDir + "incoming/incoming/"
   lcIncProcDir = lcRootDir + "incoming/processed/"
   lcNotProcDir = lcRootDir + "incoming/not_processed/"
   lcLogDir = lcRootDir + "incoming/log/"
   ldaInvDate = DATE(MONTH(TODAY),1,YEAR(TODAY)).

/* File reading and parsing */
/* Only the latest file (by filename) is handled */
INPUT STREAM sFile THROUGH VALUE("ls -1r " + lcIncDir).
FILE_LOOP:
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
 
   lcInputFile = lcIncDir + lcFileName.
   
   IF SEARCH(lcInputFile) NE ? THEN 
      INPUT STREAM sin FROM VALUE(lcInputFile).
   ELSE NEXT.

   liFiles = liFiles + 1.
   IF liFiles > 1 THEN DO:
      fMove2TransDir(lcInputFile, "", lcNotProcDir). 
      NEXT.
   END.
      
   ASSIGN
      liNumYoigo = 0
      liNumYoigoTF = 0
      liNumTF = 0
      liNumErr = 0
      liLineNum = 0
      ldThisRun = fMakeTS().
   
   RUN pMarkStarted. 
   IF RETURN-VALUE NE "OK" THEN LEAVE FILE_LOOP. 

   fSplitTS(ldThisRun, OUTPUT ldaToday, OUTPUT liTime).
   
   IF SESSION:BATCH THEN fBatchLog("START", lcInputFile). 

   ASSIGN
      lcYoigoLog = lcOutSpoolDir + "YOIGO_FUSION_EMAIL_" +
       STRING(YEAR(ldaToday) * 10000 + MONTH(ldaToday) * 100 + DAY(ldaToday)) + 
            "_" + REPLACE(STRING(liTime,"HH:MM:SS"),":","") + ".txt"
      lcTFLog = lcOutSpoolDir + lcFileName + ".RESP".
      lcLogFile = lcLogDir + lcFileName + ".LOG". 

   /* First just read all the data to temp table */
   LINE_LOOP:
   REPEAT TRANS:
                  
      IMPORT STREAM sin UNFORMATTED lcLine.
      liLineNum = liLineNum + 1.

      IF NOT SESSION:BATCH AND liLineNum MOD 100 = 0 THEN DO:
         disp liLineNum with frame a.
         pause 0.
      END.
      IF TRIM(lcLine) EQ "" THEN NEXT LINE_LOOP.

      IF LENGTH(lcLine) NE 700 THEN DO:
         RUN pMarkError(SUBST("Syntax error at line &1 (length is not 700)", liLineNum)). 
         EMPTY TEMP-TABLE ttTF NO-ERROR.
         fMove2TransDir(lcInputFile, "", lcNotProcDir). 
         NEXT FILE_LOOP.
      END.

      CREATE ttTF.
      ASSIGN 
         ttTF.InvDate     = ldaInvDate
         ttTF.LinePos     = liLineNum
         ttTF.LineContent = lcLine
         ttTF.InvoiceNum  = trim(substring(lcLine,13,14)) /* #2 */
         ttTF.InvoiceSeq  = int(trim(substring(lcLine,27,3))) /* #3 */
         ttTF.CustomerID  = substring(trim(substring(lcLine,30,17)),3) /* #4 */
         ttTF.Email       = trim(substring(lcLine,197,80)) /* #6 */
         ttTF.FixedNumber = trim(substring(lcLine,297,20)) /* #9 */
         ttTF.Language    = trim(substring(lcLine,317,1)) /* #10 */

         ttTF.MonthlyFees = DEC(substring(lcLine,318,1) + /* #11#12 */
                            left-trim(substring(lcLine,319,15),"0")) / 10000
         
         ttTF.TrafficAmt  = DEC(substring(lcLine,334,1) + /* #13#14 */
                            left-trim(substring(lcLine,335,15),"0")) / 10000
         
         ttTF.OtherItems  = DEC(substring(lcLine,350,1) + /* #15#16 */
                            left-trim(substring(lcLine,351,15),"0")) / 10000
         
         ttTF.TaxIncome   = DEC(substring(lcLine,367,1) + /* #18#19 */
                            left-trim(substring(lcLine,368,15),"0")) / 10000
         
         ttTF.TaxAmt      = DEC(substring(lcLine,383,1) + /* #20#21 */
                            left-trim(substring(lcLine,384,15),"0")) / 10000
         
         ttTF.TotalAmt    = DEC(substring(lcLine,399,1) + /* #22#23 */
                            left-trim(substring(lcLine,400,13),"0")) / 100
         
         ttTF.TotalToPay  = DEC(substring(lcLine,413,1) + /* #24#25 */
                            left-trim(substring(lcLine,414,13),"0")) / 100
         
         ttTF.AddItems    = DEC(substring(lcLine,427,1) + /* #26#27 */
                            left-trim(substring(lcLine,428,15),"0")) / 10000
         
         ttTF.ProductCode = trim(substring(lcLine,443,14)) /* #28 */
         ttTF.ProductText = trim(substring(lcLine,457,150)) /* #29 */
         
         ttTF.ProductMF   = DEC(substring(lcLine,623,1) + /* #32#33 */
                            left-trim(substring(lcLine,624,13),"0")) / 10000 
         
         ttTF.FiberMF     = DEC(substring(lcLine,637,1) + /* #34#35 */
                            left-trim(substring(lcLine,638,13),"0")) / 10000 
         ttTF.Mapping     = {&FI_MAPPING_TF}
         NO-ERROR.
 
      IF ERROR-STATUS:ERROR THEN DO:
         RUN pMarkError(SUBST("Syntax error at line &1", liLineNum)).
         EMPTY TEMP-TABLE ttTF NO-ERROR.
         fMove2TransDir(lcInputFile, "", lcNotProcDir). 
         NEXT FILE_LOOP.
      END.

   END. /* LINE_LOOP */
   INPUT STREAM sin CLOSE.
   
   
   RUN pAnalyzeTelefonicaInvoices.
   RUN pCollectFusionInvoices.
   RUN pStoreData.
   RUN pWriteOutputFiles.
   
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcIncProcDir). 
   IF SESSION:BATCH AND lcProcessedFile NE "" THEN
      fBatchLog("FINISH", lcProcessedFile).
   
   RUN pMarkFinished.
END.

INPUT STREAM sFile CLOSE.

PROCEDURE pMarkStarted:

   DEF VAR lcError AS CHAR NO-UNDO. 
   
   /* check that there isn't already another run for the same purpose */
   IF CAN-FIND(FIRST ActionLog USE-INDEX ActionID WHERE
                     ActionLog.Brand        = gcBrand     AND    
                     ActionLog.ActionID     = "TELEFONICA" AND
                     ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE}) THEN
      lcError = "Batch not started due to ongoing run".
   
   IF fPendingFusionEmailRequest(ldaInvDate) THEN DO:
      lcError = "Batch not started due to already created fusion email request".
      RUN pSendErrorMail(lcError). 
   END.

   IF lcError > "" THEN DO:
      
      DO TRANS:
         CREATE ActionLog.
         ASSIGN
            ActionLog.Brand        = gcBrand
            ActionLog.ActionID     = "TELEFONICA"
            ActionLog.ActionTS     = ldThisRun
            ActionLog.TableName    = "Cron"
            ActionLog.KeyValue     = lcFileName
            ActionLog.UserCode     = katun
            ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}
            ActionLog.ActionPeriod = YEAR(ldaInvDate) * 100 + MONTH(ldaInvDate) 
            ActionLog.ActionChar   = lcError.
         RELEASE ActionLog.   
      END.
      RETURN "ERROR". 
   END.

   /* mark this run started */
   DO TRANS:
      CREATE ActionLog.
      
      ASSIGN
         ActionLog.Brand        = gcBrand
         ActionLog.ActionID     = "TELEFONICA"
         ActionLog.ActionTS     = ldThisRun
         ActionLog.TableName    = "Cron"
         ActionLog.KeyValue     = lcFileName
         ActionLog.UserCode     = katun
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE}
         ActionLog.ActionPeriod = YEAR(ldaInvDate) * 100 + MONTH(ldaInvDate).
      RELEASE ActionLog.   
   END.
   RETURN "OK".

END PROCEDURE.

PROCEDURE pMarkError:
   
   DEFINE INPUT PARAMETER icError AS CHARACTER NO-UNDO. 

   /* mark this run finished */
   FOR FIRST ActionLog USE-INDEX ActionID WHERE
             ActionLog.Brand        = gcBrand AND    
             ActionLog.ActionID     = "TELEFONICA" AND
             ActionLog.ActionTS     = ldThisRun AND
             ActionLog.TableName    = "Cron" AND
             ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE} AND
             ActionLog.KeyValue     = lcFileName 
   EXCLUSIVE-LOCK:
      ASSIGN 
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ERROR}
         ActionLog.ActionChar   = icError. 
   END.
      
   RUN pSendErrorMail(icError). 

END PROCEDURE.
   
PROCEDURE pMarkFinished:

   /* mark this run finished */
   FOR FIRST ActionLog USE-INDEX ActionID WHERE
             ActionLog.Brand        = gcBrand AND    
             ActionLog.ActionID     = "TELEFONICA" AND
             ActionLog.ActionTS     = ldThisRun AND
             ActionLog.TableName    = "Cron" AND
             ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE} AND
             ActionLog.KeyValue     = lcFileName 
   EXCLUSIVE-LOCK:
      ASSIGN 
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.ActionChar   = 
            SUBST("Yoigo+TF: &1, TF: &2, Yoigo: &3, Errors: &4", 
                  liNumYoigoTF, liNumTF, liNumYoigo, liNumErr) + CHR(10) + 
                  "Finished: " + fTS2HMS(fMakeTS()). 
   END.
   
END PROCEDURE.

PROCEDURE pCollectFusionInvoices:
   
   DEF VAR liSubs AS INT NO-UNDO. 

   FOR EACH Invoice NO-LOCK WHERE
            Invoice.Brand = gcBrand AND
            Invoice.InvDate >= ldaInvDate AND
            Invoice.InvType = 1 AND
            (Invoice.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
             Invoice.DelType = {&INV_DEL_TYPE_FUSION_EMAIL}),
      FIRST Customer NO-LOCK WHERE
            Customer.Custnum = Invoice.Custnum:

      liSubs = 0.

      SUBINVOICE_LOOP:
      FOR EACH SubInvoice OF Invoice NO-LOCK:

         liSubs = liSubs + 1.

         IF liSubs > 1 THEN DO:
            ASSIGN
               ttTF.ProductCode = "ERROR, Customer with two or more Mobile Fusion Subscription. Email will not be send"
               ttTF.ValidationEnvio = "2"
               ttTF.ValidationProceso = "4"
               liNumErr = liNumErr + 1.
            LEAVE SUBINVOICE_LOOP.
         END.

         FIND FIRST ttTF WHERE
                    ttTF.CustomerID = Customer.OrgId NO-ERROR.

         IF AVAIL ttTF THEN DO:

           ASSIGN
              ttTF.Mapping = {&FI_MAPPING_YOIGO_AND_TF}
              ttTF.ValidationEnvio = "0"
              liNumYoigoTF = liNumYoigoTF + 1.
         
            IF NOT ttTF.Email > ""
               THEN ttTF.ValidationProceso = "3".
            ELSE IF Customer.Email EQ ttTF.Email
               THEN ttTF.ValidationProceso = "0".
            ELSE ttTF.ValidationProceso = "2".

         END.
         ELSE DO:
           liLineNum = liLineNum + 1.
           CREATE ttTF.
           ASSIGN
              ttTF.LinePos = liLineNum
              ttTF.CustomerID = Customer.OrgID
              ttTF.Mapping = {&FI_MAPPING_YOIGO}
              liNumYoigo = liNumYoigo + 1.
         END.
         
         FOR EACH Invrow NO-LOCK WHERE 
                  Invrow.InvNum = Invoice.Invnum,
            FIRST BillItem NO-LOCK WHERE
                  BillItem.Brand = gcBrand AND
                  BillItem.BillCode = InvRow.BillCode:

             IF LOOKUP(BillItem.BillCode,"CONTFF2MF,CONTSF14MF,CONTSF10MF") > 0 THEN 
                ASSIGN
                   ttTF.MSubsType = REPLACE(BillItem.BillCode,"MF","")
                   ttTF.MTariffMF = ttTF.MTariffMF + InvRow.Amt.
             ELSE IF (BillItem.BillCode BEGINS "PAYTERM" OR 
                      BillItem.BillCode BEGINS "RVTERM") AND
                      BillItem.BiGroup EQ "33" THEN 
                ttTF.MTermFinancing = ttTF.MTermFinancing + InvRow.Amt.
             ELSE IF LOOKUP(BillItem.BIGroup,"3,13,31,32") > 0 OR
                            BillItem.BIGroup  EQ "18"          THEN 
                ttTF.MOtherMF = ttTF.MOtherMF + InvRow.Amt.
             ELSE IF LOOKUP(BillItem.BIGroup,"1,4,5,6") > 0 THEN
                ttTF.MTraffic = ttTF.MTraffic + InvRow.Amt.
             ELSE IF BillItem.BillCode EQ "TERMPERIOD" THEN  
                ttTF.MPermPenalty = ttTF.MPermPenalty + InvRow.Amt.
         END.
         
         ASSIGN
            ttTF.InvDate = ldaInvDate
            ttTF.InvNum = Invoice.InvNum
            ttTF.Custnum = Customer.Custnum
            ttTF.Language = STRING(Customer.Language)
            ttTF.MsSeq = SubInvoice.MsSeq
            ttTF.ValidatedEmail = (Invoice.DelType EQ {&INV_DEL_TYPE_FUSION_EMAIL})
            ttTF.Email = Customer.Email
            ttTF.MTaxableIncome = Invoice.AmtExclVAT - ttTF.MTermFinancing
            ttTF.MVatAmt = Invoice.VatAmt
            ttTF.MTotalInvoice = Invoice.InvAmt - ttTF.MTermFinancing
            ttTF.MInvAmt = Invoice.InvAmt.
      END.
   END. /* INVOICE LOOP */

  RETURN "OK".

END PROCEDURE. 
   
PROCEDURE pAnalyzeTelefonicaInvoices:

   DEF BUFFER bttTF FOR ttTF.
   DEF VAR ldeProductMFSum AS DECIMAL NO-UNDO. 
   DEF VAR ldeMonthlyFeesSum AS DECIMAL NO-UNDO. 
   DEF VAR ldeFiberMFSum AS DECIMAL NO-UNDO. 

   FOR EACH ttTF USE-INDEX InvoiceNum BREAK BY InvoiceNum:
      
      IF ttTF.Mapping = {&FI_MAPPING_YOIGO} THEN NEXT.
      
      IF FIRST-OF(ttTF.InvoiceNum) THEN ASSIGN
         ldeProductMFSum = ttTF.ProductMF
         ldeMonthlyFeesSum = ttTF.MonthlyFees 
         ldeFiberMFSum = ttTF.FiberMF. 
      ELSE IF NOT LAST-OF(ttTF.InvoiceNum) THEN ASSIGN
         ldeProductMFSum = ldeProductMFSum + ttTF.ProductMF 
         ldeMonthlyFeesSum = ldeMonthlyFeesSum + ttTF.MonthlyFees 
         ldeFiberMFSum = ldeFiberMFSum + ttTF.FiberMF. 
      
      IF LAST-OF(ttTF.InvoiceNum) THEN DO:

         FIND FIRST bttTF WHERE
                    bttTF.InvoiceNum = ttTF.InvoiceNum AND
                    bttTF.InvoiceSeq = 0 AND 
                NOT bttTF.ProductCode BEGINS "ERROR" NO-ERROR.
         
         IF AVAIL bttTF THEN DO:
            ASSIGN
               bttTF.ProductCode = ttTF.ProductCode
               bttTF.ProductText = ttTF.ProductText.
         
            CASE ttTF.ProductCode:
               WHEN "AG000000548265" OR WHEN "AG000000548266" THEN ASSIGN
                  bttTF.BaseServiceMF = ldeProductMFSum 
                  bttTF.OtherMF =  ldeMonthlyFeesSum - ldeProductMFSum.
               WHEN "AG000000548264" THEN ASSIGN
                  bttTF.BaseServiceMF = ldeProductMFSum + ldeFiberMFSum 
                  bttTF.OtherMF = ldeMonthlyFeesSum - 
                                          ldeProductMFSum - ldeFiberMFSum.
               OTHERWISE ASSIGN
                  bttTF.ProductCode = "ERROR, incorrect product code"
                  bttTF.ValidationEnvio = "2"
                  bttTF.ValidationProceso = "4"
                  liNumErr = liNumErr + 1.
            END.
        END.
      END.
      
      IF ttTF.Mapping = {&FI_MAPPING_TF} THEN DO: 
         
         FIND Customer NO-LOCK WHERE
              Customer.Brand = gcBrand AND
              Customer.OrgId = ttTF.CustomerId AND
              Customer.CustIdType NE "passport" AND
              Customer.Roles NE "inactive" NO-ERROR.
         IF AVAIL Customer THEN ASSIGN
            ttTF.Email = Customer.Email WHEN Customer.Email > ""
            ttTF.Custnum = Customer.Custnum.

         ASSIGN
            liNumTF = liNumTF + 1
            ttTF.ValidationEnvio = "1"
            ttTF.ValidationProceso = (IF ttTF.Email > "" THEN "4" ELSE "3")
            ttTF.ProductCode = "ERROR, no email address" WHEN NOT ttTF.Email > "".
      END.

      IF ttTF.CustomerId > "" AND 
         CAN-FIND(FIRST bttTF WHERE
                        bttTF.CustomerID = ttTF.CustomerId AND
                  ROWID(bttTF) NE ROWID(ttTF)) THEN ASSIGN
            ttTF.ProductCode = "ERROR, Customer with two or more TF invoices. Email will not be send"
            ttTF.ValidationEnvio = "2"
            ttTF.ValidationProceso = "4"
            liNumErr = liNumErr + 1.
      
      IF ttTF.CustomerID EQ "" AND
         ttTF.InvoiceSeq > 0 THEN DO:
         FIND FIRST bttTF NO-LOCK WHERE
                    bttTF.InvoiceNum = ttTF.InvoiceNum AND
                    bttTF.InvoiceSeq = 0 NO-ERROR.
         IF AVAIL bttTF THEN ASSIGN
            ttTF.ValidationEnvio = bttTF.ValidationEnvio
            ttTF.ValidationProceso = bttTF.ValidationProceso.

      END.
      
   END.

   RETURN "OK".

END PROCEDURE. 
   
PROCEDURE pWriteOutputFiles:
   
   /* Write Yoigo log */
   OUTPUT STREAM sYoigoLog TO VALUE(lcYoigoLog).
   OUTPUT STREAM sTFLog TO VALUE(lcTFLog).

   FOR EACH ttTF USE-INDEX LinePos:
   
      IF NOT ttTF.InvoiceSeq > 0 THEN
      PUT STREAM sYoigoLog UNFORMATTED
         ttTF.CustomerId ";"
         ttTF.Custnum ";"
         ttTF.MsSeq ";"
         (IF ttTF.Mapping EQ {&FI_MAPPING_TF} OR 
             ttTF.ValidatedEmail
          THEN ttTF.Email
          ELSE "") ";"
         ttTF.Language ";"
         ttTF.ProductCode ";"
         ttTF.BaseServiceMF ";"
         ttTF.OtherMF ";"
         ttTF.TrafficAmt ";"
         ttTF.OtherItems ";"
         ttTF.TaxIncome ";"
         ttTF.TaxAmt ";"
         ttTF.TotalAmt ";"
         ttTF.AddItems ";"
         ttTF.TotalToPay ";"
         ttTF.MSubsType ";" 
         ttTF.MTariffMF ";" 
         ttTF.MOtherMF ";" 
         ttTF.MTraffic ";" 
         ttTF.MTaxableIncome ";" 
         ttTF.MVatAmt ";" 
         ttTF.MTotalInvoice ";" 
         ttTF.MTermFinancing ";" 
         ttTF.MInvAmt 
      SKIP.
              
      /* Do not print Yoigo only cases to TF log file */
      IF ttTF.Mapping = {&FI_MAPPING_YOIGO} THEN NEXT.

      OVERLAY(ttTF.LineContent,651,2) = 
          FILL("0",2 - LENGTH(ttTF.ValidationEnvio)) +
          ttTF.ValidationEnvio.
      OVERLAY(ttTF.LineContent,653,2) = 
          FILL("0",2 - LENGTH(ttTF.ValidationProceso)) +
          ttTF.ValidationProceso.
      PUT STREAM sTFLog UNFORMATTED ttTF.LineContent.
      PUT STREAM sTFLog CONTROL CHR(13) CHR(10).
   END.
   
   OUTPUT STREAM sYoigoLog CLOSE.
   OUTPUT STREAM sTFLog CLOSE.
   
   fMove2TransDir(lcYoigoLog, "", lcOutDir).  
   fMove2TransDir(lcTFLog, "", lcOutDir). 

   RETURN "OK".

END PROCEDURE. 

PROCEDURE pStoreData:

   DEFINE VARIABLE liTFId AS INTEGER NO-UNDO. 

   FOR EACH FusionInvoice EXCLUSIVE-LOCK WHERE
            FusionInvoice.InvDate >= ldaInvDate AND
            FusionInvoice.DeliveryState = 0:
      DELETE FusionInvoice.
   END.
   
   FIND LAST FusionInvoice NO-LOCK USE-INDEX FuInvNum NO-ERROR.
   IF AVAIL FusionInvoice THEN
      liTFId = FusionInvoice.FuInvNum.
   ELSE liTFId = 0.

   FOR EACH ttTF NO-LOCK :

      IF ttTF.ProductCode BEGINS "ERROR" THEN NEXT.
      
      liTFId = liTFId + 1.
      CREATE FusionInvoice.
      ASSIGN
        FusionInvoice.FuInvNum = liTFId.

      BUFFER-COPY ttTF EXCEPT FUInvNum TO FusionInvoice.
   END.

   RETURN "OK".

END PROCEDURE. 

PROCEDURE pSendErrorMail:

   DEF INPUT PARAM pcErrorText AS CHAR NO-UNDO. 
   IF NOT pcErrorText > "" THEN LEAVE.
   
   OUTPUT STREAM sLog TO VALUE(lcLogFile).
   PUT STREAM sLog unformatted pcErrorText SKIP.
   OUTPUT STREAM sLog CLOSE.

   /* Send an email to configure list*/
   IF lcAddrConfDir > "" THEN DO:
      lcAddrConfDir = lcAddrConfDir + "telefonica_file_read.email".
      /* Mail recipients */
      GetRecipients(lcAddrConfDir).
      /* Send via mail */
      SendMail(lcLogFile,"").
   END.

END PROCEDURE. 
