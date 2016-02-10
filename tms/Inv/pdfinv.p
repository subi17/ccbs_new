/* ---------------------------------------------------------------------------
  MODULE .......: PDFINV
  FUNCTION .....: Print invoices to PDF files
  APPLICATION ..: TMS
  CREATED ......: 30.04.03/aam 
  MODIFIED .....: 22.08.03/aam send errorlog via email
                  08.10.03/aam optionally mail to user,
                               bar code (numeric),
                               body text for mail from InvText
                  25.11.03/aam also reminders             
                  16.02.04/aam fdivtxt.i
                  16.06.04/aam xmlinv.p can return multiple file names
                  16.06.06/aam ClaimState instead of ClaimQty
  VERSION ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/eventval.i}
{Func/refcode.i}
{Func/fxmlfile.i}
{Inv/pdfinvdf.i}
{Func/invotxtp.i} 
{Func/timestamp.i}
{Func/finvbal.i}
{Func/fdivtxt.i}

DEF INPUT-OUTPUT PARAMETER TABLE FOR ttPDFInv.
DEF INPUT        PARAMETER ilPrintRep    AS LOG  NO-UNDO.
DEF INPUT        PARAMETER ilFormPDF     AS LOG  NO-UNDO. 
DEF INPUT        PARAMETER iiSendMail    AS INT  NO-UNDO.
DEF INPUT        PARAMETER iiPrintType   AS INT  NO-UNDO. /* 0=inv, 1=rem */
DEF INPUT        PARAMETER idtRemDate    AS DATE NO-UNDO. 
DEF OUTPUT       PARAMETER oiInvQty      AS INT  NO-UNDO. 
DEF OUTPUT       PARAMETER oiMailQty     AS INT  NO-UNDO. 
DEF OUTPUT       PARAMETER oiErrQty      AS INT  NO-UNDO. 

DEF VAR lcXLTFile    AS CHAR  NO-UNDO. 
DEF VAR llOk         AS LOG   NO-UNDO.
DEF VAR lcMailAddr   AS CHAR  NO-UNDO. 
DEF VAR lcBodyTxt    AS CHAR  NO-UNDO. 
DEF VAR lcBarCode    AS CHAR  NO-UNDO.
DEF VAR lcBankAcc    AS CHAR  NO-UNDO.
DEF VAR lcRefNum     AS CHAR  NO-UNDO. 
DEF VAR liCount      AS INT   NO-UNDO. 
DEF VAR lcText       AS CHAR  NO-UNDO. 
DEF VAR ldtBodyDate  AS DATE  NO-UNDO. 
DEF VAR lcPrintType  AS CHAR  NO-UNDO. 
DEF VAR lcMailSubj   AS CHAR  NO-UNDO. 
DEF VAR lcRemText    AS CHAR  NO-UNDO. 
DEF VAR lcLayout     AS CHAR  NO-UNDO. 
DEF VAR liFileCnt    AS INT   NO-UNDO. 
DEF VAR lcXMLFile    AS CHAR  NO-UNDO. 
DEF VAR lcPDFFile    AS CHAR  NO-UNDO.
DEF VAR lcPDFMail    AS CHAR  NO-UNDO.
DEF VAR llInvFile    AS LOG   NO-UNDO. 
DEF VAR lcTxtTarget  AS CHAR  NO-UNDO.
DEF VAR lcTxtKey     AS CHAR  NO-UNDO. 

{Func/fpdfrun.i}

DEF BUFFER bInvoice FOR Invoice.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER bInvoice:HANDLE.
   RUN StarEventInitialize(lhInvoice).
              
END.

FUNCTION fRemTxt RETURNS LOGICAL
   (iiPos AS INT).
   
   FOR EACH ttInvoTxt WHERE
            ttInvoTxt.ITPos = iiPos AND
            ttInvoTxt.ITTxt > ""
   BY ttInvoTxt.ITOrd:
         
      lcRemText = fSeparateInvoTxt(ttInvoTxt.ITTxt,85).
            
      DO liCount = 1 TO NUM-ENTRIES(lcRemText,CHR(9)):
      
         IF ENTRY(liCount,lcRemText,CHR(9)) = "" THEN NEXT.
            
         lcBodyTxt = lcBodyTxt + ENTRY(liCount,lcRemText,CHR(9)) + 
                     CHR(10).
      END.
            
      lcBodyTxt = lcBodyTxt + CHR(10) + CHR(10).
   END.
    
END FUNCTION.

/* mail to user */
IF iiSendMail = 2 THEN DO:
   FIND TMSUser WHERE TMSUser.UserCode = katun NO-LOCK NO-ERROR.
   IF AVAILABLE TMSUser THEN lcMailAddr = TMSUser.EMail.
END. 

IF iiPrintType = 1 
THEN lcPrintType = "rem".
ELSE lcPrintType = "inv".
             
/* init values */
fPDFInit(lcPrintType,
         ilFormPDF).

lcLayout = fCParamC("PDFInvLayout").
IF lcLayout = "" OR lcLayout = ? THEN DO:
   fErrorLog("PDF layout for invoices has not been defined").
END. 

ELSE 
FOR EACH ttPDFInv,
   FIRST Invoice WHERE
         Invoice.InvNum = ttPDFInv.InvNum,
   FIRST Customer of Invoice NO-LOCK,
   FIRST invgroup OF Customer NO-LOCK:

   /* check debt amount for reminders, new payments may have been made 
      during this process */
   IF iiPrintType = 1 THEN DO:
       ttPDFInv.ClaimAmt = fInvBal(BUFFER Invoice,
                                   TODAY).
       
       IF ttPDFInv.ClaimAmt <= 0 OR
          ttPDFInv.ClaimAmt < ttPDFInv.MinClAmt
       THEN DO:
          fErrorLog("Payment booked after process started"). 
          DELETE ttPDFInv.
          NEXT.
       END.
   END.

   /* xml-file */
   RUN xmlinv (Invoice.InvNum,
               /* form an xml-file or just get output parameters 
                  (if pdf-formation is not chosen then just try to send eMail)
                   force the formation, if invoice to be reminded 
                   is not yet in pdf */
               (IF iiPrintType = 1 AND Invoice.PrintState < 5
                THEN TRUE
                ELSE ilFormPDF),
               ilPrintRep,
               OUTPUT lcFile,
               OUTPUT ttPDFInv.InvForm,
               OUTPUT ttPDFInv.BarCode,
               OUTPUT ttPDFInv.BankAcc,
               OUTPUT ttPDFInv.RefNum).
   
   /* if pdf-formation not chosen but printstate shows that pdf has not
      been previously formed -> abort */
   IF iiPrintType = 0 AND NOT ilFormPDF AND Invoice.PrintState < 5 THEN NEXT.
      
   FIND bInvoice WHERE RECID(bInvoice) = RECID(Invoice) EXCLUSIVE-LOCK.
 
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).

   IF ilFormPDF OR
      (iiPrintType = 1 AND Invoice.PrintState < 5)
   THEN DO:
      
      /* mark that xml-file has been made */
      IF bInvoice.DelType = 2 THEN bInvoice.PrintState = 4.

      /* layout-file */
      lcXLTFile = lcXLTDIR + lcLayout.
      
      DO liFileCnt = 1 TO NUM-ENTRIES(lcFile):
      
         lcXMLFile = ENTRY(liFileCnt,lcFile).
         
         RUN pRunFOPD(lcXLTFile,
                      /* 2. entry contains mail address for cli reports */
                      ENTRY(1,lcXMLFile,"¤"),
                      OUTPUT llOk).
                   
         IF llOk THEN DO:
    
            /* if method is "wait" and fopd process was succesful
               then eMail can be sent rightaway */
            IF lcFopMethod = "WAIT" THEN DO: 
        
               /* mark that pdf-file is done */
               IF bInvoice.DelType = 2 THEN ASSIGN 
                  bInvoice.PrintState = 5
                  ttPDFInv.PDFFile    = ttPDFInv.PDFFile +
                                        (IF ttPDFInv.PDFFile > "" 
                                         THEN "," 
                                         ELSE "") + 
                                        REPLACE(lcXMLFile,".xml",".pdf").

               oiInvQty = oiInvQty + 1.
         
            END.
         END.    
      END.
      
   END.

   /* name for previously formed pdf-file */
   ELSE DO:
      IF bInvoice.DelType = 2 
      THEN ttPDFInv.PDFFile = fXMLInvFileName(".pdf").
   END.

END.


/* send eMail */
IF iiSendMail > 0 THEN DO:

   /* give FOPD some time to finish it's work */
   PAUSE 5 NO-MESSAGE. 

   FOR EACH ttPDFInv WHERE
            ttPDFInv.PDFFile > "",
      FIRST Invoice NO-LOCK WHERE
            Invoice.InvNum = ttPDFInv.InvNum AND
            Invoice.PrintState >= 5,
      FIRST Customer OF Invoice NO-LOCK:
   
      IF iiSendMail = 1 THEN lcMailAddr = Customer.EMail.

      DO liFileCnt = 1 TO NUM-ENTRIES(ttPDFInv.PDFFile):
      
         lcPDFFile = ENTRY(liFileCnt,ttPDFInv.PDFFile).
         /* mail can be in 2. entry for cli reports */
         IF INDEX(lcPDFFile,"¤") > 0
         THEN ASSIGN lcPDFMail = ENTRY(2,lcPDFFile,"¤")
                     lcPDFFile = ENTRY(1,lcPDFFile,"¤").
         ELSE lcPDFMail = lcMailAddr.
                     
         /* does file exist */
         IF SEARCH(lcPDFFile) = ? THEN DO:
            fErrorLog("PDF-file was not found"). 
            DELETE ttPDFInv.
            NEXT. 
         END. 
    
         /* invoice or cli report */
         llInvFile = (iiPrintType = 1 OR
                      lcPDFFile MATCHES ("*/inv_*")).
         
         lcBodyTxt = "".
      
         IF iiPrintType = 1 THEN DO:
            ASSIGN lcMailSubj  = fTeksti(148,Customer.Language)
                   ldtBodyDate = idtRemDate.
         
            /* reminder text */
            fCollRemTxt(idtRemDate,
                        Invoice.ClaimState + 1,
                        "InvCust").

            /* texts before invoice data */
            fRemTxt(1).
         
         END.
      
         ELSE ASSIGN 
            lcMailSubj  = fTeksti((IF llInvFile THEN 105 ELSE 184),
                                  Customer.Language).
            ldtBodyDate = Invoice.InvDate.
      
         /* form body text; first invoice data, then text from InvText */
         IF llInvFile THEN lcBodyTxt = lcBodyTxt + 
            (IF iiPrintType = 1 
             THEN fTeksti(149,Customer.Language)
             ELSE fTeksti(106,Customer.Language)) + CHR(10) + CHR(10) +
            fTeksti(141,Customer.Language) + 
               " " + STRING(Invoice.InvNum) + CHR(10) +
            fTeksti(142,Customer.Language) + 
               " " + STRING(Invoice.InvDate,"99.99.9999") + CHR(10) +
            fTeksti(143,Customer.Language) +
               " " + STRING(Invoice.DueDate,"99.99.9999") + CHR(10) + 
            fTeksti(144,Customer.Language) + 
               " " + STRING(Invoice.InvAmt) + CHR(10) + 
     
           /* debt for reminders */   
           (IF iiPrintType = 1 
            THEN fTeksti(78,Customer.Language) +
                 " " + STRING(ttPDFInv.ClaimAmt) + CHR(10) + CHR(10) 
            ELSE "") + 

            fTeksti(145,Customer.Language) + 
               " " + ttPDFInv.RefNum + CHR(10) +
            fTeksti(146,Customer.Language) + 
               " " + ttPDFInv.BankAcc + CHR(10) + CHR(10) +
            fTeksti(147,Customer.Language) +
               " " + ttPDFInv.BarCode + CHR(10) + CHR(10) + CHR(10).

         ELSE lcBodyTxt = 
            fTeksti(185,Customer.Language) + CHR(10) + CHR(10) +
            fTeksti(141,Customer.Language) + 
               " " + STRING(Invoice.InvNum) + CHR(10) +
            fTeksti(142,Customer.Language) + 
               " " + STRING(Invoice.InvDate,"99.99.9999") + CHR(10) + CHR(10).
            
         /* reminder texts after invoice data */
         IF iiPrintType = 1 THEN DO:
            fRemTxt(2).
         END.

         IF llInvFile 
         THEN ASSIGN lcTxtTarget = "Invoice"
                     lcTxtKey    = "PdfMail".
         ELSE ASSIGN lcTxtTarget = "General"
                     lcTxtKey    = "SpecMail".
                     
         /* longer text from information text */
         FOR FIRST InvText NO-LOCK WHERE
                   InvText.Target    = lcTxtTarget             AND
                   InvText.KeyValue  = lcTxtKey                AND
                   InvText.Report    = iiPrintType             AND 
                   InvText.FromDate <= ldtBodyDate             AND
                   InvText.ToDate   >= ldtBodyDate             AND
                   InvText.Language  = Customer.Language:

            /* possible tags */
            ASSIGN lcText = REPLACE(lcText,"#INVNUM",STRING(Invoice.InvNum))
                   lcText = REPLACE(lcText,"#CUSTNUM",STRING(Invoice.CustNum))
                   lcText = REPLACE(lcText,"#INVAMT",STRING(Invoice.InvAmt))
                   lcText = REPLACE(lcText,"#DUEDATE",
                                    STRING(Invoice.DueDate,"99.99.9999"))
                   lcText = REPLACE(lcText,"#REMDATE",
                                    STRING(idtRemDate,"99.99.9999")).
                
            /* divide into lines */
            lcText = fSeparateInvoTxt(InvText.InvText,90).

            DO liCount = 1 TO NUM-ENTRIES(lcText,CHR(9)):
               lcBodyTxt = lcBodyTxt + ENTRY(liCount,lcText,CHR(9)) + CHR(10).
            END.

            /* title as subject */
            IF InvText.TxtTitle > "" THEN lcMailSubj = InvText.TxtTitle.
         END. 
         
         IF llInvFile THEN DO:
            FIND bInvoice WHERE RECID(bInvoice) = RECID(Invoice) 
            EXCLUSIVE-LOCK.
    
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).
         END.
         
         IF fSendPDFMail(lcPDFFile,
                         lcPDFMail,  
                         Customer.Language,
                         lcBodyTxt,
                         lcMailSubj,
                         lcPrintType)
         THEN DO:

            oiMailQty = oiMailQty + 1.
            
            IF llInvFile THEN DO:
                      /* mark that email has been sent */
               ASSIGN bInvoice.PrintState = 6
                      bInvoice.WInvDisp   = TRUE
                      ttPDFInv.Printed    = 1.
            END.
                
            /* write log */
            CREATE ITSendLog.           
            ASSIGN ITSendLog.Brand      = Invoice.Brand
                   ITSendLog.TxtType    = 1
                   ITSendLog.SendMethod = 1
                   ITSendLog.CustNum    = Invoice.CustNum
                   ITSendLog.InvNum     = Invoice.InvNum
                   ITSendLog.EMail      = lcMailAddr
                   ITSendLog.RepType    = IF llInvFile
                                          THEN (IF iiPrintType = 1 
                                                THEN "PDFRem"
                                                ELSE "PDFInv")
                                          ELSE "PDFCLISpec"
                   ITSendLog.UserCode   = katun.
                   ITSendLog.SendStamp  = fMakeTS().
                         
         END.
         
         ELSE DO:
            fErrorLog("Send via eMail failed").
            DELETE ttPDFInv.
         END. 
         
         IF llInvFile THEN DO:
            IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).
         END.   
      
      END.
      
   END.
      
END. /* FOR EACH Invoice */

oiErrQty = liErrQty.

/* finish PDF printing (send errorlog via email etc.) */
fPDFFinish(lcPrintType,
           IF iiPrintType = 1 THEN "reminders" ELSE "invoices"). 

