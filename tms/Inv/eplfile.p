/* ----------------------------------------------------------------------------
  MODULI .......: EPLFILE.P
  TEHTAVA ......: CREATE INVOICES WITH ELETTER LIMITS
                  GET'S INPUT FROM ELETTERINV.P
  SOVELLUTUS ...: NN
  TEKIJA .......: JR
  LUONTIPVM ....: 27.08.01
  MUUTOSPVM ....: 25.10.01/jr CHOOSE all/not printed invoices
                  21.11.01 ht few functions into refcode.i
                  04.12.01 ht RUN reports
                  07.12.01/aam variables TO edefine.i
                  07.12.01 ht  DDCustomer
                  12.12.01/aam get product name from invlang,
                               DISPLAY country FOR customer,
                  21.12.01/aam FOR Jippii,
                               use cparam2.i etc.
                  06.02.02/aam advance payments
                  10.02.02/aam NEW report FOR contracts (nncore1),
                               test sp-code[1] (deny printing),
                               separate files FOR invoices containing calls
                  11.02.02/aam invgroup.ig-cpers left out 
                  13.02.02/aam InvRow.FromDate AND ToDate can be blank 
                  25.02.02/aam credit invoices can be printed optionally
                  08.03.02/aam move the files TO a transfer directory
                  11.03.02/aam send errorlog WITH email,
                               overtime interest percent from fOtIntPerc,
                               reminders,
                               renamed as "eplfile.p", eletterinv.p only
                               collects invoices TO a TEMP-TABLE 
                  02.04.02/aam DELETE wInvoice when error occurs (fErrLine) 
                  07.06.02/aam use Invoice.oikera FOR overpayment
                  06.08.02/aam show vat% for each invoice line,
                               total for each vat% after invoice lines,
                               assign header texts to temptable  
                  02.09.02/aam text FOR customers who have remaining 
                               overpaym. balance (85-88)
                  04.09.02/aam barcode into use
                  11.09.02/aam shorter names for output files 
                  12.12.02/jp  National postcode length must be less than      
                               5 character
                  07.01.03/aam bitem-lines are combined by product,
                               subtitles for all rows,
                               contact-person for testing (EPLContact)
                  23.01.03/jp  fatime              
                  24.01.03/aam nncore1 not for type 3 or 4 invoices  
                  11.02.03/aam new channel 9 for invoice sheet's backside,
                               letter class from parameters 
                  26.02.03/aam direct debit              
                  11.03.03/aam check debt once more for reminders
                  31.03.03/aam boxes between product lines and bank-transfer
                  28.04.03/aam use fprintinv (common routines for all
                               different types of invoice printing routines)
                  20.05.03/aam use finvbal             
                  21.05.03/aam routines into functions to edefine.i
                  22.08.03/aam set ws-disp = true,
                               form pdf-invoices 
                  09.10.03/aam fBarCodeNum
                  27.10.03/aam multiple vat percents
                  16.12.03/aam VatUsage,
                               new layout (IDel* etc.) 
                  20.01.04/aam specifications on cli level             
                  22.03.04/aam invoice rows on cli level
                  07.04.04/aam divide into files according to sheet qty
                  07.05.04/aam ItSendLog
                  28.05.04/aam SMS,
                               compare cli address to customer (spec reports)
                  02.06.04/aam don't print Sender under logo              
                  03.01.05/aam RepCode from SubSer (MsOwner)
                  04.02.05/aam use fCallSpecDuring()
                  06.05.05/aam invoice row amounts with 3 decimals
                  02.08.05/aam sender address under logo
                  06.09.05/aam info text under invrow (type 6) with font J
                  07.10.05/aam print by zipcode (not an absolute order though)
                  14.12.05/aam username from customer, not msowner
                  21.12.05/aam icFile may contain transfer directory 
                  12.01.06/aam nul line counter after texts before 1. row
                  30.01.06/aam print a letter as attachment
                  02.05.06/aam check if address data is empty
                  10.05.06/aam return last error message to caller
                  21.06.06/aam iiFileType, EndInvoice etc. 
                  07.08.06/aam letterclass to fPrintSpecRep
VERSIO .......: M15
---------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/feplform.i}
{Func/ftransdir.i}
{Func/email.i}
{Func/fotint.i}
{Func/refcode.i}
{Inv/edefine.i}
{Syst/utumaa.i new}
{Func/finvbal.i}
{Inv/pdfinvdf.i}
{Func/fbarcode.i}
{Func/fprintinv.i}
{Inv/invprdf.i}
{Func/fgetclis.i}
{Func/fsubser.i}
{Inv/eplinvatt.i}

DEF TEMP-TABLE wError NO-UNDO
    FIELD Inv    AS INT
    FIELD Cust   AS INT
    FIELD ErrMsg AS CHAR.

DEF TEMP-TABLE ttHead NO-UNDO
    FIELD Lang  AS INT
    FIELD Nbr   AS INT
    FIELD HTxt  AS CHAR
    INDEX Lang IS UNIQUE Lang Nbr.

   /* invoices TO be printed */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR wInvoice.

   /* print DATE (FOR reminders)  */
DEFINE INPUT  PARAMETER iPrintDate   AS DATE NO-UNDO. 
   /* type of file 0=invoices, 1= reminders, 2=print service */
DEFINE INPUT  PARAMETER iPrintType   AS INT  NO-UNDO.  
   /* letter class */
DEFINE INPUT  PARAMETER iLetterClass AS INT  NO-UNDO. 
   /* how many invoices are TO be printed */
DEFINE INPUT  PARAMETER iInvCount    AS INT  NO-UNDO. 
   /* printing file */
DEFINE INPUT  PARAMETER icFile       AS CHAR NO-UNDO.
   /* 1=normal file (separate),       
      4=attachment i.e. printed in the end of another letter (not for ps) */    
DEFINE INPUT  PARAMETER iiFileType   AS INT  NO-UNDO.
   /* how many were printed */
DEFINE OUTPUT PARAMETER oInvCount    AS INT  NO-UNDO. 

DEF STREAM slog.
DEF STREAM sRead.

DEFINE VARIABLE LCtiedosto  AS CHAR      NO-UNDO EXTENT 3.
DEFINE VARIABLE tilinro     AS CHAR      NO-UNDO.
DEFINE VARIABLE vkorko      AS DEC       NO-UNDO.

DEF VAR cur         AS CHAR  NO-UNDO.
DEF VAR xProd       AS CHAR  NO-UNDO.
DEF VAR xOk         AS LOGIC NO-UNDO.
DEF VAR xDelivery   AS CHAR  NO-UNDO.
DEF VAR xCount      AS INT   NO-UNDO.

DEF VAR xRefAmt     AS CHAR  NO-UNDO. 
DEF VAR xRemLang    AS INT   NO-UNDO. 
DEF VAR xHeadTxt    AS CHAR  NO-UNDO. 

DEF VAR lcRefLine   AS CHAR  NO-UNDO. 
DEF VAR lcRefAmt    AS CHAR  NO-UNDO. 
DEF VAR lcSubHeader AS CHAR  NO-UNDO.
DEF VAR ldDispAmt   AS DEC   NO-UNDO. 
DEF VAR lcErrTxt    AS CHAR  NO-UNDO. 
DEF VAR llFormPDF   AS INT   NO-UNDO. 
DEF VAR lcRemText   AS CHAR  NO-UNDO. 
DEF VAR lcVatUsage  AS CHAR  NO-UNDO. 
DEF VAR liCustCnt   AS INT   NO-UNDO. 
DEF VAR lcTmpFile   AS CHAR  NO-UNDO. 
DEF VAR lcLine      AS CHAR  NO-UNDO. 
DEF VAR lcRepCode   AS CHAR  NO-UNDO. 
DEF VAR lcAbsDir    AS CHAR  NO-UNDO.
DEF VAR lcCLIName   AS CHAR  NO-UNDO. 
DEF VAR llAttach    AS LOG   NO-UNDO. 
DEF VAR lcLastError AS CHAR  NO-UNDO.

DEF BUFFER bInv FOR Invoice.

FUNCTION fHeadTxt RETURNS CHARACTER
   (iNbr AS INT,
    iLang  AS INT).
    
   FIND FIRST ttHead WHERE
      ttHead.Lang = iLang AND
      ttHead.Nbr  = iNbr NO-ERROR.
   IF NOT AVAILABLE ttHead THEN 
   FIND FIRST ttHead WHERE
      ttHead.Lang = 1 AND
      ttHead.Nbr  = iNbr NO-ERROR.
   
   IF AVAILABLE ttHead THEN RETURN ttHead.HTxt.
   ELSE RETURN "".
   
END FUNCTION.
 
FUNCTION fLinePaging RETURNS LOGICAL
    (iAddLine AS INT).

    IF lierrivi + iAddLine >= xInvLines THEN DO:

        IF etusivuerit THEN DO:
            RUN tulostalaosa.
            
            ASSIGN lierrivi    = 0
                   etusivuerit = FALSE.
                   
            RUN erittely.
        END.

        ELSE DO:
            ASSIGN lierrivi = 0
                   lisaerit = TRUE.
            RUN erittely.
        END.
         
        RETURN TRUE.
    END.

    ELSE RETURN FALSE. 
    
END FUNCTION.

FUNCTION fLineHead RETURNS LOGICAL
   (ilNewSection AS LOG).

    IF iPrintType NE 1 THEN DO:
       PUT STREAM ekirje UNFORMATTED
          (IF ilNewSection
           THEN "3" ELSE " ")
          /* bold */
          "8"                      
          FILL(" ",49).
          
       IF lcRowHeader[11] > "" OR lcRowHeader[12] > "" OR lcRowHeader[16] > ""
       THEN DO:
          PUT STREAM ekirje UNFORMATTED
             STRING(lcRowHeader[11],"x(19)") 
             SPACE(1)
             STRING(lcRowHeader[12],"x(5)")
             SPACE(2)
             STRING(lcRowHeader[16],"x(13)")
             MY-NL
             " 8"
             FILL(" ",49).
          lierrivi = lierrivi + 1.
       END.
       
       PUT STREAM ekirje UNFORMATTED
          STRING(lcRowHeader[1],"x(19)") 
          SPACE(1)
          STRING(lcRowHeader[2],"x(5)")
          SPACE(2)
          STRING(lcRowHeader[6],"x(13)")
          MY-NL.
       lierrivi = lierrivi + 1.
    END.

END FUNCTION.
         
FUNCTION fErrLine RETURNS LOGICAL
    (iMessage AS CHAR).

    CREATE wError.
    ASSIGN wError.Inv    = Invoice.InvNum
           wError.Cust   = Invoice.CustNum
           wError.ErrMsg = iMessage
           lcLastError   = iMessage.

    /* delete the temp-table, so that "printstate" doesn't get marked */
    DELETE wInvoice. 

END FUNCTION.


FUNCTION fPrintInvoTxt RETURNS LOGICAL
   (iiPosition AS INT,   /* position, 1=beginning of invoice etc. */
    icTarget   AS CHAR,  /* table name for some cases   */
    icKey      AS CHAR). /* "" for header level texts, InvRowNum for InvRows */
   
   DEF VAR llCoverSheet AS LOG NO-UNDO.
   
   llCoverSheet = FALSE.
   
   FOR EACH ttInvoTxt WHERE
            ttInvoTxt.ITPos = iiPosition AND
            (IF icTarget NE ""
             THEN ttInvoTxt.ITTarg = icTarget
             ELSE TRUE)                  AND 
            (IF icKey NE ""
             THEN ttInvoTxt.ITKey = icKey
             ELSE TRUE)                  AND
            (ttInvoTxt.ITTxt > "" OR ttInvoTxt.ITTitle > "")
   BY ttInvoTxt.ITOrd:
      
      /* replace tags */
      ttInvoTxt.ITTitle = fReplaceTag(ttInvoTxt.ITTitle,0).
      ttInvoTxt.ITTxt   = fReplaceTag(ttInvoTxt.ITTxt,0).
             
      /* divide into lines */
      ttInvoTxt.ITTxt = fSeparateInvoTxt(ttInvoTxt.ITTxt,75).
            
      IF ttInvoTxt.ITPos = -1 AND llCoverSheet = FALSE THEN DO:
      
         ASSIGN llCoverSheet = TRUE
                xInvLines    = liCoverPage1.
         
         PUT STREAM ekirje UNFORMATTED
            "5H" 
            STRING(ttInvoTxt.ITMain,"X(35)")
            MY-NL
            "0I"
            lcDateHead " " 
            STRING(pvm,"99.99.9999")
            MY-NL
            "37"
            SPACE(4)
            ttInvoTxt.ITTitle 
            MY-NL
            " I"
            MY-NL.
         lierrivi = 2. 
      END.
            
      IF iiPosition = 2 OR iiPosition = 4 THEN DO:
         fLinePaging(0).
         PUT STREAM ekirje UNFORMATTED
            " H" 
            MY-NL.
         lierrivi = lierrivi + 1.
      END.
      
      IF ttInvoTxt.ITTitle > "" AND 
         (Invoice.InvType = 3 OR Invoice.InvType = 4) AND
         ttInvoTxt.ITPos NE -1
      THEN DO:
         fLinePaging(2).
         PUT STREAM eKirje UNFORMATTED
            (IF lierrivi = 0 
             THEN "3"
             ELSE " ")
            "7"
            SPACE(4)
            ttInvoTxt.ITTitle
            MY-NL
            " I"
            MY-NL.
         lierrivi = lierrivi + 2.
      END.
      
      DO xCount = 1 TO NUM-ENTRIES(ttInvoTxt.ITTxt,CHR(9)):

         fLinePaging(0).
      
         IF ENTRY(xCount,ttInvoTxt.ITTxt,CHR(9)) = "." OR
            ENTRY(xCount,ttInvoTxt.ITTxt,CHR(9)) = ""
         THEN PUT STREAM ekirje UNFORMATTED
            " I"
            MY-NL.
         
         ELSE PUT STREAM ekirje UNFORMATTED 
            (IF ttInvoTxt.ITPos = 6
             THEN " J"
             ELSE " I")
            (IF ttInvoTxt.ITPos = -1 OR Invoice.InvType = 3 OR 
                Invoice.InvType = 4
             THEN FILL(" ",11)
             ELSE "")
            ENTRY(xCount,ttInvoTxt.ITTxt,CHR(9))
            MY-NL.
         
         lierrivi = lierrivi + 1.
      END.                         

      IF iiPosition = 1 OR iiPosition = 3 THEN DO:
         fLinePaging(0).
         PUT STREAM ekirje UNFORMATTED
            " H" 
            MY-NL.
         ASSIGN lierrivi = lierrivi + 1.
      END.

      ELSE IF iiPosition = -1 THEN DO:
         /* contact info to channel 4 */
         fContactInfo().
      END.

      /* delete printed texts, this way row specific texts won't be repeated */
      DELETE ttInvoTxt.
   END.
   
   /* if a separate cover -> start a new page for invoice */  
   IF llCoverSheet THEN DO:
      
      xEplForm = "EPL" + lcInvForm.
      
      fEPLReceiver(""). 
      
      ASSIGN lierrivi  = 0
             xInvLines = LiInvPage1
             liSheets  = liSheets + 1.
                  
   END.

   /* if in the beginning of invoice -> nul line counter */
   IF iiPosition = 1 THEN lierrivi = 0.
         
   RETURN TRUE. 
   
END FUNCTION.

/* printing as an attachment is not possible to printservice */ 
IF iiFileType = 4 AND iPrintType = 2 THEN RETURN "Invalid printing definition".

IF iiFileType NE 4 THEN DO:
   fEPLInit().
END.

/* transfer directory given */
IF INDEX(icFile,"*") > 0 THEN ASSIGN 
   xTransDir   = ENTRY(1,icFile,"*")
   icFile      = ENTRY(2,icFile,"*")
   xPSTransDir = "".

ASSIGN cur           = fCParamC("DefCurrency")
       llFormPDF     = fCParamI("EPLFormPDF")
       LCtiedosto    = icFile
       LLfirst       = TRUE
       xRefAmt       = IF iInvCount = 0
                       THEN ""
                       ELSE " / " + STRING(iInvCount)
       liTextLength  = 90.

/* use printservice instead of eletter */
IF iPrintType = 2 THEN DO:
   ASSIGN lcPrintHouse = "ps"
          iPrintType   = 0.
   
   /* for printservice invoices are divided into separate files according
      to sheet quantity */
   ASSIGN LCtiedosto[1] = LCtiedosto[1] + "_c5"
          LCtiedosto[2] = LCtiedosto[2] + "_c4"
          lcTiedosto[3] = lcTiedosto[3] + "_mn"
          lcTmpFile     = "ps" + STRING(TODAY,"999999") +
                          STRING(TIME) + ".tmp".
END. 

/* make sure that values are valid */
IF iPrintDate = ? THEN iPrintDate = TODAY.
IF iLetterClass = ? OR
   iLetterClass < 1 OR 
   iLetterClass > 4 
THEN iLetterClass = 2.


FIND FIRST Company WHERE
           Company.Brand = gcBrand NO-LOCK NO-ERROR.
IF NOT AVAIL Company THEN RETURN.

/* header texts to temp-table */
FOR EACH HdrText NO-LOCK WHERE
         HdrText.Brand = gcBrand:
      CREATE ttHead.
      ASSIGN ttHead.Lang = HdrText.te-kie
             ttHead.Nbr  = HdrText.te-nro
             ttHead.HTxt = HdrText.te-text.
END.

EMPTY TEMP-TABLE wError. 


EPLMainLoop:
FOR EACH wInvoice,
   FIRST Invoice OF wInvoice NO-LOCK,
   FIRST Customer OF Invoice NO-LOCK
BY wInvoice.ZipCode
BY wInvoice.InvNum:

   /* check debt amount for reminders, new payments may have been made 
      during this process */
   IF iPrintType = 1 THEN DO: 

      wInvoice.Amt = fInvBal(BUFFER Invoice,
                             TODAY).
       
      IF wInvoice.Amt <= 0 OR
         wInvoice.Amt < wInvoice.MinAmt
      THEN DO:
         fErrLine("Payment booked after process started"). 
         NEXT.
      END.
   END.
    
   /* if specifications on cli level wanted -> check cli data */
   IF iPrintType NE 1 AND Invoice.InvType NE 3 AND Invoice.InvType NE 4 
   THEN DO:
 
      /* get clis that are involved in this invoice */
      fGetCLIs(""). 
     
      FOR EACH wCLI WHERE 
               wCLI.OwnerID > 0,
         FIRST MSOwner NO-LOCK WHERE
               RECID(MsOwner) = wCLI.OwnerID,
         FIRST bCLICust NO-LOCK WHERE
               bCLICust.CustNum = MsOwner.CustNum:
          
         lcRepCode = STRING(fCallSpecDuring(MSOwner.MsSeq,
                                            Invoice.InvDate)).
         IF lcRepCode = "" THEN NEXT. 

         /* check that address etc. are valid */
         lcErrTxt = fEPLCheckAddr(iLetterClass,
                                  Customer.CustName, /* just checked if empty
                                                     -> lasname is enough */
                                  Customer.ZipCode,
                                  Customer.Country,
                                  OUTPUT wCLI.Country).
         IF lcErrTxt NE "" THEN DO:
            fErrLine(lcErrTxt).
            NEXT EPLMainLoop.
         END. 
      END.
          
   END.
        
   /* invoice form and according bank account  */
   fInvoiceForm(iPrintType). 

   xEplForm = "EPL" + lcInvForm.
   
   /* bankaccount is mandatory for bar code */
   IF lcBarCodeBank = "" THEN DO:
      fErrLine("Bank Account is missing for form " + xEplForm). 
      NEXT EPLMainLoop.
   END. 
 
   /* get customer names, payment terms etc. */
   fSetHeaders(0).
    
   ASSIGN lcEPLRName    = lcCustName
          lcEPLRLast    = ""
          lcEPLRFirst   = ""
          lcEPLRCoName  = lcCoName
          lcEPLRAddr    = lcAddress
          lcEPLRZipCode = ENTRY(1,lcPost," ")
          lcEPLRPost    = lcPost
          lcEPLRCountry = lcCountry.

   /* check that address etc. are valid */
   lcErrTxt = fEPLCheckAddr(iLetterClass,
                            lcEPLRName,
                            lcEPLRZipCode,
                            lcEPLRCountry,
                            OUTPUT lcEPLACountry).

   IF lcErrTxt NE "" THEN DO:
      fErrLine(lcErrTxt).
      NEXT EPLMainLoop.
   END. 

   IF lcEPLRAddr = "" AND lcEPLRZipCode = "" AND lcEPLRPost = "" THEN DO:
      fErrLine("Address data is missing").
      NEXT EPLMainLoop.
   END.
      
   /* dd-handling not for reminders */
   IF iPrintType = 1 THEN ASSIGN
      DDCustomer = FALSE
      DDText     = "".

   liPituus = 1.

   IF LLfirst[LIpituus] AND lcPrintHouse NE "ps" THEN DO:

      IF iiFileType NE 4 THEN DO:
         /* check that file doesn't exist and form the complete name */
         LCtiedosto[LIpituus] = fEPLFileName(LCtiedosto[LIpituus]).
      
         OUTPUT STREAM ekirje TO VALUE(LCtiedosto[LIpituus]).
       
         /* main header for EPL file (sender data) */
         fEPLMainHeader(iLetterClass).
   
         OUTPUT STREAM ekirje CLOSE. 
      END.
      
      LLfirst[LIpituus] = FALSE.
   END.

   /* if printed to printhouse then first print to a temp file and
      then divide it according to sheet quantities, 
      else just print directly the final file 
   */
   
   IF lcPrintHouse = "ps" 
   THEN OUTPUT STREAM ekirje TO VALUE(lcTmpFile) APPEND.
   ELSE OUTPUT STREAM ekirje TO VALUE(LCtiedosto[LIpituus]) APPEND.

   ASSIGN
      LLcakanto   = FALSE
      alaosa      = FALSE
      lierrivi    = 0
      lisaerit    = FALSE
      etusivuerit = TRUE
      xInvLines   = LiInvPage1.

   oInvCount  = oInvCount + 1.

   IF oInvCount MOD 100 = 0 THEN DO:
       PAUSE 0.
       DISPLAY oInvCount FORMAT ">>>>>>>9"
               xRefAmt   FORMAT "x(12)"
       WITH NO-LABELS OVERLAY ROW 10 CENTERED
            TITLE " Printing " FRAME fPrint.
   END.

  
   IF iPrintType NE 1 THEN DO: 
      /* all sorts of texts that should be printed */
      fGetInvoiceTxt().
   END.
   ELSE DO:
   END. 
 
   lcRepFLine = "".  
   /* is a separate cover sheet defined */
   FOR FIRST ttInvoTxt WHERE
             ttInvoTxt.ITPos = -1:
      IF ttInvoTxt.ITForm > ""
      THEN ASSIGN xEPLForm   = "EPL" + ttInvoTxt.ITForm
                  lcRepFLine = FILL(" ",74) + "1".
                 
   END.
   
   /* this is FOR sorting purposes, NOT included in final epl-file */
   IF lcPrintHouse = "ps" THEN 
   PUT STREAM ekirje UNFORMATTED
     "%%" STRING(Invoice.InvNum)
     MY-NL.
                     
   lcRepFLine = lcRepFLine + "¤" + lcBTName + 
                             "¤" + lcHSender +
                             "¤" + lcBTCCAddr[1] +
                             "¤" + lcBTCCAddr[2].
 
   /* receiver data; for attachment print only receiver address, for others
      print also an EPLK-line as the beginning of a new letter */
   IF iiFileType = 4 THEN DO:
      fEPLReceiver(lcRepFLine).
   END.
   ELSE DO:
      fEPLCustHeader(lcRepFLine).
   END.

   lcRepFLine = "". 
   
   /* separate cover sheet */
   fPrintInvoTxt(-1,
                 "",
                 "").
   
   /* different form FOR different type of invoice */

   PUT STREAM ekirje UNFORMATTED
       "5I"
       lcMainTitle
       MY-NL
       "0I".
 
   IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN 
   PUT STREAM ekirje UNFORMATTED
       STRING(lcInvHeader[4],"X(20)")
       STRING(Invoice.CustNum)
       MY-NL
       " I"
       STRING(lcInvHeader[8],"X(20)")
       STRING(lcTagOwner,"X(22)")
       MY-NL
       " I".
         
   PUT STREAM ekirje UNFORMATTED
       STRING(lcInvHeader[1],"X(20)")
       STRING(IF iPrintType = 1
              THEN iPrintDate
              ELSE Invoice.InvDate,"99.99.9999")
       MY-NL
       " I"
       STRING(lcInvHeader[2],"X(20)")
       Invoice.InvNum
       MY-NL
       " I"
       STRING(lcInvHeader[3],"X(20)")
       (IF Invoice.EndInvoice > 0 
        THEN lcPaymTerm
        ELSE STRING(IF iPrintType = 1
                    THEN iPrintDate
                    ELSE Invoice.DueDate,"99.99.9999"))
       MY-NL.

   IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN DO:

      PUT STREAM ekirje UNFORMATTED
         "0I"
         STRING(lcInvHeader[5],"X(20)")
         lcDelInt
         MY-NL
         " I"
         STRING(lcInvHeader[6],"X(20)")
         lcRemFee
         MY-NL.
       
      IF ldtNextDueDate NE ? AND iPrintType NE 1 THEN DO:
         lcInvHeader[7] = fSeparateInvoTxt(lcInvHeader[7],20).
      
         DO xCount = 1 TO NUM-ENTRIES(lcInvHeader[7],CHR(9)):    
            PUT STREAM ekirje UNFORMATTED
            (IF xCount = 1 THEN "0" ELSE " ")
            "I"
            STRING(ENTRY(xCount,lcInvHeader[7],CHR(9)),"X(20)").
         
            IF xCount = NUM-ENTRIES(lcInvHeader[7],CHR(9)) THEN 
            PUT STREAM ekirje UNFORMATTED
            STRING(ldtNextDueDate,"99.99.9999").
         
            PUT STREAM ekirje UNFORMATTED MY-NL.
         END. 
      END.
         
   END.
   
   IF iPrintType NE 1 THEN DO: 
      /* print out invoice texts with position 1 */
      fPrintInvoTxt(1,
                    "",
                    "").
   END.
                 
   IF Invoice.InvType NE 3  AND
      Invoice.InvType NE 4  AND 
      iPrintType NE 1 AND 
      CAN-FIND(FIRST InvRow OF Invoice) 
   THEN DO:

        RUN erittely.

        fSortInvRows().

        FOR EACH ttLine
        BY ttLine.Order:
        
           fLinePaging(0).

           DO xCount = 1 TO NUM-ENTRIES(ttLine.RowID): 
              /* print row specific invoice texts with position 5 */
              fPrintInvoTxt(5,
                            "",
                            ENTRY(xCount,ttLine.RowID)).
           END.
                         
           PUT STREAM ekirje UNFORMATTED
               " I"                          
               STRING(ttLine.RowName,"x(48)")    
               FILL(" ",1) 

               (IF ttLine.FromDate = ?
                THEN FILL(" ",8)
                ELSE STRING(ttLine.FromDate,"99.99.99"))
               (IF ttLine.FromDate = ?
                THEN FILL(" ",3)
                ELSE " - ")
               (IF ttLine.ToDate = ?
                THEN FILL(" ",8)
                ELSE STRING(ttLine.ToDate,"99.99.99"))
               FILL(" ",1)
               
               (IF ttLine.VatPerc >= 0 
                THEN STRING(ttLine.VatPerc,">>9.9")
                ELSE FILL(" ",5))
               FILL(" ",3) 
               
               STRING(ttLine.Amount,"->>>,>>9.999")
               MY-NL.

           lierrivi = lierrivi + 1.

           DO xCount = 1 TO NUM-ENTRIES(ttLine.Memo,"¤"):

              lcText = ENTRY(xCount,ttLine.Memo,"¤").
              
              IF lcText = "" THEN NEXT.
              
              fLinePaging(0).
              
              IF lcText BEGINS "#" OR lcText = "."
              THEN PUT STREAM ekirje UNFORMATTED
                  " 2" MY-NL.
              ELSE PUT STREAM ekirje UNFORMATTED
                  " 2" lcText MY-NL.
              lierrivi = lierrivi + 1.
           END.

           DO xCount = 1 TO NUM-ENTRIES(ttLine.RowID): 
              /* print row specific invoice texts with position 6 */
              fPrintInvoTxt(6,
                            "",
                            ENTRY(xCount,ttLine.RowID)).
           END.
                          
        END. 

        fLinePaging(1).
        PUT STREAM ekirje UNFORMATTED
            " I"
            MY-NL.
        ASSIGN lierrivi = lierrivi + 1.
            
        xCount = 0.        
        FOR EACH ttVat:        
           xCount = xCount + 1.
        END.
        
        IF Invoice.VatUsage < 3 THEN 
        FOR EACH ttVAT:
           
           fLinePaging(xCount - 1).
           
           /* basis for vat */
           IF ttVAT.VatPerc > 0 THEN DO:
              ASSIGN ttVAT.SlsAmt = 0
                     ttVAT.VATAmt = 0.
              DO xCount = 1 TO 10:
                 IF Invoice.VatPercent[xCount] = ttVat.VatPerc
                 THEN ASSIGN ttVAT.SlsAmt = ttVAT.SlsAmt + 
                                            Invoice.VATBasis[xCount] -
                                            (IF Invoice.VATIncl
                                             THEN Invoice.VatAmount[xCount]
                                             ELSE 0)
                             ttVat.VatAmt = ttVat.VatAmt +
                                            Invoice.VatAmount[xCount].
              END.                                            
           END.
                         
           PUT STREAM ekirje UNFORMATTED
              " I"
              ttVat.Text1
              ": "
              STRING(ttVAT.SlsAmt,"->>>>>9.999")
              FILL(" ",3)
              ttVat.Text2
              " "
              STRING(ttVAT.VatPerc,">9")
              "%".             

           /* vat amount */           
           IF ttVAT.VatPerc NE 0 THEN 
           PUT STREAM ekirje UNFORMATTED
              ": "
              STRING(ttVat.VatAmt,"->>>>9.999").

           PUT STREAM ekirje UNFORMATTED
              " " 
              ttVat.Text3.
              
           PUT STREAM ekirje UNFORMATTED 
              MY-NL.
              
           ASSIGN lierrivi = lierrivi + 1.
        END.
        ELSE DO:
           lcVatUsage = fVatUsageTitle(Invoice.VatUsage).
           
           IF lcVatUsage > "" THEN DO:
           
              fLinePaging(0).
              
              PUT STREAM ekirje UNFORMATTED
                 " I"
                 lcVatUsage 
                 MY-NL.
                 
              lierrivi = lierrivi + 1.
           END.        
        END.
        
   END.

   IF iPrintType NE 1 THEN DO: 
      /* print out invoice texts with position 2 */
      fPrintInvoTxt(2,
                    "",
                    "").
   END.
   
   /* reminders have a layout of their own FOR line section */
   IF iPrintType = 1 THEN DO:
      RUN erittely.

      /* print out invoice texts with position 1 */
      fPrintInvoTxt(1,
                    "",
                    "").
                    
      PUT STREAM ekirje UNFORMATTED 
         " H"
         STRING(fHeadTxt(65,LIkieli) + ":","x(8)")
         fill(" ",2)
         string(Invoice.InvNum,">>>>>>>9")
         fill(" ",2)
         string(Invoice.InvDate,"99.99.9999") 
         fill(" ",2)
         /* invoicing customer's name for agr.customer */
         (IF Customer.CustNum NE Invoice.CustNum
          THEN Customer.CustName
          ELSE "")
         MY-NL
         " H"
         STRING(fHeadTxt(79,LIkieli) + ":","x(9)")
         fill(" ",11)
         string(Invoice.DueDate,"99.99.9999")
         MY-NL
         " H"
         string(fHeadTxt(80,LIkieli) + ":","x(8)")
         fill(" ",9)
         STRING(Invoice.InvAmt,"->>>>>>>>9.99")
         MY-NL.

      ASSIGN lierrivi = lierrivi + 3.

      fLinePaging(1).
      PUT STREAM ekirje UNFORMATTED
         " H"
         MY-NL
         " H"
         STRING(fHeadTxt(78,LIkieli) + ":","x(8)")
         FILL(" ",9)
         STRING(wInvoice.Amt,"->>>>>>>>9.99")
         MY-NL.

      ASSIGN lierrivi = lierrivi + 2.

      /* print out invoice texts with position 2 */
      fPrintInvoTxt(2,
                    "",
                    "").
                    
   END.  /* iprinttype = 1 */

   IF NOT alaosa THEN RUN tulostalaosa.

   ASSIGN LLcasivu = 1
          lckutsu  = xEplForm.

   /* contract specification (started from the back side of invoice sheet
      when possible) */
   IF iPrintType NE 1      AND 
      Invoice.InvType NE 3 AND
      Invoice.InvType NE 4 AND
      llFeeLine
   THEN DO:
       IF CAN-FIND(FIRST InvRow OF Invoice WHERE
                         /* InvRow.Qty > 1 AND */
                         LOOKUP(STRING(InvRow.RowType),"3,4,7") > 0)
       THEN RUN Mc/nncore1 (Invoice.InvNum, TRUE). 
   END.
  
   /* call reports are always started from separate report sheet 
   ASSIGN etusivuerit = FALSE.
   */

   llAttach = FALSE.
   
   /*************** CALL SPECIFICATION ***************************************/
   IF Invoice.InvType  NE 3 AND
      Invoice.InvType  NE 4 AND
      iPrintType NE 1 
   THEN DO:
    
      fPrintSpecRep(Customer.RepCodes,
                    "",
                    1,
                    iLetterClass).
        
      /* specifications to cli level 
         first print clis that have same address as customer (continue with 
         same letter), then clis that have different address (separate letters)
      */
      DO xCount = 1 TO 2:

         IF xCount = 2 THEN DO:
            /* attachment letter has to be printed here, before a new
               letter starts for specification with different address */
            IF Invoice.InvDate >= 2/1/6 AND Invoice.InvType < 3 THEN DO:
               fPrintAttachment(Invoice.CustNum,
                                iLetterClass).
               llAttach = TRUE.
            END.
         END.
         
         FOR EACH wCLI WHERE 
                  wCLI.OwnerID > 0,
            FIRST MSOwner NO-LOCK WHERE
                  RECID(MsOwner) = wCLI.OwnerID,
            FIRST bCLICust NO-LOCK WHERE
                  bCLICust.CustNum = MsOwner.CustNum:
                  
            lcRepCode = STRING(fCallSpecDuring(MSOwner.MsSeq,
                                               Invoice.InvDate)).
            IF lcRepCode = "" THEN NEXT. 
         
            IF INDEX(lcRepCode,"3") > 0 OR
               INDEX(lcRepCode,"4") > 0
            THEN DO:

               /* user = invoice customer */
               IF bCLICust.CustNum = Invoice.CustNum THEN DO:
                  IF xCount = 2 THEN NEXT.
               END.
               ELSE IF xCount = 1 THEN NEXT. 
 
               lcCLIName = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                            BUFFER bCLICust).

               /* start a new letter */
               IF xCount = 2 THEN ASSIGN 
                  etusivuerit   = FALSE
                  llCaKanto     = FALSE
                  llCaSivu      = -1
                  lcEPLRName    = lcCLIName
                  lcEPLRLast    = bCLICust.CustName
                  lcEPLRFirst   = bCLICust.FirstName
                  lcEPLRCoName  = bCLICust.COName
                  lcEPLRAddr    = bCLICust.Address
                  lcEPLRZipCode = bCLICust.ZipCode
                  lcEPLRPost    = bCLICust.ZipCode + " " + 
                                  bCLICust.PostOffice
                  lcEPLRCountry = bCLICust.Country
                  lcEPLACountry = wCli.Country.
                  
               fPrintSpecRep(lcRepCode,
                             MsOwner.CLI,
                             1,
                             iLetterClass).
            END.
                             
         END. 
      END.
         
   END. /* specifications */

   /* attachment letter */
   IF NOT llAttach AND Invoice.InvDate >= 2/1/6 AND Invoice.InvType < 3 
   THEN DO:
      fPrintAttachment(Invoice.CustNum,
                       iLetterClass).
   END.
 
   OUTPUT STREAM ekirje CLOSE.

   IF AVAILABLE wInvoice THEN DO:
      IF lcPrintHouse NE "ps" THEN wInvoice.Printed = 1.
      wInvoice.Sheets  = liSheets.
 
      /* log from print */
      DO FOR ITSendLog TRANS:
         CREATE ITSendLog.
         ASSIGN ITSendLog.Brand      = gcBrand 
                ITSendLog.TxtType    = IF iPrintType = 1 THEN 4 ELSE 3
                ITSendLog.ITNum      = 0
                ITSendLog.CustNum    = Invoice.CustNum
                ITSendLog.InvNum     = Invoice.InvNum
                ITSendLog.SendMethod = IF lcPrintHouse = "ps"
                                       THEN 3
                                       ELSE 2
                ITSendLog.EMail      = ""
                ITSendLog.RepType    = "Inv"
                ITSendLog.UserCode   = katun.
                ITSendLog.SendStamp  = fMakeTS().
      END.
 
   END. 
  
END.     /* END OF INVOICES             */

HIDE FRAME fPrint NO-PAUSE.

/* sort invoices for printservice according to sheet quantity */
IF lcPrintHouse = "ps" THEN DO:

   ASSIGN llFirst  = TRUE
          liPituus = 0.
   
   INPUT STREAM sRead FROM VALUE(lcTmpFile) NO-ECHO.
   
   REPEAT:
       IMPORT STREAM sRead UNFORMATTED lcLine.
       
       /* invoice begins */
       IF lcLine BEGINS "%%" THEN DO:

          OUTPUT STREAM ekirje CLOSE.
       
          FIND FIRST wInvoice WHERE 
                     wInvoice.InvNum = INTEGER(SUBSTRING(lcLine,3)) NO-ERROR.
          IF NOT AVAILABLE wInvoice THEN liPituus = 0.
          ELSE DO:
       
             IF wInvoice.Sheets <= 6 THEN liPituus = 1.
             ELSE IF wInvoice.Sheets <= 40 THEN liPituus = 2.
             ELSE liPituus = 3. 
       
             wInvoice.Printed = 1.
          END.
       
          IF liPituus NE 0 THEN DO:
       
             /* start a new file */
             IF LLfirst[liPituus] THEN DO:

                /* check that file doesn't exist and form the complete name */
                LCtiedosto[LIpituus] = fEPLFileName(LCtiedosto[LIpituus]).

                OUTPUT STREAM ekirje TO VALUE(LCtiedosto[LIpituus]).
       
                /* main header for EPL file (sender data) */
                fEPLMainHeader(iLetterClass).
                     
                LLfirst[LIpituus] = FALSE.
             END.  
          
             /* append to an existing file */
             ELSE OUTPUT STREAM ekirje TO VALUE(lcTiedosto[liPituus]) APPEND.
          END.
          
       END.    

       /* print line as it is */
       ELSE DO:
          IF liPituus NE 0 THEN
          PUT STREAM ekirje UNFORMATTED lcLine MY-NL.
       END.
       
   END.

   INPUT STREAM sRead CLOSE.
   OUTPUT STREAM ekirje CLOSE.

   /* remove the tmp-file */
   OS-DELETE VALUE(lcTmpFile).

END. /* printservice */

/* mark invoices AS printed */
IF iPrintType NE 1 THEN 
FOR EACH wInvoice WHERE
         wInvoice.Printed = 1,
FIRST Invoice OF wInvoice exclusive-lock,
FIRST Customer OF Invoice NO-LOCK:
       
   IF LOOKUP(STRING(Invoice.InvType),"3,4") = 0 THEN DO:
      /* form pdf-files */
      IF xEPLTest NE "T" AND llFormPDF = 1 THEN DO:
         CREATE ttPDFInv.
         ttPDFInv.InvNum = Invoice.InvNum.
      END. 
         
   END.

   ASSIGN Invoice.PrintState = 1
          Invoice.WInvDisp   = TRUE.

   RELEASE Invoice.    
END.

/* move the NEW files TO the actual transfer directory */
IF iiFileType NE 4 THEN DO LiPituus = 1 TO 3:
    fTransDir(LcTiedosto[LiPituus],
              xFileExt,
              IF lcPrintHouse = "ps"
              THEN xPSTransDir
              ELSE xTransDir).
END. 

/* possible errors */
IF CAN-FIND(FIRST wError) THEN DO:

    ASSIGN xErrFile = xErrFile + "_" + 
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99")  +
                                STRING(DAY(TODAY),"99")    + 
                                "_" + STRING(TIME) + ".txt".                                       
    OUTPUT STREAM slog TO VALUE(xErrFile).
    PUT STREAM slog UNFORMATTED
        "Invoice"   TAB
        "Customer"  TAB
        "Error"     MY-NL.

    FOR EACH wError:
        PUT STREAM slog UNFORMATTED
            wError.Inv    TAB
            wError.Cust   TAB
            wError.ErrMsg MY-NL.
    END.

    OUTPUT STREAM slog CLOSE. 

    /* send the report AS email */
    ASSIGN xMailAttach = xErrFile
           xErrFile    = "/tmp/" +
                         (IF iPrintType = 1 THEN "rem" ELSE "inv") +
                         "epl_errmsg.txt".
    OUTPUT STREAM slog TO VALUE(xErrFile).
    PUT STREAM slog UNFORMATTED
        "Errors from creating " + 
        (IF iPrintType = 1 THEN "a reminder" ELSE "an invoice") +
        " EPL-file " + 
        STRING(TODAY,"99.99.9999") + "." + my-nl + my-nl +
        "Open the attachment file in Excel." + my-nl + my-nl + "  ".
    OUTPUT STREAM slog CLOSE.

    /* mail recipients AND actual sending */
    GetRecipients(xConfDir + 
                 (IF iPrintType = 1 THEN "rem" else "inv") +
                 "epl_error.email").
    SendMail(xErrFile,xMailAttach).

END.

/* form pdf-invoices */
IF CAN-FIND(FIRST ttPDFInv) THEN DO:
   RUN Inv/pdfinv(INPUT-OUTPUT TABLE ttPDFInv,
              INPUT  TRUE,      /* print specifications */
              INPUT  TRUE,      /* form pdf */
              INPUT  FALSE,     /* send pdf via email */
              INPUT  0,         /* printtype, 0=invoice */
              INPUT  ?,         /* reminding day */
              OUTPUT xCount,    /* nbr of pdf-files */
              OUTPUT xCount,    /* nbr of emails */
              OUTPUT xCount).   /* nbr of errors */
END.

/* return last error message, useable in case one invoice is printed */
RETURN lcLastError.


PROCEDURE tulostalaosa:

    IF iPrintType NE 1 THEN DO:
   
       /* text before bank transfer */
       IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 AND 
          lcBoxText > "" 
       THEN DO:

          IF liErrivi < xInvLines THEN DO:
             REPEAT:
                /* try to print as few lines as possible; 
                   "-" makes 2 empty lines ("I" makes the 3rd) */
                IF liErrivi + 3 <= xInvLines THEN DO:
                   PUT STREAM ekirje UNFORMATTED
                      "-I" MY-NL.
                   liErrivi = liErrivi + 3.
                END.
                /* "0" makes 1 empty line */
                ELSE IF liErrivi + 2 <= xInvLines THEN DO:
                   PUT STREAM ekirje UNFORMATTED
                      "0I" MY-NL.
                   liErrivi = liErrivi + 2.
                END.
                ELSE LEAVE.
             END.

             IF liErrivi < xInvLines THEN
             DO xCount = liErrivi + 1 TO xInvLines:
               PUT STREAM ekirje UNFORMATTED
                 " I" MY-NL.
             END.
          END.
          
          PUT STREAM ekirje UNFORMATTED
            "0J" 
            lcBoxText
            MY-NL.
       END.
       
       /* info before bank-transfer */
       fContactInfo().
    END.

    /* bank transfer */
    PUT STREAM ekirje UNFORMATTED
    "6I"                                    /* BANK SPECS. FIRST ROW        */
    lcBank[1]
    MY-NL

    " I"                                
    lcBank[2]
    MY-NL

    " I"                  
    lcBank[3]
    MY-NL

    "0I"                    
    STRING(lcBTName,"X(39)")
    FILL(" ",6)                     
    DDText[1] FORMAT "x(43)"               
    MY-NL

    " I"                                
    STRING(lcBTAddr,"x(39)")
    FILL(" ",6)            
    DDText[2] FORMAT "x(43)"       
    MY-NL

    " I"                           
    STRING(lcBTPost,"x(39)")
    FILL(" ",6)                 
    DDText[3] FORMAT "x(43)"       
    MY-NL

    "0I"          
    lcCustName                              /* PAYER NAME                   */
    MY-NL

    " I"                        
    STRING(lcAddress,"X(39)")               /* PAYER ADDRESS                */
    ""                              
    MY-NL

    " I"                          
    STRING(lcPost,"x(39)")                  /* PAYER POSTAL ADDRESS         */
    ""                           
    MY-NL

    " H"                      
    FILL(" ",39) 
    ""                  
    MY-NL

    "0H"                                    /* REFERENCE NUMBER FIRST ROW   */
    FILL(" ",37)   
    fViite(lcRefNum)                        /* REFERENCE NUMBER             */
    MY-NL

    "0H"                                    /* DATE & TOTAL FIRST ROW       */
    (IF DDCustomer 
     THEN STRING(DDBAccBT,"X(31)")          /* Customer's bank account (DD) */
     ELSE FILL(" ",31)) 
     
    FILL(" ",6) 
    (IF FALSE /* DDCustomer */
     THEN FILL("*",10)
     ELSE IF Invoice.EndInvoice > 0 
          THEN lcPaymTerm
          ELSE STRING(Invoice.DueDate,"99.99.9999"))  /* DUEDATE         */     

    (IF DDCustomer THEN
        FILL(" ",6) + "********"                             
     ELSE
        FILL(" ",6) +
        STRING(IF iPrintType = 1 
               THEN wInvoice.Amt
               ELSE Invoice.InvAmt,"->>>,>>9.99"))       /* TOTAL       */
    MY-NL.

    /* reference line for bar code */
    IF Invoice.InvAmt > 0 
    THEN lcRefLine = fBarCodeNum(Invoice.InvAmt,
                                 Invoice.DueDate,
                                 lcRefNum,
                                 lcBarCodeBank).
    ELSE lcRefLine = "".                            

    /* bar code, only for positive amounts */
    IF Invoice.InvAmt > 0 AND NOT DDCustomer AND
       LENGTH(lcRefLine) = 54
    THEN PUT STREAM ekirje UNFORMATTED
    "EPLB"                                  /* BAR CODE CALL EPLB     */
    "128C"                                  /* BANK BAR CODE FONT     */
    "8"                                     /* CHANNEL CODE           */
    "54"                                    /* BAR CODE LENGTH        */
    lcRefLine                               /* REFERENCE LINE         */
    MY-NL
    .

    alaosa = TRUE.                              /* BOTTOM PART IS WRITTEN   */

END PROCEDURE.

PROCEDURE erittely:

    /* additional pages */
    IF NOT etusivuerit THEN DO:

        /* back page of invoice form */
        IF NOT lisaerit THEN ASSIGN
            xEplForm    = REPLACE(xEplForm,"EPL5","EPL6")
            xInvLines   = LiInvPage2
            eritlaskuri = 2. 

        /* blanc form FOR specifications IF there are so many lines that
           invoice form's front AND back page aren't enough
        */
        ELSE DO:
            IF xEplForm = "EPL" + lcSpecForm
            THEN xEplForm = REPLACE(xEplForm,"EPL5","EPL6").
            ELSE ASSIGN xEplForm = "EPL" + lcSpecForm
                        /* new sheet begins */
                        liSheets = liSheets + 1.

            ASSIGN xInvLines   = LiRepPage        
                   eritlaskuri = eritlaskuri + 1.                    
        END.

        PUT STREAM ekirje UNFORMATTED                               
            "1H"                               /* STANDARD                   */
            MY-NL
            xEplForm
            MY-NL
            " I"                               /* CALL SPEC. FIRST ROW       */
            FILL(" ",70)
            fHeadTxt(28,LIkieli)                /* PAGE TEXT                  */
            " "                                /* EMPTY                      */
            eritlaskuri
            MY-NL
            " I"
            MY-NL. 
        lierrivi = 3.

        IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN DO:
           fLineHead(FALSE). 
        END. 
    END.

    /* specification TO invoice sheet (front side) */
    ELSE IF lierrivi = 0 THEN DO:
    
       IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN DO:
          fLineHead(TRUE).
       END.
       ELSE DO:
          PUT STREAM ekirje UNFORMATTED
            "3I"        
            MY-NL.

          lierrivi = 1. 
       END. 
       
    END. 

END PROCEDURE.


