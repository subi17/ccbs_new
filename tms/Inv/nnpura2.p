/* ------------------------------------------------------
  MODULE .......: NNPURA2.P
  FUNCTION .....: Erittely tuotteittain/puheluittain
  APPLICATION ..: NN
  AUTHOR .......: TT
  CREATED ......: 02.02.96
  MODIFIED .....: 30.04.96 /tt --> Mukaan erittely tuotteittain
                  03.05.96 /tt --> Sivunvaihtotestaus AINA ennen print-lineta
                  21.05.96 /tt --> Lisatty tuntien formaattia yhdella (hhhh)
                  07.09.96 /tt --> Muutettu raportin ulkoasua, viivoja pois ym.
                  28.11.96 /tt --> Yhteensa-summiin vain Totalt, ei katkonimea
                  28.11.96 /tt --> Numeropeitto (***) pois
                  07.01.97 /tt --> add-new kenttia, uusi fontti/linejako
                  13.01.96 /tt --> Layout muutettu, boxit makrolla
                  16.01.97 /tt --> Aleprosentti suoraan puhelulta
                  24.03.97 /pt --> poistetaan 5 nollaa B-numerosta jos on
                  25.03.97 /pt --> optimoitu poimintaa
                  15.05.97 /pt --> 020-puheluiden A-tilaajat 1 linelle, otsi-
                                   koksi otsi[40]
                  10.10.97 /pt --> company name from invgroup record
                  09.02.98 /kl --> DATE FORMAT BY udate2c -module
                  31.03.98 /kl --> unumpref module call
                  04.06.98 /kl --> invno
                  25.11.98 /kl --> topay variables, ckesto into "x(12)"
                  23.05.99 /pt --> DISP owner ow a-sub NEXT  TO it
                  17.02.00 /kl  --> xbsub.i + actions
                  19.10.01/aam layout FOR Nevatel
                  30.10.01 ht  Normal price off
                  23.11.01 ht  VALUE krmin 
                  29.11.01 ht  VAT excluded
                  07.12.2001/aam eLetter 
                  11.12.2001/aam product name from invlang
                  18.12.2001/aam vat excluded also from "ertpris"
                  04.01.2002 ht VAT handling off from report, VAT text added,
                                topay sums AS euros, Total EUR OR FIM
                  09.01.2002 ht When FIM tariffs, LAST total also AS EUR
                  11.01.2002/aam longer FORMAT FOR krmin
                  17.01.2002 ht  Invoice no more mandatory (from nnpurv2)
                  05.02.2002/aam print product name UNFORMATTED 
                  06.03.2002/aam tax method from Invoice.VatIncl
                  29.04.2002/ht  INPUT PARAMETER lcAtil (A-subscriber)
                  10.12.2002 kl  new break by, fRecId & fBService -functions
                  19.03.2003/aam rewritten;
                                 MobCDR added,  
                                 VAT handling changed,
                                 collect calls to temp-table,
                                 use InvSeq-find if invoice selected,
                                 individual calls with original currency unit
                                 and summary lines with full currency unit
                  25.03.2003/jp  xbsub replaced func.i
                                 NEW field ttcall: bdest
                  30.06.2003/aam hide last characters of PNP numbers also      
                  15.07.2003/jp  starttime format (hh:mm:ss) only for paper 
                                 print.                              
                  23.08.2003/aam PDF-printing                               
                  17.02.2004/aam cover sheet
                  06.04.2004/aam no sub-totals or totals
                  14.04.2004/aam index change on mobcdr
                  23.04.2004/aam use fHideBSub from func.i
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/refcode.i}
{Func/cparam2.i}
/* print-linemuuttujat */
{Syst/utumaa.i}
{Inv/edefine.i}
{Inv/nnpura.i}
{Func/fdivtxt.i}

/* xml / pdf */
{Inv/xmlpura2.i}
{Func/fpdfrun.i}

def input  parameter CustNum1  as int format "zzzzzz9"    NO-UNDO.
def input  parameter CustNum2  as int format "zzzzzz9"    NO-UNDO.
def input  parameter pvm1      as date format "99-99-99"  NO-UNDO.
def input  parameter pvm2      as date format "99-99-99"  NO-UNDO.
def input  parameter tilak     as int format "9"          NO-UNDO.
DEF INPUT  PARAMETER invno     LIKE invseq.InvNum         NO-UNDO.
DEF INPUT  PARAMETER lcAtil    LIKE FixCDR.CLI            NO-UNDO.
DEF INPUT  PARAMETER iiTarg    AS INT                     NO-UNDO.
DEF INPUT  PARAMETER iiMail    AS INT                     NO-UNDO.
DEF OUTPUT PARAMETER oiPDFQty  AS INT                     NO-UNDO. 
DEF OUTPUT PARAMETER oiMailQty AS INT                     NO-UNDO. 
DEF OUTPUT PARAMETER oiErrQty  AS INT                     NO-UNDO. 


def var dtunimi    as char format "x(15)"  NO-UNDO.
DEF VAR lcCCN      AS CHAR                 NO-UNDO. 
DEF VAR rlx        AS INT                  NO-UNDO. 
DEF VAR epltul     AS LOG                  NO-UNDO. 
DEF VAR llPDFPrint AS LOG                  NO-UNDO. 

def var ckestos   as char format "x(12)"       NO-UNDO.

def var btilnro   as char format "x(24)" NO-UNDO.
DEF VAR prefix    AS CHAR                NO-UNDO.
def var dalepro   as dec format "zz9.99" NO-UNDO.
DEF VAR lcSubHead AS CHAR                NO-UNDO.
DEF VAR viiva5    AS CHAR                NO-UNDO FORMAT "X(110)". 

DEF VAR lcXLTFile    AS CHAR  NO-UNDO. 
DEF VAR lcXMLFile    AS CHAR  NO-UNDO. 
DEF VAR lcPDFFile    AS CHAR  NO-UNDO. 
DEF VAR llOk         AS LOG   NO-UNDO.
DEF VAR lcMailAddr   AS CHAR  NO-UNDO. 
DEF VAR liMSSeq      AS INT   NO-UNDO. 
DEF VAR liPrCust     AS INT   NO-UNDO.
DEF VAR lcDateHead   AS CHAR  NO-UNDO.


DEF BUFFER bRateCust FOR Customer.
DEF BUFFER bCust     FOR Customer. 

{Inv/nnpurac.i}
{Inv/spechead.i}
{Inv/ereppage.i}

DEF TEMP-TABLE ttCall NO-UNDO
   FIELD Date       AS DATE
   FIELD TimeStart  AS INT
   FIELD CallCust   AS INT
   FIELD CLI        AS CHAR
   FIELD BillCode   AS CHAR
   FIELD CCN        AS INT
   FIELD Price      AS DEC
   FIELD Duration   AS DEC
   FIELD BSub       AS CHAR
   FIELD BDest      AS CHAR
   FIELD BType      AS INT  
   FIELD VatIncl    AS LOG 
   INDEX CallCust VatIncl CallCust CLI BillCode CCN.

/* Is this  a PNP number */
FUNCTION fIsPNP RETURNS LOGICAL
  (INPUT  iCustNum AS INT,
   INPUT  iBSub    AS CHAR).
 
   RETURN FALSE.
END.   

   
FUNCTION fCollFixCDR RETURNS LOGICAL.

   CREATE ttCall.
   ASSIGN ttCall.CallCust   = FixCDR.CustNum
          ttCall.BillCode   = FixCDR.BillCode
          ttCall.CCN        = FixCDR.CCN
          ttCall.VatIncl    = FixCDR.VatIncl
          ttCall.Duration   = FixCDR.Duration
          ttCall.BSub       = FixCDR.BSub
          ttCall.Date       = FixCDR.Date
          ttCall.TimeStart  = FixCDR.TimeStart
          ttCall.Price      = FixCDR.GrossPrice - FixCDR.DiscValue
          ttCall.CLI        = FixCDR.CLI
          ttCall.BDest      = FixCDR.BDest.
          
END FUNCTION.

FUNCTION fCollMobCDR RETURNS LOGICAL.

   CREATE ttCall.
   ASSIGN ttCall.CallCust   = MobCDR.CustNum
          ttCall.BillCode   = MobCDR.BillCode
          ttCall.CCN        = MobCDR.CCN
          ttCall.VatIncl    = MobCDR.VatIncl
          ttCall.Duration   = MobCDR.BillDur
          ttCall.BSub       = MobCDR.GSMBNr
          ttCall.Date       = MobCDR.DateSt
          ttCall.TimeStart  = MobCDR.TimeStart
          ttCall.Price      = MobCDR.Amount
          ttCall.BDest      = MobCDR.BDest
          ttCall.BType      = MobCDR.BType
          ttCall.CLI        = MobCDR.CLI.

END FUNCTION.

IF epltul THEN ASSIGN lev = 86. 
else lev = 80.

ASSIGN
viiva1     = fill("=",lev)
viiva2     = fill("=",lev)
viiva3     = FILL("-",lev)
viiva4     = FILL(" ",4) + fill("-",lev - 4)
viiva5     = FILL(" ",2) + fill("-",lev - 2)
/* printing type */
epltul     = (iiTarg = 1)
llPDFPrint = (iiTarg = 3).

form header
   viiva1 AT 1 SKIP
   ynimi at 1 format "x(30)"
      lcRepHeader format "x(35)" AT 35
      otsi[28] format "x(4)" AT 72                  /* Sivu */
      sl format "ZZZ9" SKIP
   lcAtil at 1 format "x(15)"
      lcSubHead AT 35 FORMAT "X(35)"
      pvm at 71 format "99-99-9999" SKIP
   viiva2 AT 1 skip(1)
WITH width 130 NO-LABEL no-box FRAME sivuotsi.

form header
   otsi[1]       format "x(20)"    AT 1
      liCallCust FORMAT ">>>>>>>9" AT 22
      SAsNimi    FORMAT "x(30)"    AT 31
      SKIP
   otsi[3]       format "x(20)"    AT 1
      liInvCust  FORMAT ">>>>>>>9" AT 22
      LAsNimi    FORMAT "x(30)"    AT 31
      SKIP(1)
   otsi[30] format "x(5)"  AT 5                     /* date           */
   otsi[21] format "x(6)"  AT 18                    /* start time     */
   otsi[22] format "x(17)" AT 26                    /* bdest          */
   otsi[29] format "x(12)" TO 61                    /* Duration       */
   otsi[33] format "x(12)" TO 80                    /* to pay         */ SKIP
   "" /* viiva3 */ AT 1 SKIP
WITH width 130 NO-LABEL no-box FRAME sarotsi.

FUNCTION fChgPage RETURNS LOGICAL
   (iiAddLine AS INT).

   IF llPDFPrint OR epltul THEN RETURN FALSE.
 
   IF rl + iiAddLine >= skayt1 THEN DO:
      
      IF sl > 0 THEN DO:
         {Syst/uprfeed.i rl}
      END. 
      ELSE DO:
      END.
       
      sl = sl + 1.
      view STREAM tul FRAME sivuotsi.  
      rl = 12.
      
      view STREAM tul FRAME sarotsi.   
      ASSIGN rl  = rl + 5
             rlx = 0.
      
      RETURN TRUE.
   END.

   ELSE RETURN FALSE. 

END FUNCTION.


ASSIGN
sl = 0
rl = 0
otsi[24] = "Printing interrupted !".

tila1 = FALSE.
tila2 = TRUE.

IF tilak = 0 THEN tila2 = FALSE.
IF tilak = 1 THEN tila1 = TRUE.

/* init PDF printing */
IF llPDFPrint THEN DO:
   fPDFInit("rep2",
            TRUE).
            
   lcXLTFile = lcXLTDIR + "pura2.xsl".            
   
   /* mail to user */
   IF iiMail = 2 THEN DO:
      FIND TMSUser WHERE TMSUser.UserCode = katun NO-LOCK NO-ERROR.
      IF AVAILABLE TMSUser THEN lcMailAddr = TMSUser.EMail.
   END. 
      
END. 
 
/* from ui with chosen invoice or from invoice printing */
IF invno NE 0 THEN DO:

   FIND Invoice WHERE Invoice.InvNum = invno NO-LOCK NO-ERROR.
   IF NOT AVAIL Invoice THEN RETURN.

   FOR FIRST Customer OF Invoice NO-LOCK:
   
       IF iiMail = 1 THEN lcMailAddr = Customer.EMail.
       
   END.
   
   FOR EACH SubInvoice OF Invoice NO-LOCK:
   
      FOR EACH FixCDR NO-LOCK WHERE
               FixCDR.InvSeq = SubInvoice.InvSeq AND
              (IF lcAtil NE "" 
               THEN FixCDR.CLI = lcAtil 
               ELSE TRUE):
             
         fCollFixCDR().
      END.
   
      FOR EACH MobCDR NO-LOCK WHERE
               MobCDR.InvCust   = Invoice.CustNum AND 
               MobCDR.InvSeq    = SubInvoice.InvSeq AND
               MobCDR.ErrorCode = 0              AND
             (IF lcAtil NE "" 
               THEN MobCDR.CLI = SUBSTRING(lcAtil,4)
               ELSE TRUE):
       
         fCollMobCDR().
      END.   
   END. 
            
END.

/* from ui, invoice has not been chosen */
ELSE DO:

   FOR EACH Customer NO-LOCK WHERE
            Customer.Brand    = gcBrand  AND
            Customer.CustNum >= CustNum1 AND
            Customer.CustNum <= CustNum2:

      FIND bRateCust NO-LOCK WHERE
           bRateCust.CustNum = Customer.RateCust NO-ERROR.
      IF NOT AVAILABLE bRateCust THEN 
      FIND bRateCust NO-LOCK WHERE
           bRateCust.CustNum = Customer.CustNum NO-ERROR.
      
      /* collect calls into a temp-table */
      FOR EACH FixCDR NO-LOCK WHERE 
               FixCDR.RepCust  = Customer.RepCust  AND 
               FixCDR.CustNum  = Customer.CustNum  AND
               FixCDR.Date  >= pvm1              AND
               FixCDR.Date  <= pvm2              AND
              (IF lcAtil NE "" 
               THEN FixCDR.CLI = lcAtil 
               ELSE TRUE),

         FIRST InvSeq NO-LOCK where
               InvSeq.InvSeq  = FixCDR.InvSeq AND 
               InvSeq.billed >= tila1         AND
               InvSeq.billed <= tila2:

         fCollFixCDR().
    
      END.      

      FOR EACH MobCDR NO-LOCK USE-INDEX CustNum  where
               MobCDR.CustNum  = Customer.CustNum   AND
               MobCDR.DateSt  >= pvm1               AND
               MobCDR.DateSt  <= pvm2               AND
              (if lcAtil ne "" 
               THEN MobCDR.CLI = lcAtil
               ELSE TRUE),
     
         FIRST InvSeq NO-LOCK where
               InvSeq.InvSeq  = MobCDR.InvSeq AND 
               InvSeq.billed >= tila1         AND
               InvSeq.billed <= tila2:

         fCollMobCDR().
   
      END.      
   
   END.
   
END.

/* nothing to be printed */
IF NOT CAN-FIND(FIRST ttCall) THEN oiErrQty = -1.

/* print collected data */
FOR EACH ttCall
BREAK BY ttCall.VatIncl
      BY ttCall.CallCust
      BY ttCall.CLI
      BY ttCall.BillCode
      BY ttCall.CCN 
      BY ttCall.Date
      BY ttCall.TimeStart:


      /* Customer vaihtui */
      IF first-of(ttCall.CallCust) THEN DO:

         FIND FIRST Customer where
                    Customer.CustNum = ttCall.CallCust
         NO-LOCK NO-ERROR.
         
         fCustHeader().

         ASSIGN lcRepHeader  = otsi[19]
                lcEPLRepHead = otsi[19]
                otsi[29]     = FILL(" ",4) + otsi[29]
                otsi[33]     = FILL(" ",12 - LENGTH(otsi[33])) + otsi[33]
                sl           = 0.

         IF invno > 0 THEN DO:
             lcSubHead = otsi[65] + " " + STRING(invno).
             fSpecGenHeader(Invoice.CustNum,
                            Invoice.InvNum,
                            Customer.Language).
         END.                   
         ELSE DO:
            lcSubHead = STRING(pvm1,"99.99.9999") + " - " + 
                        STRING(pvm2,"99.99.9999"). 
            fSpecGenHeader(Customer.CustNum,
                           0,
                           Customer.Language).
         END.

         /* cover sheet */
         IF NOT llPDFPrint AND llCaSivu < 0 THEN DO:        

            ASSIGN liMSSeq  = 0
                   liPrCust = CustNum1.
                   
            IF lcAtil > "" THEN 
            FOR FIRST MsOwner NO-LOCK WHERE
                      MsOwner.CustNum = ttCall.CallCust AND
                      MsOwner.CLI     = ttCall.CLI:
               ASSIGN liMSSeq  = MsOwner.MSSeq
                      liPrCust = MsOwner.CustNum.
            END.
            
            RUN printxt (liPrCust,
                         liMsSeq, 
                         "",
                         1,                      /* 1=invtext */
                         7,                      /* 5=address already set */
                         "General",
                         "SpecCover",
                         0,
                         IF epltul THEN 3 ELSE 2, /* print target */
                         0,
                         OUTPUT lcErrFile).
            llCaSivu = 1.             
         END.
         
         
         IF epltul THEN DO:

            ASSIGN licalask = lireppage.
            fNewPage(3).
            {Inv/nnpura2e.i}
            
            PUT STREAM eKirje UNFORMATTED
               " I" 
               MY-NL
               " I"
                  otsi[1] format "x(20)" 
                  " "
                  liCallCust FORMAT ">>>>>>>9"
                  " "
                  SAsNimi
               MY-NL
               " I"
                  otsi[3] format "x(20)"
                  " "
                  liInvCust  FORMAT ">>>>>>>9"
                  " " 
                  LAsNimi
               MY-NL.
               licalask = licalask + 3.
         END.
         
         ELSE IF llPDFPrint THEN DO:

            /* mail to invoicing customer */
            IF invno = 0 AND iiMail = 1 THEN DO:
               FIND bCust WHERE 
                    bCust.CustNum = Customer.InvCust NO-LOCK NO-ERROR.
               IF AVAILABLE bCust THEN lcMailAddr = bCust.EMail.
            END.
            
            fXMLInit(lcRepHeader,
                     lcAtil,
                     lcSubHead,
                     otsi[1] + " " + STRING(liCallCust) + " " + sasnimi,
                     otsi[3] + " " + STRING(liInvCust) + " " + lasnimi,
                     otsi[30],
                     otsi[21],
                     otsi[22],
                     otsi[29],
                     otsi[33]).
         END.
         
         ELSE DO:
            fChgPage(999).
         END.
      END.

      /* Soittaja vaihtui */
      IF first-of(ttCall.CLI) THEN DO:

         IF epltul THEN DO:
            fNewPage(5).
            {Inv/nnpura2e.i}
         END.
             /* Tarvitaanko uusi sivu */
         ELSE DO:
            fChgPage(5).
         END.

         atil = fSpecCLIHeader(ttCall.CLI,
                               ttCall.Date,
                               kieli).

         IF epltul THEN DO:
            PUT STREAM eKirje UNFORMATTED 
                " I" 
                MY-NL 
                " I" 
                atil 
                MY-NL.
            ASSIGN licalask = licalask + 2. 
         END.

         ELSE IF llPDFPrint THEN DO.
            fXMLCLIHeader(atil).
         END.
         
         ELSE DO:
            PUT STREAM tul skip(1)
               atil at 1 format "x(60)" skip.
            
            rl = rl + 2.
         END.
      END.

      /* Tulostetaan tuotteen nimi */
      IF first-of(ttCall.BillCode) THEN DO:

         dtunimi = fSpecBItemHeader(ttCall.BillCode,
                                    kieli,
                                    ttCall.Date).

         /* VAT header */
         lcVat = fVatTitle(kieli,
                           invno,
                           ttCall.BillCode,
                           llVatUsed,
                           ttCall.VatIncl).

         dtunimi = dtunimi + fill(" ",6) + lcVat.
         
         IF epltul THEN DO:
            fNewPage(6).
            {Inv/nnpura2e.i}
         END.
         ELSE DO:
            fChgPage(6).
         END.

         IF epltul THEN DO:
            PUT STREAM eKirje UNFORMATTED
               " I"
               MY-NL
               " I" 
               SPACE(2)
               dtunimi
               MY-NL.
            ASSIGN licalask = licalask + 2. 
         END. 
         
         ELSE IF llPDFPrint THEN DO.
            fXMLProdHeader(dtunimi).
         END.
         
         ELSE DO:
            PUT STREAM tul UNFORMATTED 
               SKIP(1)
               dtunimi AT 3
               skip.
            rl = rl + 2.
            rlx = 0.
         END.
      END.

      /* CCN */
      IF first-of(ttCall.CCN) THEN DO:

         lcCCN = fSpecCCNHeader(ttCall.CCN,
                                kieli,
                                ttCall.Date).

         IF epltul THEN DO:
            fNewPage(4).
            {Inv/nnpura2e.i}
         END.
         ELSE DO:
            fChgPage(4).
         END.        

         IF epltul THEN DO:
            PUT STREAM eKirje UNFORMATTED
               " I"
               MY-NL
               " I" 
               SPACE(4)
               lcCCN
               MY-NL.
            ASSIGN licalask = licalask + 2. 
         END. 
         
         ELSE IF llPDFPrint THEN DO.
            fXMLCCNHeader(lcCCN).
         END.
          
         ELSE DO:
            PUT STREAM tul UNFORMATTED 
               SKIP(1)
               lcCCN AT 5
               skip.
            rl = rl + 2.
            rlx = 0.
         END.
      END.

      ACCUMULATE ttCall.Duration (TOTAL BY ttCall.CallCust
                                        BY ttCall.CLI
                                        BY ttCall.BillCode
                                        BY ttCall.CCN)
                 ttCall.Price    (TOTAL BY ttCall.CallCust
                                        BY ttCall.CLI
                                        BY ttCall.BillCode
                                        BY ttCall.CCN)
                 ttCall.Date     (COUNT BY ttCall.CallCust
                                        BY ttCall.CLI
                                        BY ttCall.BillCode
                                        BY ttCall.CCN).
                                        
      btilnro = ttCall.BSub.
      if btilnro begins "00000" THEN btilnro = substr(btilnro,6).

      ckestos = fSec2C(ttCall.Duration,12).

      /* Modify BSUB FOR reporting: fXBSub uses {&country} */
      btilnro = DYNAMIC-FUNCTION("fHideBSub" IN ghFunc1,
                                 ttcall.bsub,
                                 ttcall.callcust,
                                 ttcall.bdest,
                                 ttCall.BType,
                                 "",
                                 TRUE).
                                 
      /* Tarvitaanko uusi sivu */
      IF epltul THEN DO:
            fNewPage(0).
            {Inv/nnpura2e.i}
      END.
      ELSE DO:
         fChgPage(0).
      END.

      /* Tulostetaan soittoline */
      IF epltul THEN DO:
         PUT STREAM eKirje UNFORMATTED 
            " I" 
            space(4)
            ttCall.Date              format "99.99.99"
            space(1)
            string(ttCall.TimeStart,"hh:mm:ss") format "x(8)" 
            space(1)    
            btilnro                  format "x(18)"
            space(1)
            ckestos                  format "x(12)"
            space(5)
            ttCall.Price             format "->>>>>>>9.99"
            MY-NL.
            ASSIGN licalask = licalask + 1.
      END. 

      ELSE IF llPDFPrint THEN DO.
         fXMLCallLine(STRING(ttCall.Date,"99.99.99"),
                      STRING(ttCall.TimeStart,"hh:mm:ss"),
                      btilnro,
                      ckestos,
                      STRING(ttCall.Price,"->>>>>>>9.99")).
      END.
 
      ELSE DO:

         DISPLAY STREAM tul
            ttCall.Date          AT 5  format "99.99.99"
            string(ttCall.TimeStart,"hh:mm:ss") format "x(8)" 
                                 AT 15
            btilnro              AT 26
            ckestos              TO 61
            ttCall.Price         FORMAT "->>>>>>>9.99" TO 80 
            SKIP
         WITH NO-LABELS no-box width 90 FRAME puline.
         DOWN STREAM tul WITH FRAME puline.
      
         /* line- ja katkolaskurit */
         ASSIGN
         rl = rl + 1
         rlx = rlx + 1.
       
      END.

      /* Tulostetaan Customer-yhteensa-line */
      IF last-of(ttCall.CallCust) THEN DO:
         
         ckestos = fSec2C((ACCUM TOTAL BY ttCall.CallCust ttCall.Duration),12).

         IF epltul THEN DO:
         END.

         ELSE IF llPDFPrint THEN DO.

            FIND Customer WHERE Customer.CustNum = ttCall.CallCust NO-LOCK.
            
            fXMLFinish(ttCall.CallCust,
                       OUTPUT lcXMLFile).
        
            /* form PDF */
            RUN pRunFOPD(lcXLTFile,
                         lcXMLFile,
                         OUTPUT llOk).
        
            /* send PDF via eMail */
            IF llOk AND lcFopMethod = "WAIT" THEN DO:

               oiPDFQty = oiPDFQty + 1.
               
               IF iiMail > 0 THEN DO:

                  /* give FOPD some time to finish it's work */
                  PAUSE 5 NO-MESSAGE. 
               
                  lcPDFFile = REPLACE(lcXMLFile,".xml",".pdf").
               
                  message "Send mail to:" lcmailaddr
                  view-as alert-box
                  question
                  buttons yes-no
                  update llok.
                  
                  if llok then do:
                    
                  IF fSendPDFMail(lcPDFFile,
                                  lcMailAddr,
                                  Customer.Language,
                                  fTeksti(134,Customer.Language),
                                  fTeksti(133,Customer.Language),
                                  "rep2")
                  THEN DO:
                     oiMailQty        = oiMailQty + 1.
                  END.
         
                  ELSE DO:
                     fErrorLog("Send via eMail failed").
                  END. 
                  
                  end.
                  
               END.
               
            END.
            
         END.

      END.

END. 

IF llPDFPrint THEN DO:

   oiErrQty = liErrQty.
   
   /* finish PDF printing (send errorlog via email etc.) */
   fPDFFinish("rep2",
              "call report"). 
END.               



