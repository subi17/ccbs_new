/* ------------------------------------------------------
  MODULE .......: NNPURA4.P
  FUNCTION .....: Erittely tuotteittain/puheluittain
  APPLICATION ..: NN
  AUTHOR .......: TT
  CREATED ......: 02.02.96
  MODIFIED .....: 
                  05.02.2002/aam print product name UNFORMATTED 
                  06.03.2002/aam tax method from Invoice.VATIncl
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
                  15.07.2003/jp  starttime FORMAT (hh:mm:ss) only for paper 
                                 print.                              
                  23.08.2003/aam PDF-printing                               
                  08.12.2003/aam from nnpura2; 
                                 show amounts (incl/excl vat) according to 
                                 customer's/invoice's VATIncl,
                                 unit price, price apiece & dataamt added
                  16.02.2004/aam cover sheet               
                  14.04.2004/aam index change on mobcdr
                  23.04.2004/aam use fHideBSub from func.i
                  17.05.2004/aam unit price for vas tickets from netprice
                  15.06.2004/aam functions to nnpura4.i
                  22.06.2004/aam pulses 
                  09.08.2004/aam no effects if printed to file
                  26.10.2004/aam new option: show full b-numbers
                  09.11.2004/aam skip products listed in parameter SpecSkipProd
                  12.04.2005/aam new columns for mpm and service,
                                 only one unit price (price apiece removed),
                                 use 3 decimals for all amounts,
                                 use smaller font (J=15chr/inch)
                  07.08.2006/aam iiLetterclass to printxt.p
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/refcode.i}
{Func/cparam2.i}
/* print-linemuuttujat */
{Syst/utumaa.i}
{Inv/edefine.i}
{Inv/nnpura.i}
{Func/fcurrency.i}

/* xml / pdf */
{Inv/xmlpura2.i}
{Func/fpdfrun.i}
{Func/fdivtxt.i}

def input  parameter CustNum1      as int FORMAT "zzzzzz9"   NO-UNDO.
def input  parameter CustNum2      as int FORMAT "zzzzzz9"   NO-UNDO.
def input  parameter pvm1          as date FORMAT "99-99-99" NO-UNDO.
def input  parameter pvm2          as date FORMAT "99-99-99" NO-UNDO.
def input  parameter tilak         as int  FORMAT "9"        NO-UNDO.
DEF INPUT  PARAMETER invno         LIKE invseq.InvNum        NO-UNDO.
DEF INPUT  PARAMETER ilFullB       AS LOG                    NO-UNDO. 
DEF INPUT  PARAMETER lcAtil        LIKE FixCDR.CLI           NO-UNDO.
DEF INPUT  PARAMETER iiTarg        AS INT                    NO-UNDO.
DEF INPUT  PARAMETER iiMail        AS INT                    NO-UNDO.
DEF INPUT  PARAMETER icFrom        AS CHAR                   NO-UNDO. 
DEF INPUT  PARAMETER iiLetterClass AS INT                    NO-UNDO.
DEF OUTPUT PARAMETER oiPDFQty      AS INT                    NO-UNDO. 
DEF OUTPUT PARAMETER oiMailQty     AS INT                    NO-UNDO. 
DEF OUTPUT PARAMETER oiErrQty      AS INT                    NO-UNDO. 

DEF VAR rlx        AS INT                  NO-UNDO. 
DEF VAR epltul     AS LOG                  NO-UNDO. 
DEF VAR llPDFPrint AS LOG                  NO-UNDO. 

DEF VAR prefix    AS CHAR                  NO-UNDO.
DEF VAR viiva5    AS CHAR                  NO-UNDO FORMAT "X(110)". 

DEF VAR lcXLTFile     AS CHAR  NO-UNDO. 
DEF VAR lcXMLFile     AS CHAR  NO-UNDO. 
DEF VAR lcPDFFile     AS CHAR  NO-UNDO. 
DEF VAR lcMailAddr    AS CHAR  NO-UNDO. 
DEF VAR lcMPM         AS CHAR  NO-UNDO. 
DEF VAR llDispMPM     AS LOG   NO-UNDO. 
                                       
DEF BUFFER bRateCust FOR Customer.
DEF BUFFER bCust     FOR Customer. 

{Inv/nnpurac.i}
{Inv/ereppage.i}
{Inv/nnpura4.i}
{Inv/spechead.i}

/* printing type */
ASSIGN epltul     = (iiTarg = 1)
       llPDFPrint = (iiTarg = 3)
       viiva1     = fill("=",lev)
       viiva2     = fill("=",lev).
       
IF epltul 
THEN ASSIGN lev        = 101
            viiva3     = FILL(" ",9)  + fill("-",lev)
            viiva4     = FILL(" ",10) + fill("-",lev - 1)
            viiva5     = FILL(" ",9)  + fill("-",lev).
ELSE ASSIGN lev        = 106
            viiva3     = FILL(" ",4)  + fill("-",lev - 4)
            viiva4     = FILL(" ",5)  + fill("-",lev - 5)
            viiva5     = FILL(" ",4)  + fill("-",lev - 4).

form header
   viiva1 AT 1 SKIP
   ynimi at 1 FORMAT "x(30)"
      lcRepHeader FORMAT "x(35)" AT 45
      otsi[28] FORMAT "x(4)" AT 97     
      sl FORMAT "ZZZ9" SKIP
   lcAtil at 1 FORMAT "x(15)"
      lcRep4SubHead AT 45 FORMAT "X(35)"
      pvm at 97 FORMAT "99-99-9999" SKIP
   viiva2 AT 1 skip(1)
WITH width 130 NO-LABEL no-box FRAME sivuotsi.

form header
   otsi[1]       FORMAT "x(20)"    AT 1
      liCallCust FORMAT ">>>>>>>9" AT 22
      SAsNimi    FORMAT "x(30)"    AT 31
      SKIP
   otsi[3]       FORMAT "x(20)"    AT 1
      liInvCust  FORMAT ">>>>>>>9" AT 22
      LAsNimi    FORMAT "x(30)"    AT 31
      SKIP

   lcRep4UHead[1]  FORMAT "x(8)"  AT 5 
   lcRep4UHead[2]  FORMAT "x(8)"  AT 14             
   lcRep4UHead[3]  FORMAT "x(13)" AT 23                    
   lcRep4UHead[4]  FORMAT "x(8)"  TO 46                    
   lcRep4UHead[9]  FORMAT "x(5)"  TO 52
   lcRep4UHead[5]  FORMAT "x(7)"  TO 60   
   lcRep4UHead[7]  FORMAT "x(8)"  TO 70
   lcRep4UHead[10] FORMAT "x(11)" TO 82 
   lcRep4UHead[11] FORMAT "x(11)" TO 94 
   lcRep4UHead[8]  FORMAT "x(11)" TO 106 
   SKIP
   
   lcRep4Head[1]   FORMAT "x(8)"  AT 5                /* date           */
   lcRep4Head[2]   FORMAT "x(8)"  AT 14               /* start time     */
   lcRep4Head[3]   FORMAT "x(13)" AT 23               /* bdest          */
   lcRep4Head[4]   FORMAT "x(8)"  TO 46               /* Duration       */
   lcRep4Head[9]   FORMAT "x(5)"  TO 52               /* pulses         */
   lcRep4Head[5]   FORMAT "x(7)"  TO 60               /* data amt       */
   lcRep4Head[7]   FORMAT "x(8)"  TO 70               /* unit price     */
   lcRep4Head[10]  FORMAT "x(11)" TO 82               /* mpm            */
   lcRep4Head[11]  FORMAT "x(11)" TO 94               /* service        */
   lcRep4Head[8]   FORMAT "x(11)" TO 106              /* to pay         */ 
   SKIP
   "" /* viiva3*/  AT 1 
   SKIP
WITH width 130 NO-LABEL no-box FRAME sarotsi.

/* report's printer effect */
FUNCTION fRepEffect RETURNS LOGICAL
   (icPrinter AS CHAR,
    icReport  AS CHAR).

   DEF VAR llEffect AS LOGIC NO-UNDO.

   ASSIGN llEffect = FALSE
          lcEffOn  = "". 

   IF icPrinter NE "" THEN 
   FIND FIRST TMSPrinter WHERE
              TMSPrinter.PrinterId = icPrinter
   NO-LOCK NO-ERROR.

   IF icPrinter = "" OR
      (AVAILABLE TMSPrinter        AND 
       LOOKUP(TMSPrinter.Device,"-,EMail") = 0)
   THEN DO:

      /* Haetaan raporttitehoste */
      FOR FIRST TMSRepCfg NO-LOCK WHERE
                TMSRepCfg.RepName   = icReport AND 
                TMSRepCfg.UserCode  = "",
          FIRST PrintCodes NO-LOCK WHERE 
                PrintCodes.PrinterId = TMSRepCfg.PrinterId AND
                PrintCodes.Effect    = TMSRepCfg.Effect:

         ASSIGN spit1    = PrintCodes.PageLength
                skayt1   = PrintCodes.AvailLines
                lcEffOn  = PrintCodes.EffOn[2]
                llEffect = TRUE.

      END.

   END. 

   RETURN llEffect. 

END FUNCTION.


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
      rl = 5.
      
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
   fPDFInit("rep4",
            TRUE).
            
   lcXLTFile = lcXLTDIR + "pura4.xsl".            
   
   /* mail to user */
   IF iiMail = 2 THEN DO:
      FIND TMSUser WHERE TMSUser.UserCode = katun NO-LOCK NO-ERROR.
      IF AVAILABLE TMSUser THEN lcMailAddr = TMSUser.EMail.
   END. 
      
END. 
ELSE IF NOT epltul AND icFrom NE "menu" AND oso = "" THEN DO:
   /* default effect to printer */
   IF fRepEffect("","nnpura4")
   THEN PUT STREAM tul control lcEffOn.
END.

/* from ui with chosen invoice or from invoice printing */
IF invno NE 0 THEN DO:

   FIND Invoice WHERE Invoice.InvNum = invno NO-LOCK NO-ERROR.
   IF NOT AVAIL Invoice THEN RETURN.

   IF iiMail = 1 THEN FOR FIRST Customer OF Invoice NO-LOCK:
      lcMailAddr = Customer.EMail.
   END.
 
   fCollInvCDR(lcAtil). 
            
END.

/* from ui, invoice has not been chosen */
ELSE DO:

   FOR EACH Customer NO-LOCK WHERE
            Customer.Brand    = gcBrand  AND
            Customer.CustNum >= CustNum1 AND
            Customer.CustNum <= CustNum2:

      FIND bCust WHERE bCust.CustNum = Customer.InvCust NO-LOCK NO-ERROR.
      /* disp method of this report */
      IF AVAILABLE bCust 
      THEN ASSIGN llDispVAT  = bCust.VATIncl
                  llVATUsage = bCust.VATUsage.
      ELSE ASSIGN llDispVAT  = Customer.VATIncl
                  llVATUsage = Customer.VATUsage.
 
      FIND bRateCust NO-LOCK WHERE
           bRateCust.CustNum = Customer.RateCust NO-ERROR.
      IF NOT AVAILABLE bRateCust THEN 
      FIND bRateCust NO-LOCK WHERE
           bRateCust.CustNum = Customer.CustNum NO-ERROR.
   
      /* collect calls into a temp-table */
      FOR EACH FixCDR NO-LOCK WHERE 
               FixCDR.RepCust  = Customer.RepCust  AND 
               FixCDR.CustNum  = Customer.CustNum    AND
               FixCDR.Date    >= pvm1              AND
               FixCDR.Date    <= pvm2              AND
              (IF lcAtil NE "" 
               THEN FixCDR.CLI = lcAtil 
               ELSE TRUE),

         FIRST InvSeq NO-LOCK where
               InvSeq.InvSeq  = FixCDR.InvSeq AND 
               InvSeq.billed >= tila1        AND
               InvSeq.billed <= tila2:

         fCollFixCDR(invno).
    
      END.      

      FOR EACH MobCDR NO-LOCK USE-INDEX CustNum where
               MobCDR.CustNum  = Customer.CustNum AND
               MobCDR.DateSt  >= pvm1             AND
               MobCDR.DateSt  <= pvm2             AND
              (if lcAtil ne "" 
               THEN MobCDR.CLI = lcAtil
               ELSE TRUE),
     
         FIRST InvSeq NO-LOCK where
               InvSeq.InvSeq  = MobCDR.InvSeq AND 
               InvSeq.billed >= tila1         AND
               InvSeq.billed <= tila2:

         fCollMobCDR(invno).
   
      END.      
   
   END.
   
END.

/* nothing to be printed */
IF NOT CAN-FIND(FIRST ttCall) THEN oiErrQty = -1.

/* print collected data */
FOR EACH ttCall
BREAK BY ttCall.VATIncl
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
         
         fRep4CustHeader(invno).
         
         IF invno = 0 THEN DO:
            lcRep4SubHead = STRING(pvm1,"99.99.9999") + " - " +
                            STRING(pvm2,"99.99.9999").
            fSpecGenHeader(Customer.CustNum,
                           0,
                           liRep4Lang).
         END.
         ELSE DO:
             fSpecGenHeader(Invoice.CustNum,
                            Invoice.InvNum,
                            liRep4Lang).
         END.
                            
         sl = 0.

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
            
            RUN Mc/printxt.p (liPrCust,
                         liMsSeq, 
                         "",
                         1,                      /* 1=invtext */
                         7,                      /* 7=address already set */
                         "General",
                         "SpecCover",
                         0,
                         IF epltul THEN 3 ELSE 2, /* print target */
                         iiLetterClass,
                         OUTPUT lcErrFile).
            llCaSivu = 1.             
         END.
         
         
         IF epltul THEN DO:
            ASSIGN licalask = lireppage.
            fNewPage(3).
            {Inv/nnpura4e.i}
            
            PUT STREAM eKirje UNFORMATTED
               " J" 
               MY-NL
               " J"
                  SPACE(9) 
                  otsi[1] + " " + 
                  STRING(liCallCust) + " " + 
                  SAsNimi
               MY-NL.
               
            licalask = licalask + 2.
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
                     lcRep4SubHead,
                     otsi[1] + " " + STRING(liCallCust) + " " + sasnimi,
                     otsi[3] + " " + STRING(liInvCust) + " " + lasnimi,
                     lcRep4Head[1],
                     lcRep4Head[2],
                     lcRep4Head[3],
                     lcRep4Head[4],
                     lcRep4Head[5]).
         END.
         
         ELSE DO:

           fChgPage(999).
            
         END.
      END.

      /* cli changes */
      IF first-of(ttCall.CLI) THEN DO:

         IF epltul THEN DO:
            fNewPage(5).
            {Inv/nnpura4e.i}
         END.
         ELSE DO:
            fChgPage(5).
         END.

         lcRep4CLIHeader = fSpecCLIHeader(ttCall.CLI,
                                          ttCall.Date,
                                          liRep4Lang).

         IF epltul THEN DO:
            PUT STREAM eKirje UNFORMATTED 
                " J" 
                MY-NL 
                " J" 
                SPACE(9) 
                lcRep4CLIHeader 
                MY-NL.
            ASSIGN licalask = licalask + 2. 
         END.

         ELSE IF llPDFPrint THEN DO.
            fXMLCLIHeader(atil).
         END.
         
         ELSE DO:
            PUT STREAM tul skip(1)
               lcRep4CLIHeader at 1 FORMAT "x(60)" skip.
            
            rl = rl + 2.
         END.
      END.

      IF first-of(ttCall.BillCode) THEN DO:

         lcRep4BItemHeader = fSpecBItemHeader(ttCall.BillCode,
                                              liRep4Lang,
                                              ttCall.Date).

         llDispMPM = fDispMPM(ttCall.BillCode).
                  
         IF epltul THEN DO:
            fNewPage(6).
            {Inv/nnpura4e.i}
         END.
         ELSE DO:
            fChgPage(6).
         END.

         IF epltul THEN DO:
            PUT STREAM eKirje UNFORMATTED
               " J"
               MY-NL
               " J" 
               SPACE(9)
               lcRep4BItemHeader
               MY-NL.
            ASSIGN licalask = licalask + 2. 
         END. 
         
         ELSE IF llPDFPrint THEN DO.
            fXMLProdHeader(lcRep4BItemHeader).
         END.
         
         ELSE DO:
            PUT STREAM tul UNFORMATTED 
               SKIP(1)
               lcRep4BItemHeader AT 3
               skip.
            rl = rl + 2.
            rlx = 0.
         END.
      END.

      /* CCN */
      IF first-of(ttCall.CCN) THEN DO:

         lcRep4CCNHeader = fSpecCCNHeader(ttCall.CCN,
                                          liRep4Lang,
                                          ttCall.Date).
         
         IF epltul THEN DO:
            fNewPage(4).
            {Inv/nnpura4e.i}
         END.
         ELSE DO:
            fChgPage(4).
         END.        

         IF epltul THEN DO:
            PUT STREAM eKirje UNFORMATTED
               " J"
               MY-NL
               " J" 
               SPACE(10)
               lcRep4CCNHeader
               MY-NL.
            ASSIGN licalask = licalask + 2. 
         END. 
         
         ELSE IF llPDFPrint THEN DO.
            fXMLCCNHeader(lcRep4CCNHeader).
         END.
          
         ELSE DO:
            PUT STREAM tul UNFORMATTED 
               SKIP(1)
               lcRep4CCNHeader AT 5
               skip.
            rl = rl + 2.
            rlx = 0.
         END.
      END.

      ACCUMULATE ttCall.BillDur  (TOTAL BY ttCall.CallCust
                                        BY ttCall.CLI
                                        BY ttCall.BillCode
                                        BY ttCall.CCN)
                 ttCall.Amt      (TOTAL BY ttCall.CallCust
                                        BY ttCall.CLI
                                        BY ttCall.BillCode
                                        BY ttCall.CCN)
                 ttCall.Mpm      (TOTAL BY ttCall.CallCust
                                        BY ttCall.CLI
                                        BY ttCall.BillCode
                                        BY ttCall.CCN)
                 ttCall.DataAmt  (TOTAL BY ttCall.CallCust
                                        BY ttCall.CLI
                                        BY ttCall.BillCode
                                        BY ttCall.CCN)
                 ttCall.Pulses   (TOTAL BY ttCall.CallCust
                                        BY ttCall.CLI
                                        BY ttCall.BillCode
                                        BY ttCall.CCN)
                 ttCall.Date     (COUNT BY ttCall.CallCust
                                        BY ttCall.CLI
                                        BY ttCall.BillCode
                                        BY ttCall.CCN).
      
      /* hide last digits of b-number etc. */           
      fRep4SetLineValues(ilFullB).                                  

      IF llDispMPM
      THEN lcMPM = STRING(ttCall.MPM,"->>>>>9.999").
      ELSE lcMPM = "".

      IF epltul THEN DO:

         fNewPage(0).
         {Inv/nnpura4e.i}
         
         PUT STREAM eKirje UNFORMATTED 
            " J" 
            SPACE(10)
            ttCall.Date              FORMAT "99.99.99"
            SPACE(1)
            string(ttCall.TimeStart,"hh:mm:ss") FORMAT "x(8)" 
            SPACE(1)    
            lcRep4BSub               FORMAT "x(13)"
            SPACE(0)
            lcRep4Dur                FORMAT "x(10)"
            SPACE(1)
            ttCall.Pulses            FORMAT ">>>>>"
            SPACE(1)
            liRep4DataAmt            FORMAT "->>>>>>"
            SPACE(1).

          IF ttCall.UnitPrice > 0 THEN 
          PUT STREAM eKirje UNFORMATTED 
            ttCall.UnitPrice         FORMAT ">>>9.999"
            SPACE(1).
          ELSE PUT STREAM eKirje UNFORMATTED 
            ttCall.Apiece            FORMAT ">>>9.999"
            SPACE(1).
  
         /* more than 1 unit price */
         IF ttCall.CPrice > "" 
         THEN DO liTaff = 1 TO NUM-ENTRIES(ttCall.CPrice,"/"):
         
            licalask = licalask + 1.
            
            fNewPage(0).
            {Inv/nnpura4e.i}
            
            lcRep4Dur = fSec2C(DECIMAL(ENTRY(liTaff,ttCall.CDur,"/")),10).
            
            PUT STREAM eKirje UNFORMATTED 
            MY-NL
            " J"
            SPACE(41)
            lcRep4Dur FORMAT "x(10)"
            SPACE(15).
            
            IF DECIMAL((ENTRY(liTaff,ttCall.CPrice,"/"))) > 0
            THEN PUT STREAM eKirje UNFORMATTED
                 DECIMAL((ENTRY(liTaff,ttCall.CPrice,"/")))  FORMAT ">>>9.999"
                 SPACE(1).
            ELSE PUT STREAM eKirje UNFORMATTED
                 DECIMAL((ENTRY(liTaff,ttCall.CApiece,"/"))) FORMAT ">>>>9.999"
                 SPACE(1).                     
         END.

         PUT STREAM eKirje UNFORMATTED 
            lcMPM                    FORMAT "X(11)"
            SPACE(1)
            ttCall.Amt - ttCall.Mpm  FORMAT "->>>>>9.999"
            SPACE(1) 
            ttCall.Amt               FORMAT "->>>>>9.999"
            MY-NL.

         licalask = licalask + 1.
      END. 

      ELSE IF llPDFPrint THEN DO.
         fXMLCallLine(STRING(ttCall.Date,"99.99.99"),
                      STRING(ttCall.TimeStart,"hh:mm:ss"),
                      lcRep4BSub,
                      lcRep4Dur,
                      STRING(ttCall.Amt,"->>>>>9.99")).
      END.
 
      ELSE DO:

         fChgPage(0).
         
         PUT STREAM tul
            ttCall.Date          AT 5  FORMAT "99.99.99"
            string(ttCall.TimeStart,"hh:mm:ss")  
                                 AT 14 FORMAT "x(8)"
            lcRep4BSub           AT 23 FORMAT "x(13)"
            lcRep4Dur            TO 46 FORMAT "x(10)"
            ttCall.Pulses        TO 52 FORMAT ">>>>>"
            liRep4DataAmt        TO 60 FORMAT "->>>>>>".
         IF ttCall.UnitPrice > 0 THEN PUT STREAM tul   
            ttCall.UnitPrice     TO 70 FORMAT ">>>9.999".
         ELSE PUT STREAM tul
            ttCall.Apiece        TO 70 FORMAT ">>>9.999".
            
            
         /* more than 1 unit price */
         IF ttCall.CPrice > "" 
         THEN DO liTaff = 1 TO NUM-ENTRIES(ttCall.CPrice,"/"):
         
            rl = rl + 1.
            
            fChgPage(0).
            
            lcRep4Dur = fSec2C(DECIMAL(ENTRY(liTaff,ttCall.CDur,"/")),10).
            
            PUT STREAM tul 
            SKIP
            lcRep4Dur            TO 52 FORMAT "x(10)".
            
            IF DECIMAL((ENTRY(liTaff,ttCall.CPrice,"/"))) > 0
            THEN PUT STREAM tul
                 DECIMAL((ENTRY(liTaff,ttCall.CPrice,"/")))     
                                    TO 70 FORMAT ">>>9.999".
            ELSE PUT STREAM tul                       
                 DECIMAL((ENTRY(liTaff,ttCall.CApiece,"/")))        
                                    TO 70 FORMAT ">>9.999".
         END.
            
         PUT STREAM tul
            lcMpm                    TO 82  FORMAT "X(11)"  
            ttCall.Amt - ttCall.Mpm  TO 94  FORMAT "->>>>>9.999"  
            ttCall.Amt               TO 106 FORMAT "->>>>>9.999"  
            SKIP.
      
         /* line- ja katkolaskurit */
         ASSIGN
         rl = rl + 1
         rlx = rlx + 1.
       
      END.

      IF last-of(ttCall.CCN) THEN DO:

         fRep4SetPrintValues((ACCUM TOTAL BY ttCall.CCN ttCall.BillDur),
                             (ACCUM TOTAL BY ttCall.CCN ttCall.DataAmt)).

         IF llDispMPM
         THEN lcMPM = STRING((ACCUM TOTAL BY ttCall.CCN ttCall.Mpm),
                             "->>>>>9.999").
         ELSE lcMPM = "".

         IF epltul THEN DO:
            
            fNewPage(3).
            {Inv/nnpura4e.i}

            PUT STREAM eKirje UNFORMATTED
               " J" 
               viiva4
               MY-NL
               " J"
               SPACE(10)
               otsi[6] + " " +
                  STRING((ACCUM COUNT BY ttCall.CCN ttCall.Date)) 
                  + " " + otsi[16]  FORMAT "x(30)"
               SPACE(1)
               lcRep4Dur   FORMAT "x(10)"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.CCN ttCall.Pulses) FORMAT ">>>>>"
               SPACE(1)
               liRep4DataAmt FORMAT "->>>>>>"
               SPACE(6)
               otsi[50]  FORMAT "x(3)"
               SPACE(1)
               lcMPM     FORMAT "X(11)"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.CCN ttCall.Amt) -
               (ACCUM TOTAL BY ttCall.CCN ttCall.Mpm) FORMAT "->>>>>9.999"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.CCN ttCall.Amt) FORMAT "->>>>>9.999"
               MY-NL.
            licalask = licalask + 2. 
            
         END.
 
         ELSE IF llPDFPrint THEN DO.
            fXMLCCNTotal("* " + otsi[6],
                         STRING((ACCUM COUNT BY ttCall.CCN ttCall.Date))
                            + " " + otsi[16],
                         lcRep4Dur,
                         STRING((ACCUM TOTAL BY ttCall.CCN ttCall.Amt),
                                 "->>>>>9.99")
                        ).
         END.
          
         ELSE DO:

            fChgPage(3).
            
            DISPLAY STREAM tul 
               viiva4                  AT 1   SKIP
               "*"                     AT 5
               otsi[6] + " " + 
                  STRING((ACCUM COUNT BY ttCall.CCN ttCall.Date))
                  + " " + otsi[16]
                        FORMAT "x(30)" AT 7 /* No. of calls  */
               lcRep4Dur  FORMAT "x(10)" TO 46
               (ACCUM TOTAL BY ttCall.CCN ttCall.Pulses)
                   FORMAT ">>>>>"      TO 52
               liRep4DataAmt
                   FORMAT "->>>>>>"    TO 60
               lcMPM FORMAT "X(11)"    TO 82
               (ACCUM TOTAL BY ttCall.CCN ttCall.Amt) -
               (ACCUM TOTAL BY ttCall.CCN ttCall.Mpm)
                   FORMAT "->>>>>9.999" TO 94
               (ACCUM TOTAL BY ttCall.CCN ttCall.Amt)
                   FORMAT "->>>>>9.999" TO 106
               SKIP
            WITH NO-LABELS no-box width 130 FRAME ccnline.
            DOWN STREAM tul WITH FRAME ccnline.
 
            rl = rl + 2.
            
         END.

      END.
     
      IF last-of(ttCall.BillCode) THEN DO:

         fRep4SetPrintValues((ACCUM TOTAL BY ttCall.BillCode ttCall.BillDur),
                             (ACCUM TOTAL BY ttCall.BillCode ttCall.DataAmt)).

         IF llDispMPM
         THEN lcMPM = STRING((ACCUM TOTAL BY ttCall.BillCode ttCall.Mpm),
                             "->>>>>9.999").
         ELSE lcMPM = "".

         IF epltul THEN DO:

            fNewPage(3).
            {Inv/nnpura4e.i}
            
            PUT STREAM eKirje UNFORMATTED
               " J" viiva5
               MY-NL
               " J"
               SPACE(9)
               "* "
               otsi[6] + " " + 
                  STRING((ACCUM COUNT BY ttCall.BillCode ttCall.Date)) 
                  + " " + otsi[16]  FORMAT "x(29)"
               SPACE(1)
               lcRep4Dur  FORMAT "x(10)"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.BillCode ttCall.Pulses) FORMAT ">>>>>"
               SPACE(1)
               liRep4DataAmt FORMAT "->>>>>>"
               SPACE(6)
               otsi[50] FORMAT "x(3)"
               SPACE(1)
               lcMPM    FORMAT "X(11)"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.BillCode ttCall.Amt) -
               (ACCUM TOTAL BY ttCall.BillCode ttCall.Mpm) FORMAT "->>>>>9.999"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.BillCode ttCall.Amt) FORMAT "->>>>>9.999"
               MY-NL.
            licalask = licalask + 2. 
         END.

         ELSE IF llPDFPrint THEN DO.
 
            fXMLProdTotal("* " + otsi[6],
                          STRING((ACCUM COUNT BY ttCall.BillCode ttCall.Date)) 
                             + " " + otsi[16],
                          lcRep4Dur,
                          STRING((ACCUM TOTAL BY ttCall.BillCode
                                                 ttCall.Amt),"->>>>>9.99")
                         ).
         END.
          
         ELSE DO:
            
            fChgPage(2).
            
            DISPLAY STREAM tul 
               viiva5                  AT 1   SKIP
               "*"                     AT 3
               otsi[6] + " " + 
                  STRING((ACCUM COUNT BY ttCall.BillCode ttCall.Date))
                  + " " + otsi[16]
                        FORMAT "x(31)" AT 5 /* No. of calls  */
               lcRep4Dur  FORMAT "x(10)" TO 46
               (ACCUM TOTAL BY ttCall.BillCode ttCall.Pulses)
                   FORMAT ">>>>>"      TO 52
               liRep4DataAmt
                   FORMAT "->>>>>>"    TO 60
               lcMPM FORMAT "X(11)"    TO 82    
               (ACCUM TOTAL BY ttCall.BillCode ttCall.Amt) -
               (ACCUM TOTAL BY ttCall.BillCode ttCall.Mpm)
                   FORMAT "->>>>>9.999" TO 94
               (ACCUM TOTAL BY ttCall.BillCode ttCall.Amt)
                   FORMAT "->>>>>9.999" TO 106
               SKIP
            WITH NO-LABELS no-box width 130 FRAME tuline.
            DOWN STREAM tul WITH FRAME tuline.
            
            rl = rl + 2.
         END.

      END.
      
      IF last-of(ttCall.CLI) THEN DO:
        
         fRep4SetPrintValues((ACCUM TOTAL BY ttCall.CLI ttCall.BillDur),
                             (ACCUM TOTAL BY ttCall.CLI ttCall.DataAmt)).

         liCLIQTy  = liCLIQty + 1.

         IF epltul THEN DO:

            fNewPage(3).
            {Inv/nnpura4e.i}

            PUT STREAM eKirje UNFORMATTED
               " J" viiva3 
               MY-NL
               " J" 
               SPACE(9) 
               ttCall.CLI + " " +
               otsi[6] + ": " +
                  STRING((ACCUM COUNT BY ttCall.CLI ttCall.Date)) 
                  + " " + otsi[16]  FORMAT "x(31)"
               SPACE(1)
               lcRep4Dur   FORMAT "x(10)"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.CLI ttCall.Pulses) FORMAT ">>>>>"
               SPACE(1)
               liRep4DataAmt FORMAT "->>>>>>"
               SPACE(6)
               otsi[50]  FORMAT "x(3)"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.CLI ttCall.Mpm) FORMAT "->>>>>9.999"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.CLI ttCall.Amt) -
               (ACCUM TOTAL BY ttCall.CLI ttCall.Mpm) FORMAT "->>>>>9.999"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.CLI ttCall.Amt) FORMAT "->>>>>9.999"
               MY-NL.
               
            licalask = licalask + 2. 
         END.

         ELSE IF llPDFPrint THEN DO.
         
            fXMLCLITotal(ttCall.CLI + " " + otsi[6],
                         STRING((ACCUM COUNT BY ttCall.CLI ttCall.Date)) 
                            + " " + otsi[16],
                         lcRep4Dur,
                         STRING((ACCUM TOTAL BY ttCall.CLI ttCall.Amt),
                                 "->>>>>9.99")
                        ).
         END.
 
         ELSE DO:

            fChgPage(3).
            
            DISPLAY STREAM tul 
               SKIP(1)
               fill("-",lev) FORMAT "X(106)" AT 1   SKIP
               ttCall.CLI + " " +
                  otsi[6] + " " + 
                  STRING((ACCUM COUNT BY ttCall.CLI ttCall.Date))
                  + " " + otsi[16]
                        FORMAT "x(35)" AT 1 /* No. of calls  */
               lcRep4Dur  FORMAT "x(10)" TO 46
               (ACCUM TOTAL BY ttCall.CCN ttCall.Pulses)
                   FORMAT ">>>>>"      TO 52
               liRep4DataAmt
                   FORMAT "->>>>>>"    TO 60
               (ACCUM TOTAL BY ttCall.CLI ttCall.Mpm)
                   FORMAT "->>>>>9.999" TO 82
               (ACCUM TOTAL BY ttCall.CLI ttCall.Amt) -
               (ACCUM TOTAL BY ttCall.CLI ttCall.Mpm)
                   FORMAT "->>>>>9.999" TO 94
               (ACCUM TOTAL BY ttCall.CLI ttCall.Amt)
                   FORMAT "->>>>>9.999" TO 106
               SKIP
            WITH NO-LABELS no-box width 130 FRAME soline.
            DOWN STREAM tul WITH FRAME soline.
   
            rl = rl + 3.
         END.


         IF erisivu THEN DO:
            IF epltul THEN DO:
               ASSIGN licalask = lireppage. 
               fNewPage(3).
               {Inv/nnpura4e.i}
            END.
            ELSE DO:
               fChgPage(999).
            END.
         END.

      END.
      
      IF last-of(ttCall.CallCust) THEN DO:

         fRep4SetPrintValues((ACCUM TOTAL BY ttCall.CallCust ttCall.BillDur),
                             (ACCUM TOTAL BY ttCall.CallCust ttCall.DataAmt)).

         IF epltul THEN DO:
            IF liCLIQTy > 1 THEN DO:

               fNewPage(3).
               {Inv/nnpura4e.i}
            
               PUT STREAM eKirje UNFORMATTED
               " J"  viiva2
               MY-NL
               " J" 
               SPACE(9) 
               otsi[14] + " " +
               STRING(ttCall.CallCust) + " " + 
                  otsi[6] + " " + 
                  STRING((ACCUM COUNT BY ttCall.CallCust ttCall.Date)) 
                  + " " + otsi[16]  FORMAT "x(31)"
               SPACE(1)
               lcRep4Dur   FORMAT "x(10)"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.CallCust ttCall.Pulses) FORMAT ">>>>>"
               SPACE(1)
               liRep4DataAmt FORMAT "->>>>>>"
               SPACE(6)
               otsi[50]  FORMAT "x(3)"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.CallCust ttCall.Mpm) FORMAT "->>>>>9.999"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.CallCust ttCall.Amt) -
               (ACCUM TOTAL BY ttCall.CallCust ttCall.Mpm) FORMAT "->>>>>9.999"
               SPACE(1)
               (ACCUM TOTAL BY ttCall.CallCust ttCall.Amt) FORMAT "->>>>>9.999"
               MY-NL.

               ASSIGN licalask = licalask + 2. 
            END.
            
         END.

         ELSE IF llPDFPrint THEN DO.

            FIND Customer WHERE Customer.CustNum = ttCall.CallCust NO-LOCK.
         
            IF liCLIQTy > 1 THEN DO:
               fXMLCustTotal(otsi[14] + " " + 
                          STRING(ttCall.CallCust) + " " + otsi[6],
                          STRING((ACCUM COUNT BY ttCall.CallCust ttCall.Date)) 
                             + " " + otsi[16],
                          lcRep4Dur,
                          otsi[50] + " " +
                             STRING((ACCUM TOTAL BY ttCall.CallCust
                                                    ttCall.Amt),"->>>>>9.99")
                         ).
            END.
            
            fXMLFinish(ttCall.CallCust,
                       OUTPUT lcXMLFile).
        
            /* form PDF */
            RUN pRunFOPD(lcXLTFile,
                         lcXMLFile,
                         OUTPUT llRep4).
        
            /* send PDF via eMail */
            IF llRep4 AND lcFopMethod = "WAIT" THEN DO:

               oiPDFQty = oiPDFQty + 1.
               
               IF iiMail > 0 THEN DO:

                  /* give FOPD some time to finish it's work */
                  PAUSE 5 NO-MESSAGE. 
               
                  lcPDFFile = REPLACE(lcXMLFile,".xml",".pdf").
               
                  message "Send mail to:" lcmailaddr
                  view-as alert-box
                  question
                  buttons yes-no
                  update llRep4.
                  
                  if llRep4 then do:
                    
                  IF fSendPDFMail(lcPDFFile,
                                  lcMailAddr,
                                  Customer.Language,
                                  fTeksti(134,Customer.Language),
                                  fTeksti(133,Customer.Language),
                                  "rep4")
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
          
         ELSE DO:
         
            IF liCLIQty > 1 THEN DO:

               fChgPage(3).
            
               DISPLAY STREAM tul 
               SKIP(1)
               viiva2                  AT 1   SKIP
               otsi[14] + " " +
                  STRING(ttCall.CallCust) + " " +
                  otsi[6] + " " + 
                  STRING((ACCUM COUNT BY ttCall.CallCust ttCall.Date))
                  + " " + otsi[16]
                  FORMAT "x(35)"  AT 1
               lcRep4Dur  FORMAT "x(10)" TO 46
               (ACCUM TOTAL BY ttCall.CallCust ttCall.Pulses)
                  FORMAT ">>>>>"      TO 52
               liRep4DataAmt
                  FORMAT "->>>>>>"    TO 60
               (ACCUM TOTAL BY ttCall.CallCust ttCall.Mpm)
                   FORMAT "->>>>>9.999" TO 82
               (ACCUM TOTAL BY ttCall.CallCust ttCall.Amt) -
               (ACCUM TOTAL BY ttCall.CallCust ttCall.Mpm)
                   FORMAT "->>>>>9.999" TO 94
               (ACCUM TOTAL BY ttCall.CallCust ttCall.Amt)
                   FORMAT "->>>>>9.999" TO 106
               SKIP
               WITH NO-LABELS no-box width 130 FRAME asline.
               DOWN STREAM tul WITH FRAME asline.

               rl = rl + 3.
            END.
            
            {Syst/uprfeed.i rl}
            
         END.

      END.

END. 

IF llPDFPrint THEN DO:

   oiErrQty = liErrQty.
   
   /* finish PDF printing (send errorlog via email etc.) */
   fPDFFinish("rep4",
              "call report"). 
END.               



