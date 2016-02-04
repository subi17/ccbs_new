/* -----------------------------------------------------------------------------
  MODULE .......: NNPURA3.P
  FUNCTION .....: Puhelut maittain / liittymA -raportin print-line
  APPLICATION ..: NN
  AUTHOR .......: TT
  CREATED ......: 02.02.96
  MODIFIED .....: 26.04.96 /pt --> maan nimet taulusta nnmaa
                  03.05.96 /tt --> Sivunvaihtotestaus AINA ennen print-lineta
                  21.05.96 /tt --> Lisatty tuntien formaattia yhdella (hhhh)
                  28.11.96 /tt --> Yhteensa-summiin vain Totalt, ei katkonimea
                  07.01.97 /tt --> add-new kenttia, uusi fontti/linejako
                  13.01.96 /tt --> Layout muutettu, boxit makrolla
                  16.01.97 /tt --> Aleprosentti suoraan viim. puhelulta (sama)
                  25.03.97 /pt --> optimoitu poimintaa
                  15.05.97 /pt --> 020-puheluiden A-tilaajat 1 linelle, otsi-
                                   koksi otsi[40]
                  10.10.97 /pt --> company name from invgroup record
                  15.10.97 /pt --> adnormhin laskenta ja FORMAT
                  09.02.98 /kl --> DATE FORMAT BY udate2c -module
                  04.06.98 /kl --> invno
                  25.11.98 /kl --> topay variables, ckesto into "x(12)"
                  29.04.99 /pt --> show B-no in case of A-no in case of freeph
                  23.05.99 /pt --> show owner ow A-sub NEXT TO A-sub. no
                  29.03.00 /kl --> ertpris printing changed
                  22.09.00 /kl --> krmin "" with "NF"
                  04.12.00 ht  uses InvRowCounter instead of nnpuh (FOR EACH)
                  22.11.01 ht  FOR Nevatel
                  29.11.01 ht  VAT excluded
                  07.12.2001/aam eLetter 
                  11.12.2001/aam country name from invlang
                  12.12.2001/aam divide different prices into separate lines
                                 (BREAK BY tid) 
                  19.12.2001/aam exclude vat from "your price"
                  03.01.2002 ht VAT handling off from report, VAT text added
                  04.01.2002 ht VAT handling off from report, VAT text added,
                                topay sums AS euros, Total EUR OR FIM
                  09.01.2002 ht When FIM tariffs, LAST total also AS EUR
                  17.01.2002/aam your price AND discount data removed
                  18.01.2002/aam product level added 
                  24.01.2002/aam paper version's layout changed 
                  25.01.2002/aam round from product level onwards TO 2 decs,
                                 newpage -> fnewpage
                  19.04.2002/aam get cli from nnatno OR mobsub
                  07.08.2002/aam header texts with function fTeksti
                  15.11.2002/jp  msowner timestamp erased
                  07.01.2003/jp  GPRS
                  07.01.2003/jp  dmanimi x(30)
                  08.01.2003/jp  lev = 75
                  08.01.2003/aam test for ? in dataamt 
                  27.10.2003/aam vat header to product level               
                  31.10.2003/aam use >>>> for formats 
                  20.01.2004/aam input CLI
                  16.02.2004/aam cover sheet
                  30.04.2004/aam output oiErrQty
                  09.08.2004/aam no effects if printed to file
                  09.11.2004/aam skip products listed in parameter SpecSkipProd
                  11.04.2005/aam separate columns for mpm and service prices,
                                 use 3 decimals for all amounts 
                  07.08.2006/aam iiLetterClass to printxt.p               
  VERSION ......: M15
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/refcode.i}

/* print-linemuuttujat */
{Syst/utumaa.i}
{Func/cparam2.i}
{Inv/edefine.i}

{Inv/nnpura.i}
{Func/fdivtxt.i}

def input  parameter CustNum       as int  format "zzzzzz9"   NO-UNDO.
def input  parameter pvm1          as date format "99-99-99"  NO-UNDO.
def input  parameter pvm2          as date format "99-99-99"  NO-UNDO.
def input  parameter tilak         as int  format "9"         NO-UNDO.
DEF INPUT  PARAMETER invno         LIKE Invoice.InvNum        NO-UNDO.
DEF INPUT  PARAMETER icCLI         AS CHAR                    NO-UNDO. 
DEF INPUT  PARAMETER iiTarg        AS INT                     NO-UNDO. 
DEF INPUT  PARAMETER iiLetterClass AS INT                     NO-UNDO.
DEF OUTPUT PARAMETER oiErrQty      AS INT                     NO-UNDO. 

DEF VAR epltul  AS LOG  NO-UNDO. 

def var ckestos     as char NO-UNDO.
def var ddataamt    as dec  NO-UNDO.
def var dkestos     as dec  NO-UNDO.
def var dsokpl      as int  NO-UNDO.
def var dtopay      as dec  NO-UNDO.
def var dMpmPrice   as dec  NO-UNDO.
def var ldServPrice LIKE dMpmPrice.

def var yddataamt   as dec  NO-UNDO.
def var ydkestos    as dec  NO-UNDO.
def var ydsokpl     as int  NO-UNDO.
def var ydtopay     as dec  NO-UNDO.
def var ydMpmPrice  as dec  NO-UNDO.

def var pddataamt   as dec  NO-UNDO.
def var pdkestos    as dec  NO-UNDO.
def var pdsokpl     as int  NO-UNDO.
def var pdtopay     as dec  NO-UNDO.
def var pdMpmPrice  as dec  NO-UNDO.

def var addataamt   as dec  NO-UNDO.
def var adkestos    as dec  NO-UNDO.
def var adsokpl     as int  NO-UNDO.
def var adtopay     as dec  NO-UNDO.
def var adMpmPrice  as dec  NO-UNDO.

DEF VAR xProdName  AS CHAR NO-UNDO.
DEF VAR liMSSeq    AS INT  NO-UNDO. 
DEF VAR lcErrFile  AS CHAR NO-UNDO. 
DEF VAR liPrCust   AS INT  NO-UNDO.
DEF VAR liCliQty   AS INT  NO-UNDO. 
DEF VAR lcMPM      AS CHAR NO-UNDO. 
DEF VAR llDispMPM  AS LOG  NO-UNDO. 

DEF VAR CallCustNum LIKE Customer.CustNum  NO-UNDO.
DEF VAR InvCustNum  LIKE Customer.InvCust  NO-UNDO.
DEF VAR RateCustNum LIKE Customer.RateCust NO-UNDO.

{Inv/nnpurac.i}
{Inv/nnpura3.i}
{Inv/ereppage.i}
{Inv/spechead.i}

epltul = (iiTarg = 1).

IF epltul 
THEN lev = 88.
ELSE lev = 104.

ASSIGN
viiva1 = fill("=",lev)
viiva2 = fill("=",lev)
viiva3 = fill("-",lev)
viiva4 = fill("-",lev).

form header
   viiva1 AT 2 SKIP
   company at 2 format "x(25)"
      lcASubHeader[1] format "x(30)" AT 45 /* PUHELUT MAITTAIN / LIITTYMA */
      lcASubHeader[2] format "x(4)" AT 96                  /* Sivu */
      sl format "ZZZZ9" SKIP
   lcASubDateHeader AT 45 FORMAT "X(30)"
      pvm AT 96 FORMAT "99.99.9999" SKIP
   viiva2 AT 2 skip
   WITH width 130 NO-LABEL no-box FRAME sivuotsi.

form header
   lcASubHeader[13] format "x(20)" AT 4 
   lcASubHeader[14] format "x(8)"  TO 50                    
   lcASubHeader[15] format "x(7)"  to 60
   lcASubHeader[16] format "x(3)"  TO 69                    
   lcASubHeader[19] format "x(11)" TO 81                    
   lcASubHeader[20] format "x(11)" TO 93                    
   lcASubHeader[17] format "x(11)" TO 105                    
   SKIP
   lcASubHeader[3]  format "x(20)" AT 4                     /* ccn      */
   lcASubHeader[4]  format "x(8)"  TO 50                    /* duration */
   lcASubHeader[5]  format "x(7)"  to 60                    /* data     */
   lcASubHeader[6]  format "x(3)"  TO 69                    /* qty      */
   lcASubHeader[9]  format "x(11)" TO 81                    /* mpm      */
   lcASubHeader[10] format "x(11)" TO 93                    /* service  */
   lcASubHeader[7]  format "x(11)" TO 105                   /* total price */ 
   SKIP
   "" /* viiva3 */ AT 2 
   SKIP
WITH width 130 NO-LABEL no-box FRAME sarotsi.

FUNCTION fChgPage RETURNS LOGICAL
   (iiAddLine AS INT).

   IF rl + iiAddLine > skayt1 THEN DO:
   
      IF sl > 0 THEN DO:
         {Syst/uprfeed.i rl}
      END.
      
      sl = sl + 1.
      view STREAM tul FRAME sivuotsi. 
      rl = 4.
      
      view STREAM tul FRAME sarotsi.   
      rl = rl + 3.
   END.
   
END FUNCTION.

ASSIGN
   sl       = 0
   rl       = 0.

/* Seek customer's VAT-info */
FIND Invoice WHERE Invoice.InvNum = invno NO-LOCK NO-ERROR.
IF NOT AVAIL Invoice THEN RETURN.

FIND FIRST Customer of Invoice no-lock no-error.
IF NOT AVAILABLE Customer THEN RETURN.

fSetASubSubHeaders(pvm1,
                   pvm2).

IF NOT epltul AND oso = "" THEN DO:
    /* Haetaan raporttitehoste */
    FOR FIRST TMSRepCfg NO-LOCK WHERE
              TMSRepCfg.RepName   = "nnpura3" AND 
              TMSRepCfg.UserCode  = "",
        FIRST PrintCodes NO-LOCK WHERE 
              PrintCodes.PrinterId = TMSRepCfg.PrinterId AND
              PrintCodes.Effect    = TMSRepCfg.Effect:

         ASSIGN spit1    = PrintCodes.PageLength
                skayt1   = PrintCodes.AvailLines.
         
         PUT STREAM tul control PrintCodes.EffOn[2].
    END.     
END.

/* collect CLIs */
fGetClis(icCLI).

/* nothing to be printed */
oiErrQty = -1.

print-line:
repeat:

   FOR EACH InvRowCounter NO-LOCK WHERE 
            InvRowCounter.InvNum = invno AND
            LOOKUP(InvRowCounter.BillCode,lcSkipProd) = 0,

      FIRST wCli where 
            wCli.CLI = InvRowCounter.CLI,

      FIRST Customer no-lock where
            Customer.CustNum = wCli.CustNum

      BREAK
      BY Customer.CustNum
      BY InvRowCounter.CLI
      BY InvRowCounter.BillCode
      BY InvRowCounter.ccn:


      /* Customer vaihtui */
      IF first-of(Customer.CustNum) THEN DO:

         ASSIGN 
            CallCustNum = Customer.CustNum
            InvCustNum  = Customer.InvCust
            RateCustNum = Customer.RateCust
            oiErrQty    = 0
            liCLIQty    = 0.

         fSetAsubCustHeaders().

         fSpecGenHeader(Customer.CustNum,
                        Invoice.InvNum,
                        liASubLang).

         /* company name */
         FIND invgroup of Customer no-lock no-error.
         IF AVAIL invgroup AND InvGroup.CompName > "" 
         THEN company = InvGroup.CompName.
         ELSE company = ynimi.

         /* cover sheet txt */ 
         IF llCaSivu < 0 THEN DO:
             
            ASSIGN liMSSeq  = 0
                   liPrCust = CustNum.
               
            IF icCLI > "" AND wCLI.OwnerID > 0 THEN 
            FOR FIRST MsOwner NO-LOCK WHERE
                      RECID(MsOwner) = wCLI.OwnerID:
               ASSIGN liMSSeq  = MsOwner.MSSeq
                      liPrCust = MsOwner.CustNum.
            END.
            
            RUN Mc/printxt (liPrCust,
                         liMsSeq, 
                         "",
                         1,                      /* 1=invtext */
                         7,                      /* 7=address already set */
                         "General",
                         "SpecCover",
                         0,
                         IF epltul THEN 3 ELSE 2,   /* print target  */
                         iiLetterClass,
                         OUTPUT lcErrFile).
         END.
 
         IF epltul THEN DO:
            fNewPage(999).
            {Inv/nnpura3e.i}
         END.

         ELSE DO:
             fChgPage(999).
         END.
      END.

      /* Lasketaan maakohtaiset maarat */
      ASSIGN
        dsokpl    = dsokpl  + InvRowCounter.Quantity
        dkestos   = dkestos + InvRowCounter.Duration
        dtopay    = dtopay  + InvRowCounter.Amount
        dMpmPrice = dMpmPrice + InvRowCounter.RefPrice
        ddataamt  = ddataamt + 
                    (IF InvRowCounter.DataAmt NE ?
                     THEN InvRowCounter.DataAmt  
                     ELSE 0) /* Data amount */
                
        /* tuotekohtaiset laskurit */
        pdsokpl    = pdsokpl  + InvRowCounter.Quantity  
        pdkestos   = pdkestos + InvRowCounter.Duration 
        pdtopay    = pdtopay  + InvRowCounter.Amount 
        pdMpmPrice = pdMpmPrice + InvRowCounter.RefPrice
        pddataamt  = pddataamt + 
                     (IF InvRowCounter.DataAmt NE ?
                      THEN InvRowCounter.DataAmt 
                      ELSE 0)                /* Data amount */.
        
      /* Soittaja vaihtui */
      IF first-of(InvRowCounter.CLI) THEN DO:

         lcASubASubHeader = fSpecCLIHeader(InvRowCounter.CLI,
                                           InvRowCounter.ToDate,
                                           liAsubLang). 
         
         /* Tarvitaanko uusi sivu */
         IF epltul THEN DO:
            fNewPage(5).
            {Inv/nnpura3e.i}
         END.
         ELSE DO:
            fChgPage(5).
         END.

         IF epltul THEN DO:
         
            IF NOT FIRST(InvRowCounter.CLI) THEN DO:
               PUT STREAM eKirje UNFORMATTED
                   " I"
                   MY-NL.
               licalask = licalask + 1.
            END.
            
            PUT STREAM eKirje UNFORMATTED
                " I" 
                SPACE(4) 
                lcASubASubHeader
                MY-NL
                " I" 
                MY-NL.
            licalask = licalask + 2.
         END. 
         ELSE DO:
         
            IF NOT FIRST(InvRowCounter.CLI) THEN DO:
               PUT STREAM tul SKIP(1).
               rl = rl + 1.
            END.
            
            PUT STREAM tul  
               lcASubASubHeader AT 2 FORMAT "X(70)" skip(1).
            rl = rl + 2.
         END.   
      END.

      /* product */
      IF first-of(InvRowCounter.BillCode) THEN DO:

         /* Tarvitaanko uusi sivu */
         IF epltul THEN DO:
            fNewPage(3).
            {Inv/nnpura3e.i}
         END.
         ELSE DO:
            fChgPage(3).
         END.

         lcASubProdHeader = fSpecBItemHeader(InvRowCounter.BillCode,
                                             liASubLang,
                                             pvm2).
         
         llDispMPM = fDispMPM(InvRowCounter.BillCode).
         
         IF epltul THEN DO:
            
            IF NOT FIRST(InvRowCounter.BillCode) AND 
               NOT FIRST-OF(InvRowCounter.CLI)
            THEN DO:
               PUT STREAM eKirje UNFORMATTED
                   " I"
                   MY-NL.
               licalask = licalask + 1.
            END.
            
            PUT STREAM eKirje UNFORMATTED
                " I"
                SPACE(5)
                lcASubProdHeader
                MY-NL.
            licalask = licalask + 1.
         END.
          
         ELSE DO:
            IF NOT FIRST(InvRowCounter.CLI)  AND 
               NOT FIRST-OF(InvRowCounter.CLI)
            THEN DO:
               PUT STREAM tul SKIP(1).
               rl = rl + 1.
            END.
            
             PUT STREAM tul UNFORMATTED 
                lcASubProdHeader AT 3 SKIP.
             rl = rl + 1.
         END.
      END.

      IF last-of(InvRowCounter.CCN) THEN DO:
      
         lcASubCCNHeader = fSpecCCNHeader(InvRowCounter.CCN,
                                          liASubLang,
                                          pvm2).

         /* Tarvitaanko uusi sivu */
         IF epltul THEN DO:
            fNewPage(0).
            {Inv/nnpura3e.i}
         END.
         ELSE DO:
            fChgPage(0).
         END.

         ASSIGN ckestos     = fSec2C(dkestos,10)
                ldServPrice = dtopay - dMpmPrice.
                
         IF llDispMPM
         THEN lcMPM = STRING(dMpmPrice,"->>>>>9.999").
         ELSE lcMPM = "".

         IF epltul THEN DO:
            PUT STREAM eKirje UNFORMATTED
               " I" 
               space(6)
               lcASubCCNHeader    format "x(25)"
               space(1)
               ckestos            format "x(10)"
               space(1)
               ddataamt / 1024    format "->>>>>>"
               space(1)
               dsokpl             format "->>>>9"
               lcMPM              format "x(11)"
               space(1)
               ldServPrice        format "->>>>>9.999"
               space(1) 
               dtopay             format "->>>>>9.999"
               MY-NL.
               ASSIGN licalask = licalask + 1.
         END.
         ELSE DO:
            DISPLAY STREAM tul 
               lcASubCCNHeader AT 4   FORMAT "X(30)" 
               ckestos         TO 50  FORMAT "X(10)"
               ddataamt / 1024 TO 60  FORMAT "->>>>>>"
               dsokpl          TO 69  FORMAT "->>>>>9"
               lcMPM           TO 81  FORMAT "X(11)"
               ldServPrice     TO 93  FORMAT "->>>>>9.999"
               dtopay          TO 105 FORMAT "->>>>>9.999" SKIP
            WITH NO-LABELS no-box width 130 FRAME puline.
            DOWN STREAM tul WITH FRAME puline.
         END.

         ASSIGN dkestos = 0 dsokpl = 0 ddataamt = 0 dtopay = 0 dMpmPrice = 0.

         /* line- ja katkolaskurit */
         ASSIGN rl = rl + 1.

         /* tarvittaessa otetaan uusi sivu seuraavaa a-tilajaa ja/tai
           raporttiasiakkaan loppusummaa varten */
         IF erisivu THEN DO:
             IF epltul THEN DO:
                ASSIGN licalask = lireppage. 
                fNewPage(0).
                {Inv/nnpura3e.i}
             END.
             ELSE DO:
                fChgPage(999).
            END.
         END.
      END.

      /* sub-total FOR product */
      IF last-of(InvRowCounter.BillCode) THEN DO:

         /* Tarvitaanko uusi sivu */
         IF epltul THEN DO:
            fNewPage(2).
            {Inv/nnpura3e.i}
         END.
         ELSE DO:
            fChgPage(2).
         END.

         ASSIGN ckestos     = fSec2C(pdkestos,10)
                pdtopay     = round(pdtopay,3)
                ldServPrice = pdtopay - pdMpmPrice.

         IF llDispMPM
         THEN lcMPM = STRING(pdMpmPrice,"->>>>>9.999").
         ELSE lcMPM = "".
         
         /* Tulostetaan tuote-yhteensA-line */
         IF epltul THEN DO:
             PUT STREAM eKirje UNFORMATTED
                " I" 
                space(5)
                substring(viiva4,2)
                MY-NL
                " I"
                SPACE(5) 
                "* " 
                lcASubHeader[8] format "x(12)"
                space(13)
                ckestos         format "x(10)"
                space(1)
                pddataamt / 1024 format "->>>>>>"
                pdsokpl         format "->>>>>9"
                lcMPM           format "x(11)"
                space(1)
                ldServPrice     format "->>>>>9.999"
                space(1) 
                pdtopay         format "->>>>>9.999"
                MY-NL
                " I" 
                MY-NL.
                ASSIGN licalask = licalask + 3.
         END. 
      
         ELSE DO:
            DISPLAY STREAM tul 
               viiva4 format "x(103)"  AT 3   SKIP
               "* "                    AT 4
               lcASubHeader[8]         AT 6   format "x(12)" 
               ckestos                 TO 50  FORMAT "X(10)"
               pddataamt / 1024        TO 60  FORMAT "->>>>>>"
               pdsokpl                 TO 69  FORMAT "->>>>>9"
               lcMPM                   TO 81  FORMAT "X(11)"
               ldServPrice             TO 93  FORMAT "->>>>>9.999"
               pdtopay                 TO 105 FORMAT "->>>>>9.999" 
               skip(1)
             WITH NO-LABELS no-box width 130 FRAME soline.
             DOWN STREAM tul WITH FRAME soline.
         END.

         rl = rl + 3.
          
         ASSIGN 
         /* Soittajakohtaiset laskurit */
         ydsokpl    = ydsokpl  + pdsokpl       /* Soittojen maara kpl */
         ydkestos   = ydkestos + pdkestos     /* Soiton kesto sekunteina */
         ydtopay    = ydtopay  + pdtopay      /* Soittojen kokonaisbruttoh. */
         ydMpmPrice = ydMpmPrice + pdMpmPrice
         yddataamt  = yddataamt + pddataamt   /* Soiton datamäärä */


         /* Customerkohtaiset laskurit */
         adsokpl    = adsokpl  + pdsokpl       /* Soittojen maara kpl */
         adkestos   = adkestos + pdkestos     /* Soiton kesto sekunteina */
         adtopay    = adtopay  + pdtopay     /* Soittojen kokonaisbruttoh. */
         adMpmPrice = adMpmPrice + pdMpmPrice
         addataamt  = addataamt  + pddataamt.

         ASSIGN pdkestos = 0 pdsokpl = 0 pdtopay = 0 pddataamt = 0 
                pdMpmPrice = 0.
      END.


      IF last-of(InvRowCounter.CLI) THEN DO:

         /* Tarvitaanko uusi sivu */
         IF epltul THEN DO:
            fNewPage(2).
            {Inv/nnpura3e.i}
         END.
         ELSE DO:
            fChgPage(2).
         END.

         ASSIGN ckestos     = fSec2C(ydkestos,10)
                liCLIQty    = liCLIQty + 1
                ldServPrice = ydtopay - ydMpmPrice.

         /* Tulostetaan soittaja-yhteensA-line */
         IF epltul THEN DO:
             PUT STREAM eKirje UNFORMATTED
                " I" 
                SPACE(4) viiva4
                MY-NL
                " I" 
                SPACE(4) 
                InvRowCounter.CLI + " " + 
                lcASubHeader[8] format "x(27)"
                space(1)
                ckestos         format "x(10)"
                space(1)
                yddataamt / 1024 format "->>>>>>"
                ydsokpl         format "->>>>>9"
                ydMpmPrice      format "->>>>>9.999"
                space(1)
                ldServPrice     format "->>>>>9.999"
                space(1) 
                ydtopay         format "->>>>>9.999"
                MY-NL.
             licalask = licalask + 2.
                
             IF NOT LAST-OF(Customer.CustNum) THEN DO:
                PUT STREAM eKirje UNFORMATTED
                   " I" 
                   MY-NL.
                licalask = licalask + 1.
             END. 
                
         END. 

         ELSE DO:
            DISPLAY STREAM tul 
               viiva4                  AT 2   SKIP
               "* "                    AT 2
               lcASubHeader[8]         AT 4   format "x(12)"
               ckestos                 TO 50  FORMAT "X(10)"
               yddataamt / 1024        TO 60  FORMAT "->>>>>>"
               ydsokpl                 TO 69  FORMAT "->>>>>9"
               ydMpmPrice              TO 81  FORMAT "->>>>>9.999"
               ldServPrice             TO 93  FORMAT "->>>>>9.999"
               ydtopay                 TO 105 FORMAT "->>>>>9.999" 
               skip(1)
             WITH NO-LABELS no-box width 130 FRAME soline2.
             DOWN STREAM tul WITH FRAME soline2.
         END.

         rl = rl + 3.
         
         ASSIGN ydkestos = 0 ydsokpl = 0 ydtopay = 0 yddataamt =  0
                ydMpmPrice = 0.
      END.


      /* Tulostetaan Customer-yhteensA-line */
      IF last-of(Customer.CustNum) THEN DO:

         ASSIGN ckestos     = fSec2C(adkestos,12)
                ldServPrice = adtopay - adMpmPrice.

         IF epltul THEN DO:

            IF liCLIQty > 1 THEN DO:
               fNewPage(1).
               {Inv/nnpura3e.i}
               
               PUT STREAM eKirje UNFORMATTED
               " I" 
               SPACE(4) viiva1
               MY-NL
               " I" 
               SPACE(4) 
               "** "
               lcASubHeader[8] format "x(12)"
               space(11)
               ckestos         format "x(12)"
               space(1)
               addataamt / 1024 format "->>>>>>"
               adsokpl         format "->>>>>9"
               adMpmPrice      format "->>>>>9.999"
               space(1)
               ldServPrice     format "->>>>>9.999"
               space(1) 
               adtopay         format "->>>>>9.999"
               MY-NL.

               ASSIGN licalask = licalask + 2.
            END.
               
         END.

         ELSE DO:

            IF liCLIQty > 1 THEN DO:

               fChgPage(1).
               
               DISPLAY STREAM tul 
               viiva1                  AT 2   SKIP
               "** "                   AT 2
               lcASubHeader[8]         AT 5   format "x(12)" 
               ckestos                 TO 50  FORMAT "X(12)"
               addataamt / 1024        TO 60  FORMAT "->>>>>>"
               adsokpl                 TO 69  FORMAT "->>>>>9"
               adMpmPrice              TO 81  FORMAT "->>>>>9.999"
               ldServPrice             TO 93  FORMAT "->>>>>9.999"
               adtopay                 TO 105 FORMAT "->>>>>9.999" 
               SKIP
               WITH NO-LABELS no-box width 130 FRAME asline.

               DOWN STREAM tul WITH FRAME asline.
    
               rl = rl + 2.
            END.   
         
            {Syst/uprfeed.i rl}

         END.

         ASSIGN adkestos = 0 adsokpl = 0 adtopay = 0 addataamt = 0 
                adMpmPrice = 0.
      END.
   END.

   LEAVE print-line.
END. /* print-line */


