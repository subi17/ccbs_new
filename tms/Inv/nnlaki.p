/* ---------------------------------------------------------------------------
  MODULE .......: NNLAKI.P
  FUNCTION .....: Laskujen tulostus  
  APPLICATION ..: NN
  CREATED ......: 26.03.99 pt
  MODIFIED .....: 22.06.99 pt ellei puheluita, ei yriteta tulostaa raportteja
                  04.07.99 kl sp-code[1], estä tulostus
                  09.08.99 pt rapas temp table ym.
                  15.08.99 pt edita-tulostus 2 eri tiedostoon new/old, ks.
                              hakusanalla aik-Invoice
                  23.08.99 pt edellinen rajattu vain ei-sopimusasiakkaille
                              (kuluttajille), ks. hakusanalla sopimus-as
                              OLETETAAN etta InvGroup "1" on sopimusas. ryhma
                  27.08.99 pt hvteks            
                  22.11.99 ht suoraveloituslaskutus
                  09.12.99 pt+ht edell. viritysta
                  19.01.00 ht tilin tulostaminen sv-laskulle korjattu
                  05.04.00 ht sv-laskun kirjoitus perustuu ~Invoice.sv-kenttään
                  17.05.00 ht PrintState ei muutu 2:sta 1:een
                  30.05.00 ht rap-as: -lohkossa myös itse laskutusasiakkaan 
                              omien puheluiden olemassaolo testataan, jolloin 
                              tyhjiä raporttisivuja ei tulostu edita-tiedostoon
                  16.10.01 jr Muutettu vastaamaan Ekirje laskua
                  17.10.01/aam use umakro.p instead of direct lpr-command,
                               use nnpurv FOR reports (1-7)
                  06.11.01 jr InvoiceGroup can be blank             
                  07.11.01/aam las_teh assigned in correct place
                  20.11.01 ht functions into refcode.i
                  02.11.01 ht NEW reports nnpura1 AND nnpura3 in use
                  12.12.01/aam DISPLAY country FOR customer,
                               DISPLAY lasposno UNFORMATTED,
                               payment term is 14 FOR ALL dated 12.12.2001,
                               display prefix (is-code) for product "I"
                  07.01.02/aam DISPLAY cust nbr above name 
                  11.01.02  ht Jippii version
                  24.01.02/aam longer FORMAT FOR custnbr,
                               directory from macros from cparam,
                               product name from invlang
                  10.02.02/aam NEW report FOR contracts (nncore1)
                  11.02.02/aam invgroup.ig-cpers left out (conper)
                  13.02.02/aam InvRow.FromDate AND ToDate can be blank 
                  20.02.02/aam paging FOR lines corrected 
                  26.02.02/aam invoice text NOT printed FOR credit inv.
                  12.03.02/aam overtime interest percent from fOtIntPerc
                  20.03.02/aam one line less FOR invoice line section (35)
                  26.03.06 lp added "Do you want update unvoice's state (Y/N)?"
                              messages --> TO english
                      02.05.02/aam don't try TO FIND an invoice matching the 
                               definitions before the actual print loop
                  07.06.02/aam use Invoice.OverPaym FOR overpayment
                  06.08.02/aam show vat% for each invoice line,
                               total for each vat% after invoice lines,
                               header texts for lines
                  02.09.02/aam text FOR Customer who have remaining 
                               overpaym. balance (85-88)
                  29.11.02/aam PMI text after invoice rows 
                  04.12.02/aam PVM text after invoice rows, 
                               subtitles for PMI-rows 
                  17.12.02/jr  Eventlog
                  07.01.03/aam bitem-lines are combined by product,
                               subtitles for all rows,
                               same layout for contract and other rows
                  23.01.03 jp  FAtime RowType = 7             
                  07.02.03/aam new texts (use as-equ[4])
                  17.02.03/aam new macro 7056
                  26.02.03/aam direct debit
                  07.04.03/aam boxes before bank-transfer changed 
                  15.04.03/aam don't use invgroup.intype in reference nbr
                  16.04.03 kl  PMI & 991 texts commented
                  28.04.03/aam use fprintinv (common routines for all
                               different types of invoice printing routines)
                  12.08.03/aam vasrep             
                  22.08.03/aam set ws-disp = true,
                               print dd-texts with smaller font
                  13.10.03/aam use si-recid2 to get invnum from calling prog.
                  12.12.03/aam report 4,
                               VatUsage,
                               lcMFont etc. 
                  20.01.04/aam specifications on cli level             
                  23.03.04/aam deposit invoice, 
                               shark layout
                  07.05.04/aam ItSendLog             
                  11.06.04/aam cancellation of printstate -update corrected
                  09.08.04/aam no effects if printed to file
                  03.01.05/aam RepCode from SubSer (MsOwner)
                  06.05.05/aam invoice row amounts with 3 decimals
                  30.01.06/aam country before zipcode for foreign addresses
                  07.08.06/aam letterclass (0) to fPrintSpecRep
                  
 VERSION ......: M15
  -------------------------------------------------------------------------- */

&GLOBAL-DEFINE LocalContactInfo YES

{Syst/commali.i}
{Func/refcode.i}
{Syst/utumaa.i "new"}
{Func/cparam2.i}
{Inv/edefine.i new}
{Func/feplform.i}
{Func/fsubser.i}
{Func/fotint.i}
{Syst/eventval.i} 
{Func/feffect.i}
{Func/fprintinv.i}
{Func/fgetclis.i}

ASSIGN tuni1 = "nnlaki"
       tuni2 = "".

DEF STREAM invoice.

DEF BUFFER aik-Invoice FOR Invoice.      

   
DEF NEW SHARED VAR laskuno AS INT  FORMAT ">>>>>>>9" NO-UNDO.
DEF NEW SHARED VAR viite   AS CHAR FORMAT "x(25)"    NO-UNDO.

DEF VAR c-inv        AS CHAR                         NO-UNDO.
DEF VAR bchk         AS LOG                          NO-UNDO.
DEF VAR bold-on      AS CHAR                         NO-UNDO.
DEF VAR bold-off     AS CHAR                         NO-UNDO. 
DEF VAR topay        LIKE Invoice.InvAmt.
DEF VAR llok         AS logic                        NO-UNDO.

DEF VAR las_teh      AS CHAR                         NO-UNDO.
DEF VAR m-spit1      AS INT                          NO-UNDO.
DEF VAR m-skayt1     AS INT                          NO-UNDO.

DEF VAR pmtied       AS CHAR                         NO-UNDO.

DEF VAR paivays      AS CHAR FORMAT "x(10)"          NO-UNDO.
DEF VAR lrivi-FromDate AS CHAR FORMAT "x(10)"          NO-UNDO.
DEF VAR lrivi-ToDate AS CHAR FORMAT "x(10)"          NO-UNDO.
DEF VAR ots-pvm      AS CHAR FORMAT "x(10)"          NO-UNDO.
DEF VAR sl           AS INT  FORMAT "zz"             NO-UNDO.
DEF VAR rl           AS INT                          NO-UNDO.
DEF VAR trl          AS INT                          NO-UNDO.
DEF VAR pyormk       AS DEC  FORMAT "zzzzzzz9.99-"   NO-UNDO.
DEF VAR tex          AS CHAR FORMAT "x(65)" EXTENT 6 NO-UNDO.
DEF VAR latx         AS CHAR EXTENT 60               NO-UNDO.
DEF VAR ke           AS LOG  FORMAT "Yes/No"         NO-UNDO INIT "No".
DEF VAR i            AS INT                          NO-UNDO.
DEF VAR j            AS INT                          NO-UNDO.
DEF VAR atpvm        AS DATE FORMAT "99-99-99"       NO-UNDO.
def var i-date1      AS DATE FORMAT "99-99-99"       NO-UNDO.
def var i-date2      AS DATE FORMAT "99-99-99"       NO-UNDO.
DEF VAR lano1        AS INT  FORMAT "zzzzzzz9"       NO-UNDO.
DEF VAR lano2        AS INT  FORMAT "zzzzzzz9"       NO-UNDO.
DEF VAR asno1        AS INT  FORMAT "zzzzzzz9"       NO-UNDO.
DEF VAR asno2        AS INT  FORMAT "zzzzzzz9"       NO-UNDO.
DEF VAR status1      AS INT  FORMAT "9"              NO-UNDO.
DEF VAR status2      AS INT  FORMAT "9"              NO-UNDO.
DEF VAR dlayht       AS CHAR FORMAT  "x(13)"         NO-UNDO.
DEF VAR apumk        AS INT                          NO-UNDO.
DEF VAR pyorp        AS DEC  FORMAT "zz,zzz,zz9.99-" NO-UNDO.
DEF VAR kesk         AS LOG                          NO-UNDO INIT FALSE.
DEF VAR epvm         AS DATE FORMAT "99-99-99"       NO-UNDO.
DEF VAR epaiva       AS CHAR FORMAT "x(10)"          NO-UNDO.
DEF VAR dtunimi      AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR asrap        AS CHAR                         NO-UNDO.
DEF VAR texkpl       AS INT                          NO-UNDO.
DEF VAR rap          AS LOG                          NO-UNDO INIT TRUE.
DEF VAR uusia        AS LOG                          NO-UNDO.
DEF VAR rap_num      AS INT                          NO-UNDO.
DEF VAR claskuno     AS CHAR                         NO-UNDO.
DEF VAR alinrivi     AS INT                          NO-UNDO INIT 38.

DEF VAR lasnro       LIKE Customer.InvCust           NO-UNDO.
DEF VAR InvGroup     LIKE invgroup.InvGroup          NO-UNDO.
DEF VAR cg-code      LIKE CustGroup.CustGroup        NO-UNDO.

DEF VAR xpvm1        AS DA                           NO-UNDO.
DEF VAR xpvm2        AS DA                           NO-UNDO.
DEF VAR xpvm3        AS DA                           NO-UNDO.
DEF VAR sop-as       AS LOG                          NO-UNDO.
DEF VAR lasnro2      AS INT                          NO-UNDO.
DEF VAR ptili        AS CHAR FORMAT "x(14)"          NO-UNDO INIT ?.
DEF VAR lcigroup     AS CHAR                         NO-UNDO.
DEF VAR apu          AS CHAR                         NO-UNDO.
DEF VAR cur          AS C                            NO-UNDO.
DEF VAR rc           AS INT                          NO-UNDO.

DEF VAR tuni1_orig   LIKE tuni1                      NO-UNDO.
DEF VAR xPapMacro    AS CHAR                         NO-UNDO.
DEF VAR lcMacroEff   AS CHAR                         NO-UNDO.
DEF VAR xMacDir      AS CHAR                         NO-UNDO.
DEF VAR UpdState     AS LOG FORMAT "Yes/No" INIT "Y" NO-UNDO.
DEF VAR xHeadTxt     AS CHAR                         NO-UNDO. 
DEF VAR lcSubHeader  AS CHAR                         NO-UNDO.
DEF VAR lcVatUsage   AS CHAR                         NO-UNDO. 
DEF VAR lii          AS INT                          NO-UNDO. 
DEF VAR lcMacros     AS CHAR                         NO-UNDO.
DEF VAR llBeg        AS LOG                          NO-UNDO. 
DEF VAR lcErrFile    AS CHAR                         NO-UNDO. 
DEF VAR lcNextDue    AS CHAR                         NO-UNDO EXTENT 2. 
DEF VAR lcRepCode    AS CHAR                         NO-UNDO. 

DEF TEMP-TABLE ttInv  NO-UNDO
   FIELD InvNum    AS INT
   FIELD PrevState AS INT.
   
IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.
   RUN StarEventInitialize(lhInvoice).
              
END.

ASSIGN cur      = fCParamC("DefCurrency")
       llBeg  = TRUE
       
       xMacDir  = fCParamC("MacroDir")
       lcMacros = fCParamC("MacroInvoice").


IF lcMacros > "" AND lcMacros NE ? 
THEN lcMacros = xMacDir + lcMacros.
ELSE lcMacros = "". 

form
   skip(17)
   WITH TITLE COLOR value(ctc)
   " " + ynimi + " INVOICE PRINTOUT " + STRING(pvm,"99-99-99") + " "
COLOR value(cfc) width 80 OVERLAY FRAME taka.

form
   lano1 label " Invoice number .........."
      help "Invoices from number ..."
   "-" lano2 NO-LABEL 
      help "Invoices to number ..."                                 SKIP
   " Invoice group  ..........:" InvGroup  NO-LABEL
      help "Invoice group to print, empty for all"
      IGName no-label format "x(20)"SKIP
   " Customer number .........:" asno1    NO-LABEL AT 29 
      help "Customer from number ..."
   "-" asno2 NO-LABEL 
      help "Customer to number ..."                                SKIP
   " External Customer Group .:" cg-code NO-LABEL
      help "Code of an External Customer Group; (Empty = NONE)"
   CustGroup.CGName no-label  format "x(24)"  SKIP
   i-date1 label " Invoice date ............"
      help "From date (INVOICE DATE) ..." 
   "-" i-date2 NO-LABEL
      help "To date (INVOICE DATE) ..."                             SKIP
   " Printing status .........:" status1  NO-LABEL AT 36
      help "Invoices from status code ..."
   "-" status2 NO-LABEL
      help "Invoices to status code ..."                            SKIP
   " Update Status ...........:"  UpdState NO-LABEL                
      HELP "Do you want to update invoice's printing status (Y/N)?" SKIP
   " Specification reports ...:"  rap no-label format "Yes/No"
      help "Print call specification reports for each invoice (Y/N)" SKIP

with title color value(ctc) " INVOICE CRITERIA " side-labels
COLOR value(cfc) ROW 3 centered OVERLAY FRAME rajat.

{Inv/invsta.frm}


FUNCTION fLineHead RETURNS LOGICAL.

   /* bold */
   IF lcBFontOn > "" THEN 
   PUT STREAM tul CONTROL lcBFontOn.
   
   PUT STREAM tul 
      lcRowHeader[11] FORMAT "x(19)" AT 51
      SPACE(1)
      lcRowHeader[12] FORMAT "x(5)"
      SPACE(1)
      lcRowHeader[16] FORMAT "x(13)"
      SKIP
      lcRowHeader[1] FORMAT "x(19)" AT 51
      SPACE(1)
      lcRowHeader[2] FORMAT "x(5)"
      SPACE(1)
      lcRowHeader[6] FORMAT "x(13)"
      SKIP.    
   ASSIGN rl = rl + 2.
 
   IF lcBFontOff > "" THEN 
   PUT STREAM tul CONTROL lcBFontOff.
    
END FUNCTION.

FUNCTION fChgPage RETURNS LOGICAL
   (iiAddLine AS INT).
 
   IF rl + iiAddLine < alinrivi THEN RETURN FALSE.

   IF sl > 0 THEN 
   PUT STREAM tul UNFORMATTED CHR(12).
   
   IF las_teh > "" THEN PUT STREAM tul UNFORMATTED las_teh.

   ASSIGN sl    = sl + (IF sl = 0 THEN 2 ELSE 1)       
          llBeg = FALSE.
         
   PUT STREAM tul SKIP(4).
   rl = 4. 
   
   /* medium font on */
   IF lcMFontOn > "" 
   THEN PUT STREAM tul CONTROL lcMFontOn.

   IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN DO:

      PUT STREAM tul UNFORMATTED
         STRING(lcInvHeader[4],"X(20)") AT 49
         STRING(Invoice.CustNum)
         SKIP
         STRING(lcInvHeader[8],"X(20)") AT 49
         STRING(lcTagOwner,"X(22)")
         SKIP.
      
   END. 
   ELSE PUT STREAM tul SKIP(2).
   
   PUT STREAM tul UNFORMATTED
       STRING(lcInvHeader[1],"X(20)") AT 49
       STRING(Invoice.InvDate,"99.99.9999")
       SKIP
       STRING(lcInvHeader[2],"X(20)") AT 49
       Invoice.InvNum
       SKIP
       STRING(lcInvHeader[3],"X(20)") AT 49
       STRING(Invoice.DueDate,"99.99.9999")
       SKIP.
       
   /* normal font on */
   IF lcMFontOff > "" 
   THEN PUT STREAM tul CONTROL lcMFontOff.

   PUT STREAM tul UNFORMATTED
       lcCustName AT 9 FORMAT "X(35)" SKIP
       lcCOName   AT 9 FORMAT "X(32)".

   IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN DO:
      IF lcMFontOn > "" 
      THEN PUT STREAM tul CONTROL lcMFontOn.

      PUT STREAM tul UNFORMATTED
         STRING(lcInvHeader[5],"X(20)") AT 41
         lcDelInt.

      IF lcMFontOff > "" 
      THEN PUT STREAM tul CONTROL lcMFontOff.
   END.
      
   PUT STREAM tul
       SKIP
       lcAddress AT 9 FORMAT "X(32)".
      
   IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN DO:
      IF lcMFontOn > "" 
      THEN PUT STREAM tul CONTROL lcMFontOn.

      PUT STREAM tul UNFORMATTED 
         STRING(lcInvHeader[6],"X(20)") AT 41
         lcRemFee.
       
      IF lcMFontOff > "" 
      THEN PUT STREAM tul CONTROL lcMFontOff.
   END.
   
   PUT STREAM tul 
      SKIP ""
      (IF LOOKUP(lcEPLACountry,",FI") = 0 AND
       lcEPLRCountry > "" 
       THEN lcEPLACountry + "-"
       ELSE "") +  
      lcPost    AT 9 FORMAT "X(35)" SKIP.
      
   IF LOOKUP(lcEPLACountry,",FI,FIN,FINLAND") = 0
   THEN PUT STREAM tul
      lcCountry AT 9 FORMAT "X(32)".
      
   /* rest and invoice lines with medium font */
   IF lcMFontOn > "" 
   THEN PUT STREAM tul CONTROL lcMFontOn.
 
   IF lcNextDue[1] > "" THEN DO:
      
      PUT STREAM tul 
         lcNextDue[1] AT 41 FORMAT "X(35)" SKIP
         lcNextDue[2] AT 49 FORMAT "X(35)".
   END. 
   ELSE PUT STREAM tul SKIP(1).

   PUT STREAM tul 
      SKIP(2).
    
   rl = rl + 13.    

   /* line headers */
   IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN DO:
      fLineHead().
   END.
   
END FUNCTION.

FUNCTION fPrintTxt RETURNS LOGICAL
   (icText  AS CHAR,
    iiEmpty AS INT).

   IF icText = "" THEN RETURN FALSE.

   IF iiEmpty = 1 OR iiEmpty = 3 THEN DO:
      fChgPage(0).
      PUT STREAM tul SKIP(1).
      ASSIGN rl = rl + 1.
   END.

   icText = fSeparateInvoTxt(icText,
                             80).

   DO lii = 1 TO NUM-ENTRIES(icText,CHR(9)):

      fChgPage(0).

      PUT STREAM tul UNFORMATTED 
         ENTRY(lii,icText,CHR(9)) AT 5 SKIP.
      rl = rl + 1.
   END.                         

   IF iiEmpty = 2 OR iiEmpty = 3 THEN DO:
      fChgPage(0).
      PUT STREAM tul SKIP(1).
      ASSIGN rl = rl + 1.
   END.

   RETURN TRUE. 

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
            ttInvoTxt.ITTxt > ""
   BY ttInvoTxt.ITOrd:
      
      /* replace tags */
      ttInvoTxt.ITTitle = fReplaceTag(ttInvoTxt.ITTitle,0).
      ttInvoTxt.ITTxt   = fReplaceTag(ttInvoTxt.ITTxt,0).
             
      /* divide into lines */
      ttInvoTxt.ITTxt = fSeparateInvoTxt(ttInvoTxt.ITTxt,75).
            
      IF ttInvoTxt.ITPos = -1 THEN DO:
      
         RUN Mc/printxt.p (Invoice.CustNum,
                      0,                      /* msseq */
                      lcTagCLI,
                      1,                      /* 1=invtext */
                      7,                      /* address already set */
                      "",
                      "",
                      ttInvoTxt.ITNum,
                      2,                     /* 2=local */
                      0,
                      OUTPUT lcErrFile).
        
         
         NEXT.              
      END.
            
      IF iiPosition = 2 OR iiPosition = 4 THEN DO:
         fChgPage(0).
         PUT STREAM tul UNFORMATTED SKIP(1).
         rl = rl + 1.
      END.
      
      IF ttInvoTxt.ITTitle > "" AND 
         Invoice.InvType = 3    AND
         Invoice.InvType NE 4   AND
         ttInvoTxt.ITPos NE -1
      THEN DO:
         fChgPage(2).

         /* title with bold */   
         IF lcBFontOn > "" 
         THEN PUT STREAM tul CONTROL lcBFontOn.
         
         PUT STREAM tul UNFORMATTED
            ttInvoTxt.ITTitle AT 3 
            SKIP(1).
 
         IF lcBFontOff > "" 
         THEN PUT STREAM tul CONTROL lcBFontOff.
 
         rl = rl + 2.
      END.
      
      DO liCount = 1 TO NUM-ENTRIES(ttInvoTxt.ITTxt,CHR(9)):

         fChgPage(0).
      
         IF ENTRY(liCount,ttInvoTxt.ITTxt,CHR(9)) = "." OR
            ENTRY(liCount,ttInvoTxt.ITTxt,CHR(9)) = ""
         THEN PUT STREAM tul UNFORMATTED SKIP(1).
         
         ELSE PUT STREAM tul UNFORMATTED 
            (IF ttInvoTxt.ITPos = -1 OR 
                ttInvoTxt.ITPos = 6  OR
                Invoice.InvType = 3  OR
                Invoice.InvType = 4
             THEN FILL(" ",7)
             ELSE FILL(" ",4))
            ENTRY(liCount,ttInvoTxt.ITTxt,CHR(9))
            SKIP.
         
         rl = rl + 1.       
      END.                         

      IF iiPosition = 1 OR iiPosition = 3 THEN DO:
         fChgPage(0).
         PUT STREAM tul UNFORMATTED SKIP(1).
         ASSIGN rl = rl + 1.
      END.

      /* delete printed texts, this way row specific texts won't be repeated */
      DELETE ttInvoTxt.
   END.
   
   RETURN TRUE. 
   
END FUNCTION.


ASSIGN 
   i-date1  = pvm
   i-date2  = pvm.

cfc = "sel". RUN Syst/ufcolor.p. ccc = cfc.
view FRAME taka. PAUSE 0 no-message.

ehto = 9. RUN Syst/ufkey.p.
ASSIGN lano1 = 000000 lano2 = 99999999
       asno1  = 0 asno2 = 99999999.

view FRAME rajat. view FRAME statu.

disp "NONE" @ CustGroup.CGName WITH FRAME rajat.

IF si-recid2 NE ? AND si-recid2 NE 0 THEN DO:
   FIND Invoice WHERE RECID(Invoice) = si-recid2 NO-LOCK NO-ERROR.

   FIND FIRST Customer no-lock where
              Customer.CustNum = Invoice.CustNum.
   ASSIGN
      lano1   = Invoice.InvNum
      lano2   = lano1
      InvGroup = Customer.InvGroup
      i-date1 = Invoice.InvDate
      i-date2 = Invoice.InvDate
      asno1   = Customer.CustNum
      asno2   = Customer.CustNum
      status1 = Invoice.PrintState
      status2 = Invoice.PrintState
      si-recid2 = ?.

   DISP 
      lano1   lano2
      InvGroup 
      i-date1 i-date2
      asno1 asno2 
      status1 status2
      UpdState
   WITH FRAME rajat.

END.   

PAUSE 0 no-message.
LOOP:
repeat:
   /* KysellAAn rajaukset */
   ehto = 9. RUN Syst/ufkey.p.
   PAUSE 0 no-message.
   UPDATE
      lano1    lano2
      InvGroup
      asno1    asno2
      cg-code
      i-date1  i-date2
      status1  status2
      UpdState
      rap
   WITH FRAME rajat EDITING:
      READKEY. nap = keylabel(LASTKEY).
      IF lookup(nap,poisnap) > 0 THEN DO:
         PAUSE 0.
         if frame-field = "lano2" THEN DO:
            ASSIGN INPUT lano1 INPUT lano2.
            IF INPUT lano2 = 0 THEN DO:
               lano2 = INPUT lano1.
               DISP lano1 @ lano2 WITH FRAME rajat.
               ASSIGN lano2.
            END.

            ELSE IF INPUT lano1 > INPUT lano2 THEN DO:
               lano1 = INPUT lano2.
               lano2 = INPUT lano1.
               DISP lano1 lano2
               WITH FRAME rajat.
               ASSIGN INPUT lano1 INPUT lano2.
            END.

            IF INPUT lano1 = INPUT lano2 THEN DO:

               FIND FIRST Invoice no-lock where
                          Invoice.InvNum  = INPUT lano1.
               FIND FIRST Customer no-lock where
                          Customer.CustNum = Invoice.CustNum.
               ASSIGN
                  InvGroup = Customer.InvGroup
                  i-date1 = Invoice.InvDate
                  i-date2 = Invoice.InvDate
                  asno1   = Customer.CustNum
                  asno2   = Customer.CustNum
                  status1 = Invoice.PrintState
                  status2 = Invoice.PrintState.
               DISP 
                  InvGroup 
                  i-date1 i-date2
                  asno1 asno2 
                  status1 status2
                  UpdState
               WITH FRAME rajat.

            END.
         END.

         else if frame-field = "InvGroup" THEN DO:
             ASSIGN INPUT InvGroup.
             if InvGroup ne "" AND NOT
                can-find(invgroup where 
                         InvGroup.Brand    = gcBrand AND
                         invgroup.InvGroup = InvGroup) THEN DO:
                BELL.
                message "UNKNOWN INVOICEGROUP !".
                NEXT-PROMPT InvGroup.
                NEXT.
             END.
         END.


         else if frame-field = "cg-code" THEN DO WITH FRAME rajat:
             ASSIGN INPUT FRAME rajat cg-code.
             if cg-code = "" then disp "NONE" @ CustGroup.CGName.
             ELSE DO:
                FIND CustGroup where 
                     CustGroup.Brand     = gcBrand AND
                     CustGroup.custGroup = cg-code 
                no-lock no-error.
                IF NOT AVAIL CustGroup THEN DO:
                   BELL.
                   message "UNKNOWN EXTERNAL CUSTOMER GROUP !".
                   NEXT.
                END.
                DISP CustGroup.CGName.
             END.   
         END.


         if frame-field = "asno2" THEN DO:
            ASSIGN INPUT asno1 INPUT asno2.
            IF INPUT asno2 = 0 THEN DO:
               asno2 = INPUT asno1.
               DISP asno1 @ asno2 WITH FRAME rajat.
               ASSIGN asno2.
            END.

            IF INPUT asno1 > INPUT asno2 THEN DO:
               asno1 = INPUT asno2.
               asno2 = INPUT asno1.
               DISP asno1 asno2
               WITH FRAME rajat.
               ASSIGN INPUT asno1 INPUT asno2.
            END.

         END.
         if frame-field = "status2" THEN DO:
            IF INPUT status1 > INPUT status2 THEN DO:
               status1 = INPUT status2.
               status2 = INPUT status1.
               DISP status1 status2
               WITH FRAME rajat.
               ASSIGN INPUT status1 INPUT status2.
            END.
         END.

      END.
      APPLY LASTKEY.
   END.

   toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:
      ASSIGN
      ufk = 0 ufk[1] = 132 ufk[4] = 0 /* 797*/ ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.p.

      IF toimi = 1 THEN NEXT LOOP.

      ELSE IF toimi = 5 THEN DO:
         IF INPUT asno2  = 0  THEN lano2 = 99999999.
         if input asno2   = "" THEN asno2  = 9999999.
         llok = FALSE.
         /* are Customer in the ext group in correct i-group also ? */
         if cg-code ne "" THEN DO:
            FOR EACH  cgmember no-lock where 
                      cgMember.Brand     = gcBrand AND
                      cgmember.custgroup = cg-code,
                FIRST Customer no-lock where
                      Customer.CustNum  = cgmember.custnum AND
                      Customer.InvGroup = InvGroup:
                llok = TRUE.
                LEAVE.
            END.
            IF NOT llok THEN DO:
               PAUSE 0.
               DISP                                         skip(1)
               " All Customer in the selected external "   SKIP
               " CUSTOMER  Group belong wrong "             SKIP
               " INVOICING Group !"                         skip(1)
               " Change either of those 2 group codes and " SKIP
               " try again."                                skip(1)
               " PRESS NOW ENTER TO CONTINUE"               skip(1)
               WITH centered OVERLAY ROW 8 FRAME mm  TITLE
               " INVOICING / CUSTOMER GROUP MISMATCH ! ".
               PAUSE no-message.
               NEXT TOIMI.
            END.

         END.

         LEAVE toimi.
      END. /* toimi = 5 */

      ELSE IF toimi = 8 THEN LEAVE LOOP.
   END. /* toimi */


tila = TRUE.
{Syst/utuloste.i "return"}

las_teh = "".

/* if printed to file -> no macros or effects */
IF oso = "" THEN DO: 

   /* copy macros TO printer */
   RUN Syst/umakro.p(lcMacros).

   /* macro nbr is in it's name */
   IF lcMacros > "" THEN lcMacroEff = SUBSTRING(lcMacros,LENGTH(lcMacros),1).

   IF lcMacroEff > "" THEN DO:
      FIND FIRST PrintCodes NO-LOCK WHERE 
                 PrintCodes.PrinterId = TMSPrinter AND
                 PrintCodes.Effect    = lcMacroEff NO-ERROR.

      IF AVAILABLE PrintCodes THEN las_teh = PrintCodes.EffOn[2].
   END.    
END.
ELSE ASSIGN lcSFontOn  = ""
            lcSFontOff = ""
            lcMFontOn  = ""
            lcMFontOff = ""
            lcBFontOn  = ""
            lcBFontOff = "".

MESSAGE "Printing, please wait !".
uusia = FALSE.

print-line:
repeat:

   /* don't check delivery type here, allow printing of all invoices */
   FOR EACH  Invoice  where                 
             Invoice.Brand       = gcBrand AND
             Invoice.InvNum     >= lano1   AND
             Invoice.InvNum     <= lano2   AND
             Invoice.InvDate    >= i-date1 AND
             Invoice.InvDate    <= i-date2 AND
             Invoice.CustNum    >= asno1   AND
             Invoice.CustNum    <= asno2   AND
             Invoice.PrintState    >= status1 AND
             Invoice.PrintState    <= status2 AND
             Invoice.InvCfg[1] = FALSE,
       FIRST Customer of Invoice no-lock where
             (if InvGroup ne "" 
              THEN Customer.InvGroup = InvGroup 
              ELSE TRUE)                AND

             /* is an ext cust group selected ? */
             (if cg-code ne "" THEN 
                 can-find(cgmember where 
                          cgmember.custnum   = Customer.CustNum and 
                          cgmember.custgroup = cg-code) ELSE TRUE),

       FIRST invgroup of Customer no-lock
   BREAK
   BY Invoice.CustNum
   BY Invoice.InvNum:

      /* onko kjA pyytAnyt keskeytystA ? */
      READKEY PAUSE 0.
      nap = keylabel(LASTKEY).
      if nap = "END" THEN DO:
         message "Are You sure You want to cancel printing (Y/N) ?"
         UPDATE ke.
         IF ke THEN DO:
            display stream tul "Printout cancelled !" WITH NO-LABEL no-box.
            rl = rl + 1.
            DO i = rl TO spit1:
               PUT STREAM tul skip(1).
            END.
            kesk = TRUE.
            LEAVE print-line.
         END.
      END.

      ASSIGN
         lasnro   = Invoice.CustNum
         lasnro2  = lasnro
         apu      = Customer.RepCodes
         paivays  = string(Invoice.InvDate,"99.99.9999"). 

      /* get customer names, payment terms etc. */
      fSetHeaders(0).
      
      ASSIGN lcEPLRName    = lcCustName
             lcEPLRCoName  = lcCoName
             lcEPLRAddr    = lcAddress
             lcEPLRZipCode = ENTRY(1,lcPost," ")
             lcEPLRPost    = lcPost
             lcEPLACountry = lcCountry
             lcEPLRCountry = ""
             lcEPLRLast    = ""
             lcEPLRFirst   = "".

      IF LOOKUP(lcEPLACountry,",FI,FIN,FINLAND") = 0 THEN DO:
         FIND Country WHERE Country.Country = lcEPLACountry NO-LOCK NO-ERROR.
         IF AVAILABLE Country THEN lcEPLRCountry = Country.COName.
      END.

      lcNextDue = "".
   
      IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN DO:

         IF ldtNextDueDate NE ? THEN DO:
            lcInvHeader[7] = fSeparateInvoTxt(lcInvHeader[7],20).
      
            DO liCount = 1 TO 2:
            
               lcNextDue[liCount] =
                  STRING(ENTRY(liCount,lcInvHeader[7],CHR(9)),"X(20)") +
                  IF liCount = NUM-ENTRIES(lcInvHeader[7],CHR(9)) 
                  THEN STRING(ldtNextDueDate,"99.99.9999")
                  ELSE "".
            END. 
         END.
      END.
      
      PAUSE 0.
      DISP 
      Invoice.CustNum      column-label "Customer"
      Invoice.InvNum       column-label "Invoice"
      Invoice.CustName     column-label "Name"
      Invoice.InvAmt    column-label "Amount"
      apu                 column-label "Rep"  format "x(8)" when rap
      with overlay centered row 2 13 down title " Printing ... "
      FRAME LOG. 
      DOWN WITH FRAME LOG.


      IF Invoice.PrintState = 0 THEN uusia = TRUE.

      ASSIGN
         epvm      = Invoice.DueDate
         epaiva    = string(epvm,"99.99.9999").

     
      ASSIGN laskuno  = Invoice.InvNum
             claskuno = string(Invoice.InvNum)
             sl       = 0.

      /* all sorts of texts that should be printed */
      fGetInvoiceTxt().

      /* separate cover sheet */
      fPrintInvoTxt(-1,
                    "",
                    "").
          
      IF oso = "" THEN RUN Syst/ucurpos.p(30,0).

      fChgPage(999).
      
      fSortInvRows().

      /* print out invoice texts with position 1 */
      fPrintInvoTxt(1,
                    "",
                    ""). 

      IF Invoice.InvType NE 3 AND Invoice.InvType NE 4  THEN 
      FOR EACH ttLine
      BY ttLine.Order:
        
           fChgPage(0).

           DO liCount = 1 TO NUM-ENTRIES(ttLine.RowID): 
              /* print row specific invoice texts with position 5 */
              fPrintInvoTxt(5,
                            "",
                            ENTRY(liCount,ttLine.RowID)).
           END.

           /* bold */
           IF ttLine.RowFont = "B" AND lcBFontOn > "" THEN 
           PUT STREAM tul CONTROL lcBFontOn.
                          
           PUT STREAM tul UNFORMATTED
               STRING(ttLine.RowName,"x(45)")    AT 5
               SPACE(1)
               (IF ttLine.FromDate = ?
                THEN FILL(" ",8)
                ELSE STRING(ttLine.FromDate,"99.99.99"))
               (IF ttLine.FromDate = ?
                THEN FILL(" ",3)
                ELSE " - ")
               (IF ttLine.ToDate = ?
                THEN FILL(" ",8)
                ELSE STRING(ttLine.ToDate,"99.99.99"))
               SPACE(1)
               (IF ttLine.VatPerc >= 0
                THEN STRING(ttLine.VatPerc,">>9.9")
                ELSE FILL(" ",5))
               FILL(" ",2) 
               STRING(ttLine.Amount,"->>>,>>9.999")
               SKIP.

           rl = rl + 1.

           /* bold off */
           IF ttLine.RowFont = "B" AND lcBFontOff > "" THEN 
           PUT STREAM tul CONTROL lcBFontOff.
 
           DO liCount = 1 TO NUM-ENTRIES(ttLine.Memo,"¤"):

              lcText = ENTRY(liCount,ttLine.Memo,"¤").
              
              IF lcText = "" THEN NEXT.
              
              fChgPage(0).
              
              IF lcText BEGINS "#" OR lcText = "."
              THEN PUT STREAM tul SKIP.
              ELSE PUT STREAM tul UNFORMATTED
                  lcText AT 5 SKIP.
              rl = rl + 1.
           END.

           DO liCount = 1 TO NUM-ENTRIES(ttLine.RowID): 
              /* print row specific invoice texts with position 6 */
              fPrintInvoTxt(6,
                            "",
                            ENTRY(liCount,ttLine.RowID)).
           END.
                          
      END. /* FOR EACH InvRow of Invoice */

      PUT STREAM tul SKIP(1).
      ASSIGN rl = rl + 1. 

      liCount = 0.
      FOR EACH ttVat:
         liCount = liCount + 1.
      END.
      
       /* totals by vat percent */
      IF Invoice.VatUsage < 3 AND Invoice.InvType NE 3 AND 
         Invoice.InvType NE 4 THEN 
      FOR EACH ttVAT:
           
         /* basis for vat */
         IF ttVAT.VatPerc > 0 THEN DO:
            ASSIGN ttVAT.SlsAmt = 0
                   ttVAT.VATAmt = 0.
            DO i = 1 TO 10:
               IF Invoice.VatPercent[i] = ttVat.VatPerc
               THEN ASSIGN ttVAT.SlsAmt = ttVAT.SlsAmt + Invoice.VATBasis[i] -
                                          (IF Invoice.VATIncl
                                           THEN Invoice.VatAmount[i]
                                           ELSE 0)
                           ttVat.VatAmt = ttVAT.VatAmt + Invoice.VatAmount[i].
            END.                           
         END.
 
         fChgPage(liCount - 1).
           
         PUT STREAM tul UNFORMATTED
            ttVat.Text1 AT 5
            ": "
            STRING(ttVAT.SlsAmt,"->>>>>9.999")
            FILL(" ",3)
            ttVat.Text2 
            " "
            STRING(ttVAT.VatPerc,">9")
            "%".             
            
         /* vat amount */           
         IF ttVAT.VatPerc NE 0 THEN 
         PUT STREAM tul UNFORMATTED
            ": "
            STRING(ttVat.VatAmt,"->>>>9.999").
          
         PUT STREAM tul UNFORMATTED
            " " 
            ttVat.Text3.
            
         PUT STREAM tul SKIP.
              
         ASSIGN rl = rl + 1.
      END.
  
      ELSE IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN DO:
         lcVatUsage = fVatUsageTitle(Invoice.VatUsage).
           
         IF lcVatUsage > "" THEN DO:
           
            fChgPage(0).
              
            PUT STREAM tul UNFORMATTED
               lcVatUsage AT 5 
               SKIP.
                 
            rl = rl + 1.
         END.        
      END.
  
      /* print out invoice texts with position 2 */
      fPrintInvoTxt(2,
                    "",
                    "").

      IF rl < alinrivi THEN 
      DO i = rl TO alinrivi - 1:
         PUT STREAM tul skip(1).
         rl = rl + 1.
      END.

      /* text before bank transfer */
      IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 AND lcBoxText > "" 
      THEN DO:
          IF lcSFontOn > ""
          THEN PUT STREAM tul CONTROL lcSFontOn.
          
          PUT STREAM tul UNFORMATTED
             lcBoxText SKIP.
          IF lcMFontOn > "" 
          THEN PUT STREAM tul CONTROL lcMFontOn.
      END.
      ELSE PUT STREAM tul SKIP(1).

      /* tilisiirto-osa */

        
      PUT STREAM tul UNFORMATTED
      
      SKIP(7)                          
      
      DDText[1] AT 50 FORMAT "x(42)"     
      SKIP
      
      DDText[2] AT 50 FORMAT "x(42)"      
      SKIP

      DDText[3] AT 50 FORMAT "x(42)"      
      SKIP
      
      lcCustName  AT 9 FORMAT "X(35)" SKIP
      lcAddress   AT 9 FORMAT "X(35)" SKIP       
      lcPost      AT 9 FORMAT "X(35)" SKIP(4)

      fViite(lcRefNum) AT 55 FORMAT "X(30)" SKIP(1)

      (IF DDCustomer 
       THEN DDBAccBT         /* Customer's bank account (DD) */
       ELSE "")     AT 9 FORMAT "X(31)" 
     
      (IF FALSE /* DDCustomer */
       THEN FILL("*",10)
       ELSE STRING(Invoice.DueDate,"99.99.9999")) AT 55 FORMAT "X(10)"

      (IF DDCustomer 
       THEN "********"                             
       ELSE STRING(Invoice.InvAmt,"->>>,>>9.99")) AT 73 FORMAT "X(12)" 
      SKIP.   
      
      PUT STREAM tul UNFORMATTED chr(12).

      sl = 0.

      IF rap AND Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN DO:

         /* vas-report */
         IF CAN-FIND(FIRST InvRow OF Invoice WHERE InvRow.RowType = 8)
         THEN RUN vasrep.p (Invoice.InvNum, FALSE).
            
         /* contracts */
         IF CAN-FIND(FIRST InvRow OF Invoice WHERE
                           LOOKUP(STRING(InvRow.RowType),"3,4,7") > 0)
         THEN RUN Mc/nncore1.p (Invoice.InvNum, FALSE).
         
         fPrintSpecRep(Customer.RepCodes,
                       "",
                       2,
                       0).
        
         fGetCLIs("").
         
         /* specifications to cli level */
         FOR EACH wCLI WHERE 
                  wCLI.OwnerID > 0,
         FIRST MSOwner NO-LOCK WHERE
               RECID(MsOwner) = wCLI.OwnerID:
               
            lcRepCode = STRING(fCallSpecReport(MsOwner.MsSeq)).
            IF lcRepCode = "" THEN NEXT.
         
            IF INDEX(lcRepCode,"3") > 0 OR
               INDEX(lcRepCode,"4") > 0
            THEN DO:
         
               fPrintSpecRep(lcRepCode,
                             MsOwner.CLI,
                             2,
                             0).
            END. 
         END.
      
      END. /* jos halutaan raportteja nyt */

      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).

      IF UpdState THEN DO:

         CREATE ttInv.
         ASSIGN ttInv.InvNum    = Invoice.InvNum
                ttInv.PrevState = Invoice.PrintState.

         /* NOT SENT TO print house file before */
         IF      Invoice.PrintState = 0 THEN Invoice.PrintState = 2.
         /* SENT TO print house file before */
         ELSE IF Invoice.PrintState = 1 THEN Invoice.PrintState = 3.
         
         /* show in web */
         Invoice.WInvDisp = TRUE.
         
      END.
      
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).

      /* log from print */
      DO FOR ITSendLog:
         CREATE ITSendLog.
         ASSIGN ITSendLog.Brand      = gcBrand 
                ITSendLog.TxtType    = 3
                ITSendLog.ITNum      = 0
                ITSendLog.CustNum    = Invoice.CustNum
                ITSendLog.InvNum     = Invoice.InvNum
                ITSendLog.SendMethod = 4
                ITSendLog.EMail      = ""
                ITSendLog.RepType    = "Inv"
                ITSendLog.UserCode   = katun.
                ITSendLog.SendStamp  = fMakeTS().
      END.
 
   END. /* FOR EACH Invoice */
   
   LEAVE print-line.  
   
END. /* print-line  */

ASSIGN tila = FALSE.
{Syst/utuloste.i}

IF kesk AND UpdState THEN DO:
   /* jos print-rivi keskeytettiin, nollataan tulostettujen 
      laskujen tilakoodit */

   FOR EACH ttInv,
      FIRST Invoice EXCLUSIVE-LOCK WHERE
            Invoice.InvNum = ttInv.InvNum:

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).
       ASSIGN Invoice.PrintState = ttInv.PrevState.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).
   END.
END.
ELSE IF uusia THEN DO:
   /* Kysytaan hyvaksynta */
   MESSAGE "Mark invoices as printed (Y/N)?" UPDATE ke.
   IF NOT ke THEN DO:
      HIDE MESSAGE NO-pause.

      FOR EACH ttInv,
         FIRST Invoice EXCLUSIVE-LOCK WHERE
               Invoice.InvNum = ttInv.InvNum:
       
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).
         ASSIGN Invoice.PrintState = ttInv.PrevState.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).
      END.
   END.
   LEAVE.
END.
LEAVE. /* toist. talla tavalla */

END.  /* LOOP */

HIDE MESSAGE NO-pause.
HIDE FRAME pmfile NO-pause.
HIDE FRAME rajat  NO-pause.
HIDE FRAME statu  NO-pause.
HIDE FRAME taka   NO-pause.


