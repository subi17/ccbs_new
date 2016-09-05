/* -----------------------------------------------------------------
  MODULE .......: NNILEX.P
  TASK .........: creates an Excel File of invoices
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 21.01.1998 kl
  CHANGED ......: 07.11.1997
                  01.02.2000 BY kl: KID number
                  02.02.2000 BY kl: prev. fixed
                  31.03.2000 BY kl: advance payment added
                  10.07.2000 by kl: ocr2 as "0" FOR NOR
                  31.08.2000 BY pt: NEW column totcf
                  21.09.2000 by kl: ocr2 as "", instead a "0" in front
                  28.05.2002 BY kl: DEN commented

  Version ......: M15
  ------------------------------------------------------------------ */

{Syst/commali.i}

{Syst/country.i}

{Func/excel.i}

{Func/function.i}

DEF VAR exdir     AS c  NO-UNDO.
DEF VAR exName    AS c  NO-UNDO.
DEF VAR exdate1   AS DA NO-UNDO.
DEF VAR exdate2   AS DA NO-UNDO.
def var exdeci    as lo format "Period/Comma" NO-UNDO.
def var ok        as lo format "Yes/No"      NO-UNDO.
DEF VAR i         AS i  NO-UNDO.
DEF VAR InvGroup   LIKE  InvGroup.InvGroup.
DEF VAR AmtExclVAT   AS c  NO-UNDO.
DEF VAR VATAmt       AS c  NO-UNDO.
DEF VAR Interest  AS c  NO-UNDO.
DEF VAR roundi    AS c  NO-UNDO.
DEF VAR AdvPaym   AS c  NO-UNDO.
DEF VAR InvAmt  AS c  NO-UNDO.
DEF VAR head-line AS c  NO-UNDO.
DEF VAR ocr1      AS c  NO-UNDO.
DEF VAR ocr2      AS c  NO-UNDO.
DEF VAR ref       AS i  NO-UNDO.
DEF VAR ref1      AS i  NO-UNDO.
DEF VAR ref2      AS i  NO-UNDO.
DEF VAR totcf     AS DE NO-UNDO.


/* get default direcory Name FOR OUTPUT */
DO FOR TMSUser:
   FIND TMSUser where TMSUser.UserCode = katun no-lock.
   ASSIGN exdir = TMSUser.RepDir.
END.

/* initial values */
ASSIGN
   tab       = chr(9)
   exName    = exdir + "/" + "invlist.txt"
   head-line = "Inv. no"     + tab +
               "Date"        + tab +
               "KID"         + tab +
               "Cust. no"    + tab +
               "Pers. no"    + tab +
               "Cust. name"  + tab +
               "Address"     + tab +
               "Post adress" + tab +
               "Contr fees"  + tab +
               "TOT ex VAT"  + tab +
               "VAT"         + tab +
               "Adv.Paym."   + tab +
               "Interest"    + tab +
               "Rounding"    + tab +
               "TOTAL"       + tab +
               "Exp. date".

form
   skip(1)
"  Information:  This program writes a summary of all invoices "
"                that are written within the time InstDuePeriod given below."
skip(1)
"                Summary is written as a TAB-separated ASCII-file,"
"                one row / invoice."
skip(1)
"                File can be opened in Ms Excel as it is."  skip(1)
"                File's Name is" exname format "x(40)" no-label skip(2)
"                Invoices written during:" exdate1 format "99-99-99"
help "Earliest invoicing date"
"-" exdate2 format "99-99-99" help "Latest invoicing date"   skip
"                Inv.group .............:" InvGroup
help "Invoicing group's code, empty for all"  SKIP
"                Decimal separator .....:" exdeci help "Period/Comma" skip(2)
WITH
   width 80 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " EXCEL-SUMMARY OF INVOICES " +
   string(pvm,"99-99-99") + " " NO-LABELS FRAME start.

exdate2 = date(month(TODAY),1,year(TODAY)) + 32.
exdate2 = date(month(exdate2),1,year(exdate2)) - 1.
exdate1 = date(month(exdate2),1,year(exdate2)).

cfc = "sel". RUN Syst/ufcolor.p.

CRIT:
repeat WITH FRAME start:
   ehto = 9. RUN Syst/ufkey.p.
   DISP exName.
   UPDATE
      exdate1  validate(exdate1 ne ?,"Give first Date !")
      exdate2  validate(input exdate2 >= input exdate1,"Invalid order !")
      InvGroup  validate(InvGroup = "" OR can-find(InvGroup where
                                 InvGroup.InvGroup = InvGroup),
                                 "Group does not exist !")
      exdeci
   WITH FRAME start EDITING.
      READKEY.
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
         PAUSE 0.
      END.
      APPLY LASTKEY.
   END. /* EDITING */

task:
   repeat WITH FRAME start:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.p.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are you SURE you want to star processing (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE task.
      END.
   END.

   OUTPUT STREAM excel TO value(exName).

   /* file's header */
   if InvGroup ne "" THEN DO:
      FIND FIRST InvGroup where InvGroup.InvGroup = InvGroup.
      PUT STREAM excel UNFORMATTED
         "INVOICE LIST for invoice group: " InvGroup + " - " + IGName.
   END.
   ELSE PUT STREAM excel UNFORMATTED
      "INVOICE LIST for all invoice groups".
   PUT STREAM excel UNFORMATTED my-nl.

   PUT STREAM excel UNFORMATTED
      "Dates: " + string(exdate1) + " - " + string(exdate2).
   PUT STREAM excel UNFORMATTED my-nl my-nl.

   PUT STREAM excel UNFORMATTED head-line.
   PUT STREAM excel UNFORMATTED my-nl.

   message "Browsing and sorting data ...".
   FOR
      EACH  Invoice no-lock where
            Invoice.InvDate >= exdate1        AND
            Invoice.InvDate <= exdate2,
      FIRST Customer no-lock where
            Customer.CustNum = Invoice.CustNum AND
            (if InvGroup ne "" THEN Customer.InvGroup = InvGroup
            ELSE TRUE)

   BY Invoice.InvNum.

      /* W/O VATAmt */
      IF exdeci THEN 
         AmtExclVAT = string(Invoice.AmtExclVAT,"-zzzzzzz9.99").
      ELSE
         AmtExclVAT = fDecToC(Invoice.AmtExclVAT,"-zzzzzzz9.99").

      /* VATAmt */
      IF exdeci THEN
         VATAmt = string(Invoice.VATAmt,"-zzzzzz9.99").
      ELSE   
         VATAmt = fDecToC(Invoice.VATAmt,"-zzzzzz9.99").

      /* OverTime Interest */
      IF exdeci THEN 
         Interest = string(Invoice.InterestAmt,"-zzzzz9.99").
      ELSE
         Interest = fDecToC(Invoice.InterestAmt,"-zzzzz9.99").

      /* Rounding */
      IF exdeci THEN 
         roundi = string(Invoice.Rounding,"-9.99").
      ELSE
         roundi = fDecToC(Invoice.Rounding,"-9.99").

      /* Rounding */
      IF exdeci THEN 
         AdvPaym = string(Invoice.OverPaym + Invoice.AdvPaym,"-zzzzzz9.99").
      ELSE
         AdvPaym = fDecToC(Invoice.OverPaym + Invoice.AdvPaym,"-zzzzzz9.99").

      case {&country}:

         when {&SWE} OR when {&FIN} THEN DO:
            /* count the LENGTH of cusromer reference : LENGTH of inv.no. + 2).
            IF LENGTH > 10 take away number 1s */
            ref = (InvNum * 10) + ( (length(string(InvNum)) + 2) MODULO 10).

            /* calculate the LAST number of the reference */
            RUN Mf/nnrswe.p(INPUT ref, OUTPUT ref1).

            /* reference into a STRING */
            ocr1 = string(ref) + string(ref1).

            /* checksum due TO the amount of crowns */ 
            RUN Mf/nnrswe.p(Invoice.InvAmt * 100, OUTPUT ref2).
            ocr2 = string(ref2).
         END.

         when {&NOR} THEN DO:
            /* mode 10, LENGTH 20 */
            RUN nnrnor(Invoice.InvNum,10,output ocr1).
            ASSIGN
               ocr1 = string(Invoice.InvNum) + ocr1
               ocr1 = fill("0",14 - length(ocr1)) + ocr1
               ocr2 = "" 
               .
         END.
      END.

      /* aggregate ALL contract fees from invoice rows */
      totcf = 0.
      FOR EACH InvRow OF Invoice WHERE InvRow.FFRow:
         totcf = totcf + InvRow.Amt.
      END.


      /* convert sum into STRING AND substitute dec point BY comma */
      InvAmt = string(Invoice.InvAmt,"-zzzzzzz9.99").
      if exdeci = false then substr(InvAmt,10,1) = ",".

      PUT STREAM excel UNFORMATTED
        Invoice.InvNum       tab
        string(Invoice.InvDate,"99.99.9999")       tab
        ocr1 + ocr2         tab
        Invoice.CustNum      tab
        Customer.OrgId      tab
        Customer.CustName     tab
        Customer.Address      tab
        Customer.ZipCode  + " " +
        Customer.PostOffice      tab
        totcf               tab
        AmtExclVAT             tab
        VATAmt                 tab
        AdvPaym             tab
        Interest            tab
        roundi              tab
        InvAmt            tab
        string(Invoice.DueDate,"99.99.9999").

      PUT STREAM excel UNFORMATTED my-nl.

   END.

   LEAVE.

END.
HIDE FRAME start no-pause.

