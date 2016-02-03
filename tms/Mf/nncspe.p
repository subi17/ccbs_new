/* --------------------------------------------------------------------
  MODULE .......: NNCSPE.P
  TASK .........: Make an Excel table of calls sorted BY salesman/product
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 15.10.1997
  CHANGED ......: 07.11.1997 pt get default OUTPUT dir from user File
                  22.01.1998 kl ohti into Salesman
                  29.01.1998 kl as-myyja => Salesman
                  28.05.2002 aam BUFFER myyja removed 
  Version ......: M15
  ------------------------------------------------------------------ */

{Syst/commali.i}

DEF NEW shared STREAM excel.

def var exdate1  as da no-undo format "99-99-99".
def var exdate2  as da no-undo format "99-99-99".
DEF VAR tab      AS c  NO-UNDO.
def var ok       as lo no-undo format "Yes/No".
DEF VAR netto    AS DE NO-UNDO.
DEF VAR i        AS i  NO-UNDO.
DEF VAR hdr      AS c  NO-UNDO.
DEF VAR exdir    AS c  NO-UNDO.
DEF VAR exPaymFile   AS c  NO-UNDO.
DEF VAR SmName  AS c  NO-UNDO.

/* get default direcory name FOR OUTPUT */
DO FOR TMSUser:
   FIND TMSUser where TMSUser.UserCode = katun no-lock.
   ASSIGN exdir = TMSUser.RepDir.
END.

form
   skip(1)
"  Instruction:  This program prints out a TAB-separated ASCII-file with all"
"                calls during the period defined below. calls are sorted"
"                by Salesman code and by BillCode code." skip(1)
"                File's name is  'smprod.txt' and it shall be placed into the"
"                directory definned below." skip(1)

"                calls during time ....:" exdate1
help "Earliest call date"
"-" exdate2 format "99-99-99" help "Latest call date"  skip
"                Directory for output .:" exdir format "x(30)" skip(7)
WITH
   width 80 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " SUMMARY calls SMAN/PROD " + string(pvm,"99-99-99") + " "
   NO-LABELS FRAME rajat.


cfc = "sel". RUN ufcolor.

ASSIGN
exdate2 = date(month(TODAY),1,year(TODAY)) - 1
exdate1 = date(month(exdate2),1,year(exdate2))
exPaymFile  = exdir + "/" + "smprod.txt"
tab     = chr(9)
hdr     = "Sman,Name,ProdCode,ProdName,AmtCalls,AmtMinPeak," +
          "AmtMinOffpeak,TotBilled".

rajat:
repeat WITH FRAME rajat:

   ehto = 9. RUN ufkey.
   UPDATE
   exdate1
   exdate2 validate(input exdate2 >= input exdate1,"Invalid order !")
   exdir.

toimi:
   repeat WITH fram rajat:
      ASSIGN ufk = 0 ufk[1] = 15 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN ufkey.

      IF toimi = 1 THEN NEXT rajat.
      IF toimi = 8 THEN LEAVE rajat.
      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Do You REALLY want to start printing (Y/N) ?"
         UPDATE ok.
         IF ok THEN LEAVE toimi.
      END.
   END.

   message "Sorting ...".                 
   if substr(exdir,length(exdir),1) = "/" THEN
      exdir = substr(exdir,1,length(exdir) - 1).
   exPaymFile = exdir + "/smprod.txt".
   OUTPUT STREAM excel TO value(exPaymFile).

   PUT STREAM excel UNFORMATTED ynimi.
   RUN uexskip(2).
   PUT STREAM excel UNFORMATTED
    "All calls by salesman/product during "
    string(exdate1,"99.99.9999") " - " string(exdate2,"99.99.9999").
   RUN uexskip(2).
   DO i = 1 TO num-entries(hdr).
      PUT STREAM excel UNFORMATTED entry(i,hdr) tab.
   END.
   RUN uexskip(2).
   i = 0.
   FOR

       EACH FixCDR no-lock where
            FixCDR.Date >= exdate1    AND
            FixCDR.Date <= exdate2,
       FIRST Customer no-lock where
             Customer.CustNum = FixCDR.InvCust

   BREAK
   BY Customer.Salesman
   BY FixCDR.BillCode:
      i = i + 1.
      IF i = 1 THEN DO:
         HIDE MESSAGE no-pause.
         message "Printing in progress ...".
      END.
      PUT SCREEN ROW 20 col 70 string(i).
      netto = FixCDR.GrossPrice - FixCDR.DiscValue.

      accumulate
      FixCDR.Date   (sub-count BY FixCDR.BillCode)
      netto          (sub-total BY FixCDR.BillCode)
      FixCDR.PKDuration (sub-total BY FixCDR.BillCode)
      FixCDR.OPDuration (sub-total BY FixCDR.BillCode).

      IF last-of(FixCDR.BillCode) THEN DO:

         FIND BillItem where BillItem.BillCode = FixCDR.BillCode no-lock no-error.

         IF NOT AVAIL BillItem THEN DO:
            BELL.
            message "Product" FixCDR.BillCode "IS UNKNOWN !".
         END.

         FIND Salesman where Salesman.Salesman = string(Customer.Salesman)
         no-lock no-error.

         IF AVAIL Salesman THEN SmName = Salesman.SmName.
         else SmName = "!! UNKNOWN !!".
         PUT STREAM excel UNFORMATTED
         Customer.Salesman                                           tab
         SmName                                                   tab
         FixCDR.BillCode                                             tab
         BillItem.BIName                                             tab
         (accum sub-count BY FixCDR.BillCode FixCDR.Date)           tab
         round(
         (accum sub-total BY FixCDR.BillCode FixCDR.PKDuration) / 60,0) tab
         round(
         (accum sub-total BY FixCDR.BillCode FixCDR.OPDuration) / 60,0) tab
         round(
         (accum sub-total BY FixCDR.BillCode netto), 0)              tab.

         RUN uexskip(1).

      END. 

   END.      
   OUTPUT STREAM excel CLOSE.

   MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

   LEAVE rajat.

END. /* rajat */

HIDE FRAME rajat no-pause.

