/* --------------------------------------------------------------------
  MODULE .......: NNCDPE.P
  TASK .........: Make an Excel table of Calls sorted BY day/product
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 12.10.1997
  CHANGED ......: 07.11.1997 default OUTPUT dir from user record
                  21.09.1998 pt sort BY BillCode. group/prod, 
                                DISP BillCode. code also
                  09.10.1998 pt more info on opening screen AND File heading
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
DEF VAR exName   AS c  NO-UNDO.
DEF VAR exPaymFile   AS c  NO-UNDO.


/* get default direcory Name FOR OUTPUT */
DO FOR TMSUser:
   FIND TMSUser where TMSUser.UserCode = katun no-lock.
   ASSIGN exdir = TMSUser.Repdir.
END.

form
   skip(1)
"  Instruction:  This program writes an summary of ALL Calls (Billed or not)"
"                during the Period given below.  Summary is sorted by " skip(1)
"                 - call date"                                          skip
"                   - BillCode GROUP"                                   skip
"                     - BillCode CODE"                                  skip(1)
"                and is to be written to a TAB separated ASCII file"    skip(1)

"                Calls during .........:" exdate1
help "Earliest call date"
"-" exdate2 format "99-99-99" help "Latest call date"  skip
"                Output Directory .....:" exdir  format "x(30)"
help "Directory where summary File is to be written" SKIP
"                Output File ..........:" exName format "x(20)"
help "Name of File where summary PaymFile is to be written"
skip(4)
WITH
   width 80 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " CALL SUMMARY BY BillCode " + string(pvm,"99-99-99") + " "
   NO-LABELS FRAME rajat.


cfc = "sel". RUN Syst/ufcolor.

ASSIGN
exdate2 = date(month(TODAY),1,year(TODAY)) - 1
exdate1 = date(month(exdate2),1,year(exdate2))
exName  = "prodsum.txt"
tab     = chr(9)
hdr     = "ProdGroup,GroupName,Date,ProdCod,ProdName,AmtCalls," +
          "AmtMinPeak,AmtMinOffpeak,TotValue".

rajat:
repeat WITH FRAME rajat:

   ehto = 9. RUN Syst/ufkey.
   UPDATE
   exdate1
   exdate2 validate(input exdate2 >= input exdate1,"Impossible order !")
   exdir
   exName.

toimi:
   repeat WITH fram rajat:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.

      IF toimi = 1 THEN NEXT rajat.
      IF toimi = 8 THEN LEAVE rajat.
      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are You Sure You want to start (Y/N) ?"
         UPDATE ok.
         IF ok THEN LEAVE toimi.
      END.
   END.

   message "Collecting and sorting data, wait ...".

   exPaymFile = exdir + "/" + exName.
   OUTPUT STREAM excel TO value(exPaymFile).

   PUT STREAM excel UNFORMATTED ynimi.  RUN Syst/uexskip(2).
   PUT STREAM excel UNFORMATTED 
     "SUMMARY OF ALL Calls BY DATE/PRODUCT GROUP/PRODUCT".  RUN Syst/uexskip(2).
   PUT STREAM excel UNFORMATTED
     "Printed by" tab katun tab pvm format "99.99.9999".  RUN Syst/uexskip(2).
   DO i = 1 TO num-entries(hdr).
      PUT STREAM excel UNFORMATTED entry(i,hdr) tab.
   END.
   RUN Syst/uexskip(2).

   FOR EACH FixCDR no-lock where
            FixCDR.Date >= exdate1    AND
            FixCDR.Date <= exdate2,

       FIRST BillItem no-lock where
             BillItem.Brand    = gcBrand AND
             BillItem.BillCode = FixCDR.BillCode

   BREAK
   BY FixCDR.Date
   BY BillItem.BIGroup
   BY FixCDR.BillCode:

      netto = FixCDR.GrossPrice - FixCDR.DiscValue.

      accumulate
      FixCDR.Date   (sub-count BY FixCDR.BillCode)
      netto          (sub-total BY FixCDR.BillCode)
      FixCDR.PKDuration (sub-total BY FixCDR.BillCode)
      FixCDR.OPDuration (sub-total BY FixCDR.BillCode).

      IF last-of(FixCDR.BillCode) THEN DO:

         FIND BItemGroup where BItemGroup.BIGroup = BillItem.BIGroup 
         no-lock no-error.
         PUT STREAM excel UNFORMATTED

         BItemGroup.BIGroup                                             tab
         BItemGroup.BIGName                                             tab
         FixCDR.Date format "99.99.9999"                          tab
         FixCDR.BillCode                                             tab
         BillItem.BIName                                             tab
         (accum sub-count BY FixCDR.BillCode FixCDR.Date)           tab
         round(
         (accum sub-total BY FixCDR.BillCode FixCDR.PKDuration) / 60,0) tab
         round(
         (accum sub-total BY FixCDR.BillCode FixCDR.OPDuration) / 60,0) tab
         round(
         (accum sub-total BY FixCDR.BillCode netto), 0)              tab.

         RUN Syst/uexskip(1).
      END.
   END.
   OUTPUT STREAM excel CLOSE.

   MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

   LEAVE rajat.

END. /* rajat */

HIDE FRAME rajat no-pause.
HIDE MESSAGE.

