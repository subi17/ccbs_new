/* --------------------------------------------------
  MODULE .......: NNSWAMT.P
  FUNCTION .....: Calculate calls / ExCode
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 15.12.98 kl
  MODIFIED .....: 30.06.99 kl - procedures FOR daily / hourly summary
                  30.05.00 kl - ALERT-BOX AT the END
  Version ......: M15
------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}

DEF TEMP-TABLE days NO-UNDO
   FIELD dte AS DA
   FIELD hh  AS i.

DEF TEMP-TABLE sw   NO-UNDO
   FIELD sw    AS c.

DEF TEMP-TABLE calls NO-UNDO
   FIELD sw    AS c
   FIELD ident AS c
   FIELD DAY   AS DA
   FIELD hh    AS i
   FIELD Qty   AS i
   FIELD sec   AS i
   FIELD Amt   AS DE

   INDEX sw AS primary
      sw
      DAY.

DEF VAR datex AS DA NO-UNDO.
def var date1 as da no-undo format "99-99-99".
def var date2 as da no-undo format "99-99-99".
def var fname as c  no-undo format "x(35)".
def var ok    as lo no-undo format "Yes/No".
DEF VAR cval  AS c  NO-UNDO.
DEF VAR i     AS i  NO-UNDO.
def var hrly  as lo no-undo format "Yes/No".

form
   skip(1)
   "INSTRUCTION:  This module creates as tab separated ascii file of"
   "              all INVOICABLE calls per EXCHANGE / Date (/ HOUR) "
   "              during time Period determined below."                skip(3)
   "              From Date ...:" date1  HELP "Earlist date"          SKIP
   "              To Date .....:" date2  HELP "Latest date"            SKIP
   "              Hourly calls :" hrly   HELP "Hourly calls"           skip(1) 
   "              File name ...:" fname  Help "File and Path name"     skip(5)
with centered width 80 no-label title " calls per exchange " FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = katun.
   fname = TMSUser.RepDir + "/callamt.txt".
END.

ASSIGN
   date2 = TODAY
   date1 = date(month(date2),1,year(date2)).

CRIT:
repeat WITH FRAME frm:

   HIDE MESSAGE no-pause.

   ehto = 9. RUN ufkey.
   UPDATE 
      date1 
      date2 validate( input date2 >= input date1, "Check order !")
      hrly  
      fname 
   WITH FRAME frm.

task:
   repeat WITH FRAME frm ON ENDKEY UNDO, RETURN:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN ufkey.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are you SURE you want to start processing (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE task.
      END.
   END.

   message "Browsing call data ...".

   /* FOR getting also zero values */
   DO datex = date1 TO date2:
      CREATE days.
      ASSIGN days.dte = datex.
   END.

   IF hrly THEN RUN pHourlyCalls.
   ELSE         RUN pDailyCalls.

   MESSAGE 
   "Task Complete"
   VIEW-AS ALERT-BOX TITLE "INFORMATION".

   LEAVE CRIT.

END.

/* calls DAY BY DAY */
PROCEDURE pDailycalls:

   FOR EACH FixCDR no-lock where
            FixCDR.Date >= date1 AND
            FixCDR.Date <= date2.

      FIND FIRST calls where
                 calls.sw  = FixCDR.ExCode AND
                 calls.day = FixCDR.Date
      no-error.

      IF NOT AVAIL calls THEN DO:
         /* FOR getting also zero values */
         FIND FIRST sw where
                    sw.sw = FixCDR.ExCode
         no-error.
         IF NOT AVAIL sw THEN DO:
            CREATE sw.
            sw.sw = FixCDR.ExCode.
         END.
         /* actual values */
         CREATE calls.
         ASSIGN 
            calls.sw  = FixCDR.ExCode
            calls.day = FixCDR.Date.
      END.

      ASSIGN 
         calls.Qty = calls.Qty + 1
         calls.sec = calls.sec + FixCDR.Duration
         calls.Qty = calls.Qty + FixCDR.GrossPrice - FixCDR.DiscValue.

   END.

   OUTPUT STREAM excel TO value(fname).

   DO datex = date1 TO date2:
      FOR EACH sw:
         FIND FIRST calls where
                    calls.day = datex AND
                    calls.sw  = sw.sw
         no-error.
         IF NOT AVAIL calls THEN DO:
            CREATE calls.
            ASSIGN
               calls.sw  = sw.sw
               calls.day = datex
               calls.Qty = 0.
         END.
      END.
   END.

   PUT STREAM excel UNFORMATTED
      "SWITCH"   tab
      "DATE"     tab
      "#CALLS"   tab
      "#MINUTES" tab
      "VALUE"    my-nl.

   FOR EACH calls
   BY calls.sw
   BY calls.day:

      ASSIGN
         cval = string(calls.Qty,"-zzzzzz9.99")
         substr(cval,9,1) = ",".

      PUT STREAM excel UNFORMATTED
         calls.sw                       tab
         string(calls.day,"99-99-9999") tab
         calls.Qty                      tab
         integer(calls.sec / 60)        tab
         cval                           my-nl.

   END.

   OUTPUT STREAM excel CLOSE.

END.

/* calls hour BY hour */
PROCEDURE pHourlycalls:

   FOR EACH FixCDR no-lock where
            FixCDR.Date >= date1 AND
            FixCDR.Date <= date2.

      i = integer(substr(string(FixCDR.TimeStart,"hh:mm:ss"),1,2)).

      FIND FIRST calls where
                 calls.sw  = FixCDR.ExCode AND
                 calls.day = FixCDR.Date    AND
                 calls.hh  = i
      no-error.

      IF NOT AVAIL calls THEN DO:
         /* FOR getting also zero values */
         FIND FIRST sw where
                    sw.sw = FixCDR.ExCode
         no-error.
         IF NOT AVAIL sw THEN DO:
            CREATE sw.
            ASSIGN sw.sw = FixCDR.ExCode.
         END.
         /* actual values */
         FIND FIRST Exchange where
                    Exchange.ExNum = integer(FixCDR.ExCode)
         no-lock no-error.
         CREATE calls.
         ASSIGN 
            calls.sw    = FixCDR.ExCode
            calls.day   = FixCDR.Date 
            calls.hh    = i.
         IF AVAIL Exchange THEN
            calls.ident = Exchange.Ident.
      END.

      ASSIGN 
         calls.Qty = calls.Qty + 1
         calls.sec = calls.sec + FixCDR.Duration
         calls.Qty = calls.Qty + FixCDR.GrossPrice - FixCDR.DiscValue.

   END.

   OUTPUT STREAM excel TO value(fname).

   DO datex = date1 TO date2:
      FOR EACH sw:
         DO i = 0 TO 23:
            FIND FIRST calls where
                       calls.day = datex AND
                       calls.sw  = sw.sw AND
                       calls.hh  = i
            no-error.
            IF NOT AVAIL calls THEN DO:
               FIND FIRST Exchange where
                          Exchange.ExNum = integer(sw.sw)
               no-lock no-error.
               CREATE calls.
               ASSIGN
                  calls.sw    = sw.sw
                  calls.ident = Exchange.Ident
                  calls.day   = datex
                  calls.hh    = i
                  calls.Qty   = 0.
            END.
         END.
      END.
   END.

   PUT STREAM excel UNFORMATTED
      "SWITCH"   tab
      "DATE"     tab
      "HOUR"     tab
      "#CALLS"   tab
      "#MINUTES" tab
      "VALUE"    my-nl.

   FOR EACH calls
   BY calls.sw
   BY calls.day
   BY calls.hh:

      ASSIGN
         cval = string(calls.Qty,"-zzzzzz9.99")
         substr(cval,9,1) = ",".

      PUT STREAM excel UNFORMATTED
         calls.ident                    tab
         string(calls.day,"99-99-9999") tab
         calls.hh format "99"           tab
         calls.Qty                      tab
         integer(calls.sec / 60)        tab
         cval                           my-nl.

   END.

   OUTPUT STREAM excel CLOSE.
   HIDE MESSAGE no-pause.

   MESSAGE
      "File " + fname + " is now ready !"
   VIEW-AS ALERT-BOX MESSAGE.

END PROCEDURE.


