/* --------------------------------------------------
  MODULE .......: NNOPAMT.P
  FUNCTION .....: Calculate calls per CCN / CGR
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 15.02.99 kl
  MODIFIED .....: 17.09.02/jr added help texts
  Version ......: M15
------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}
{Func/date.i}

DEF TEMP-TABLE calls
   FIELD io         AS lo
   FIELD cn-name    AS c
   FIELD cgr        AS c
   FIELD Qty        AS i
   FIELD pksec      AS i
   FIELD opsec      AS i
   FIELD Amt        AS DE
   FIELD mob        AS lo

   INDEX io    
      io
      cgr.

def var date1     as da no-undo format "99-99-99".
def var date2     as da no-undo format "99-99-99".
DEF VAR cday1     AS c  NO-UNDO.
DEF VAR cday2     AS c  NO-UNDO.
DEF VAR cval      AS c  NO-UNDO.
def var fname     as c  no-undo format "x(35)".
def var ok        as lo no-undo format "Yes/No".
DEF VAR io        AS lo NO-UNDO.
DEF VAR MobPref   AS c  NO-UNDO.
DEF VAR line      AS c  NO-UNDO.
DEF VAR i         AS i  NO-UNDO.
DEF VAR bin       AS lo NO-UNDO.
DEF VAR bout      AS lo NO-UNDO.
DEF VAR Operator   LIKE Operator.Operator NO-UNDO.

DEF STREAM mob.

form
   skip(1)
   "INSTRUCTION:  This module creates a tab separated ascii File of"
   "              all calls from - to selected operator during time"
   "              Period determined below. "                           skip(4)
   "              Operator ....:" Operator help "Operator code" 
   Operator.OperName                                                   SKIP
   "              Date Period .:" 
   date1 HELP "Beginning date" "-" 
   date2 HELP "Ending date" skip(1)
   "              FileName ....:" 
   fname HELP "Filename of printout"  skip(5)
with centered width 80 no-label title " Operator traffic " FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = UserCode.
   fname = TMSUser.RepDir + "/opamt.txt".
END.

ASSIGN   
   date2 = date(month(TODAY),1,year(TODAY)) - 1
   date1 = date(month(date2),1,year(date2)).

CRIT:
repeat WITH FRAME frm ON ENDKEY UNDO, RETURN:

   HIDE MESSAGE no-pause.

   ehto = 9. RUN Syst/ufkey.p.
   UPDATE 
      Operator
      date1     
      date2 
      fname 
   WITH FRAME frm EDITING.
      READKEY. nap = keylabel(LASTKEY).
      IF lookup(nap,poisnap) > 0 THEN DO:
         if frame-field = "Operator" THEN DO:
            ASSIGN INPUT Operator.
            if Operator = "" THEN LEAVE CRIT.
            FIND FIRST Operator where
                       Operator.Operator = Operator
            no-lock no-error.
            IF NOT AVAIL Operator THEN DO:
               BELL.
               message "Operator does not exists ! -hit ENTER- ".
               PAUSE no-message.
               NEXT.
            END.
            ELSE DISP Operator.OperName WITH FRAME frm.
         END.
         IF frame-field = "date2" and input date2 < input date1 THEN DO:
            BELL.
            MESSAGE "check order !".
            next-prompt date1.
            next.
         END.
      END.
      APPLY LASTKEY.
   END.

task:
   repeat WITH FRAME frm ON ENDKEY UNDO, RETURN:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.p.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are you SURE you want to start processing (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE task.
      END.
   END.

   message "Browsing call data ...".

   FOR EACH FixCDR no-lock where
            FixCDR.Date >= date1 AND
            FixCDR.Date <= date2.
      ASSIGN
         bin  = (FixCDR.OperIn  = Operator)
         bout = (FixCDR.OperOut = Operator).

      IF NOT bin AND NOT bout THEN NEXT.

      /* incoming cgr is operators */
      IF bin THEN DO:
         FIND FIRST calls where
                    calls.io  = TRUE              AND
                    calls.cgr = FixCDR.TrunkIn
         no-error.
         IF NOT AVAIL calls THEN DO:
            CREATE calls.
            ASSIGN
               calls.io  = TRUE
               calls.cgr = FixCDR.TrunkIn.
         END.
         ASSIGN 
            calls.Qty   = calls.Qty   + 1
            calls.pksec = calls.pksec + FixCDR.PKDuration
            calls.opsec = calls.opsec + FixCDR.OPDuration
            calls.Qty   = calls.Qty   + FixCDR.GrossPrice - FixCDR.DiscValue.
      END. /* bin */

      /* outgoing cgr is operators */
      IF bout THEN DO:
         FIND FIRST calls where
                    calls.io  = FALSE          AND
                    calls.cgr = FixCDR.TrunkOut
         no-error.
         IF NOT AVAIL calls THEN DO:
            CREATE calls.
            ASSIGN
               calls.io  = FALSE
               calls.cgr = FixCDR.TrunkOut.
         END.
         ASSIGN 
            calls.Qty   = calls.Qty   + 1
            calls.pksec = calls.pksec + FixCDR.PKDuration
            calls.opsec = calls.opsec + FixCDR.OPDuration
            calls.Qty   = calls.Qty   + FixCDR.GrossPrice - FixCDR.DiscValue.
      END. /* bout */

    END.

   LEAVE CRIT.

/* commented out because code is unreachable */
/*

OUTPUT STREAM excel TO value(fname).

ASSIGN
   cday1 = fDateFmt(date1,"yyyy-mm-dd") 
   cday2 = fDateFmt(date1,"yyyy-mm-dd").

PUT STREAM excel UNFORMATTED
  "Operator " + Operator + " - " + Operator.OperName + " calls between " 
   + cday1 + " - "  + cday2 my-nl
   "IN-OUT"                 tab
   "CGR"                    tab
   "#Calls"                 tab
   "#PeakMin"               tab
   "#OffPeakMin"            tab
   "Value"                  my-nl.

FOR EACH calls
BREAK 
   BY calls.io
   BY calls.cgr.

   ASSIGN
      cval = string(calls.Qty,">>>>>>>9.99")
      substr(cval,9,1) = ",".

   PUT STREAM excel UNFORMATTED
      calls.io format "In/Out"    tab
      calls.cgr                   tab
      calls.Qty                   tab
      integer(calls.pksec / 60)   tab
      integer(calls.opsec / 60)   tab
      cval                        my-nl.

END.

OUTPUT STREAM excel CLOSE.

MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.
*/
END. /* CRIT */
