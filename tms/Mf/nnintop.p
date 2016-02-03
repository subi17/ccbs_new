/* --------------------------------------------------
  MODULE .......: NNINTOP.P
  FUNCTION .....: Calculate Calls per CCN / CGR
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 15.02.99 kl
  MODIFIED .....: 
  Version ......: M15
------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}
{Func/date.i}

DEF TEMP-TABLE Calls
   FIELD CCN        AS i
   FIELD cn-name    AS c
   FIELD ExCode     AS c
   FIELD cgr        AS c
   FIELD TrName   AS c
   FIELD OperName    AS c
   FIELD Qty        AS i
   FIELD pksec      AS i
   FIELD opsec      AS i
   FIELD Amt        AS DE
   FIELD mob        AS lo
   FIELD mobop      AS c

   INDEX CCN    
      CCN
      cgr
      mob
      OperName.

def var date1     as da no-undo format "99-99-99".
def var date2     as da no-undo format "99-99-99".
DEF VAR cday1     AS c  NO-UNDO.
DEF VAR cday2     AS c  NO-UNDO.
DEF VAR cval      AS c  NO-UNDO.
def var fname     as c  no-undo format "x(35)".
def var ok        as lo no-undo format "Yes/No".
DEF VAR mob       AS lo NO-UNDO.
DEF VAR norop     AS c  NO-UNDO.
DEF VAR denop     AS c  NO-UNDO.
DEF VAR normob    AS c  NO-UNDO.
DEF VAR denmob    AS c  NO-UNDO.
DEF VAR opname    AS c  NO-UNDO.
DEF VAR line      AS c  NO-UNDO.
DEF VAR i         AS i  NO-UNDO.
DEF VAR OperName   AS c  NO-UNDO.
DEF VAR cn-name   AS c  NO-UNDO.
DEF VAR ExName   AS c  NO-UNDO.
DEF VAR Operator   LIKE Operator.Operator NO-UNDO.
DEF VAR CCN       LIKE CCN.CCN   NO-UNDO.


DEF STREAM mob.

form
   skip(1)
   "INSTRUCTION:  This module creates a tab separated ascii File of all"
   "              CUSTOMERS international Calls by CGR / CCN during time" 
   "              Period determined below. Mobile Calls are separated" SKIP
   "              into their own row."                                 skip(3)  
   "              From Date ...:" date1                                SKIP
   "              To Date .....:" date2                                SKIP
   "              Operator ....:" Operator Operator.OperName AT 39         SKIP
   "              Country .....:" CCN     CCN.CCNName  AT 39         skip(1) 
   "              File Name ...:" fname                                skip(3)
WITH centered width 80 NO-LABEL TITLE 
   " Customers International Calls per CGR - CCN " FRAME frm.

/* read in norwegian mobile prefixes */
INPUT STREAM mob from /home/nn/nor-mob.i01.
repeat.
   IMPORT STREAM mob line.
   i = index(line,tab).
   ASSIGN
      normob = normob + "," + substr(line,1,i - 1)
      norop  = norop  + "," + substr(line,i + 1).
END.
ASSIGN
   normob = substr(normob,2)
   norop  = substr(norop,2).

/* read in danish mobile prefixes */
INPUT STREAM mob from /home/nn/den-mob.i01.
repeat.
   IMPORT STREAM mob line.
   i = index(line,tab).
   ASSIGN
      denmob = denmob + "," + substr(line,1,i - 1)
      denop  = denop  + "," + substr(line,i + 1).
END.
ASSIGN
   denmob = substr(denmob,2)
   denop  = substr(denop,2).


DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = katun.
   fname = TMSUser.RepDir + "/intop.txt".
END.

ASSIGN   
   date2 = date(month(TODAY),1,year(TODAY)) - 1
   date1 = date(month(date2),1,year(date2)).


DISP 
   "ALL" @ Operator.OperName
   "ALL" @ CCN.CCNName
WITH FRAME frm.

CRIT:
repeat WITH FRAME frm ON ENDKEY UNDO, RETURN:

   HIDE MESSAGE no-pause.

   ehto = 9. RUN ufkey.
   UPDATE 
      date1     date2
      Operator
      CCN
      fname 
   WITH FRAME frm EDITING.
      READKEY. nap = keylabel(LASTKEY).
      IF lookup(nap,poisnap) > 0 THEN DO:
         if frame-field = "op-code" THEN DO:
            ASSIGN INPUT Operator.
            if Operator ne "" THEN DO:
               FIND FIRST Operator where
                          Operator.Operator = Operator
               no-lock no-error.
               IF NOT AVAIL Operator THEN DO:
                  message "Unknown operator !".
                  PAUSE no-message.
                  HIDE MESSAGE no-pause.
                  NEXT.
               END.
               ELSE DISP Operator.OperName WITH FRAME frm.
            END.
            else disp "ALL" @ Operator.OperName WITH FRAME frm.
         END.
         if frame-field = "ccn" THEN DO:
            ASSIGN INPUT CCN.
            IF CCN NE 0 THEN DO:
               FIND FIRST CCN where
                          CCN.CCN = CCN                                         no-lock no-error.
               IF NOT AVAIL CCN THEN DO:
                  message "Unknown Country number !".
                  PAUSE no-message.
                  HIDE MESSAGE no-pause.
                  NEXT.
               END.
               ELSE DISP CCN.CCNName WITH FRAME frm.
            END.
            else disp "ALL" @ CCN.CCNName WITH FRAME frm.
         END.
      END.
      APPLY LASTKEY.
   END.

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

   FOR EACH FixCDR no-lock where
            FixCDR.Date >= date1         AND
            FixCDR.Date <= date2         AND
            FixCDR.BillCode = "U".

      /* SKIP NOT wanted operators */
      if Operator ne "" THEN
         IF FixCDR.OperOut NE Operator THEN NEXT.
      /* SKIP NOT wanted countries */
      IF CCN NE 0 THEN
         IF FixCDR.CCN NE CCN THEN NEXT.

      ASSIGN
         mob    = FALSE
         opname = "".

      /* norway mobiles */
      IF FixCDR.CCN = 271 THEN   
         DO i = 1 TO num-entries(normob).
            mob = FixCDR.BSub BEGINS entry(i,normob).
            IF mob THEN ASSIGN
               opname = entry(i,norop)
               i      = 9999.
      END.
      /* danish mobiles */
      ELSE IF FixCDR.CCN = 326 THEN   
         DO i = 1 TO num-entries(denmob).
            mob = FixCDR.BSub BEGINS entry(i,denmob).
            IF mob THEN ASSIGN
               opname = entry(i,denop)
               i      = 9999.
      END.

      FIND FIRST Calls where
                 Calls.CCN    = FixCDR.CCN    AND
                 Calls.ExCode = FixCDR.ExCode AND
                 Calls.cgr    = FixCDR.TrunkOut  AND
                 Calls.mob    = mob             AND
                 Calls.mobop  = opname
      no-error.
      IF NOT AVAIL Calls THEN DO:
         FIND FIRST CCN where 
                    CCN.CCN = FixCDR.CCN 
         no-lock no-error.
         FIND FIRST Trunk where
                    Trunk.ExCode = FixCDR.ExCode AND
                    Trunk.TrunkCode = FixCDR.TrunkOut
         no-lock no-error.
         IF AVAIL Trunk THEN DO:
            cn-name = Trunk.TrunkName.
            FIND FIRST Operator where 
                       Operator.Operator = Trunk.OpCode
            no-lock no-error.
            IF AVAIL Operator THEN OperName = Operator.OperName.
            else OperName = "-UNKNOWN-".
         END.
         ELSE ASSIGN
            cn-name = "-UNKNOWN-"
            OperName = "-UNKNOWN-".
         CREATE Calls.
         ASSIGN
            Calls.CCN      = CCN.CCN
            Calls.cn-name  = CCN.CCNName
            Calls.ExCode   = FixCDR.ExCode
            Calls.cgr      = FixCDR.TrunkOut
            Calls.TrName = cn-name
            Calls.OperName  = OperName
            Calls.mob      = mob
            Calls.mobop    = opname.
      END.
      ASSIGN 
         Calls.Qty   = Calls.Qty   + 1
         Calls.pksec = Calls.pksec + FixCDR.PKDuration
         Calls.opsec = Calls.opsec + FixCDR.OPDuration
         Calls.Qty   = Calls.Qty   + FixCDR.GrossPrice - FixCDR.DiscValue.

   END.

   LEAVE CRIT.

END.

OUTPUT STREAM excel TO value(fname).

ASSIGN
   cday1 = fDateFmt(date1,"yyyy-mm-dd") 
   cday2 = fDateFmt(date2,"yyyy-mm-dd").

if Operator ne "" THEN 
   PUT STREAM excel UNFORMATTED 
      "OPERATOR" tab
      Operator + " - " + Operator.OperName.
ELSE
   PUT STREAM excel UNFORMATTED 
      "OPERATOR" tab
      "ALL".
PUT STREAM excel UNFORMATTED my-nl.

IF CCN NE 0 THEN 
   PUT STREAM excel UNFORMATTED 
      "COUNTRY" tab
      string(CCN) + " - " + CCN.CCNName.
ELSE
   PUT STREAM excel UNFORMATTED 
      "COUNTRY" tab
      "ALL".
PUT STREAM excel UNFORMATTED my-nl.

PUT STREAM excel UNFORMATTED
  "ALL CUSTOMERS International Calls Between " 
   + cday1 + " - "  + cday2 my-nl
   "Operator"               tab
   "Switch"                 tab 
   "CGR"                    tab
   "CGR Name"               tab
   "Country"                tab
   "#Calls"                 tab
   "#PeakMin"               tab
   "#OffPeakMin"            tab
   "Value"                  tab
   "Mobile"                 my-nl.

FOR EACH Calls
BREAK 
   BY Calls.OperName
   BY Calls.ExCode
   BY Calls.cgr
   BY Calls.cn-name.

   ASSIGN
      cval = string(Calls.Qty,">>>>>>>9.99")
      substr(cval,9,1) = ",".

   IF first-of(Calls.ExCode) THEN DO:
      FIND FIRST Exchange where
                 Exchange.ExCode = Calls.ExCode
      no-lock no-error.
      IF AVAIL Exchange THEN ExName = Exchange.ExName.
      else ExName = "??" + Calls.ExCode + "??".
   END.

   PUT STREAM excel UNFORMATTED
      Calls.OperName               tab
      ExName                     tab
      Calls.cgr                   tab
      Calls.TrName              tab
      Calls.cn-name               tab
      Calls.Qty                   tab
      integer(Calls.pksec / 60)   tab
      integer(Calls.opsec / 60)   tab
      cval                        tab
      Calls.mobop                 my-nl.

   IF last-of(Calls.OperName) THEN 
      PUT STREAM excel UNFORMATTED my-nl.

END.

OUTPUT STREAM excel CLOSE.



