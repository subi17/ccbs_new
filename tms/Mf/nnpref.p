/* --------------------------------------------------
  MODULE .......: NNPREF.P
  FUNCTION .....: Calculate prefix traffic into an excel File
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 01.02.00 kl
  MODIFIED .....: 02.02.00 kl: more details
                  20.07.01 kl: use OperIndir FOR indirect prefix
                  17.09.02/jr added help texts and fixed text 

  Version ......: M15
------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}
{Func/date.i}
{Func/tmsparam2.i}

DEF TEMP-TABLE Calls
   FIELD pref       AS c
   FIELD swid       AS c
   FIELD cgr        AS c
   FIELD Operator    AS c
   FIELD DAY        AS DA
   FIELD Qty        AS i
   FIELD pksec      AS i
   FIELD opsec      AS i

   INDEX pref AS primary
      pref
      swid
      cgr.

def var date1   as da no-undo format "99-99-99".
def var date2   as da no-undo format "99-99-99".
DEF VAR cday1   AS c  NO-UNDO.
DEF VAR cday2   AS c  NO-UNDO.
def var fname   as c  no-undo format "x(35)".
def var ok      as lo no-undo format "Yes/No".
DEF VAR amtc    AS i  NO-UNDO.
DEF VAR amtpk   AS i  NO-UNDO.
DEF VAR amtop   AS i  NO-UNDO.
DEF VAR opname  AS c  NO-UNDO.
DEF VAR cgrname AS c  NO-UNDO.
DEF VAR cgrop   AS c  NO-UNDO.
DEF VAR exName  AS c  NO-UNDO.
DEF VAR t1eprf  AS c  NO-UNDO.

t1eprf = fCParamC("Tele1Pref").

form
   skip(1)
   "INSTRUCTION:  This module creates as tab separated ascii File of"
   "              all PREFIX Calls per OPERATOR / PREFIX / CGR during"
   "              time determined below."                              skip(4)
   "              Dates .......:" 
   date1 HELP "Beginning date"   "-" 
   date2 HELP "Ending date"  skip(1)
   "              FileName ....:" 
   fname HELP "Filename of printout"  skip(6)
with centered width 80 no-label title " Prefix traffic report " FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = UserCode.
   fname = TMSUser.RepDir + "/preftraf.txt".
END.

ASSIGN   
   date2 = date(month(TODAY),1,year(TODAY)) - 1
   date1 = date(month(date2),1,year(date2)).

CRIT:
repeat WITH FRAME frm:

   HIDE MESSAGE no-pause.

   ehto = 9. RUN Syst/ufkey.
   UPDATE 
      date1 date2 validate(input date2 >= input date1, "check order !")
      fname 
   WITH FRAME frm.

task:
   repeat WITH FRAME frm ON ENDKEY UNDO, RETURN:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.
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
            FixCDR.Date <= date2 AND
            FixCDR.Prefix ne "".

      /* list Calls TO operator */
      FIND FIRST Calls where
                 Calls.pref = FixCDR.Prefix  AND
                 Calls.swid = FixCDR.ExCode  AND
                 Calls.cgr  = FixCDR.TrunkIn
      no-error.

      IF NOT AVAIL Calls THEN DO:
         CREATE Calls.
         ASSIGN 
            Calls.pref = FixCDR.Prefix
            Calls.swid = FixCDR.ExCode
            Calls.cgr  = FixCDR.TrunkIn.
      END.

      ASSIGN 
         Calls.Qty   = Calls.Qty   + 1
         Calls.pksec = Calls.pksec + FixCDR.PKDuration
         Calls.opsec = Calls.opsec + FixCDR.OPDuration.

   END.

   OUTPUT STREAM excel TO value(fname).

   ASSIGN
      cday1 = fDateFmt(date1,"yyyy-mm-dd")
      cday2 = fDateFmt(date2,"yyyy-mm-dd").

   PUT STREAM excel UNFORMATTED
      "Prefix traffic between " 
      + cday1 + " - "  + cday2 my-nl.

   PUT STREAM excel UNFORMATTED
      "Prefix operator" tab
      "Prefix"          tab
      "Switch"          tab
      "CGR"             tab
      "CGR name"        tab
      "CGR Operator"    tab
      "#Calls"          tab
      "#PeakMin"        tab
      "#OffPeakMin"     my-nl.

   FOR EACH Calls
   BREAK 
      BY Calls.pref:

      FIND FIRST OperIndir where
                 OperIndir.Prefix = Calls.pref
      no-lock no-error.

      IF AVAIL OperIndir THEN DO:
         FIND FIRST Operator where
                    Operator.Operator = OperIndir.Operator
         no-lock no-error.
         IF AVAIL Operator THEN opname = Operator.OperName.
         else opname = "-UNKNOWN-".
      END.
      ELSE DO:
         if lookup(Calls.pref,t1eprf) > 0 then opname = "Tele1Europe".
         else opname = "-UNKNOWN-".
      END.   

      FIND FIRST Exchange where
                 Exchange.ExCode = Calls.swid
      no-lock no-error.
      IF AVAIL Exchange THEN exName = Exchange.ExName.
      else exName = "-UNKNOWN-".

      FIND FIRST Trunk where
                 Trunk.ExCode = Calls.swid AND
                 Trunk.TrunkCode = Calls.cgr
      no-lock no-error.
      IF AVAIL Trunk THEN cgrname = Trunk.TrunkName.
      else cgrname = "-UNKNOWN-".

      IF AVAIL Trunk THEN DO:
         FIND FIRST Operator where
                    Operator.Operator = Trunk.OpCode
         no-lock no-error.
         IF AVAIL Operator THEN cgrop = Operator.OperName.
         else cgrop = "-UNKNOWN-".
      END.

      PUT STREAM excel UNFORMATTED
         opname                      tab
         Calls.pref                  tab
         exName                      tab
         Calls.cgr                   tab
         cgrname                     tab
         cgrop                       tab
         Calls.Qty                   tab
         integer(Calls.pksec / 60)   tab
         integer(Calls.opsec / 60)   my-nl.

   END.

   OUTPUT STREAM excel CLOSE.

   MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

   LEAVE CRIT.

END.



