/*------------------------------------------------------------------------------
  MODULE .......: arpurel.p
  FUNCTION .....: ui for average revenue report 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 30.08.02/aam 
  MODIFIED .....: 19.11.02 lp moved from JG
                  12.09.03/aam brand
  VERSION ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}

{Syst/utumaa.i "new"}

ASSIGN tuni1 = "arpurel"
       tuni2 = "".

DEF VAR ufkey      AS LOG                     NO-UNDO.
DEF VAR ok         AS LOG   FORMAT "Yes/No"   NO-UNDO.
DEF VAR ldtDate1   AS DATE                    NO-UNDO.
DEF VAR ldtDate2   AS DATE                    NO-UNDO. 
DEF VAR ig-code    AS CHAR  FORMAT "X(10)"    NO-UNDO EXTENT 2.
DEF VAR lcCLIType      AS CHAR  FORMAT "X(10)"    NO-UNDO EXTENT 2. 

form
   SKIP(4)
   "     This program prints out an average revenue report from"   SKIP
   "     invoiced events. Report is printed by subscription"       SKIP
   "     type and product."                                        SKIP
   SKIP(3)
   ldtDate1  AT 10
        LABEL "Time period"
        HELP  "Time period for invoiced events"
        FORMAT "99-99-9999"
   "-"
   ldtDate2    
        NO-LABEL
        HELP "Time period for invoiced events"
        VALIDATE (INPUT ldtDate2 ge INPUT ldtDate1,
                  "Upper limit must be at least equal to lower limit")
        FORMAT "99-99-9999"
   SKIP        
   ig-code[1] AT 10
        LABEL "Inv. groups"
        HELP  "Invoicing group"
   "-" 
   ig-code[2] 
        NO-LABEL
        HELP "Invoicing group"
        VALIDATE (INPUT ig-code[2] ge INPUT ig-code[1],
                  "Upper limit must be at least equal to lower limit")
   SKIP
   lcCLIType[1]  AT 10
        LABEL "Subs. types"
        HELP  "Mobile subscription type"
   "-" 
   lcCLIType[2] 
        NO-LABEL 
        HELP  "Mobile subscription type"
        VALIDATE (INPUT lcCLIType[2] GE INPUT lcCLIType[1],
                  "Upper limit must be at least equal to lower limit")
   SKIP(4)
   WITH ROW 1 SIDE-LABELS WIDTH 80
        TITLE " " + ynimi + " AVERAGE REVENUE REPORT " +
        STRING(pvm,"99-99-99") + " "
        frame valinta.


/* previous month as default */
ASSIGN
   ldtDate2 = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
   ldtDate1 = DATE(MONTH(ldtDate2),1,YEAR(ldtDate2)).

FIND LAST InvGroup WHERE InvGroup.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE InvGroup THEN ig-code[2] = InvGroup.InvGroup.

FIND LAST CLIType WHERE CLIType.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE CLIType THEN lcCLIType[2] = CLIType.CLIType.

DISPLAY
   ldtDate1
   ldtDate2
   ig-code
   lcCLIType
WITH frame valinta. 

ASSIGN
   ufkey = false
   nap   = "first". 

toimi:
   REPEAT WITH frame valinta ON ENDKEY UNDO toimi, NEXT toimi:
      IF ufkey THEN DO:
         ASSIGN
            ufk[1] = 132 
            ufk[2] = 0
            ufk[3] = 0
            ufk[4] = 0
            ufk[5] = 63
            ufk[6] = 0
            ufk[7] = 0
            ufk[8] = 8 
            ufk[9] = 1
            ehto   = 3
            ufkey  = false.
         RUN Syst/ufkey.p.
      END.

      IF nap NE "first" THEN DO:
          READKEY.
          ASSIGN
          nap = keylabel(lastkey).
      END.
      ELSE ASSIGN nap = "1". 

      IF LOOKUP(nap,"1,f1") > 0 THEN DO:
         ehto = 9. 
         RUN Syst/ufkey.p.
         REPEAT WITH frame valinta ON ENDKEY UNDO, LEAVE:
            UPDATE 
                ldtDate1
                ldtDate2
                ig-code
                lcCLIType.
            LEAVE. 
         END.
         ufkey = true.
         NEXT toimi.
      END.

      ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:
         LEAVE toimi.
      END.

      ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
         RETURN.
      END.
   END. /* toimi */

/* Avataan striimi */
ASSIGN tila = true.
{Syst/utuloste.i "RETURN"}

MESSAGE "Printing in process...".   

RUN Ar/arpurep.p (ldtDate1, 
               ldtDate2,
               ig-code[1],
               ig-code[2],
               lcCLIType[1],
               lcCLIType[2]).

ASSIGN tila = false.
{Syst/utuloste.i}

MESSAGE "Average revenue report is finished."
VIEW-AS ALERT-BOX
TITLE " Report done ".  

HIDE MESSAGE NO-PAUSE.
HIDE frame valinta NO-PAUSE.    
