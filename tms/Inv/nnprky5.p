/* ------------------------------------------------------
  MODULE .......: NNPRKY5.P
  FUNCTION .....: Erittely puheluittain raportin print-linerajat
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 11.05.98
  MODIFIED .....: 04.06.98 kl 0 PARAM FOR nnpura5 invno
  VERSION ......: M15
  ------------------------------------------------------ */

{commali.i}
{cparam2.i}
{utumaa.i "new"}
{edefine.i "new"}

assign tuni1 = "nnpura5"
       tuni2 = "".

DEF NEW SHARED VAR lcMacros AS CHAR                  NO-UNDO.

DEF VAR ufkey AS LOG NO-UNDO.
def var CustNum1 as int format "zzzzzzz9" init "0" NO-UNDO.
def var CustNum2 as int format "zzzzzzz9" init "99999999" NO-UNDO.
def var pvm1  as date format "99-99-99" init ? NO-UNDO.
def var pvm2  as date format "99-99-99" init TODAY NO-UNDO.
def var tilak as int format "9" NO-UNDO.
DEF VAR dkk AS INT NO-UNDO.
DEF VAR dvv AS INT NO-UNDO.

FIND FIRST Company NO-LOCK NO-ERROR.

form
   skip(2)
   "        This programs creates a report of all Free Phone numbers  "
   SKIP
   "        of the period / customer(s) determined below." SKIP

   skip(13)
   WITH ROW 1 side-labels width 80
        title " " + ynimi + " FREEPHONECALL REPORT " +
        string(pvm,"99-99-99") + " "
        FRAME valinta.

form
   skip(1)
   CustNum1 label "Customer number " help "First customer in report"
   " - " CustNum2 no-label help "Last customer in report" SKIP
   pvm1  label "Date ..........." help "From .."                      
   " - " pvm2 no-label help "To .." SKIP
   tilak label "State code ....."
   help "State code, 0 = uninvoiced,  1 = invoiced, 2 = both"     
   with title " REPORT LIMITS " side-labels
   ROW 8 centered OVERLAY FRAME rajat.

view FRAME valinta.
PAUSE 0 no-message.

/* pvm-oletukset: kuluvan kuun alku ja loppu */
dkk = month(TODAY).
dvv = year(TODAY).
pvm1 = date(dkk,1,dvv).
dkk = dkk + 1.  IF dkk = 13 THEN ASSIGN dkk = 1 dvv = dvv + 1.
pvm2 = date(dkk,1,dvv) - 1.

DISPLAY CustNum1 CustNum2 pvm1 pvm2 tilak WITH FRAME rajat.
ufkey = TRUE.

toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 63 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.

      READKEY.
      nap = keylabel(LASTKEY).

      if lookup(nap,"1,f1") > 0 THEN DO:
         ehto = 9. RUN ufkey.p.
         UPDATE CustNum1
             CustNum2 validate(input CustNum2 >= input CustNum1, "Invalid order !")
                pvm1
                pvm2  validate(input pvm2 >= input pvm1, "Invalid date !")
                tilak validate(INPUT tilak >= 0 AND INPUT tilak < 3,
                               "State has to be 0 .. 2 !")
         WITH FRAME rajat.
         IF CustNum2 = 0 THEN CustNum2 = 999999.
         FIND FIRST Customer where 
            Customer.CustNum >= CustNum1 AND 
            Customer.CustNum <= CustNum2
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Customer THEN DO:
            message "No customers - nothing to print !".
            BELL.
            RETURN.
         END.
         ufkey = TRUE.      
         NEXT toimi.
      END.
      else if lookup(nap,"5,f5") > 0 THEN DO:
         LEAVE toimi.
      END.
      else if lookup(nap,"8,f8") > 0 THEN DO:
         RETURN.
      END.
   END. /* toimi */

/* Avataan striimi */
ASSIGN tila = TRUE.
{utuloste.i "return"}

RUN umakro (TRUE).

message "Printing, ESC = break".

RUN nnpura5.p(INPUT CustNum1,input CustNum2,
            input pvm1,input pvm2,
            input tilak,0,FALSE).

/* Suljetaan striimi */
ASSIGN tila = FALSE.
{utuloste.i}

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.
HIDE FRAME valinta no-pause.

