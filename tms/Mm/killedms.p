/* -----------------------------------------------
  MODULE .......: killedms.p
  FUNCTION .....: counts killed mobile subscriptions
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 19.06.03
  MODIFIED .....: 15.09.03 jp Brand 

  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/timestamp.i}

DEF VAR lkm       AS i  NO-UNDO.
def var pvm1      as da no-undo format "99-99-99".
def var pvm2      as da no-undo format "99-99-99".
def var stamp1    as de no-undo.
def var stamp2    as de no-undo.
def var ok        as lo no-undo.

def var stat1     as i  no-undo.
def var stat45    as i  no-undo.
def var stat7     as i  no-undo.
def var statother as i  no-undo.


form
   skip(1)
"  Note:   This program shows the total amount of killed mobile" skip
"          subscriptions and SIM status codes."                  skip(1)
"          Subscriptions between ..:" pvm1
help "Earliest Date of call" "-" pvm2 
help "Latest Date of call" skip(1)
"          Subscriptions total.....:" lkm skip(1)
"          SIM Status Codes" skip(1)
"          InStock, NotPers .......:"  stat1 skip
"          AtCust, NoAgr OR"  skip
"          AtCust, Agr ............:" stat45 skip
"          AtCust, Lost ...........:" stat7 skip
"          other ..................:" statother 
skip(1)
WITH
   width 80 overlay no-labels title " " + ynimi +
   " Count mobile subscriptions " FRAME rajat.

pvm1 = date(month(TODAY),1,year(TODAY)).
pvm2 = pvm1 + 40.
pvm2 = date(month(pvm2),1,year(pvm2)) - 1.


rajat:
repeat WITH FRAME rajat:

   PAUSE 0.
   ehto = 9. RUN ufkey.
   UPDATE
   pvm1
   pvm2
   validate (input pvm2 >= input pvm1,"Incorrect order !").

toimi:
   repeat WITH FRAME toimi:
      ASSIGN ufk = 0 ehto = 0 ufk[1] = 132 ufk[5] = 63 ufk[8] = 8.
      RUN ufkey.
      IF toimi = 1 THEN NEXT  rajat.
      IF toimi = 8 THEN LEAVE rajat.
      IF toimi = 5 THEN LEAVE toimi.
   END.
   ASSIGN 
      lkm = 0
      stat1 = 0
      stat45 = 0
      stat7 = 0
      statother = 0.
   message "Calculating ...".                   

   stamp1 = fHMS2TS(pvm1,"00:00:00").
   stamp2 = fHMS2TS(pvm2,"23:59:59").

   FOR EACH MSOwner no-lock where
            MSOwner.TsBegin >= stamp1  AND
            MSOwner.TsEnd   <= stamp2  AND 
            MSOwner.Brand    = gcBrand :

      lkm = lkm + 1.

      if msowner.imsi = "" then do:
         statother = statother + 1.
         next.
      end.

      find imsi where 
           imsi.imsi = msowner.imsi
      no-lock no-error.

      if not avail imsi then do:
         statother = statother + 1.
         next.
      end.

      find sim where
           sim.icc = imsi.icc
      no-lock no-error.

      if not avail sim then do:
         statother = statother + 1.
         next.
      end.

      case sim.simstat:
         when 1 then 
            stat1 = stat1 + 1.
         when 4 or when 5 then
            stat45 = stat45 + 1.
         when 7 then 
            stat7 = stat7 + 1. 
         otherwise
            statother = statother + 1.

      end.
   END.
   PAUSE 0.
   disp lkm stat1 stat45 stat7 statother.
   message "Press ENTER to continue !".
   PAUSE no-message.
   CLEAR FRAME rajat.

   NEXT rajat.
END.

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.

