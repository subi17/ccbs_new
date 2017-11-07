/* ------------------------------------------------------
  MODULE .......: NNPRKY7.P
  FUNCTION .....: Puheluraportin print-linerajat
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 21.03.03 (from nnprky2)
  MODIFIED .....: 12.09.03/aam brand
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

{Func/cparam2.i}
{Syst/utumaa.i "new"}
{Inv/edefine.i "new"}

assign tuni1 = "nnpura7"
       tuni2 = "".

DEF VAR lcMacros AS CHAR                               NO-UNDO.
DEF VAR ufkey    AS LOG                                NO-UNDO.
def var CustNum as int format "zzzzzz9" init "0"      NO-UNDO.
def var pvm1     as date format "99-99-99" init ?      NO-UNDO.
def var pvm2     as date format "99-99-99" init TODAY  NO-UNDO.
def var tilak    as int format "9"                     NO-UNDO.
DEF VAR dkk      AS INT                                NO-UNDO.
DEF VAR dvv      AS INT                                NO-UNDO.
DEF VAR llStart  AS LOGIC                              NO-UNDO. 
DEF VAR InvNum   LIKE InvSeq.InvNum                    NO-UNDO.


form
   skip(1)
   "Report is printed on summary level and sorted by"      AT 10 SKIP
   "CLI / Billing Item / CCN." AT 10 
   skip(14)
   WITH ROW 1 side-labels width 80
        title " " + Syst.Var:ynimi + 
        " CALL SUMMARY PER CCN (REPORT 7) " +
        string(TODAY,"99-99-99") + " "
        FRAME valinta.

form
   skip(1)
   InvNum  
      label "Invoice number ........." 
      help "Invoice number, 0 = all"                  
      format ">>>>>>>9"
      SKIP
   CustNum  
      label "Report Customer ........" 
      help "Customer, to which reports are assigned to"
      format ">>>>>>>9"
      SKIP
   pvm1   
      label "Dates .................." 
      help "From "
   " - " 
   pvm2 
      no-label 
      help "Till "                        
      SKIP
   tilak  
      label "Status code ............"
      help "Status, 0 = not invoiced, 1 = invoiced, 2 = both" 
      SKIP
   with title " PRINTING CRITERIA " side-labels
   ROW 8 centered OVERLAY FRAME rajat.


lcMacros = fCParamC("MacroDir") + fCParamC("MacroSpec").

view FRAME valinta.
PAUSE 0 no-message.

/* pvm-oletukset: kuluvan kuun alku ja loppu */
dkk = month(TODAY).
dvv = year(TODAY).
pvm1 = date(dkk,1,dvv).
dkk = dkk + 1.  IF dkk = 13 THEN ASSIGN dkk = 1 dvv = dvv + 1.
pvm2 = date(dkk,1,dvv) - 1.
llStart = TRUE.


DISPLAY CustNum pvm1 pvm2 tilak InvNum WITH FRAME rajat.
ufkey = FALSE.

toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:

      IF ufkey THEN DO:
         ASSIGN
         Syst.Var:ufk[1]= 132 Syst.Var:ufk[2]= 0 Syst.Var:ufk[3]= 0 Syst.Var:ufk[4]= 0
         Syst.Var:ufk[5]= 63 Syst.Var:ufk[6]= 0 Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
         Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      IF llStart THEN ASSIGN Syst.Var:nap     = "1"
                             llStart = FALSE.
      ELSE DO:
         READKEY.
         Syst.Var:nap = keylabel(LASTKEY).
      END.

      if lookup(Syst.Var:nap,"1,f1") > 0 THEN DO:
         ASSIGN Syst.Var:ehto = 9 ufkey = TRUE. 
         RUN Syst/ufkey.p.

         REPEAT ON ENDKEY UNDO, LEAVE:
            UPDATE 
            InvNum
            VALIDATE(INPUT InvNum = 0 OR
                     CAN-FIND (FIRST Invoice WHERE
                               Invoice.Brand  = Syst.Var:gcBrand AND
                               Invoice.InvNum = INPUT invnum),
            "Unknown Invoice Number!")                   
            WITH FRAME rajat.

            IF InvNum NE 0 THEN DO:
               FIND Invoice WHERE Invoice.InvNum = InvNum NO-LOCK NO-ERROR.
               IF AVAIL Invoice THEN DO: 
                  ASSIGN
                  CustNum = Invoice.CustNum
                  pvm1  = Invoice.FromDate
                  pvm2  = Invoice.ToDate
                  tilak = 1.

                  DISPLAY CustNum pvm1 pvm2 tilak WITH FRAME rajat.
               END.
            END.

            ELSE DO:
               UPDATE
               CustNum
                  validate(CAN-FIND(FIRST Customer WHERE 
                                    Customer.Brand   = Syst.Var:gcBrand AND
                                    Customer.CustNum = INPUT CustNum),
                           "Unknown customer")                            
               pvm1
               pvm2  validate(input pvm2 >= input pvm1, "Check the date !")
               tilak validate(INPUT tilak >= 0 AND INPUT tilak < 3,
                              "Status must be 0 - 2 !")
               WITH FRAME rajat.
            END.

            LEAVE.
         END.

         DISPLAY InvNum 
                 CustNum
                 pvm1 
                 pvm2
                 tilak
         WITH FRAME rajat. 

      END.

      else if lookup(Syst.Var:nap,"5,f5") > 0 THEN DO:
         LEAVE toimi.
      END.

      else if lookup(Syst.Var:nap,"8,f8") > 0 THEN DO:
         RETURN.
      END.
END. /* Syst.Var:toimi */

/* Avataan striimi */
ASSIGN tila = TRUE.
{Syst/utuloste.i "return"}

RUN Syst/umakro.p (TRUE,
            lcMacros).

message "Printing in progress, ESC = cancel".

RUN nnpura7 (INPUT CustNum,
             INPUT pvm1,
             INPUT pvm2,
             INPUT tilak,
             INPUT InvNum,
             INPUT FALSE,
             INPUT FALSE).

/* Suljetaan striimi */
ASSIGN tila = FALSE.
{Syst/utuloste.i}

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.
HIDE FRAME valinta no-pause.

