/* ------------------------------------------------------
  MODULE .......: NNPRKY6.P
  FUNCTION .....: Erittely puheluittain raportin print-linerajat
  APPLICATION ..: NN
  AUTHOR .......: TT
  CREATED ......: 01.02.96
  MODIFIED .....: 24.09.96 /tt --> Ruotsinnettu, tulostukset NN-ohjelmalla
                  04.05.98 /kl --> 0 PARAM FOR nnpura2 InvNum 
                  21.12.99 /kl --> CustNum2 into nnpura6
                  06.09.02 /jp --> some validation
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}

{utumaa.i "new"}

assign tuni1 = "nnpura6"
       tuni2 = "".

DEF VAR ufkey AS LOG NO-UNDO.
def var CustNum1 as int format "zzzzzz9" init "0" NO-UNDO.
def var CustNum2 as int format "zzzzzz9" init "999999" NO-UNDO.
def var Date1  as Date format "99-99-99" init ? NO-UNDO.
def var Date2  as Date format "99-99-99" init TODAY NO-UNDO.
def var tilak as int format "9" NO-UNDO.
DEF VAR dkk   AS INT NO-UNDO.
DEF VAR dvv   AS INT NO-UNDO.
DEF VAR InvNum AS i format "zzzzzz9" init "0" NO-UNDO.


FIND FIRST Company no-lock no-error.

form
   skip(1)
   "    This program prints out a mobile call specification, sorted by " SKIP
   "    GSM subscriper / Destination / Date                            " SKIP

   skip(13)
   WITH ROW 1 side-labels width 80
        title " " + ynimi + " MOBILE calls SPECS # 2  " +
        string(today,"99-99-99") + " "
        FRAME valinta.

form
   skip(1)
   CustNum1 label "Customer numbers..... .." help "Customer number "   SKIP
   Date1     label "Calls during ..........." help "Eariest call Date"
   " - " Date2 no-label help "Latest call Date"                      SKIP
   tilak label "Call status code ......."
   help "Status: 0 = Not (yet) billed, 1 = Billed, 2 = Both" SKIP
   InvNum label "Invoice Number ........." 
   help "Calls Billed with this Invoice No. (empty = any invoice)"

   skip(1)
   with title " PRINTOUT CRITERIA " side-labels
   ROW 8 centered OVERLAY FRAME rajat.

view FRAME valinta.
PAUSE 0 no-message.

/* Date-oletukset: kuluvan kuun alku ja loppu */
dkk = month(TODAY).
dvv = year(TODAY).
Date1 = date(dkk,1,dvv).
dkk = dkk + 1.  IF dkk = 13 THEN ASSIGN dkk = 1 dvv = dvv + 1.
Date2 = date(dkk,1,dvv) - 1.

DISPLAY CustNum1 /*CustNum2*/ Date1 Date2 tilak InvNum WITH FRAME rajat.
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
         UPDATE 
            CustNum1
            validate(can-find(first customer where 
                                    customer.custnum = input CustNum1),
            "Unknown Customer!")                         
            Date1
            Date2  validate(input Date2 >= input Date1, "Invalid order !")
            tilak  validate(INPUT tilak >= 0 AND INPUT tilak < 3,
                           "Status must be 0 ...  2 !")
            invnum 
            VALIDATE(Can-find(first invoice WHERE
                                    invoice.InvNum = INPUT invnum),
            "Unknown Invoice Number!")                        
         WITH FRAME rajat.
         IF CustNum2 = 0 THEN CustNum2 = 999999.
         FIND FIRST Customer where CustNum >= CustNum1 AND CustNum <= CustNum2
         no-lock no-error.
         IF NOT AVAILABLE Customer THEN DO:
            message "Unknown customer - no printout !".
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
{tmsreport.i "return"}

message "Printing in process".            

RUN nnpura6(INPUT CustNum1, INPUT CustNum1,
            INPUT Date1,  INPUT Date2,
            INPUT tilak, InvNum,TRUE).

/* Suljetaan striimi */
ASSIGN tila = FALSE.
{tmsreport.i}

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.
HIDE FRAME valinta no-pause.

