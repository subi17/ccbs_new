/*------------------------------------------------------
  MODULE .......: NNASTT.P
  FUNCTION .....: Asiakkaan tarkat tiedot -valikko
  SOVELLUTUS ...: NN
  AUTHOR .......: PT
  CREATED ......: 12.02.1996
  changePVM ....: 07.01.1997 pt  'volymrabatt' --> 'rabatt' ym. nimimuutoksia
                  24.02.1997 pt lisAtty laskujen selailu
                  08.01.1998 pt In English
                  04.06.1998 pt warning prior TO 'RUN nnasal'
                  26.08.1998 pt FUNCTION 5: RUN nnlaen2
                  03.12.1998 pt DOWN payment plan
                  27.12.1998 pt customer groups
                  06.03.2002 jp CREATE invoice
                  09.04.02 lp  use nnlamu3.p instead of custlamu.p
                  20.05.02 tk  BillCode packages
                  20.05.02 tk  invoice texts
                  21.05.02 aam "customer produ.." -> "customer's produ.." 
                  05.09.02 aam billing structure moved to commontt 
                  19.09.02 aam "Price list history" removed,
                               "Invoice texts" and "Create Invoice"
                               moved to commontt
                  11.11.02 lp added evbrcust.p
  Version ......: M15
  SHARED .......: INPUT: CustNum
  ------------------------------------------------------ */

DEF INPUT PARAMETER CustNum AS INT.

{Syst/commali.i}

DEF VAR i AS INT.
DEF VAR val  AS CHAR EXTENT 7.
DEF VAR vlkm AS INT  init 7.

ASSIGN

val[1]   = "A)  Create/List cust. internet pwds"
val[2]   = "B)  Customer's Product Packages"
val[3]   = "C)  Customer's EVENT LOG "

val[6]   = "F8) RETURN".


form
  val[i] format "x(50)"  help "Choose desired function and press ENTER !"

WITH vlkm DOWN NO-LABEL centered ROW 3
title " " + substr(Customer.CustName,1,16) + " " OVERLAY FRAME valikko.

FIND Customer where Customer.CustNum = CustNum no-lock.


PAUSE 0 no-message.
DO i = 1 TO vlkm WITH FRAME valikko.
   DISPLAY val[i].
   IF i < vlkm THEN DOWN.
   ELSE up FRAME-LINE - 1.
END.

repeat:
   ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN ufkey.
   CHOOSE ROW val[i] no-error auto-return WITH FRAME valikko.
   i = frame-line(valikko).
   IF lookup(keylabel(lastkey),"8,f8") > 0 OR i = vlkm THEN DO:
      HIDE FRAME valikko no-pause.
      RETURN.
   END.
   ELSE DO: /* call a program */
     IF i = 1 THEN DO: /* Ivoicing Targets */
        RUN nnpwd(CustNum).
     END.

     ELSE IF i = 2 THEN RUN custpp2(CustNum).

     ELSE IF i = 3 THEN RUN evbrcust(CustNum).
   END.

END.

