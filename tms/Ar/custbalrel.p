/*-----------------------------------------------------------------------------
  MODULE .......: CUSTBALREL.P
  FUNCTION .....: UI for report on customer's open invoices, payments and
                  interests
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 14.08.02
  MODIFIED .....: 26.09.02/aam customer balances in table CustBal 
                  13.11.02 lp definition's view changed
                  12.09.03/aam brand,
                               explanation of report etc.
                  12.02.04/aam logic separated to custbalrep,
                               input iiCustNum
                  23.02.04 jp  Creasfee               
                  30.04.04/aam ask user about the fee
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/utumaa.i "new"}

assign tuni1 = "custbalrep"
       tuni2 = ""
       tila = TRUE.

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO. 

DEF VAR liCustNum   LIKE Customer.CustNum  NO-UNDO. 
DEF VAR lcCustName  LIKE Customer.CustName NO-UNDO.
DEF VAR ufkey       AS LOG                 NO-UNDO.
DEF VAR ldtFromDate AS DATE                NO-UNDO. 
DEF VAR lcInfo      AS CHAR                NO-UNDO. 
DEF VAR llCreaFee   AS LOG                 NO-UNDO. 

form
   SKIP(1)
   "      This program prints out the AR status of the chosen customer, i.e." 
   SKIP
   "      - open invoices and their payment status"    SKIP
   "      - paid invoices from given date until today" SKIP
   "      - claiming history of invoices"              SKIP
   "      - deposit, overpayment and advance payment balances" SKIP
   "      - interest events" SKIP
   SKIP(2)
   "      Customer ...:" 
   liCustNum 
      FORMAT ">>>>>>>9"
      help "Customer number" 
      lcCustName
   SKIP
   "      From .......:" ldtFromDate
      HELP "Paid invoices from this on are printed" 
      FORMAT "99-99-99"
   SKIP(1)
   "      Create a Fee:" llCreaFee
      HELP "Create a fee for customer for printing this report"
      FORMAT "Yes/No"
   SKIP
WITH TITLE " " + ynimi +  " A/R CUSTOMER REPORT " + 
           STRING(pvm,"99-99-99") + " "
   ROW 1 centered Size 80 by 19 NO-LABELS OVERLAY FRAME valinta.

PAUSE 0.
view frame valinta.
pause 0 no-message.

ASSIGN liCustNum   = iiCustNum
       ldtFromDate = DATE(1,1,YEAR(TODAY))
       llCreaFee   = TRUE

       ufkey = TRUE
       nap   = "first". 

toimi:
repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:
 
      lcCustName = "".
      FIND Customer NO-LOCK WHERE
           Customer.Brand   = gcBrand AND
           Customer.CustNum = liCustNum NO-ERROR.
      IF AVAILABLE Customer THEN lcCustName = Customer.CustName.
                 
      DISPLAY 
         liCustNum 
         lcCustName
         ldtFromDate
         llCreaFee
      WITH FRAME valinta. 
   
      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 132 
         ufk[2]= 0  ufk[3]= 0 ufk[4]= 0 
         ufk[5]= 63 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.
      pause 0.

      if nap ne "first" THEN DO: 
          READKEY.
          ASSIGN
          nap = keylabel(LASTKEY).
      END.
      else assign nap = "1". 

      if lookup(nap,"1,f1") > 0 THEN DO:

         ehto = 9. RUN Syst/ufkey.p.
         
         repeat WITH FRAME valinta ON ENDKEY UNDO, LEAVE:
            UPDATE 
               liCustNum WHEN iiCustNum = 0
               ldtFromDate
               llCreaFee
            WITH FRAME valinta EDITING:
               READKEY.
               IF lookup(keylabel(lastkey),poisnap) > 0 THEN DO:
                  if frame-field = "liCustNum" THEN DO:
                     IF INPUT liCustNum > 0 THEN DO:
                     
                        FIND Customer WHERE Customer.CustNum = INPUT liCustNum
                        NO-LOCK NO-ERROR.
                        IF NOT AVAIL Customer OR Customer.Brand NE gcBrand
                        THEN DO:
                           MESSAGE "Unknown Customer !".
                           NEXT.
                        END.
                        ELSE DISP Customer.Custname @ lcCustname.
                     END.
                  END.
               END.
               APPLY LASTKEY.
            END.   
            LEAVE.   
         END.
         ufkey = TRUE.
         NEXT toimi.
      END.

      else if lookup(nap,"5,f5") > 0 THEN DO:
         IF liCustNum > 0 THEN LEAVE toimi.
      END.
      else if lookup(nap,"8,f8") > 0 THEN DO:
         HIDE FRAME valinta NO-PAUSE.
         RETURN.
      END.

END. /* toimi */

{Syst/utuloste.i "return"}

RUN Ar/custbalrep.p (liCustNum,
                ldtFromDate).

tila = FALSE.
{Syst/utuloste.i}

IF llCreaFee THEN 
RUN Mc/creasfee.p (Customer.CustNum,
              0,
              TODAY,
              "PRINTS",
              "Balance",
              2,
              ?, 
              "",
              TRUE,
              katun,
              "",
              0,
              "",
              "",
              OUTPUT lcInfo).

MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.



HIDE FRAME valinta NO-PAUSE.

