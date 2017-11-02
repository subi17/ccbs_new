/* -----------------------------------------------------
  MODULE .......: NNCLTU.P
  KUTSUVAMODULI : NN.P
  TEHTAVA ......: Customer letter printing
  SOVELLUTUS ...: NN
  TEKIJA .......: KL
  CREATED ......: 18.03.98
  changePVM ....: 16.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

def var paivays  as char format "x(10)"         NO-UNDO.
DEF VAR cust-nr1 LIKE Customer.CustNum   NO-UNDO.
DEF VAR cust-nr2 LIKE Customer.CustNum   NO-UNDO.
DEF VAR InvGroup  LIKE InvGroup.InvGroup NO-UNDO.
DEF VAR margin   AS i  NO-UNDO.
DEF VAR fst      AS lo NO-UNDO.  

DEF VAR i        AS i NO-UNDO.
DEF VAR rl       AS i NO-UNDO.

{Syst/commali.i}

{Syst/utumaa.i "new"}

assign tuni1 = "nncltu"
       tuni2 = "".

FIND FIRST Customer no-lock WHERE Customer.Brand = Syst.CUICommon:gcBrand no-error.
FIND FIRST CustLetter WHERE CustLetter.Brand = Syst.CUICommon:gcBrand no-lock no-error.
IF NOT AVAILABLE CustLetter THEN DO:
    MESSAGE "No customer letters available."
    VIEW-AS ALERT-BOX
    INFORMATION.
    RETURN.
END.

form header
   skip(5)
   paivays                         AT 54 skip(1)
   CustName    AT 6 string(CustNum)  AT 54 SKIP
   COName     AT 6 SKIP
   Address     AT 6 SKIP
   ZipCode   AT 6 
   PostOffice          SKIP
   Country     AT 6 skip(5)
WITH FRAME yla width 77 NO-LABELS no-box.


form
    skip(1)
"  Instruction:  This program prints out a specified customer letter " skip
"                for all customers determined below.                 " skip(2)
"                Customer letter text updated " CustLetter.ChgDate skip(3)

"                Customer ..:" cust-nr1 
HELP "Customer From"         
         to 39 "-" cust-nr2  
HELP "Customer To"                                                 skip
"                Inv. group :" InvGroup           
HELP "Invoice Group" to 39                                         skip
"                Margin ....:" CustLetter.LtrMargin to 39          skip(5)
WITH
    COLOR value(Syst.CUICommon:cfc) TITLE COLOR value(Syst.CUICommon:cfc)
    " " + Syst.CUICommon:ynimi + " Customer letter printing " + string(TODAY,"99-99-99") + " "
    ROW 1 width 80 NO-LABEL
    FRAME rajat.


ASSIGN
   cust-nr1 = 1    cust-nr2 = 9999999.

rajat:
repeat WITH FRAME rajat:

   Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p.

   DISP CustLetter.ChgDate CustLetter.LtrMargin WITH FRAME rajat.
   UPDATE
      cust-nr1   cust-nr2 validate (input cust-nr2 >= input cust-nr1,
                                    "Invalid order !")
      InvGroup  VALIDATE (can-find(InvGroup where 
                                   InvGroup.Brand    = Syst.CUICommon:gcBrand AND
                                   InvGroup.InvGroup = InvGroup)
                         or InvGroup = "","UNKNOWN INVOICE GROUP")
      CustLetter.LtrMargin
   WITH FRAME rajat.

toimi:
   repeat WITH FRAME rajat:
      ASSIGN
      Syst.CUICommon:ufk = 0 Syst.CUICommon:ehto = 0
      Syst.CUICommon:ufk[1] = 7 Syst.CUICommon:ufk[5] = 63 Syst.CUICommon:ufk[8] = 8.
      RUN Syst/ufkey.p.
      IF Syst.CUICommon:toimi = 1 THEN NEXT  rajat.
      IF Syst.CUICommon:toimi = 8 THEN LEAVE rajat.
      IF Syst.CUICommon:toimi = 5 THEN  LEAVE toimi.
   END.  /* Syst.CUICommon:toimi */

   ASSIGN INPUT LtrMargin.

   tila = TRUE.
   {Syst/tmsreport.i "leave rajat"}

   message "Printing ...".

   RUN Syst/udate2c.p(INPUT TODAY, INPUT TRUE, OUTPUT paivays).

   FOR EACH  Customer no-lock  where
             Customer.Brand    = Syst.CUICommon:gcBrand AND
             Customer.CustNum >= cust-nr1     AND
             Customer.CustNum <= cust-nr2     AND
            (if InvGroup ne "" THEN Customer.InvGroup = InvGroup
             ELSE TRUE)

      BREAK
         BY Customer.CustNum:

         IF NOT fst THEN PUT STREAM tul control tehon1.
         fst = FALSE.

         view STREAM tul FRAME yla.

         DO i = 1 TO 17:
            PUT STREAM tul
               CustLetter.LtrText[i] AT CustLetter.LtrMargin.
         END.

         PUT STREAM tul UNFORMATTED skip(spit1 - rl).

   END. /* FOR EACH */

   ASSIGN tila = FALSE.
   {Syst/tmsreport.i}

   LEAVE.
END. /* rajat */
HIDE FRAME rajat.

