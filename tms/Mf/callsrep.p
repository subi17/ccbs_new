 /* -----------------------------------------------
  MODULE .......: callsrep.P
  FUNCTION .....: 
  APPLICATION ..: Master
  AUTHOR .......: JR
  CREATED ......: 13.06.02
  MODIFIED .....: 18.11.02 jr Table names jne.
                  12.09.03/aam brand
  VERSION ......: M15
------------------------------------------------------ */
{Syst/commali.i}

DEF VAR email   AS CHAR                NO-UNDO FORMAT "x(40)".
DEF VAR asno    LIKE Customer.CustNum  NO-UNDO.
DEF VAR invno   LIKE Invoice.InvNum    NO-UNDO.
DEF VAR asname  LIKE Customer.CustName NO-UNDO.
DEF VAR invname AS CHAR                NO-UNDO.

FORM 
"    Call specification for invoice or unbilled calls"      SKIP(1)
"    Customer .....:" asno 
HELP "Calls from which customer." asname SKIP
"    Invoice ......:" invno 
HELP "From specific invoice or all unbilled calls = 0." invname SKIP
"    E-mail .......:" email
WITH
  ROW 7 WIDTH 65
  NO-LABELS CENTERED OVERLAY COLOR VALUE(cfc)
  TITLE COLOR VALUE(ctc) " " + "  Call specification for invoice or unbilled calls. " 
  + STRING(pvm,"99-99-99") + " " FRAME main.

rajat:
REPEAT WITH FRAME rajat:
   PAUSE 0.
   ehto = 9. RUN Syst/ufkey.p.

   UPDATE 
   asno
   WITH FRAME MAIN EDITING:
       READKEY. nap = KEYLABEL(LASTKEY).
       IF LOOKUP(nap,poisnap) > 0 THEN 
       DO:
           HIDE MESSAGE.
           IF FRAME-FIELD = "asno" THEN 
           DO:
               ASSIGN FRAME MAIN asno.
               FIND FIRST customer WHERE 
                  Customer.Brand   = gcBrand AND
                  Customer.CustNum = asno NO-ERROR.
               IF asno = 0 OR
               NOT AVAILABLE customer THEN
               DO:
                   BELL. MESSAGE "Uknown customer !".
                   NEXT.
               END.
               ELSE 
               DO: 
                   ASSIGN
                   asname = Customer.CustName
                   email  = Customer.Email.
                   DISP asname email
                   WITH FRAME main. 
               END.
           END.
       END.
       APPLY LASTKEY.
   END.

   UPDATE
         invno
         email
   WITH FRAME main EDITING:
       READKEY. nap = KEYLABEL(LASTKEY).
       IF LOOKUP(nap,poisnap) > 0 THEN 
       DO:
           HIDE MESSAGE.
           IF FRAME-FIELD = "invno" THEN 
           DO:
               ASSIGN FRAME main invno.
               FIND FIRST invoice WHERE 
                  Invoice.Brand  = gcBrand AND
                  invoice.invnum = invno NO-ERROR.
               IF invno NE 0 AND
               NOT AVAILABLE invoice THEN
               DO:
                   BELL. MESSAGE "Uknown invoice !".
                   NEXT.
               END.
           END.
       END.
       APPLY LASTKEY.
   END.

toimi:
      REPEAT WITH FRAME toimi:
        ASSIGN ufk = 0 ehto = 0 ufk[1] = 132 ufk[5] = 63 ufk[8] = 8.
        RUN Syst/ufkey.p.
        IF toimi = 1 THEN NEXT  rajat.
        IF toimi = 8 THEN LEAVE rajat.
        IF toimi = 5 THEN 
        DO:
            RUN Mf/callemail.p(asno,invno,email).
            LEAVE toimi.
        END.      
END.
LEAVE rajat.
END.
HIDE MESSAGE NO-PAUSE.
HIDE FRAME rajat NO-PAUSE. 

