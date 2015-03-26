/* -----------------------------------------------
  MODULE .......: NNTARK.P
  FUNCTION .....: AS-VAPAA-KENTAN SISALLON TARKISTUS
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 21-01-96
  changePVM ....:
  Version ......: M15
  PARAM ........: IN  x  tarkistettava merkkijono
        OUT ok true/false
  ------------------------------------------------------ */


DEF INPUT PARAMETER x AS CHAR.
DEF OUTPUT PARAMETER  ok AS LOG.

DEF VAR a AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR a-lkm AS INT NO-UNDO.
def var asno as char format "x(20)" NO-UNDO.

  ok = TRUE.
  DO i = 1 TO length(x).
     if index("0123456789A+",substr(x,i,1)) = 0 THEN DO:
   BELL.
   message string(i,"z9.") "Invalid entry : Allowed characters are " +
      "0123456789A+  !".
   ok = FALSE.
   RETURN.
     END.
  END.

  a = index(x,"A").
  IF A > 0 THEN DO:
     /* lasketaan a-kirjaimet */
     a-lkm = 0.
     DO i = 1 TO length(x).
   if substr(x,i,1) = "a" THEN a-lkm = a-lkm + 1.
     END.
     IF a-lkm > 1 THEN DO:
   ok = FALSE.
   BELL.
   message "At least one 'A' is required !".
   RETURN.
     END.

     /* otetaan talteen a:n jAlkeinen asno */
     asno = substr(x,a + 1).
     if asno = "" THEN DO:
   ok = FALSE.
   BELL.
   message "Customer number ends after 'A' !".
   RETURN.
     END.

     /* asnon numeerisuus */
     DO i = 1 TO length(asno).
   if index("0123456789",substr(asno,i,1)) = 0 THEN DO:
      BELL.
      message "Only numbers allowed after 'A' !".
      ok = FALSE.
      RETURN.
   END.
     END.

     /*lOytyykO Customer */
     IF NOT CAN-FIND(Customer where CustNum = integer(asno))
     THEN DO:
   BELL.
   message "Customer " + asno + " NONE FOUND !".
   ok = FALSE.
   RETURN.
     END.
     message "Discount customer = " asno " -press ENTER !".
     PAUSE no-message.
  END.

