/* -----------------------------------------------------
  MODULE .......: udate2c.p
  KUTSUVAMODULI : ALL
  TEHTAVA ......: Converts Date into STRING
  SOVELLUTUS ...: NN
  TEKIJA .......: KL
  CREATED ......: 09.02.98
  changePVM ....: 24.03.03/aam use country.i -> correct format for FIN
  Version ......: M15
  SHARED .......:
  ------------------------------------------------------ */

{Syst/country.i}

DEF INPUT  PARAM my-date AS DA  NO-UNDO. /* Date TO convert */
def input  param yr-len  as log no-undo. /* if true year is "x(2)" */
DEF OUTPUT PARAM s-date  AS c   NO-UNDO. /* STRING TO RETURN */

if my-date = ? then s-date = "".
ELSE DO:

   IF "{&country}" = "{&FIN}" 
   THEN DO:
      IF yr-len 
      THEN s-date = STRING(my-date,"99-99-99").
      ELSE s-date = STRING(my-date,"99-99-9999").
   END.   

   /* Date into 1998-02-09 FORMAT */
   ELSE DO:
      s-date = string(year (my-date),"9999") + "-" +
               string(month(my-date),"99")   + "-" +
               string(day  (my-date),"99").

      /* Date into 98-02-09 FORMAT IF yr-len = TRUE */
      IF yr-len THEN ASSIGN s-date = substr(s-date,3).
   END.

END. 

