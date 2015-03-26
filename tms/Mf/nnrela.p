/* ---------------------------------------------------------------
  MODULE .......: NNRELA.P
  FUNCTION .....: Referenssi (viite)numeron laskenta ruotsal. mallin mukaan
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 23.04.97 pt
  Version ......: M15
  --------------------------------------------------------------- */


DEF INPUT  PARAMETER nro AS DE.
DEF OUTPUT PARAMETER ref AS i.

DEF VAR cnro  AS c NO-UNDO.
DEF VAR csum  AS c NO-UNDO.
DEF VAR i     AS i NO-UNDO.
DEF VAR x     AS i NO-UNDO.
DEF VAR summa AS i NO-UNDO.
DEF VAR kerr  AS i NO-UNDO.


IF nro = 0 THEN ref = ?.
ELSE DO:

   /* viedAAn nro ensin stringiksi, joka on helpompi viipaloida */
   cnro = string(nro).


   kerr = 2.
   DO i = length(cnro) TO 1 BY -1.
      x = integer(substr(cnro,i,1)).
      csum = string(x *  kerr) + csum.
      kerr = 3 - kerr.

   END.

   /* nyt lasketaan summa */
   DO i = 1 TO length(csum).
      summa = summa + integer(substr(csum,i,1)).

   END.

   csum = string(summa).
   ref = 10 - integer(substr(csum,length(csum))).
   IF ref = 10 THEN ref = 0.

END.

