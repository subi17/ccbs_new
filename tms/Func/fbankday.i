/* fbankday.i       11.06.03/aam 

   callers: nnsvti.p
            nncimu.p

*/

/* calculate nbr of banking days before given date */
FUNCTION fBankDays RETURNS INTEGER
   (idtDueDate AS DATE).

   DEF VAR liBankDays   AS INT   NO-UNDO. 
   DEF VAR ldtCntDate   AS DATE  NO-UNDO.

   liBankDays = 0.
   DO ldtCntDate = idtDueDate - 1 TO TODAY BY -1:
      IF WEEKDAY(ldtCntDate) >= 2 AND
         WEEKDAY(ldtCntDate) <= 6
      THEN liBankDays = liBankDays + 1.
   END.

   RETURN liBankDays. 

END FUNCTION.
