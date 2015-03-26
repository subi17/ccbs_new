/* fduedate.i       28.04.04/aam

   check that due date isn't on weekend or on a holiday

   callers: fprintinv.i
            lamupers.p
            
   changes:        16.11.04/aam use NatHoliday, not Holiday         
*/

&IF "{&CheckDueDateDefined}" NE "YES"
&THEN

&GLOBAL-DEFINE CheckDueDateDefined YES

FUNCTION fChkDueDate RETURNS DATE
   (idtDueDate AS DATE).
   
   DO WHILE TRUE:
      /* weekend */
      IF WEEKDAY(idtDueDate) = 7 OR
         WEEKDAY(idtDueDate) = 1 
      THEN idtDueDate = idtDueDate + 1.
      
      /* national holiday */
      ELSE IF CAN-FIND(FIRST NatHoliday WHERE
                             NatHoliday.Holiday = idtDueDate)
      THEN idtDueDate = idtDueDate + 1.
      
      ELSE LEAVE.
   END.
   
   RETURN idtDueDate.
   
END FUNCTION.
&ENDIF.


