/* fecgtask.i       19.03.03/aam 
   run a task when a member either enters or leaves a customer group

                    09.09.03/aam brand
*/

FUNCTION fECGEnterTask RETURNS CHARACTER
   (ilInteract AS LOGIC).

   DEF VAR lcEnterTask AS CHAR NO-UNDO. 

   IF NOT AVAILABLE CustGroup OR
      CustGroup.Brand     NE CGMember.Brand OR
      CustGroup.CustGroup NE CGMember.CustGroup 
   THEN FIND CustGroup OF CGMember NO-LOCK NO-ERROR.
   IF NOT AVAILABLE CustGroup THEN RETURN "GRPERROR". 

   IF CustGroup.EnterTask NE "" THEN DO:

      IF SEARCH(CustGroup.EnterTask) = ?        AND
         SEARCH(CustGroup.EnterTask + ".r") = ?
      THEN DO:
         IF ilInterAct THEN 
         MESSAGE "Task, that is defined to be run when member is added" SKIP
                 "to this group (" CustGroup.EnterTask "), can not be"  SKIP
                 "located. Check the definition and run the procedure"  SKIP
                 "manually for this member if necessary."
         VIEW-AS ALERT-BOX
         ERROR.
         RETURN "TASKERROR".
      END.

      RUN VALUE(CustGroup.EnterTask) (CGMember.CustGroup,
                                      CGMember.CustNum,
                                      OUTPUT lcEnterTask) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN RETURN "RUNERROR".

      ELSE RETURN lcEnterTask. 
   END.

   ELSE RETURN "".

END FUNCTION.


FUNCTION fECGLeaveTask RETURNS CHARACTER
   (ilInteract AS LOGIC).

   DEF VAR lcLeaveTask AS CHAR NO-UNDO. 

   IF NOT AVAILABLE CustGroup OR
      CustGroup.Brand     NE CGMember.Brand OR
      CustGroup.CustGroup NE CGMember.CustGroup 
   THEN FIND CustGroup OF CGMember NO-LOCK NO-ERROR.
   IF NOT AVAILABLE CustGroup THEN RETURN "GRPERROR". 

   IF CustGroup.LeaveTask NE "" THEN DO:

      IF SEARCH(CustGroup.LeaveTask) = ?        AND
         SEARCH(CustGroup.LeaveTask + ".r") = ?
      THEN DO:
         IF ilInterAct THEN 
         MESSAGE "Task, that is defined to be run when member is removed" SKIP
                 "from this group (" CustGroup.LeaveTask "), can not be"  SKIP
                 "located. Check the definition and run the procedure"    SKIP
                 "manually for this member if necessary."
         VIEW-AS ALERT-BOX
         ERROR.
         RETURN "TASKERROR".
      END.

      RUN VALUE(CustGroup.LeaveTask) (CGMember.CustGroup,
                                      CGMember.CustNum,
                                      OUTPUT lcLeaveTask) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN RETURN "RUNERROR".

      ELSE RETURN lcLeaveTask. 
   END.

   ELSE RETURN "".


END FUNCTION.

