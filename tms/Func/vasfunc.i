/* ----------------------------------------------------------------------
  MODULE .......: vasfunc.i
  TASK .........: Functions for handling Value Added Service related functions
                  Reference: YPRO-project
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......:
  CREATED ......: 14.6.2017
  CHANGED ......:
  ------------------------------------------------------------------------*/
 
{Syst/tmsconst.i}
&IF "{&VASFUNC_I}" NE "YES"
&THEN
&GLOBAL-DEFINE VASFUNC_I YES

{Func/orderfunc.i}

/*SVA of Yoigo PRO*/
/*This function provides a dirty solution.
Later in YPRO project we will consider if it is reason to make 
1) tmsparam to configure the SVAs
2) own table to configure SVA needs
3) make wider solution safe mapping table for the services */
FUNCTION fIsSVA RETURNS LOGICAL
   (INPUT icService AS CHAR,
    OUTPUT oiParams AS INT):
   oiParams = 0. 
   IF icService EQ "FAXTOEMAIL" THEN DO:
      oiParams = 2.
      RETURN TRUE.
   END.
   ELSE IF icService EQ "OFFICE365" THEN DO:
      oiParams = 1.
      RETURN TRUE.
   END.
   ELSE IF icService EQ "SAGEONE" THEN DO:
      RETURN TRUE.
   END.
   ELSE IF icService EQ "IPFIJA" THEN DO:
      RETURN TRUE.
   END.
   ELSE IF icService EQ "CentralitaPRO" THEN DO:
      RETURN TRUE.
   END.

   RETURN FALSE.
END.


&ENDIF


