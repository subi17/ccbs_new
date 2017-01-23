DEFine variable lStopped as logical init true.
DEFINE VARIABLE lError   AS LOGICAL NO-UNDO.
DEFINE VARIABLE liDBid AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcDBname AS CHARACTER NO-UNDO. 

DB_LOOP:
DO liDBid = 1 TO NUM-DBS with frame a.
   lcDBname = LDBNAME(liDBid).

   DISP lcDBname format "x(12)".

   DO ON STOP UNDO, LEAVE
      ON QUIT UNDO, LEAVE.
       
       /* create domain, tenants and users */
       RUN create_tenants.p(lcDBname).

       lStopped = FALSE.

       CATCH e AS Progress.Lang.Error.
           lError = TRUE.
           message "Error loading database:" e:GetMessage(1) view-as alert-box.
           leave DB_LOOP.
       END.
   END.

   IF NOT lError THEN DO:

       IF lStopped THEN DO:
         message "STOP condition raised. Failed to load database" view-as alert-box.
         LEAVE DB_LOOP.
       end.
       ELSE
           disp "Database loaded successfully".
   END.
   ELSE LEAVE DB_LOOP.

END.

