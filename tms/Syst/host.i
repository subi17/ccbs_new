{commali.i}
{cparam2.i}

DEF STREAM sHost.

FUNCTION fIsThisReplica RETURN LOG:
   
   DEF VAR lcReplica AS CHAR NO-UNDO.
   DEF VAR lcHost    AS CHAR NO-UNDO.

   lcReplica = fCParamC("ReplicaHost").
   RUN pHostName(OUTPUT lcHost).

   RETURN (lcReplica > "" AND lcHost = lcReplica).
    
END FUNCTION.
   
  
PROCEDURE pHostName:

   DEF OUTPUT PARAMETER ocHost AS CHAR NO-UNDO.

   INPUT STREAM sHost THROUGH hostname.
   IMPORT STREAM sHost ocHost.
   INPUT STREAM sHost CLOSE.
 
END PROCEDURE.


