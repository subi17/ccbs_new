/*  lib/tokenlib.i

    Token Library include + start rutine
    
    10.1.2003 TaH
 
*/

DEFINE VARIABLE ghTokenLib AS HANDLE     NO-UNDO.
                           
/* try to find tokenlib from memory */
ghTokenLib = SESSION:FIRST-PROCEDURE.
REPEAT:
    IF NOT VALID-HANDLE(ghTokenLib) THEN LEAVE.
    IF ghTokenLib:FILE-NAME MATCHES '*lib/tokenlib*' THEN LEAVE.
    ghTokenLib = ghTokenLib:NEXT-SIBLING.
END.                                     

/* prototypes... */
FUNCTION getTMSRight RETURNS CHARACTER
  (INPUT icTokens AS CHARACTER) IN ghTokenLib.

FUNCTION getTMSTableRight RETURNS CHARACTER
  (INPUT icTable AS CHARACTER) IN ghTokenLib.

FUNCTION setTMSUser RETURNS LOGICAL
  (INPUT icUserId AS CHARACTER) IN ghTokenLib.


/* if tokenlib is not running... start it */
IF NOT VALID-HANDLE(ghTokenLib) THEN DO:
    RUN Mc/lib/tokenlib PERSISTENT SET ghTokenLib.
    IF NOT setTMSUser(katun) THEN DO:
        MESSAGE 
            'Token initialize is not possible' SKIP
            'Cannot continue' SKIP
            'Abort'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN 'ERROR'.
    END.               
END.
