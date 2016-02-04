/*  lib/starfop.i

    Star fop library launch
    
    13.2.2003 TaH
    
*/

DEFINE VARIABLE ghFop AS HANDLE     NO-UNDO.
                           
/* try to find starfop from memory */
ghFop = SESSION:FIRST-PROCEDURE.
REPEAT:
    IF NOT VALID-HANDLE(ghFop) THEN LEAVE.
    IF ghFop:FILE-NAME MATCHES '*lib/starfop*' THEN LEAVE.
    ghFop = ghFop:NEXT-SIBLING.
END.                                     

/* if starfop is not running... start it */
IF NOT VALID-HANDLE(ghFop) THEN DO:
    RUN Mc/lib/starfop_hard2.p
    PERSISTENT SET ghFop NO-ERROR.
    IF NOT VALID-HANDLE(ghFop)
    THEN DO:
        MESSAGE 
            'StarFop initialize is not possible' SKIP
            'Cannot continue' SKIP
            'Abort'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN 'ERROR'.
    END.
END.            

THIS-PROCEDURE:ADD-SUPER-PROCEDURE(ghFop).

PROCEDURE addXML IN SUPER:
  DEFINE INPUT PARAMETER icXML AS CHARACTER.
  DEFINE INPUT PARAMETER iceMails AS CHARACTER.
END PROCEDURE.

PROCEDURE InitializeSession IN SUPER:
  DEFINE INPUT PARAMETER icXSL AS CHARACTER.
  DEFINE INPUT PARAMETER icFormat AS CHARACTER.
END PROCEDURE.

PROCEDURE ProcessSession IN SUPER:
  DEFINE INPUT PARAMETER icWaitMode AS CHARACTER.
END PROCEDURE.

PROCEDURE readResponce IN SUPER:
END PROCEDURE.

PROCEDURE SendString IN SUPER:
  DEFINE INPUT PARAMETER icCommand AS CHARACTER.
  DEFINE INPUT PARAMETER ilWaitResponce AS LOGICAL.
END PROCEDURE.

PROCEDURE StartSession IN SUPER:
END PROCEDURE.

PROCEDURE QuickLauncher IN SUPER:
    DEFINE INPUT  PARAMETER icXSL       AS CHARACTER.
    DEFINE INPUT  PARAMETER icFormat    AS CHARACTER.
    DEFINE INPUT  PARAMETER icXML       AS CHARACTER.
    DEFINE INPUT  PARAMETER icWaitMode  AS CHARACTER.
END PROCEDURE.
