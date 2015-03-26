&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/* MIP-GET-OBJECT-VERSION pre-processors
   The following pre-processors are maintained automatically when the object is
   saved. They pull the object and version from Roundtable if possible so that it
   can be displayed in the about window of the container */

&scop object-name       starfop.p
&scop object-version    000000

DEFINE VARIABLE ghSocket    AS HANDLE     NO-UNDO.

DEFINE VARIABLE gcResponce  AS CHARACTER  NO-UNDO.

DEFINE VARIABLE glDebug AS LOGICAL INITIAL NO NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 21.57
         WIDTH              = 70.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-addXML) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addXML Procedure 
PROCEDURE addXML :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER icXML    AS CHARACTER  NO-UNDO.

    RUN sendString ( 'XML ' + icXML, YES ).

    RETURN gcResponce.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeSession Procedure 
PROCEDURE InitializeSession :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER icXSL       AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icFormat    AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE lcc AS CHARACTER  NO-UNDO.

    RUN sendString ( 'XSL ' + icXSL, YES ).
    IF NOT gcResponce BEGINS 'OK' THEN RETURN gcResponce.

    lcc = OS-GETENV('FOPDXMLDELETE').
    IF lcc = 'YES'
    THEN DO:
        RUN sendString ( 'DELETE', yes ).
        IF NOT gcResponce BEGINS 'OK' THEN RETURN gcResponce.
    END.

    RUN sendString ( 'FORMAT ' + icFormat, YES ).
    IF NOT gcResponce BEGINS 'OK' THEN RETURN gcResponce.

    lcc = OS-GETENV('FOPDSTATUS').
    IF lcc <> '' AND lcc <> ?
    THEN DO:
        RUN sendString ( 'STATUS ' + lcc, YES ).
        IF NOT gcResponce BEGINS 'OK' THEN RETURN gcResponce.
    END.

    RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessSession Procedure 
PROCEDURE ProcessSession :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER icWaitMode AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE lcReturn AS CHARACTER INITIAL 'OK' NO-UNDO.

    CASE icWaitMode:

        WHEN 'DONE' THEN DO:

            RUN sendString (icWaitMode, YES).
            lcReturn = gcResponce.

        END.

        WHEN 'WAIT' THEN DO:

            RUN sendString (icWaitMode, YES).
            lcReturn = gcResponce.

            IF gcResponce BEGINS "OK WAIT" THEN DO:
                RUN sendString ('WAITING', NO).
                lcReturn = gcResponce.

                WAIT-FOR READ-RESPONSE OF ghSocket.
                lcReturn = gcResponce.
            END.

        END.

        OTHERWISE lcReturn = 'ERROR WaitMode "' + icWaitMode + '" unknown!'.

    END CASE.

    ghSocket:DISCONNECT().
    DELETE OBJECT ghSocket.

    RETURN lcReturn.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-QuickLauncher) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QuickLauncher Procedure 
PROCEDURE QuickLauncher :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER icXSL       AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icFormat    AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icXML       AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icWaitMode  AS CHARACTER  NO-UNDO.
    
    RUN StartSession.
    
    IF RETURN-VALUE BEGINS "ok" THEN DO:
    
        RUN InitializeSession ( icXSL, icFormat ).
        IF NOT RETURN-VALUE BEGINS "ok" THEN RETURN RETURN-VALUE.
    
        RUN addXML ( icXML ).
        IF NOT RETURN-VALUE BEGINS "ok" THEN RETURN RETURN-VALUE.

        RUN ProcessSession ( icWaitMode ).
    
    END.
          
    RETURN RETURN-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-readResponce) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE readResponce Procedure 
PROCEDURE readResponce :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Read procedure for socket */
    DEFINE VARIABLE mBuffer AS MEMPTR NO-UNDO.

    SET-SIZE(mBuffer) = SELF:GET-BYTES-AVAILABLE() + 10.

    SELF:READ(mBuffer,1,SELF:GET-BYTES-AVAILABLE()).

    gcResponce = GET-STRING(mBuffer,1). 
    
    SET-SIZE(mBuffer) = 0.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendString Procedure 
PROCEDURE SendString :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER icCommand      AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ilWaitResponce AS LOGICAL    NO-UNDO.

    DEFINE VARIABLE lmBuffer  AS MEMPTR NO-UNDO.
    DEFINE VARIABLE lcCommand AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE li        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE liLength  AS INTEGER    NO-UNDO.
    
    ASSIGN 
        lcCommand               = icCommand + "~r~n"
        SET-SIZE(lmBuffer)      = 0
        SET-SIZE(lmBuffer)      = LENGTH(lcCommand) + 4
        PUT-STRING(lmBuffer,1)  = lcCommand
        .
    ghSocket:WRITE(lmBuffer,1,LENGTH(lcCommand)).
    
    gcResponce = "OK".
    IF ilWaitResponce
    THEN DO:
        gcResponce = 'ERROR Responce timeout.~nInput "' + icCommand + '"'.
        WAIT-FOR READ-RESPONSE OF ghSocket PAUSE 60.
    END.
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSession Procedure 
PROCEDURE StartSession :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    CREATE SOCKET ghSocket.
    
    DEFINE VARIABLE lcc AS CHARACTER  NO-UNDO.

    lcc = OS-GETENV('FOPDCONNECT').
    IF lcc = '' OR lcc = ? 
    THEN RETURN 'ERROR Fpod parameter FOPDCONNECT missing'.

    ghSocket:CONNECT (lcc).

    IF NOT ghSocket:CONNECTED() 
    THEN RETURN "ERROR Could not connect".

    /* Do READ-RESPONSE event handling */
    ghSocket:SET-READ-RESPONSE-PROCEDURE( "readResponce").

    lcc = OS-GETENV('FOPDPASSWORD').
    IF lcc = '' OR lcc = ? 
    THEN RETURN 'ERROR Fopd parameter FOPDPASSWORD missing'.

    RUN sendString ( 'LOGIN ' + lcc, YES ).
    IF NOT gcResponce BEGINS 'OK' THEN RETURN gcResponce.


    RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

