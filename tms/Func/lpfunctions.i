/* ----------------------------------------------------------------------
  MODULE .......: lpfunctions.i
  TASK .........: Functions for handling LP command sending
  APPLICATION ..: tms
  AUTHOR .......: ilsavola & jtorres
  VERSION.......:
  CREATED ......: 29.8.2017
  CHANGED ......:
  ------------------------------------------------------------------------*/
&IF "{&fLPFUNCTIONS}" NE "YES"
&THEN
&GLOBAL-DEFINE fLPFUNCTIONS YES
/*{Syst/commali.i}*/
{Func/timestamp.i}
{Func/fmakemsreq.i}

/*Function creates MsRequest for Landing Page redirection settings*/
FUNCTION fMakeLPCommandRequest RETURNS LOGICAL
   (INPUT iiMsSeq   AS INT,       /*Subscription identifier*/
    INPUT icCommand AS CHAR,      /*LP command to network*/
    INPUT iiCustNum AS INT,       /*Customer number for memo*/
    INPUT icMemoTitle AS CHAR,    /*Memo title. Empty -> no memo writing*/
    INPUT icMemoText AS CHAR,     /*Memo text*/
    INPUT icMemoCreator AS CHAR,  /*Creator tag for memo*/
    INPUT-OUTPUT ocErr AS CHAR):  /*Request creation info*/
   DEF VAR liReq AS INT NO-UNDO.
   DEF VAR lcError AS CHAR NO-UNDO.

   liReq = fServiceRequest (iiMsSeq,
                            "LP",              /*servcom*/
                            1,                 /* */
                            icCommand,         /* actual cmd */
                            fSecOffSet(fMakeTS(),5),
                            "",                /* SalesMan */
                            FALSE,             /* Set fees */
                            FALSE,             /* SMS */
                            "",                /* creator */
                            "",                /* source */
                            0,                 /* orig request */
                            FALSE,             /* mandatory */
                            OUTPUT lcError).

   IF liReq = 0 OR liReq = ? THEN DO:
      ocErr = "LP Req Error: ServiceRequest failure " + lcError.
      RETURN FALSE.
   END.   
   ocErr = "LP request for: " + icCommand + " created successfully".
  
   IF icMemoTitle NE "" THEN
      DYNAMIC-FUNCTION("fWriteMemoWithType" IN ghFunc1,
                       "Mobsub",
                       STRING(iiMsSeq),
                       iiCustNum,
                       icMemoTitle,
                       icMemoText,
                       "Service",          /* memo type */
                       icMemoCreator).     /* creator */
   RETURN TRUE.
END. /* fMakeLPCommandRequest */

&ENDIF
