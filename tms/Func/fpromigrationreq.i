/* fpromigrationreq.i         29.08.17/kaaikas

   create a request for pro subscription migration
*/

&IF "{&FPROMIGRREQ_I}" NE "YES"
&THEN

&GLOBAL-DEFINE FPROMIGRREQ_I YES

{Syst/commali.i}
{Func/fcreatereq.i}
{Syst/tmsconst.i}
{Func/timestamp.i}

FUNCTION fProMigrationRequest RETURNS INTEGER
   (INPUT  iiMsseq        AS INT,        /* msseq                */
    INPUT  icCreator      AS CHARACTER,  /* who made the request */
    INPUT  icSource       AS CHARACTER,
    OUTPUT ocResult       AS CHARACTER):

   DEF VAR liReqCreated AS INT NO-UNDO.
   DEF VAR ldActStamp AS DEC NO-UNDO.
   DEF BUFFER bMsRequest FOR MsRequest.

   ocResult = fChkRequest(iiMsSeq,
                          {&REQTYPE_PRO_MIGRATION},
                          "",
                          icCreator).

   IF ocResult > "" THEN RETURN 0.

   /* set activation time */
   ldActStamp = fMakeTS().

   fCreateRequest({&REQTYPE_PRO_MIGRATION},
                  ldActStamp,
                  icCreator,
                  FALSE,    /* create fees */
                  FALSE).   /* sms */

   ASSIGN
      bCreaReq.ReqCParam1  = "MIGRATE"
      bCreaReq.ReqSource   = icSource
      liReqCreated         = bCreaReq.MsRequest.

   RELEASE bCreaReq.

   RETURN liReqCreated.

END FUNCTION.

&ENDIF

