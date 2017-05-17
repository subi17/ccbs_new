/* -----------------------------------------------
  MODULE .......: tcpgwy_fake.
  FUNCTION .....: Simulate Response from TCP port 
  APPLICATION ..: TMS
  CREATED ......: 
  MODIFIED .....: 
  VERSION ......: XFERA
------------------------------------------------------ */

{Syst/commali.i}
{Func/timestamp.i}
gcBrand = "1".

DEF VAR lcHostname AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF LOOKUP(lcHostName,'arneb,leto,maja,pallas') > 0 THEN DO:
   MESSAGE 'This script is not allowed to run in'
   lcHostName VIEW-AS ALERT-BOX.
   RETURN.
END.

/*
PARAMETERS:
 -pcRequest: message to read and response
 -pcURL: server/host to connect  -> my identifier ! 
 -piTimeOut: -> not in use
 -piLoop: -> not in use
 -pcCheck: -> not in use
*/

DEFINE INPUT PARAMETER pcRequest  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcURL      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER piTimeOut  AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER piLoops    AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER pcCheck    AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcResponse  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcedure AS CHARACTER NO-UNDO. 

lcProcedure = "mocks/Gwy/tcpgwy_fake_" + pcURL + ".p".

RUN VALUE(lcProcedure) (pcRequest) NO-ERROR.
lcResponse = RETURN-VALUE.

RETURN lcResponse.
