/*-----------------------------------------------------------------------------
  MODULE .......: msreqorsme
  FUNCTION .....: ms request menu, order request type 13
  SOVELLUTUS ...: TMS
  AUTHOR .......: ? 
  CREATED ......: 
  changePVM ....: 16.04.07/aam new parameters to tmscodesbr
                  31.10.07 jp  new parameter for msrequest
                  
  Version ......: M15
  -------------------------------------------------------------------------- */

{commali.i}
DEF INPUT PARAMETER ipMsSeq AS INTEGER NO-UNDO.
DEF VAR lcStatus AS CHAR NO-UNDO.

DO WHILE TRUE:

   RUN tmscodesbr(INPUT  "MsRequest",
                  INPUT  "ReqStatus",
                  INPUT  "",
                  INPUT  "Order requests (type 13)",
                  INPUT  "13",
                  OUTPUT lcStatus).

   IF lcStatus > "" THEN DO:
      RUN msrequest(13,INTEGER(lcStatus),ipMsSeq,0,0,"").
   END.
   ELSE LEAVE.

END.

HIDE MESSAGE NO-PAUSE.


