/*-----------------------------------------------------------------------------
  MODULE .......: msreqocme
  FUNCTION .....: ms request menu, owner changes
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 28.03.06
  changePVM ....: 
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}

DEF VAR lcStatus AS CHAR NO-UNDO.

DO WHILE TRUE:

   RUN tmscodesbr(INPUT  "MsRequest",
                  INPUT  "ReqStatus",
                  INPUT  "",
                  INPUT  "Agr.Customer Changes",
                  INPUT  "10",
                  OUTPUT lcStatus).

   IF lcStatus > "" THEN RUN ownerreq (0,0,INTEGER(lcStatus)).
   ELSE LEAVE.

END.

HIDE MESSAGE NO-PAUSE.


