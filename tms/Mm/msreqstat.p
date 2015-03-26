/*-----------------------------------------------------------------------------
  MODULE .......: msreqstat
  FUNCTION .....: ms request status menu
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 16.04.07
  changePVM ....: 31.10.07 jp  new parameter for msrequest
  
  Version ......: Yoigo
  -------------------------------------------------------------------------- */

{commali.i}

DEF INPUT PARAMETER iiReqType AS INT NO-UNDO.
DEF INPUT PARAMETER iiMsSeq   AS INT NO-UNDO.

DEF VAR lcStatus  AS CHAR NO-UNDO.
DEF VAR lcReqName AS CHAR NO-UNDO.
DEF VAR lcSkip    AS CHAR NO-UNDO.

FIND RequestType WHERE
     RequestType.Brand   = gcBrand AND
     RequestType.ReqType = iiReqType NO-LOCK NO-ERROR.
   
IF AVAILABLE RequestType THEN lcReqName = RequestType.ReqName.

/* values that should not be shown */
CASE iiReqType:
WHEN 20 THEN lcSkip = "5,6".
END CASE.

DO WHILE TRUE:

   RUN tmscodesbr("MsRequest",
                  "ReqStatus",
                  lcSkip,
                  lcReqName,
                  STRING(iiReqType) + ";" + STRING(iiMsSeq),
                  OUTPUT lcStatus).

   IF lcStatus > "" THEN 
      RUN msrequest (iiReqType,
                     INTEGER(lcStatus),
                     iiMsSeq,
                     0,
                     0,
                     "").
   ELSE LEAVE.

END.

HIDE MESSAGE NO-PAUSE.


