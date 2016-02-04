/* ----------------------------------------------------------------------
  MODULE .......: dms.p
  TASK .........: Display tables DMS and DMSDoc
  APPLICATION ..: tms
  AUTHOR .......: ivekov
  CREATED ......: 7.9.2015
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}

DEF INPUT PARAMETER iiOrderId     AS INT  NO-UNDO.
DEF INPUT PARAMETER icContractId  AS CHAR NO-UNDO.

FORM
   SKIP(1)
   DMS.CaseTypeID     COLON 20 LABEL "Case Type"          FORMAT "X(8)"             SKIP
   DMS.ContractID     COLON 20 LABEL "ContractID"         FORMAT "X(8)"             SKIP
   DMS.DmsExternalID  COLON 20 LABEL "ID in DMS"          FORMAT "X(12)"            SKIP
   DMS.DMSID          COLON 20 LABEL "DMSID"              FORMAT ">>>>>>>9"         SKIP
   DMS.HostID         COLON 20 LABEL "Host ID"            FORMAT ">>>>>>>9"         SKIP
   DMS.HostTable      COLON 20 LABEL "Host Table"         FORMAT "X(12)"            SKIP
   DMS.StatusCode     COLON 20 LABEL "Status Code"        FORMAT "X(12)"            SKIP
   DMS.StatusDesc     COLON 20 LABEL "Status Description" FORMAT "X(20)"            SKIP
   DMS.DMSStatusTS    COLON 20 LABEL "DMS Time Stamp"     FORMAT "99999999.99999"
   SKIP(1)

   WITH ROW 4 OVERLAY SIDE-LABELS CENTERED 
        TITLE " DOCUMENT MANAGEMENT, ORDER " + STRING(iiOrderId) + " " 
        FRAME lis.

FIND DMS WHERE DMS.HostTable  = "Order" AND
               DMS.HostID     = iiOrderId  AND
               DMS.ContractID = icContractId
               NO-LOCK NO-ERROR.

IF NOT AVAIL DMS THEN DO:
   MESSAGE "DMS was not found" VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

PAUSE 0 NO-MESSAGE.
VIEW FRAME lis. 
CLEAR FRAME lis NO-PAUSE.

LOOP:
REPEAT WITH FRAME lis ON ENDKEY UNDO LOOP, NEXT LOOP:

   PAUSE 0.
   DISPLAY DMS.CaseTypeID
           DMS.ContractID
           DMS.DmsExternalID
           DMS.DMSID
           DMS.HostID
           DMS.HostTable
           DMS.StatusCode
           DMS.StatusDesc
           DMS.DMSStatusTS.

   ASSIGN
      ufk   = 0  
      ufk[5]= 9850
      ufk[8]= 8 
      ehto  = 0.
   RUN Syst/ufkey.

   IF toimi = 5 AND ufk[5] > 0 THEN DO:
      RUN Mc/dmsdoc.p (DMS.DMSID).
   END.
   
   ELSE IF toimi = 8 THEN LEAVE.

END. 

HIDE MESSAGE NO-PAUSE.
HIDE FRAME lis NO-PAUSE.         
