/* ----------------------------------------------------------------------
  MODULE .......: convview.p
  TASK .........: Display Convergent offer datga
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 6.10.2016
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i}

DEF INPUT PARAMETER iiOrderId     AS INT  NO-UNDO.



FORM
   SKIP(1)
   iiOrderid     COLON 20 LABEL "Select Function"  /*FORMAT "X(8)"*/ SKIP
   SKIP(1)

   WITH ROW 4 OVERLAY SIDE-LABELS CENTERED 
        TITLE " WRITE HEADER HERE XXX "  
        FRAME fMessages.



PAUSE 0 NO-MESSAGE.
VIEW FRAME fMessages. 
CLEAR FRAME fMessages NO-PAUSE.

LOOP:
REPEAT WITH FRAME fMessages ON ENDKEY UNDO LOOP, NEXT LOOP:

   PAUSE 0.
/*   DISPLAY DMS.CaseTypeID
           DMS.ContractID
           DMS.DmsExternalID
           DMS.DMSID
           DMS.HostID
           DMS.HostTable
           DMS.StatusCode
           DMS.StatusDesc
           DMS.DMSStatusTS.
*/
   ASSIGN
      ufk   = 0  
      ufk[5]= 9850
      ufk[6]= 9850
      ufk[8]= 8 
      ehto  = 0.
   RUN ufkey.

   IF toimi EQ 5 THEN DO:
     /* RUN dmsdoc.p (DMS.DMSID).*/
     MESSAGE "5 pressed" VIEW-AS ALERT-BOX.
   END.
   IF toimi EQ 6 THEN DO:
     /* RUN dmsdoc.p (DMS.DMSID).*/
     MESSAGE "6 pressed" VIEW-AS ALERT-BOX.
   END.
   
   ELSE IF toimi = 8 THEN LEAVE.

END. 

HIDE MESSAGE NO-PAUSE.
HIDE FRAME lis NO-PAUSE.         
