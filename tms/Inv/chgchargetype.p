/* ----------------------------------------------------------------------
  MODULE .......: chgchargetype.p
  TASK .........: change charge type to odi request
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 29.11.07
  CHANGED ......:
  Version ......: yoigo
----------------------------------------------------------------------- */
{Func/msreqfunc.i}
{Func/fuserright.i}

DEFINE INPUT PARAMETER iiMsRequest  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iiFromStatus AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iiToStatus   AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER icCparam     AS CHARACTER NO-UNDO.

DEF VAR llOk         AS LOG  NO-UNDO.
DEF VAR ldtPaymDate  AS DATE NO-UNDO.
DEF VAR liChargeType AS INT  NO-UNDO.
DEF VAR lcCode       AS CHAR NO-UNDO.

/* eventlog not needed here, msrequest.p takes care of it */

FIND MsRequest WHERE MsRequest.MsRequest = iiMsRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 20 THEN DO:
   MESSAGE "Unknown request"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

IF MsRequest.ReqStat NE 0 THEN DO:
   MESSAGE "Refund is not in a valid status"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

/* has user got priviliges */
IF fTokenRights(katun,"CCSUPER") NE "RW" THEN DO:
   MESSAGE "You are not authorized to use this function"
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.

ehto = 9.
RUN Syst/ufkey.

liChargeType = MsRequest.ReqIParam2.

REPEAT ON ENDKEY UNDO, LEAVE:

   UPDATE liChargeType
       FORMAT ">9"
       HELP "Invoice charge type"
       LABEL "Charge Type"
   WITH OVERLAY ROW 10 CENTERED SIDE-LABELS TITLE " CHARGE TYPE "
      FRAME fCharge
   EDITING:
         
      READKEY.
         
      IF KEYLABEL(LASTKEY) = "F9" THEN DO:
            
         RUN Help/h-tmscodes(INPUT "Invoice",      /* TableName */
                              "ChargeType",   /* FieldName */
                              "AccRec",       /* GroupCode */
                        OUTPUT lcCode).
              
         IF lcCode ne "" AND lcCode NE ? THEN DO:
            DISPLAY INTEGER(lcCode) ;& liChargeType
            WITH FRAME fCharge.   
         END.

         ehto = 9.
         RUN Syst/ufkey.
         NEXT. 
      END.

      ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
      THEN DO WITH FRAME fCharge:
   
         IF INPUT liChargeType > 0 AND 
            NOT CAN-FIND(FIRST 
                     TMSCodes WHERE
                     TMSCodes.TableName = "Invoice"    AND
                     TMSCodes.FieldName = "ChargeType" AND
                     TMSCodes.CodeValue = STRING(INPUT liChargeType))
         THEN DO:
            MESSAGE "Unknown charge type"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
      END.
            
      APPLY LASTKEY.
   END.
 
   LEAVE.
END.

        
IF liChargeType NE MsRequest.ReqIParam2 THEN DO:

   MESSAGE "Change charge type?"
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   SET llOk.

END.

HIDE FRAME fPaymDate NO-PAUSE.

IF NOT llOk THEN RETURN.

FIND CURRENT MsRequest EXCLUSIVE-LOCK.
MsRequest.ReqIParam2 = liChargeType.


