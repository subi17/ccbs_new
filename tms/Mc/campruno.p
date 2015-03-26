 /* ------------------------------------------------------
  MODULE .......: campruno.p
  FUNCTION .....: campaign run for order
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 10.08.04
  MODIFIED .....: 11.10.04/aam don't return any error text if camprun returns 0
                  09.11.05/aam take CustNum from MobSub, not Order  
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{camprundf.i}

DEF INPUT  PARAMETER iiOrder AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER ocError AS CHAR NO-UNDO. 

DEF VAR liCount AS INT NO-UNDO.

DEF STREAM sLog.
OUTPUT STREAM sLog TO /apps/tms/snet/camruno.log append.

PUT STREAM sLog UNFORMATTED iiOrder " begin" SKIP.

FIND Order NO-LOCK WHERE 
     Order.Brand   = gcBrand AND
     Order.OrderID = iiOrder NO-ERROR.
IF NOT AVAILABLE Order THEN DO:
   ocError = "Unknown order".
   RETURN.
END.

IF Order.Campaign = "" THEN DO:
   ocError = "Campaign not defined".
   RETURN.
END.

IF Order.MSSeq = 0 THEN DO:
   ocError = "CLI not defined".
   RETURN.
END.

FIND Customer NO-LOCK WHERE 
     Customer.CustNum = Order.CustNum NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   ocError = "Unknown customer".
   RETURN.
END.

FIND MobSub WHERE MobSub.MsSeq = Order.MsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN DO:
   ocError = "CLI not found".
   RETURN.
END.

CREATE ttCust.
ASSIGN ttCust.CustNum = MobSub.CustNum
       ttCust.MSSeq   = MobSub.MSSeq.
       
RUN camprun (INPUT TABLE ttCust,
             Order.Campaign,
             Order.Campaign,
             Order.CrStamp,
             FALSE,
             OUTPUT liCount).

PUT STREAM sLog UNFORMATTED iiOrder ";" liCount " created" SKIP.
OUTPUT STREAM sLog CLOSE.

