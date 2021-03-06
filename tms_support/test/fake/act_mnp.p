{Syst/commali.i}

DEF VAR lcHostname AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF LOOKUP(lcHostName,'angetenar,alpheratz,sadachbia,yanai') = 0 THEN DO:
   MESSAGE 'This script is not allowed to run in'
   lcHostName VIEW-AS ALERT-BOX.
   RETURN.
END.

DEFINE VARIABLE lcFormREquest AS CHARACTER NO-UNDO.

UPDATE lcFormREquest LABEL "MNP Process FormRequest ID" FORMAT "x(12)".

find MNPProcess EXCLUSIVE-LOCK WHERE
   MNPProcess.Formrequest = lcFormREquest NO-ERROR.

IF NOT AVAIL MNPProcess THEN do:
   MESSAGE "Unknown formrequest:" lcFormREquest VIEW-AS ALERT-BOX.
   leave.
end.

DEFINE VARIABLE liOrderQty AS INTEGER NO-UNDO.
/* create request */
RUN Gwy/ordersender.p(MNPProcess.OrderId,
               OUTPUT liOrderQty).
FIND FIRST order EXCLUSIVE-LOCK WHERE
   order.brand = "1" and
   order.orderid = MNPProcess.orderid NO-ERROR.

IF NOT AVAIL Order THEN do:
   MESSAGE "Unknown formrequest:" lcFormREquest VIEW-AS ALERT-BOX.
   leave.
end.

MNPProcess.StatusCode = 5.
Order.MNPStatus = MNPProcess.StatusCode + 1.
MNPProcess.UpdateTS = Func.Common:mMakeTS().

MESSAGE "Order " Order.OrderID " mnp process handled" VIEW-AS ALERT-BOX.
