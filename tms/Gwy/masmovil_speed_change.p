
{Syst/tmsconst.i}
{Func/log.i}
{Func/memo.i}

DEF INPUT PARAM piMsRequest AS INT NO-UNDO.
DEF VAR lcFixedNumber AS CHAR NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR lcResultCode AS CHAR NO-UNDO. 
DEF VAR lcResultDesc AS CHAR NO-UNDO. 

FIND FIRST MSRequest WHERE
           MSRequest.MSrequest = piMsRequest NO-LOCK NO-ERROR.

IF NOT AVAIL MSRequest THEN
   RETURN "MsRequest not found".

cError = fInitMMConnection().
IF lcError NE "" THEN 
    RETURN lcError.

lcError = fSetSpeed_Masmovil(MSRequest.MSRequest,
                             OUTPUT lcResultCode,
                             OUTPUT lcResultDesc).

IF lcError EQ "OK" THEN DO:
   RETURN "".
ELSE DO:
   fCreateMemo("MsRequest",
               STRING(MsRequest.MsRequest),
               MsRequest.CustNum,
               "Masmovil order creation failed",
                 SUBST("ErrorCode: &1", (IF lcResultDesc > ""
                                       THEN lcResultDesc 
                                       ELSE lcError)),
               "",
               "TMS").

   RETURN SUBST("&1, &2, &3", lcError, lcResultCode, lcResultDesc).
END.

FINALLY:
   xmlrpc_finalize().
END.
