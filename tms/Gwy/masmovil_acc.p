{Syst/tmsconst.i}
{Func/log.i}
{Func/masmovileif.i}
{Mc/orderfusion.i}
DEF INPUT PARAM piOrderID AS INT NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR lcResultDesc AS CHAR NO-UNDO. 
DEF VAR lcResultCode AS CHAR NO-UNDO. 
DEF VAR liRetry AS INT NO-UNDO. 

lcError = fInitMMConnection().
IF lcError NE "" THEN RETURN lcError.

DO liRetry = 1 to 3:

   lcError = fMasmovil_ACC
      (piOrderID,
       OUTPUT lcResultCode,
       OUTPUT lcResultDesc).
  
   IF lcError EQ "OK" THEN RETURN "OK".
   
   IF NOT fCanRetryMasmovilMessage(
            lcError,
            lcResultCode,
            lcResultDesc) THEN LEAVE.
   PAUSE 15 NO-MESSAGE.
END.

RETURN "ERROR:" +
        SUBST("&1, &2, &3", lcError, lcResultCode, lcResultDesc).

FINALLY:
   xmlrpc_finalize().
END.
