{Syst/tmsconst.i}
{Func/log.i}
{Func/masmovileif.i}
{Mc/orderfusion.i}
DEF INPUT PARAM piOrderID AS INT NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR lcResultDesc AS CHAR NO-UNDO. 
DEF VAR lcResultCode AS CHAR NO-UNDO. 

lcError = fInitMMConnection().
IF lcError NE "" THEN RETURN lcError.

lcError = fMasmovil_ACC
   (piOrderID,
    OUTPUT lcResultCode,
    OUTPUT lcResultDesc).

IF lcError EQ "OK" THEN RETURN "OK".

RETURN "ERROR:" +
        SUBST("&1, &2, &3", lcError, lcResultCode, lcResultDesc).

FINALLY:
   xmlrpc_finalize().
END.
