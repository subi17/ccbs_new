DEF VAR lcHostname AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF LOOKUP(lcHostName,'sadira,merak,botein,merga') = 0 THEN DO:
   MESSAGE 'This script is not allowed to run in'
   lcHostName VIEW-AS ALERT-BOX.
   RETURN.
END.

input from tmsparam_staging_update.d.

DEFINE TEMP-TABLE tttmsparam like tmsparam.
repeat:
   create tttmsparam.
   import tttmsparam no-error.
end.

FOR EACH tttmsparam NO-LOCK:

   if tttmsparam.paramcode eq "" then next.

   find tmsparam EXCLUSIVE-LOCK where
        tmsparam.brand = tttmsparam.brand and
        tmsparam.paramgroup = tttmsparam.paramgroup and
        tmsparam.paramcode = tttmsparam.paramcode
        no-error.

   IF not AVAIL tmsparam then do:
      create tmsparam.
   end.

   buffer-copy tttmsparam to tmsparam.
end.
