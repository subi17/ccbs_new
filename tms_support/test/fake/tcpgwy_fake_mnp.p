
/* Fake mnp response to mnpsend */
DEF VAR lcHostname AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF LOOKUP(lcHostName,'angetenar,alpheratz,sadachbia,yanai') = 0 THEN DO:
   MESSAGE 'This script is not allowed to run in'
   lcHostName VIEW-AS ALERT-BOX.
   RETURN.
END.


DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO. 
lcResponse  = "" . /* nothing implemented by the moment */
RETURN lcResponse .
