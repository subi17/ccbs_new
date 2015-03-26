DEFINE VARIABLE lcExtInvId AS CHARACTER NO-UNDO.

SET lcExtInvId FORMAT "X(12)" LABEL "External Invoice ID".

FOR EACH Invoice WHERE 
         Invoice.Brand    = "1"              AND 
         Invoice.ExtInvId = TRIM(lcExtInvId) EXCLUSIVE-LOCK:
    ASSIGN Invoice.ddstate = 0.
END. 
