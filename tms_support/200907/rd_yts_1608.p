

 /* Request status should be changed manualy
    from status 1 -> 7 -> 8 and after that is handled */

 DEFINE VARIABLE icMSISDN  AS CHARACTER NO-UNDO. 
 
 icMSISDN = "622744694".
 
 FIND MSRequest NO-LOCK WHERE
      MSRequest.Brand = "1" AND
      MSRequest.ReqType = 35 AND
      MSRequest.ReqStatus = 7 AND
      MSRequest.CLI = icMSISDN NO-ERROR.

 IF AVAIL MSRequest THEN DO:
    FIND CURRENT MSRequest EXCLUSIVE-LOCK .
    ASSIGN MSRequest.ReqStatus = 8.
 END.

