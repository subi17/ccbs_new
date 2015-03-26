/* monitoring script for checking ROI order queue counts. SER-4202*/
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcStatuses AS CHAR EXTENT 4.
DEFINE VARIABLE liCount AS INT EXTENT 4.

ASSIGN
   lcStatuses[1] = "41"
   lcStatuses[2] = "42"
   lcStatuses[3] = "43"
   lcStatuses[4] = "44".

DO i = 1 TO 4:
   FOR EACH Order NO-LOCK WHERE 
            Order.brand = "1" and
            Order.StatusCode = lcStatuses[i]:
      liCount[i] = liCount[i] + 1.
   END.  
END.

message liCount[1] liCount[2] liCount[3] liCount[4].
quit.
