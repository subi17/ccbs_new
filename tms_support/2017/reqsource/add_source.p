/*FOR EACH tmscodes where 
         tmscodes.fieldname eq "reqsource"
   NO-LOCK:
   disp tmscodes.
END.
*/

FIND FIRST tmscodes no-lock where 
           tmscodes.tablename eq "MsRequest" AND 
           tmscodes.FieldName EQ "ReqSource" AND 
           tmscodes.CodeValue EQ "36" NO-ERROR.
IF AVAIL tmscodes THEN DO:
   MESSAGE "Already found" VIEW-AS ALERT-BOX. 
   QUIT.
END.
           
CREATE tmscodes.
ASSIGN 
   tmscodes.TableName = "MsRequest"
   tmscodes.FieldName = "ReqSource"
   tmscodes.CodeGroup = "Request"
   tmscodes.CodeValue = "36"
   tmscodes.CodeName = "Q25 HRLP"
   tmscodes.InUse = 1.


DISP tmscodes.TableName.
DISP tmscodes.FieldName.
DISP tmscodes.CodeGroup.
DISP tmscodes.CodeValue.
DISP tmscodes.CodeName.
DISP tmscodes.InUse.
