/* feventlog.i       15.02.06/aam
*/


/* log from viewing delicate information */
FUNCTION fViewEvent RETURNS LOGICAL
   (icTableName AS CHAR,
    icKey       AS CHAR):
    
    
   DO TRANS:
      CREATE EventLog.
      ASSIGN EventLog.TableName      = icTableName
             EventLog.Key            = icKey
             EventLog.EventDate      = TODAY
             EventLog.EventTime      = STRING(TIME,"hh:mm:ss")
             EventLog.UserCode       = katun
             EventLog.Action         = "View"
             EventLog.EventLogStatus = 5.
      RELEASE EventLog.
   END. 

END FUNCTION.


