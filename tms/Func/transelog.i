/* transelog.i       08.09.06/aam
   eventlog for transactions 
*/


FUNCTION fTransactionLog RETURNS LOGIC
   (icTableName AS CHAR,
    icKey       AS CHAR,
    icAction    AS CHAR,
    icValues    AS CHAR).
    
    
   CREATE EventLog.
   ASSIGN EventLog.TableName      = icTableName
          EventLog.Key            = icKey
          EventLog.UserCode       = katun 
          EventLog.EventDate      = TODAY
          EventLog.EventTime      = STRING(TIME,"hh:mm:ss")
          EventLog.Action         = icAction
          EventLog.EventLogStatus = 4
          /* structure is: Label/From/To (chr255 as separator) */
          EventLog.DataValues     = icValues.
          
END FUNCTION.
 
