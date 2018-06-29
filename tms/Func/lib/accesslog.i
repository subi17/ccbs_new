&IF "{&ACCESSLOG_I}" NE "YES"
&THEN
&GLOBAL-DEFINE ACCESSLOG_I YES
/*---------------------------------------------------------------------------------
    File        : Func/lib/accesslog.i
    Purpose     : Create AccessLog record for all read of customer data
    Author(s)   : Surbhi Yadav 
    Created     : Sat Jun 01 16:02:10 IST 2018
    Notes       : Create AccessLog for all TMS cui user search and 
                  view customer information intentionally.
  ---------------------------------------------------------------------------------*/

/***********************************  Internal Procedure ***************************/


PROCEDURE CreateReadAccess:

/*----------------------------------------------------------------------------------
  Parameters:  1. Table Name  - Stores TableName which is viewed by TMS user.
               2. User Id     - Stores TMS User's ID that views the customer data.
               3. Key         - Stores customer number which is viewed by user.
               4. Information - Stores current program name.
               5. KeyColumn   - Stores name of field whose value stored in Key field.  
  ----------------------------------------------------------------------------------*/

   DEFINE INPUT PARAMETER icTable     AS   CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icUserCode  AS   CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icKey       AS   CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icInfo      AS   CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER icKeyColumn AS   CHARACTER NO-UNDO.
   
   CREATE AccessLog.
   ASSIGN
      AccessLog.EventTs   = NOW
      AccessLog.Action    = 'Read'      
      AccessLog.UserCode  = icUserCode
      AccessLog.Key       = icKey 
      AccessLog.TableName = icTable
      AccessLog.Info      = icInfo
      AccessLog.KeyColumn = icKeyColumn
      .
       
END PROCEDURE.

&ENDIF
