&IF "{&ACCESSLOG_I}" NE "YES"
&THEN
&GLOBAL-DEFINE ACCESSLOG_I YES

/*-------------------------------------------------------------------------
   File......:  Func/lib/accesslog.i
   Purpose...:  Create Accesslog table to log data of user when 
                user view or read customer's data.

   Author(s).:  Ramesh Chand Rebari
   Created...:  FRI JUNE 01 17:33:36 IST 2018

   Notes.....:  01.06.2018/v.1 If user try to view customer's data 
                               intensionally then only user's data
                               will log into Accesslog Table. 
                          
------------------------------------------------------------------------ */

/* ***************************  Internal Procedure ***********************/


PROCEDURE CreateReadAccess:

/*-------------------------------------------------------------------------

   Purpose...:  Create Read Accesslog table
   Parameters:  1. User code - User's code that views the customer data
                2. TableName - Table name which is viewed by user   
                3. Key       - Customer's data that customer views by user         

 ------------------------------------------------------------------------*/ 
   
   DEFINE INPUT PARAMETER icUserCode   AS   CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icTable      AS   CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icKey        AS   CHARACTER NO-UNDO.

   CREATE Accesslog.
   ASSIGN
      Accesslog.EventTs   = NOW
      Accesslog.UserCode  = icUserCode
      Accesslog.TableName = icTable
      Accesslog.Action    = 'Read' 
      Accesslog.Key       = icKey
   .
       
END PROCEDURE.       

&ENDIF


