/* ----------------------------------------------------------------------
  MODULE .......: gbilling.i
  TASK .........: Functions for Google Billing needs
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......: 
  CREATED ......: 19.11.15
  CHANGED ......: 
  ------------------------------------------------------------------------*/

{commali.i}
{timestamp.i}
{cparam2.i}
{fgettxt.i}
{date.i}
{fduedate.i}
{ftransdir.i}
{tmsconst.i}

DEF VAR lcGBOutDir AS CHAR NO-UNDO.
DEF VAR lcGBInDir AS CHAR NO-UNDO.
DEF VAR lcGBLogDir AS CHAR NO-UNDO.
DEF VAR lcGBSpoolDir AS CHAR NO-UNDO.



DEF STREAM Sout.
DEF STREAM SHRLP.




FUNCTION fInitGBParameters RETURNS CHAR
   ():
   ASSIGN

      lcGBOutDir = fCParam("GBILLING","HRLPOutDir")
      lcGBInDir = fCParam("GBILLING","HRLPListInDir")
      lcGBLogDir = fCParam("GBILLING","HRLPLogDir")
      lcGBSpoolDir = fCParam("GBILLING","HRLPSpoolDir").

 
   IF lcGBOutDir  EQ "" OR lcGBOutDir EQ ? THEN lcGBOutDir = "/tmp/".
   IF lcGBInDir  EQ "" OR lcGBInDir EQ ? THEN lcGBInDir = "/tmp/".
   IF lcGBLogDir  EQ "" OR lcGBLogDir EQ ? THEN lcGBLogDir = "/tmp/".
   IF lcGBSpoolDir  EQ "" OR lcGBSpoolDir EQ ? THEN lcGBSpoolDir = "/tmp/".

END.   
    
FUNCTION fProcessPostpaidEntry RETURNS CHAR
   (icMSISDN AS CHAR, /*MSISDN*/
    icCorrId AS CHAR, /*Correlation ID*/
    idtPDate AS DATETIME, /*Purchase date*/
    ideAmount AS DECIMAL): /*Amount*/

   DEF VAR lcResponse AS CHAR NO-UNDO.
   lcResponse = {&GB_RESP_OK}.


END.
