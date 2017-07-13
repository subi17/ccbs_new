/* ---------------------------------------------------------------------------
  MODULE .......: DDPAYMENTB.P
  KUTSUVAMODULI : 
  FUNCTION .....: batch process for making payments to dd invoices     
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 23.04.07
  CHANGED ......: 10.05.07/aam ActionLog
  Version ......: M15
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "ddpaym".
       
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/timestamp.i}

DEF VAR liCount    AS INT  NO-UNDO.
DEF VAR lcError    AS CHAR NO-UNDO.
DEF VAR lcFile     AS CHAR NO-UNDO.

DEF STREAM sRead.

fELog("DDPAYM","Started").

RUN Ar/ddpaymentco.p ("",                   /* InvGroup  */
                 0,                    /* customers from */
                 999999999,             /* customers to   */
                 "",                   /* invoices from  */
                 "ZZZZZ",              /* invoices to    */
                 TODAY,                /* duedate   */
                 1,                    /* normal invoice type */
                 "",                   /* billrun not limited */
                 TODAY,                /* payment date        */
                 OUTPUT liCount,
                 OUTPUT lcError).
 
IF NOT (liCount = 0 AND lcError BEGINS "INFO") THEN DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "Cron"  
      ActionLog.KeyValue     = "" 
      ActionLog.ActionID     = "DDPayment"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                               MONTH(TODAY)
      ActionLog.ActionDec    = liCount
      ActionLog.ActionChar   = lcError
      ActionLog.ActionStatus = 3.
      ActionLog.ActionTS     = fMakeTS().
END.

fELog("DDPAYM","Stopped:" + STRING(liCount) +
                  (IF lcError > "" THEN ":" + lcError ELSE "")).


