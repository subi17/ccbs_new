/*===========================================================================
 MODULE ........: error_rate.p
 APPLICATION ...: Ticket Master
 TASK ..........: Analyse Mobile CDRs
 CREATED .......: 11.10.99 pt
 VERSION .......: M15
 ============================================================================*/

{Syst/commali.i}
{Rate/rerate_define.i}
{Rate/premiumnumber.i}

DEF INPUT PARAMETER   iiErrorCode AS INT  NO-UNDO.

/* Default starts values */
ASSIGN 
  lcRerateSource = "ERROR"
  bbatch =  session:batch
  cdate2 = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
  cdate1 = DATE(MONTH(cdate2),1,YEAR(CDATE2)).

/* BBATCH Values */
IF bbatch THEN ASSIGN 
   cdate1   = today - 50
   cdate2   = today.


fFillTT().

liRerateSeq = fRerateLogStart (
   katun,
   ?,
   ?,
   "", /* cli */
   0, /* custnum from */
   0, /* custnum to */
   "", /* clitype */
   iiErrorCode, /* ErrorCode */
   "", /* InvGroup */
   ""). /* icInvRunCode*/

ETIME(YES).

MAIN:
REPEAT WITH FRAME main:

DO:      

   MobCDR: 
   FOR EACH MobCDR NO-LOCK WHERE
        mobcdr.errorcode = iiErrorCode 
        
       TRANSACTION WITH FRAME MobCDR: 

       {Rate/man_rate2.i} 
       
fRerateLogFinish(liRerateSeq).
