{Syst/commali.i}
{Func/timestamp.i}

DEF INPUT PARAMETER icCLI      AS CHAR NO-UNDO.
DEF INPUT PARAMETER iiCustNum  AS INT  NO-UNDO.
DEF INPUT PARAMETER iiPeriod1  AS INT  NO-UNDO.
DEF INPUT PARAMETER iiPeriod2  AS INT  NO-UNDO.

DEF VAR idtActDate AS DATE NO-UNDO.
DEF VAR idtActDate2 AS DATE NO-UNDO.
DEF VAR ldtFrom  AS DATE NO-UNDO.
DEF VAR ldtTo    AS DATE NO-UNDO.

   
idtactdate = DATE(INT(SUBSTR(STRING(iiperiod1),5,2)),  /* MONTH */
                  1,  /* DAY   */
                  INT(SUBSTR(STRING(iiperiod1),1,4))). /* YEAR  */

idtactdate2 = DATE(INT(SUBSTR(STRING(iiperiod2),5,2)),  /* MONTH */
                  1,  /* DAY   */
                  INT(SUBSTR(STRING(iiperiod2),1,4))). /* YEAR  */
idtactdate2 = idtactdate2 + 35.

/* from the beginning of change month */
ldtFrom = DATE(MONTH(idtActDate),1,YEAR(idtActDate)).
ldtTo   = DATE(MONTH(idtActDate2),1,YEAR(idtActDate2)) - 1.      
/* to the end of current month */
   
IF ldtTo > ldtFrom THEN DO:

  MESSAGE "Re-rate calls ...".
  
   IF iccli ne "" THEN 
   RUN cli_rate (icCLI,
                 ldtFrom,
                 ldtTo,
                 TRUE).
   ELSE RUN cust_rate(iiCustNum,
                      ldtFrom,
                      ldtTo,
                      TRUE).
   HIDE MESSAGE  NO-PAUSE.
END.
 

