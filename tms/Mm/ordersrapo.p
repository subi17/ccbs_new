/* This module creates two reports, one on a total base and one on a  */
/* weekly base.                                                       */
/* Created 04.05.07 by vk                                             */
/* Changed 13.06.07 by vk The program is using now two output streams */
/*                        for the two reports, which it creates.      */            
DEF STREAM sDump.
DEF STREAM sWeek.
DEF NEW SHARED VAR uvpvm AS Date. /* the date as input */
DEF NEW SHARED VAR uviikko AS INT FORMAT "999999".
DEF VAR lcF      AS  CHAR NO-UNDO.
DEF VAR lcW      AS  CHAR NO-UNDO.    
DEF VAR ldtLimit AS  DATE NO-UNDO.
DEF VAR liRajapv AS  INT  NO-UNDO.

DEFINE TEMP-TABLE ttStates NO-UNDO
    FIELD Stat   AS INT
    FIELD Pieces AS INT
INDEX Stat Stat.

DEFINE TEMP-TABLE ttRows NO-UNDO
    FIELD Stat   AS INT
    FIELD Pieces AS INT
INDEX Stat Stat.

FOR EACH TMSCodes WHERE TMSCodes.TableName = "Order"      AND
                        TMSCodes.FieldName = "StatusCode" AND
                        TMSCodes.CodeGroup = "orders"     NO-LOCK:
    CREATE ttStates.
    IF TMSCodes.CodeValue = "" THEN ttStates.Stat = 0.
    ELSE ttStates.Stat = INTEGER(TMSCodes.CodeValue).
    ttStates.Pieces = 0.
                               
    CREATE ttRows.
    IF TMSCodes.CodeValue = "" THEN ttRows.Stat = 0.
                               ELSE ttRows.Stat = INTEGER(TMSCodes.CodeValue).
    ttRows.Pieces = 0.                           
END.
     
uvpvm = TODAY.

RUN Syst/uviik.p.

lcF = "/scratch/nagios/tms/order/total/totalorders" + STRING(uviikko) + 
".dump".

OUTPUT STREAM sDump TO VALUE(lcF).

/* total sums */
FOR EACH Order USE-INDEX StatusCode NO-LOCK:
    FIND FIRST ttStates WHERE ttStates.Stat = INTEGER(Order.StatusCode) 
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ttStates THEN ttStates.Pieces = ttStates.Pieces + 1.
    ELSE DO:
        CREATE ttStates.
        ASSIGN ttStates.Stat   = INTEGER(Order.StatusCode)
               ttStates.Pieces = 1.
    END.
END.

FOR EACH ttStates USE-INDEX Stat NO-LOCK:
    PUT STREAM sDump UNFORMATTED ttStates.Stat CHR(9) ttStates.Pieces SKIP.
END.

OUTPUT STREAM sDump CLOSE.

lcW = "/scratch/nagios/tms/order/weekly/weeklyorders" + STRING(uviikko) + ".dump". 

OUTPUT STREAM sWeek TO VALUE(lcW).

/* The date of the previous monday will be calculated. */
IF WEEKDAY(TODAY) = 1 THEN ldtLimit = TODAY - 6.
                      ELSE ldtLimit = TODAY - WEEKDAY(TODAY) + 2.

liRajapv = 10000 * YEAR(ldtLimit) + 100 * MONTH(ldtLimit) + DAY(ldtLimit).

/* weekly base */
FOR EACH Order WHERE 
         Order.Brand = "1" AND
         Order.CrStamp >= liRajapv NO-LOCK:
    FIND FIRST ttRows WHERE ttRows.Stat = INTEGER(Order.StatusCode) 
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ttRows THEN ttRows.Pieces = ttRows.Pieces + 1.
    ELSE DO:
        CREATE ttRows.
        ASSIGN ttRows.Stat   = INTEGER(Order.StatusCode)
               ttRows.Pieces = 1.
    END.
END.

FOR EACH ttRows USE-INDEX Stat NO-LOCK:
    PUT STREAM sWeek UNFORMATTED ttRows.Stat CHR(9) ttRows.Pieces SKIP.
END.

OUTPUT STREAM SWeek CLOSE.
