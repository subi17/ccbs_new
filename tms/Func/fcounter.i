/* common functions to counters */

{Func/timestamp.i}
/* update/create the counter */
FUNCTION fUpdateCounter RETURN LOGICAL 
         ( INPUT pcHostTable   AS CHAR,
           INPUT pcKeyValue    AS CHAR,
           INPUT piCounterType AS INT,
           INPUT pdEndTS       AS DEC,
           INPUT pdBeginTS     AS DEC,
           INPUT pdAmt         AS DEC):

 DEFINE VARIABLE ldTS AS DEC NO-UNDO.
 FIND FIRST Counter NO-LOCK WHERE 
            Counter.Brand = gcBrand AND 
            Counter.HostTable = pcHostTable AND
            Counter.KeyValue = pcKeyValue AND
            Counter.CounterType = piCounterType AND
            Counter.EndStamp <= pdEndTS AND
            Counter.BeginStamp >= pdBeginTS NO-ERROR .
 IF AVAIL Counter THEN 
    FIND CURRENT Counter EXCLUSIVE-LOCK .
 ELSE DO:
    ldTS = fMakeTS().
    CREATE Counter. 
    ASSIGN Counter.CounterSeq = NEXT-VALUE(CounterSeq)
           Counter.Brand = gcBrand
           Counter.HostTable = pcHostTable
           Counter.KeyValue = pcKeyValue 
           Counter.CounterType = piCounterType
           Counter.BeginStamp = ldTS
           Counter.EndStamp = pdEndTS .
 END.
   ASSIGN Counter.CounterAmt = CounterAmt + pdAmt .
   RELEASE Counter.

RETURN TRUE.
END FUNCTION.

FUNCTION fGetCounterAmt RETURN DECIMAL 
         ( INPUT pcHostTable   AS CHAR,
           INPUT pcKeyValue    AS CHAR,
           INPUT piCounterType AS INT,
           INPUT pdEndTS       AS DEC,
           INPUT pdBeginTS     AS DEC):

    DEFINE VARIABLE ldAmt AS DECIMAL NO-UNDO INITIAL 0. 
    FOR EACH Counter NO-LOCK WHERE 
             Counter.Brand = gcBrand AND
             Counter.HostTable = pcHostTable AND
             Counter.KeyValue = pcKeyValue AND
             Counter.CounterType = piCounterType  AND
             Counter.EndStamp <= pdEndTS AND
             Counter.BeginStamp >= pdBeginTS :

             ldAmt = ldAmt + Counter.CounterAmt.
    END.
    RETURN ldAmt. 
END FUNCTION.
