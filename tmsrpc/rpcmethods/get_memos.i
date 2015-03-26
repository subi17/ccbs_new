/* ----------------------------------------------------------------------
  MODULE .......: get_memos.i 
  TASK .........: rpc functions for fetching memos
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 10.08.09
  Version ......: xfera
----------------------------------------------------------------------- */

DEFINE TEMP-TABLE ttMemo NO-UNDO
FIELD memoseq AS INT
INDEX memoseq IS PRIMARY memoseq ASC. 

FUNCTION multi_memo RETURN LOGICAL
      ( pcHosttable AS CHAR,
        piKey AS INT,
        pcKeyFieldMode AS CHAR,
        plCheck AS LOG): /* False = fetch all memos, True =fetch only first one */

    IF pcKeyFieldMode = "custnum" THEN DO:
   
        IF LOOKUP(STRING(piKey), "233718,239696,239680,239666") > 0 AND
           NOT plCheck THEN RETURN TRUE.

        FOR EACH Memo NO-LOCK WHERE
                 Memo.brand = "1" AND
                 memo.hosttable = pcHosttable AND 
                 Memo.custnum = piKey:
            create ttMemo.
            ttMemo.memoseq = memo.memoseq.
            release ttMemo.
            IF plCheck THEN RETURN TRUE.
        END.

    END. ELSE DO:

        FOR EACH Memo NO-LOCK WHERE
                 Memo.brand = "1" AND
                 memo.hosttable = pcHosttable AND 
                 Memo.KeyValue = STRING(piKey):

            IF (Memo.MemoType EQ "Service" AND pcKeyFieldMode EQ "service") OR
            (Memo.MemoType NE "Service" AND pcKeyFieldMode NE "service") THEN 
            DO:
               create ttMemo.
               ttMemo.memoseq = memo.memoseq.
               release ttMemo.
               IF plCheck THEN RETURN TRUE.
            END.
        END.

    END.

   RETURN TRUE.
END FUNCTION.


FUNCTION fMemoCount RETURNS LOGICAL
(pcType AS CHAR,
 piKey AS INTEGER,
 plCheckMode AS LOGICAL): /* FALSE = fetch all memos, TRUE = fetch only first one (check if memos exist) */
   
   EMPTY TEMP-TABLE ttMemo.

   IF pcType = 'mobsub' THEN DO:

       multi_memo("mobsub", piKey, "keyvalue", plCheckMode).
       multi_memo("order",  piKey, "keyvalue", plCheckMode).
       multi_memo("*",      piKey, "keyvalue", plCheckMode).
       multi_memo("all",    piKey, "keyvalue", plCheckMode).

   END.  ELSE IF pcType =  'customer' THEN DO:

      multi_memo("customer", piKey, "custnum", plCheckMode).

   END.  ELSE IF pcType = 'invoice' THEN DO:

       multi_memo("invoice",   piKey, "custnum", plCheckMode).
       multi_memo("creditc",   piKey, "custnum", plCheckMode).
       multi_memo("fatime",    piKey, "custnum", plCheckMode).
       multi_memo("fixedfee",  piKey, "custnum", plCheckMode).
       multi_memo("payment",   piKey, "custnum", plCheckMode).
       multi_memo("singlefee", piKey, "custnum", plCheckMode).
       multi_memo("ddauth",    piKey, "custnum", plCheckMode).
       multi_memo("highusage", piKey, "custnum", plCheckMode).
       multi_memo("invtext",   piKey, "custnum", plCheckMode).

   END.  ELSE IF pcType = 'service' THEN DO:

       multi_memo("mobsub",    piKey, "service", plCheckMode).

   END.

   ELSE DO:
      
      multi_memo(pcType, piKey, "", plCheckMode).

   END.

   RETURN CAN-FIND(FIRST ttMemo).

END FUNCTION. 

