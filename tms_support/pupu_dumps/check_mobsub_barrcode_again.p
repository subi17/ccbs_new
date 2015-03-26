DEF TEMP-TABLE ttBarr NO-UNDO
    FIELD MsSeq     AS INT
    FIELD BarrCode  AS CHAR
    FIELD DoneStamp AS DEC.

OUTPUT TO "/store/riftp/pupu_dumps/logs/check_mobsub_barrcode_again.txt".

FOR EACH MsRequest WHERE
         MsRequest.Brand     = "1"  AND
         MsRequest.ReqType   = 35   AND
         MsRequest.ReqStatus = 2    AND
         MsRequest.ActStamp >= 20130624.66000 NO-LOCK:

   CREATE ttBarr.
   ASSIGN ttBarr.MsSeq = MsRequest.MsSeq
          ttBarr.BarrCode = MsRequest.ReqCParam1
          ttBarr.DoneStamp = MsRequest.DoneStamp.

END. /* FOR EACH MsRequest WHERE */

FOR EACH ttBarr
    BREAK BY ttBarr.MsSeq
          BY ttBarr.DoneStamp DESC:
   IF FIRST-OF(ttBarr.MsSeq) THEN DO:
      IF NOT ttBarr.BarrCode BEGINS "UN" THEN
         PUT UNFORMATTED STRING(ttBarr.MsSeq) "|" ttBarr.BarrCode "|"
             STRING(ttBarr.DoneStamp) SKIP.
   END.
END.

OUTPUT CLOSE.
