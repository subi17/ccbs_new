
DEF VAR lcBarrCode  AS CHAR NO-UNDO.
DEF VAR lcDoneStamp AS CHAR NO-UNDO.
DEF VAR liEvents AS INT NO-UNDO.

OUTPUT TO "/store/riftp/pupu_dumps/logs/check_mobsub_barrcode.txt".

FOR EACH MobSub WHERE
         MobSub.Brand = "1" AND
         MobSub.MsStatus = 8 NO-LOCK:

   ASSIGN lcBarrCode  = ""
          lcDoneStamp = "".

   FOR EACH MsRequest WHERE
            MsRequest.MsSeq = MobSub.MsSeq AND
            MsRequest.ReqType = 35 AND
            MsRequest.ReqStatus = 2 NO-LOCK
       USE-INDEX MsSeq BY DoneStamp DESC:

      ASSIGN lcBarrCode  = MsRequest.ReqCParam1
             lcDoneStamp = STRING(MsRequest.DoneStamp).
      LEAVE.
   END. /* FOR EACH MsRequest WHERE */

   IF lcBarrCode BEGINS "UN" THEN NEXT.

   liEvents = liEvents + 1.

   STATUS DEFAULT STRING(liEvents).

   PUT UNFORMATTED STRING(MobSub.MsSeq) "|" lcBarrCode "|"
       lcDoneStamp SKIP.

END. /* FOR EACH MobSub WHERE */

OUTPUT CLOSE.
