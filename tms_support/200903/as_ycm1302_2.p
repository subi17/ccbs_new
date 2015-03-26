def buffer bmobcdr for mobcdr.

FOR EACH mobcdr where
   errorcode = 9100 NO-LOCK:

   IF mobcdr.custnum = 999 THEN DO:
      
      find bmobcdr EXCLUSIVE-LOCK WHERE
         recid(bmobcdr) = recid(mobcdr).
      assign
         bmobcdr.errorcode = 8049.
      release bmobcdr.
      next.
   END.
   
   FIND BillTarget WHERE
        BillTarget.CustNum    = MobCDR.CustNum AND
        BillTarget.BillTarget = MobCDR.BillTarget NO-LOCK NO-ERROR.

   IF AVAIL BillTarget AND BillTarget.RatePlan BEGINS "TARJETA" THEN DO:
      
      find bmobcdr EXCLUSIVE-LOCK WHERE
         recid(bmobcdr) = recid(mobcdr).
      assign
         bmobcdr.errorcode = 8049.
      release bmobcdr.
      next.

   END.

END.
