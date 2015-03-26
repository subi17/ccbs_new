def buffer bmobcdr for mobcdr.

FOR EACH mobcdr where
   datest >= 2/1/2009 AND
   datest <= 2/28/2009 AND
   errorcode = 8049 NO-LOCK:

   IF mobcdr.gsmbnr begins "6" AND
      (mobcdr.rateCCN = 81 OR
      mobcdr.rateCCN = 51 OR
      mobcdr.rateCCN = 94 OR
      mobcdr.rateCCN = 31) THEN DO:
      
      find bmobcdr EXCLUSIVE-LOCK WHERE
         recid(bmobcdr) = recid(mobcdr).
      assign
         bmobcdr.errorcode = 9100.
      release bmobcdr.
   
   END.
END.
