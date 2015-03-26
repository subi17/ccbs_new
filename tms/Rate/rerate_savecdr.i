/* rerate_savecdr.i 
*/

FUNCTION fBCopy RETURNS LOGICAL:
   
   DEF VAR lcQueueTarget AS CHAR NO-UNDO.
    
   IF MobCDR.InvSeq    NE ttCall.InvSeq    OR 
      MobCDR.TariffNum NE ttCall.TariffNum OR
      MobCDR.BillCode  NE ttCall.BillCode  OR 
      MobCDR.CLI       NE ttCall.CLI       OR
      MobCDR.Amount    NE ttCall.Amount    OR 
      MobCDR.CCN       NE ttCall.CCN       OR
      MobCDR.CLIType   NE ttCall.CLIType OR
      MobCDR.BillTarget NE ttCall.BillTarget OR
      MobCDR.BDest NE ttCall.BDest OR
      MobCDR.ErrorCode NE ttCall.ErrorCode OR
      MobCDR.ServiceName NE ttCall.ServiceName OR
      MobCDR.DCEvent NE ttCall.DCEvent OR
      MobCDR.DCType NE ttCall.DCType
   THEN DO:

      /* do not update fraud counters from old events */
      IF YEAR(MobCDR.DateSt) < YEAR(TODAY) OR
         MONTH(MobCDR.DateSt) < MONTH(TODAY) THEN 
         lcQueueTarget = "InvRow".
      ELSE lcQueueTarget = "".   
         
      IF MobCDR.ErrorCode = 0 THEN DO:
         CREATE TMQueue.
         BUFFER-COPY MobCDR TO TMQueue.
            
         ASSIGN
            TMQueue.Qty     = -1
            TMQueue.EventID = MobCDR.DtlSeq
            /* agrcust may have changed, but this is a negative event -> 
               limit customer has no meaning */
            TMQueue.AgrCust = ttCall.AgrCust
            TmQueue.Source  = MobCDR.MSCID
            TmQueue.PayType = 1 + INT(MobCDR.PPFlag > 0)
            TMQueue.ReportingID = MobCDR.ServRid + "," + MobCDR.MPMRid
            TMQueue.ExtraAmount = MobCDR.MPMAmt
            TMQueue.AccumTarget = lcQueueTarget.
         RELEASE TMQueue.
      END.

      /* save history (not from all changes) */
      IF MobCDR.TariffNum NE ttCall.TariffNum OR
         MobCDR.BillCode  NE ttCall.BillCode  OR 
         MobCDR.CLI       NE ttCall.CLI       OR
         MobCDR.Amount    NE ttCall.Amount    OR 
         MobCDR.BDest     NE ttCall.BDest     OR
         MobCDR.ErrorCode NE ttCall.ErrorCode OR
         MobCDR.DCEvent   NE ttCall.DCEvent
      THEN DO:
         CREATE EDRHistory.
         BUFFER-COPY MobCDR TO EDRHistory.
         ASSIGN 
            EDRHistory.Brand = gcBrand
            EDRHistory.UpdateDate = TODAY
            EDRHistory.UpdateTime = TIME 
            EDRHistory.UpdateSource = IF lcRerateSource > "" 
                                      THEN lcRerateSource
                                      ELSE PROGRAM-NAME(2).
      END.
         
      FIND CURRENT MobCDR EXCLUSIVE-LOCK.
      BUFFER-COPY ttCall TO MobCDR.

      IF ttCall.ErrorCode = 0 THEN DO:
         CREATE TMQueue.
         BUFFER-COPY ttCall TO TMQueue.
            
         ASSIGN
            TMQueue.Qty     = 1
            TMQueue.EventID = MobCDR.DtlSeq
            TmQueue.Source  = MobCDR.MSCID
            TMQueue.PayType = 1
            TMQueue.ReportingID = MobCDR.ServRid + "," + MobCDR.MPMRid
            TMQueue.ExtraAmount = MobCDR.MPMAmt
            TMQueue.AccumTarget = lcQueueTarget.
         RELEASE TMQueue.
      END.
      
      ASSIGN 
         llChanged = TRUE
         liChangedQty  = liChangedQty + 1
         liCustChanged = liCustChanged + 1.
   END.

END.

