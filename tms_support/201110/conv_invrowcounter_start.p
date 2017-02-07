{Syst/testpaa.i}
katun = "qvantel".
{Func/timestamp.i}

DEF VAR liCount AS INT  NO-UNDO. 
DEF VAR i AS INT NO-UNDO.
DEF VAR ldFrom AS DEC NO-UNDO.
DEF VAR ldTo AS DEC NO-UNDO.

DEF STREAM sLog.

OUTPUT STREAM sLog TO /tmp/conv_invrowcounter.log APPEND.

DEF BUFFER bInvSeq FOR InvSeq.

FOR EACH Customer NO-LOCK,
    EACH InvSeq NO-LOCK WHERE
         InvSeq.CustNum = Customer.CustNum AND
         InvSeq.Billed = FALSE:
         
   i = i + 1.
   PAUSE 0.
   DISP Customer.Custnum InvSeq.ToDate i  WITH 1 DOWN.
   
   IF InvSeq.AgrCust = 0 THEN DO:
      ASSIGN 
         ldFrom = fMake2Dt(InvSeq.FromDate,0)
         ldTo   = fMake2Dt(InvSeq.ToDate,86399).
      FOR FIRST MsOwner NO-LOCK WHERE
                MsOwner.MsSeq = InvSeq.MsSeq AND
                MsOwner.InvCust = InvSeq.CustNum AND
                MsOwner.TSEnd > ldFrom AND
                MsOwner.TsBeg < ldTo:
         FIND FIRST bInvSeq WHERE RECID(bInvSeq) = RECID(InvSeq) 
            EXCLUSIVE-LOCK.
         bInvSeq.AgrCust = MsOwner.AgrCust.
      END.
   END.
   
   RUN tms_support/billing/conv_invrowcounter.p(InvSeq.InvSeq,
                            OUTPUT liCount).

   PUT STREAM sLog UNFORMATTED
      Customer.CustNum "|"
      liCount.

   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      MESSAGE "Failed;" RETURN-VALUE
      VIEW-AS ALERT-BOX.

      PUT STREAM sLog UNFORMATTED
         "|" RETURN-VALUE.
   END.
   
   PUT STREAM sLog SKIP.
END.

OUTPUT STREAM sLog CLOSE.
      
