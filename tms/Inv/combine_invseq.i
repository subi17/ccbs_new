/* combine_InvSeq.i   18.01.12/aam 
*/

{invrowcounter_move.i}

DEF TEMP-TABLE ttInvSeq NO-UNDO 
   FIELD MsSeq AS INT
   FIELD InvSeq AS INT
   INDEX MsSeq MsSeq.
 
PROCEDURE pCombineInvSeq:

   DEF INPUT  PARAMETER iiInvCust   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiMsSeq     AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaToDate   AS DATE NO-UNDO. 
   DEF OUTPUT PARAMETER oiCDR       AS INT  NO-UNDO. 
  
   DEF BUFFER bDoubleSeq   FOR InvSeq.
   DEF BUFFER bRemoveSeq   FOR InvSeq.
   DEF BUFFER bInvSeq  FOR InvSeq.
   DEF BUFFER bCDR     FOR MobCDR.

   EMPTY TEMP-TABLE ttInvSeq.
   
   FOR EACH InvSeq NO-LOCK WHERE
            InvSeq.MsSeq = iiMsSeq AND
            InvSeq.CustNum = iiInvCust AND
            InvSeq.Billed = FALSE AND
            InvSeq.FromDate = idaFromDate AND
            InvSeq.ToDate <= idaToDate
   BREAK BY InvSeq.MsSeq
         /* get the oldest -> newer ones probably have less cdrs on them so 
            there will be fewer to move */
         BY RECID(InvSeq):

      IF NOT FIRST-OF(InvSeq.MsSeq) THEN NEXT.

      CREATE ttInvSeq.
      ASSIGN ttInvSeq.InvSeq = InvSeq.InvSeq
             ttInvSeq.MsSeq  = InvSeq.MsSeq.
   END.
   
   /* separate loops so that delete bufseq doesn't mess break by */    
   FOR EACH ttInvSeq,
      FIRST InvSeq NO-LOCK WHERE
            InvSeq.InvSeq = ttInvSeq.InvSeq:
      
      /* possible due to iSTC */
      IF InvSeq.Todate NE idaToDate THEN DO TRANS:
         FIND bInvSeq WHERE ROWID(bInvSeq) = ROWID(InvSeq) 
            EXCLUSIVE-LOCK.
         bInvSeq.Todate = idaToDate.
         RELEASE bInvSeq.
      END.
            
      FOR EACH bDoubleSeq NO-LOCK USE-INDEX MsSeq WHERE
               bDoubleSeq.MsSeq   = InvSeq.MsSeq   AND
               bDoubleSeq.CustNum = InvSeq.CustNum AND
               bDoubleSeq.FromDate >= idaFromDate AND
               bDoubleSeq.ToDate  = idaToDate  AND
               bDoubleSeq.Billed  = FALSE          AND
               RECID(bDoubleSeq) NE RECID(InvSeq):

         FOR EACH MobCDR NO-LOCK WHERE
                  MobCDR.InvCust = bDoubleSeq.CustNum AND
                  MobCDR.InvSeq  = bDoubleSeq.InvSeq  AND
                  MobCDR.MsSeq   = bDoubleSeq.MsSeq:

            DO TRANS:
               FIND bCDR where RECID(bCDR) = RECID(MobCDR) EXCLUSIVE-LOCK.
               bCDR.InvSeq = InvSeq.InvSeq.
               oiCDR = oiCDR + 1.
            END.      
         END.

         /* pending tmqueues */
         FOR EACH TMQueue EXCLUSIVE-LOCK WHERE
                  TMQueue.InvSeq = bDoubleSeq.InvSeq:
            TMQueue.InvSeq = InvSeq.InvSeq.      
         END.
 
         /* move counters */
         fMoveInvrowCounters(InvSeq.CustNum,
                             bDoubleSeq.InvSeq,
                             InvSeq.InvSeq).

         DO TRANS:
            FIND bRemoveSeq where RECID(bRemoveSeq) = RECID(bDoubleSeq)
               EXCLUSIVE-LOCK.
            DELETE bRemoveSeq.
         END.

      END.

   END.

END PROCEDURE.   

