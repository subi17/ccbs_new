/*
CREATED: ...: kl 24.02.2001
PURPOSE: ...: handle MthCall UPDATEating THROUGH a queue table
MODIFIED: ..:
*/

FUNCTION fAddMthQ RETURNS logical
  (INPUT pCust AS INT, INPUT pMth AS INT, INPUT pVal AS dec):

   DEF BUFFER xxqueue FOR TMSQueue.

   DO FOR xxqueue.

      CREATE xxqueue.
      ASSIGN
         xxqueue.CustNum = pCust
         xxqueue.Month     = pMth
         xxqueue.Queued     = pVal
      no-error.

   END.

   RETURN error-status:error.

END.

