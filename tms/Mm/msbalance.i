/* msbalance.i     23.01.08/aam 

   handle subscription balances 
*/

FUNCTION fGetMsBalance RETURNS DECIMAL
   (iiMsSeq   AS INT,
    iiCustNum AS INT,
    icBalType AS CHAR):
   
   FIND FIRST MsBalance WHERE
              MsBalance.MsSeq   = iiMsSeq   AND
              MsBalance.CustNum = iiCustNum AND
              MsBalance.BalType = icBalType NO-LOCK NO-ERROR.
    
   IF AVAILABLE MsBalance 
   THEN RETURN MsBalance.Amount.
   ELSE RETURN 0. 
   
END.

FUNCTION fUpdateMsBalance RETURNS LOGICAL
   (iiMsSeq   AS INT,
    iiCustNum AS INT,
    icBalType AS CHAR,
    idAmount  AS DEC):

   DO TRANS:
      FIND FIRST MsBalance WHERE
                 MsBalance.MsSeq   = iiMsSeq   AND
                 MsBalance.CustNum = iiCustNum AND
                 MsBalance.BalType = icBalType EXCLUSIVE-LOCK NO-ERROR.
    
      IF NOT AVAILABLE MsBalance THEN DO:
         CREATE MsBalance.
         ASSIGN MsBalance.MsSeq   = iiMsSeq
                MsBalance.CustNum = iiCustNum 
                MsBalance.BalType = icBalType.
      END.
                
      ASSIGN 
         MsBalance.Amount  = MsBalance.Amount + idAmount
         MsBalance.BalDate = TODAY.
   
      RELEASE MsBalance.
   END.
   
   RETURN TRUE. 
   
END.


