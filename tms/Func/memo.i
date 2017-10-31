&IF "{&MEMO_I}" NE "YES"
&THEN
&GLOBAL-DEFINE MEMO_I YES


FUNCTION fCreateMemo RETURNS LOGICAL
   (icHostTable AS CHAR,
    icKeyValue  AS CHAR,
    iiCustNum   AS INT,
    icTitle     AS CHAR,
    icText      AS CHAR,
    icType      AS CHAR,
    icCreUser   AS CHAR).
    
   CREATE Memo.
   ASSIGN Memo.Brand     = Syst.Parameters:Syst.CUICommon:gcBrand
          Memo.HostTable = icHostTable
          Memo.KeyValue  = icKeyValue
          Memo.CustNum   = iiCustNum
          Memo.Memotype  = icType
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = icCreUser 
          Memo.MemoTitle = icTitle
          Memo.MemoText  = icText.
          Memo.CreStamp  = Func.Common:mMakeTS().

   RELEASE Memo.
   
   RETURN TRUE. 
   
END FUNCTION.

&ENDIF
