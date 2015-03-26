DEFINE VARIABLE i AS INTEGER NO-UNDO. 

DEFINE TEMP-TABLE ttMemoTitle
FIELD c AS CHAR 
INDEX c IS PRIMARY UNIQUE c.

input from /apps/snet/200911/as_yts1349.txt.
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
repeat:
   import unformatted lcLine.
   create ttMemotitle.
   assign ttMemotitle.c = substring(lcLine,index(lcLine," ") + 1).
   i = i + int(entry(1, lcline, " ")).
end.

disp i.

def stream smemos.
output stream smemos to /apps/snet/200911/as_yts1349_memos.d.

FOR EACH memo EXCLUSIVE-LOCK where
   memo.brand = "1" and
   memo.custnum = 233718:
   
   if i mod 50000 = 0 then disp i.
   
   find ttMemoTitle WHERE
      ttMemoTitle.c = memo.memotitle NO-LOCK NO-ERROR.
   IF AVAIL ttMemoTitle THEN DO:
      export stream smemos memo.
      delete memo.
   END.

END.
