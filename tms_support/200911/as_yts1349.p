DEFINE VARIABLE i AS INTEGER NO-UNDO. 

DEFINE TEMP-TABLE ttMemoTitle
FIELD i AS INT
FIELD c AS CHAR 
INDEX c IS PRIMARY UNIQUE c.

FOR EACH memo NO-LOCK where
   memo.brand = "1" and
   memo.custnum = 233718:
   
   i = i + 1.

   if i mod 50000 = 0 then disp i.
   
   find ttMemoTitle WHERE
      ttMemoTitle.c = memo.memotitle EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL ttMemoTitle THEN DO:
      CREATE ttMemoTitle.
      ASSIGN
         ttMemoTitle.c = memo.memotitle
         ttMemoTitle.i = 1.
   END.
   ELSE ttMemoTitle.i = ttMemoTitle.i + 1.

END.

output to /apps/snet/200911/as_yts1349.txt.
FOR EACH ttMemoTitle where
   ttMemoTitle.c NE "Credit Reason" and
   (ttMemoTitle.i >= 50 or ttmemotitle.c begins "outported to "):
   put unformatted ttMemoTitle.i " " ttMemoTitle.c skip.
END.
