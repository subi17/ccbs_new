FOR first msowner where
         msowner.cli = "633704976" and
         msowner.tsend = 20110201.35322: 
         msowner.tsend = 20110131.86399.
   disp msowner.
end.

FOR last msowner where
         msowner.cli = "633704976" and
         msowner.tsbegin = 20110201.35323:
         msowner.tsbegin = 20110201.00000.
   disp msowner.
end.

FOR first msowner where
         msowner.msseq = 4439066 and
         msowner.tsend = 20110201.36202: 
         msowner.tsend = 20110131.86399. 
   disp msowner.
end.

FOR last msowner where
         msowner.msseq = 4439066 and
         msowner.tsbegin = 20110201.36203:
         msowner.tsbegin = 20110201.00000. 
   disp msowner.
end.
