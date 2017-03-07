{Func/timestamp.i}

def var ldadate as date no-undo.
def var litime as int no-undo.
def var ldafrom as date no-undo. 
def var i as int no-undo.

ldafrom = 10/1/10.


for each mobcdr use-index date where 
         mobcdr.readdate = ? and
         mobcdr.datest >= ldafrom:

   i = i + 1.
         
   fsplitts(mobcdr.readints,
            output ldadate,
            output litime).
   mobcdr.readdate = ldadate.
            
   if i mod 1000 = 0 then do:
      pause 0.
      disp "Mob" i mobcdr.datest 
           string(mobcdr.timest,"hh:mm:ss") with 1 down.
   end.
    
end.

i = 0.

for each prepcdr use-index date where 
         prepcdr.readdate = ? and
         prepcdr.datest >= ldafrom:

   i = i + 1.
         
   fsplitts(prepcdr.readints,
            output ldadate,
            output litime).
   prepcdr.readdate = ldadate.
            
   if i mod 1000 = 0 then do:
      pause 0.
      disp "Prep" i prepcdr.datest 
           string(prepcdr.timest,"hh:mm:ss") with 1 down.
   end.
    
end.

i = 0.

for each errorcdr use-index date where 
         errorcdr.readdate = ? and
         errorcdr.datest >= ldafrom:

   i = i + 1.
         
   fsplitts(errorcdr.readints,
            output ldadate,
            output litime).
   errorcdr.readdate = ldadate.
            
   if i mod 1000 = 0 then do:
      pause 0.
      disp "Error" i errorcdr.datest 
           string(errorcdr.timest,"hh:mm:ss") with 1 down.
   end.
    
end.

i = 0.

for each fraudcdr use-index date where 
         fraudcdr.readdate = ? and
         fraudcdr.datest >= ldafrom:

   i = i + 1.
         
   fsplitts(fraudcdr.readints,
            output ldadate,
            output litime).
   fraudcdr.readdate = ldadate.
            
   if i mod 1000 = 0 then do:
      pause 0.
      disp "Fraud" i fraudcdr.datest 
           string(fraudcdr.timest,"hh:mm:ss") with 1 down.
   end.
    
end.




