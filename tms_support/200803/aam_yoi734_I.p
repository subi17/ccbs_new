def var i as int no-undo.
def var j as int no-undo.

def buffer bcli for dccli.

for each dccli no-lock where
         brand = "1":
         
         i = i + 1.

         if dccli.contractdate = ? then do:
            j = j + 1.
            find bcli where recid(bcli) = recid(dccli) exclusive-lock.
            bcli.contractdate = bcli.validfrom.
         end.

         if i mod 1000 = 0 then do:
            pause 0.
            disp i j with 1 down.
         end.
end.

disp i j.

         
    