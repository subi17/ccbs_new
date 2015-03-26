def var i as int no-undo.
def var j as int no-undo.
def var lctrans as char no-undo.
def var lcfile as char no-undo.

lcfile = "/tmp/billing_items_ydr166.txt".

def stream slog.
output stream slog to value(lcfile).


put stream slog unformatted
    "Item"  chr(9)
    "Name"  chr(9)
    "Group" chr(9)
    "GroupName" chr(9)
    "Section" chr(9)
    "Account" chr(9)
    "Tax Class" chr(9).
    
do i = 1 to 5:
   put stream slog unformatted
      "Language "  i chr(9).
end.
      
put stream slog skip.

for each billitem no-lock where
         billitem.brand = "1":
      
   find bitemgroup where
        bitemgroup.brand = "1" and
        bitemgroup.bigroup = billitem.bigroup NO-LOCK.

   put stream slog unformatted
      billitem.billcode chr(9)
      billitem.biname   chr(9)
      billitem.bigroup  chr(9)
      bitemgroup.bigname chr(9)
      billitem.invsect  chr(9)
      billitem.accnum   chr(9)
      billitem.taxclass chr(9).
      
   do i = 1 to 5:
      lctrans = "".
      FOR FIRST RepText NO-LOCK WHERE
                RepText.Brand     = "1"   AND
                RepText.TextType  = 1 AND 
                RepText.LinkCode  = billitem.billcode  AND
                RepText.Language  = i AND 
                RepText.ToDate   >= today    AND
                RepText.FromDate <= today:
         lctrans = RepText.RepText.
      END.

      put stream slog unformatted lctrans chr(9).
   end.
   
   put stream slog unformatted skip.

   j = j + 1.
end.

message j "billing items listed to" lcfile
view-as alert-box title " DONE ".



      
    
    
