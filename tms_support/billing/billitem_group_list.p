def var i as int no-undo.
def var j as int no-undo.
def var lctrans as char no-undo.
def var lcfile as char no-undo.
def var lcdoc1 as char no-undo.
def var lcgraph as char no-undo.
def var lcfile_arda as char no-undo.
def var lcsep as char no-undo init "|".

assign lcfile = "/tmp/billitem_groups_" + STRING(TODAY,"99999999") + ".txt".
       lcfile_arda = "/tmp/billitem_groups_arda_" + STRING(TODAY,"99999999") + ".txt".

def stream slog.
def stream sarda.

output stream slog to value(lcfile).
output stream sarda to value(lcfile_arda).

put stream slog unformatted
    "Group"  chr(9)
    "Name"   chr(9)
    "DOC1"   chr(9)
    "Graph"  chr(9)
    "Inv.Order" chr(9).
    
do i = 1 to 5:
   put stream slog unformatted
      "Language "  i chr(9).
end.
      
put stream slog skip.

put stream sarda unformatted
    "h2.Billing Item Groups" skip.

put stream sarda unformatted
             lcsep
             lcsep
    "Group"  lcsep
    "Name"   lcsep
    "DOC1"   lcsep
    "Graph"  lcsep
    "Inv.Order" lcsep.
    
do i = 1 to 5:
   put stream sarda unformatted
      "Language "  i lcsep.
end.
      
put stream sarda lcsep skip.

for each bitemgroup no-lock where 
         bitemgroup.brand = "1"
         BY bitemgroup.bigroup:

   lcDoc1 = ENTRY(1,BItemGroup.ReportCode).
   IF NUM-ENTRIES(BItemGroup.ReportCode) > 1 THEN
      lcGraph = ENTRY(2,BItemGroup.ReportCode).
   ELSE lcGraph = "".   


   put stream slog unformatted
      bitemgroup.bigroup  chr(9)
      bitemgroup.bigname  chr(9)
      lcdoc1              chr(9)
      lcgraph             chr(9)
      bitemgroup.invoiceorder chr(9).

   put stream sarda unformatted
                          lcsep
      bitemgroup.bigroup  lcsep
      bitemgroup.bigname  lcsep
     (if lcdoc1 = "" then "None" else lcdoc1) lcsep
     (if lcgraph = "" then "None" else lcgraph) lcsep
     (if bitemgroup.invoiceorder = 0 then "None" else string(bitemgroup.invoiceorder)) lcsep.
      
      
   do i = 1 to 5:
      lctrans = "".
      FOR FIRST RepText NO-LOCK WHERE
                RepText.Brand     = "1"   AND
                RepText.TextType  = 6 AND 
                RepText.LinkCode  = bitemgroup.bigroup  AND
                RepText.Language  = i AND 
                RepText.ToDate   >= today    AND
                RepText.FromDate <= today:
         lctrans = RepText.RepText.
      END.

      put stream slog unformatted lctrans chr(9).

      IF lctrans = "" THEN lctrans = "None".
      put stream sarda unformatted lctrans lcsep.
   end.
   
   put stream slog unformatted skip.
   put stream sarda unformatted skip.

   j = j + 1.
end.

output stream slog close.
output stream sarda close.

message j "billing items listed to" lcfile
view-as alert-box title " DONE ".

