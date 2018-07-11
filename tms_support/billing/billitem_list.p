def var i as int no-undo.
def var j as int no-undo.
def var lctrans as char no-undo.
def var lcfile as char no-undo.
def var lcfile_arda as char no-undo.
def var lcsep as char no-undo init "|".

assign lcfile = "/tmp/billing_items_" + STRING(TODAY,"99999999") + ".txt".
       lcfile_arda = "/tmp/billing_items_arda_" + STRING(TODAY,"99999999") + ".txt".

def stream slog.
def stream sarda.

output stream slog to value(lcfile).
output stream sarda to value(lcfile_arda).

put stream slog unformatted
    "Item"  chr(9)
    "Name"  chr(9)
    "Group" chr(9)
    "Account" chr(9)
    "Section" chr(9)
    "Tax Class" chr(9)
    "SAP Code"  chr(9)
    "Cost Centre" chr(9).
    
do i = 1 to 5:
   put stream slog unformatted
      "Language "  i chr(9).
end.
      
put stream slog skip.

put stream sarda unformatted
    "h2.Billing Items" skip.

put stream sarda unformatted
    lcsep lcsep
    "Item"  lcsep
    "Name"  lcsep
    "Group" lcsep
    "Account" lcsep
    "Section" lcsep
    "Tax Class" lcsep
    "SAP Code"  lcsep
    "Cost Centre" lcsep.
    
do i = 1 to 5:
   put stream sarda unformatted
      "Language "  i lcsep.
end.
      
put stream sarda lcsep skip.

for each billitem no-lock where
         billitem.brand = "1"
    BY billitem.bigroup:
        
    FIND FIRST CCRule NO-LOCK WHERE 
               CCRule.Brand     =  BillItem.Brand    AND 
               CCRule.Category  =  "*"               AND 
               CCRule.BillCode  =  BillItem.BillCode AND 
               CCRule.CLIType   =  ""                AND 
               CCRule.ValidTo   >= TODAY NO-ERROR.

    put stream slog unformatted
        billitem.billcode chr(9)
        billitem.biname   chr(9)
        billitem.bigroup  chr(9)
        (IF AVAILABLE CCRule THEN CCRule.AccNum ELSE 0)   chr(9)
        billitem.invsect  chr(9)
        billitem.taxclass chr(9)
        (IF AVAILABLE CCRule THEN CCRule.ReportingID ELSE "") chr(9)
        (IF AVAILABLE CCRule THEN CCRule.CostCentre  ELSE "") chr(9)
       .

    put stream sarda unformatted
        lcsep
        billitem.billcode lcsep
        billitem.biname   lcsep
        billitem.bigroup  lcsep
        (IF AVAILABLE CCRule THEN CCRule.AccNum ELSE 0) lcsep
        billitem.bigroup  lcsep     
        (if billitem.invsect = "" then "None" else billitem.invsect) lcsep
        (if billitem.taxclass = "" then "None" else billitem.taxclass) lcsep
        (IF (AVAILABLE CCRule AND CCRule.ReportingID > "") THEN CCRule.ReportingID ELSE "None") lcsep
        (IF AVAILABLE CCRule THEN CCRule.CostCentre ELSE "") lcsep
    .
      
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

      IF lctrans = "" THEN lctrans = "None".
      put stream sarda unformatted lctrans lcsep.
   end.
   
   put stream slog unformatted skip.
   put stream sarda unformatted skip.

   j = j + 1.
end.

output stream slog close.
output stream sarda close.

message j "billing items listed to " lcfile
   view-as alert-box title " DONE ".

