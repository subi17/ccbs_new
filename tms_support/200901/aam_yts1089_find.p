def stream sread.
input stream sread from /apps/snet/200812/double_custid_retain.txt.

def stream slog.
output stream slog to /tmp/double_custid_yoigo_remains.txt.

def var lcline  as char no-undo.
def var lcorgid as char no-undo.
def var licust  as int  no-undo.
def var i       as int  no-undo.
def var j       as int  no-undo.
def var lidbl   as int  no-undo.
def var llsubs  as log  no-undo.

def buffer bcust for customer.

def temp-table ttdone no-undo
   field orgid as char
   index orgid orgid.

/* customer's name to be displayed */
FUNCTION fDispCustName RETURNS CHARACTER
   (BUFFER ibNameCust FOR Customer). 
   
   IF NOT AVAILABLE ibNameCust THEN RETURN "". 

  /* company name may be divided into two rows */
   IF ibNameCust.CustIDType = "CIF" AND ibNameCust.CompanyName > "" THEN
      RETURN ibNameCust.CompanyName + 
             (IF ibNameCust.CoName > "" 
              THEN " " + ibNameCust.CoName
              ELSE "").
    
   /* private customers have both lastname and firstname */
   ELSE RETURN ibNameCust.FirstName + " " + ibNameCust.CustName + 
               (IF ibNameCust.SurName2 > "" 
                THEN " " + ibNameCust.SurName2
                ELSE "").
   
END FUNCTION.

function fsubs returns logic
   (iicustnum as int):
 
   if can-find(first mobsub where 
                     mobsub.brand = "1" and
                     mobsub.agrcust = iicustnum)
   then return true.
   
   if can-find(first mobsub where 
                     mobsub.brand = "1" and
                     mobsub.invcust = iicustnum)
   then return true.

   if can-find(first mobsub where 
                     mobsub.brand = "1" and
                     mobsub.custnum = iicustnum)
   then return true.

   return false.
   
end function.

function flogheader returns logic:

   put stream slog unformatted
   "Target"      chr(9)
   "Target"      chr(9)
   "Target"      chr(9)
   "Target"      chr(9)
   "Target"      chr(9)
   "Target"      chr(9)
   "Source"      chr(9)
   "Source"      chr(9)
   "Source"      chr(9)
   "Source"      chr(9)
   "Source"      skip
   "Customer"    chr(9)
   "Type"        chr(9)
   "DNI"         chr(9)
   "Name"        chr(9)
   "Address"     chr(9)
   "has subscr." chr(9)
   "Customer"    chr(9)
   "Type"        chr(9)
   "Name"        chr(9)
   "Address"     chr(9)
   "has subscr." chr(9)
   "active"      skip.
end function.

flogheader().

repeat:

   import stream sread unformatted lcline.

   assign 
      lcorgid = entry(1,lcline,chr(9))
      licust  = integer(entry(2,lcline,chr(9)))
      no-error.

   if error-status:error or licust = 0 or lcorgid = "" then next.

   find first customer where customer.custnum = licust no-lock.
   if customer.orgid ne lcorgid then do:
      message "check:" licust lcorgid
      view-as alert-box.
      next.
   end.

   i = i + 1.
   pause 0.
   disp i customer.custnum 
        customer.custname format "x(20)"
        customer.custidtype 
        customer.orgid.

   if not can-find(first ttdone where ttdone.orgid = customer.orgid) then do:
      create ttdone.
      ttdone.orgid = customer.orgid.
   end.
   
   llsubs = fsubs(customer.custnum).
   
   run pcheck.
   
   disp lidbl.

end.

input stream sread close.
output stream slog close.

output stream slog to /tmp/double_custid_qvantel_remains.txt.

flogheader().

for each customer no-lock by custnum desc:

   if customer.orgid = "" or customer.roles = "inactive" then next.
   
   if can-find(first ttdone where ttdone.orgid = customer.orgid) then next.

   create ttdone.
   ttdone.orgid = customer.orgid.
   
   j = j + 1.
   if j mod 1000 = 0 then do:
      pause 0.
      disp j lidbl with 1 down.
   end.
   
   llsubs = fsubs(customer.custnum).
   
   run pcheck.

end.

output stream slog close.


procedure pcheck:
   
   def var llsubs as log no-undo.
   
   for each bcust no-lock use-index orgid where
            bcust.brand = "1" and
            bcust.orgid = customer.orgid and
            /*
            bcust.custidtype = customer.custidtype and
            */
            bcust.custnum ne customer.custnum:

      llsubs = fsubs(bcust.custnum).
      if not llsubs and bcust.roles = "inactive" then next.
      
      lidbl = lidbl + 1.      
       
      put stream slog unformatted
         customer.custnum    chr(9)
         customer.custidtype chr(9)
         customer.orgid      chr(9)
         fdispcustname(buffer customer)  chr(9)
         customer.address    chr(9)
         llsubs              chr(9)
         bcust.custnum       chr(9)
         bcust.custidtype    chr(9)
         fdispcustname(buffer bcust) chr(9)
         bcust.address       chr(9)
         llsubs              chr(9)
         (bcust.roles ne "inactive") skip.
         
   end.
   
end procedure.


