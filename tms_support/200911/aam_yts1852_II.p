def stream slog.
output stream slog to /apps/snet/200911/tarj3_change_address.txt.

def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.
def var lcreqaddress as char no-undo.
def var lccustname as char no-undo.
def var lccompanyname as char no-undo.
def var lcaddress as char no-undo.
def var lcpostoffice as char no-undo.
def var lddonestamp as dec no-undo.
def var lcordercustname as char no-undo.
def var lcordercompanyname as char no-undo.
def var lcorderaddress as char no-undo.
def var lcorderpostoffice as char no-undo.
def var liorderid as int no-undo.
def var lccurrname as char no-undo.
def var lccurrpost as char no-undo.

def buffer brequest for msrequest.

put stream slog unformatted
    "Customer"   chr(9)
    "Name"       chr(9)
    "Address"    chr(9)
    "Post"       chr(9)
    "Order Name" chr(9)
    "Order Addr." chr(9)
    "Order Post" chr(9)
    "Order ID"   skip.
    
    
for each customer no-lock where 
        custnum = 79903
         /*
         customer.custnum > 1000 and
         customer.custnum ne 233718
         */:

   i = i + 1.
   if i mod 100 = 0 then do:
      pause 0.
      disp i j k with 1 down frame a.
   end.
   
   assign
      lcreqaddress = ""
      lddonestamp = 0.
         
   for each msowner no-lock where
            msowner.agrcust = customer.custnum,
       each msrequest no-lock where
            msrequest.msseq = msowner.msseq and
            msrequest.reqtype = 10 and
            msrequest.reqstat = 2,
      first brequest no-lock where
            brequest.origrequest = msrequest.msrequest and
            brequest.reqtype = 0 and
            brequest.reqcparam1 = "tarj3":
      assign 
         lcreqaddress = msrequest.reqcparam1
         lddonestamp  = msrequest.donestamp.
      leave.
   end.
   

   if lcreqaddress = "" then next.
   
   j = j + 1.
   
   assign 
      lcCustName   = ENTRY(2,lcreqaddress,";") + " " +
                     ENTRY(1,lcreqaddress,";") + " " +
                     ENTRY(3,lcreqaddress,";")
      lcCompanyname = ENTRY(5,lcreqaddress,";")
      lcAddress    = ENTRY(6,lcreqaddress,";")
      lcPostOffice = ENTRY(7,lcreqaddress,";") + " " +
                     ENTRY(8,lcreqaddress,";")
      liorderid    = 0.

   if lccompanyname > "" then lccustname = lccompanyname.
   
   for each msowner no-lock where
            msowner.agrcust = customer.custnum,
      first order no-lock where
            order.msseq = msowner.msseq and
            order.ordertype ne 2 and
            order.crstamp > lddonestamp,
      first ordercustomer of order no-lock where
            ordercustomer.rowtype = 1
   by order.crstamp desc:
      if ordercustomer.custidtype = "cif" and ordercustomer.company > ""
      then lcordercustname = ordercustomer.company.
      else lcordercustname = ordercustomer.firstname + " " + 
                             ordercustomer.surname1 + " " +
                             ordercustomer.surname2.
      assign
         lcorderaddress = ordercustomer.address
         lcorderpostoffice = ordercustomer.zipcode + " " +
                             ordercustomer.postoffice
         liorderid = order.orderid.
      leave.
   end.         
    
   if liorderid = 0 then next.
      

   if customer.custidtype = "cif" and customer.companyname > "" then 
      lccurrname = customer.companyname.
   else lccurrname = customer.firstname + " " +
                     customer.custname + " " + 
                     customer.surname2.
   lccurrpost = customer.zipcode + " " + 
                customer.postoffice.
                
   if (lccustname = customer.custname and
       lccustname ne lcordercustname) or
      (lcaddress = customer.address and
       lcaddress ne lcorderaddress) or
      (lcpostoffice = customer.postoffice and
       lcpostoffice ne lcorderpostoffice)
   then do:

      put stream slog unformatted
         customer.custnum  chr(9)
         lccurrname  chr(9)
         customer.address chr(9)
         lccurrpost chr(9)
         lcordercustname chr(9)
         lcorderaddress chr(9)
         lcorderpostoffice chr(9)
         liorderid skip.
      /*
      disp customer.custnum
           customer.custname
           customer.address format "x(30)" skip
           lcordercustname format "x(20)"
           lcorderaddress format "x(30)" with frame b.
      */     
      k = k + 1.   
   end.
end.

