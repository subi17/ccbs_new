def stream sread.
input stream sread from /apps/snet/200903/ccbs_19032009060000.txt.

def var lcline  as char no-undo.
def var limsseq as int  no-undo.
def var lccontr as char no-undo.
def var lccli   as char no-undo.
def var llprod  as log  no-undo.

def var i as int no-undo.

def buffer bord for order.

repeat:

   import stream sread unformatted lcline.
   
   assign
      limsseq = integer(substring(lcline,1,8))
      lccontr = substring(lcline,20,10)
      lccli   = substring(lcline,90,12).
      
   find first order where 
              order.brand = "1" and
              order.orderid = limsseq no-lock no-error.
             
   if lccontr ne order.contract or lccli ne order.cli then do:
      message "diff" order.orderid
      view-as alert-box.
      next.
   end.

   llprod = true.
   
   find first orderaccessory of order no-lock no-error.
   if available orderaccessory then do:
      llprod = false.
     
      for first invoice no-lock where
                invoice.invnum = order.invnum,
          first invrow of invoice no-lock where
                invrow.billcode = orderaccessory.productcode:
         llprod = true.       
      end.
         
      /*
      disp orderaccessory.productcode format "x(8)".
      */
   end.

   if llprod then next.

   i = i + 1.
  
   disp i format ">>>9"
        order.orderid 
        order.contract format "x(8)"
        order.cli format "x(10)"
        order.crstamp
        order.statuscode.

   find first sim where sim.icc = order.icc no-lock no-error.
   if available sim then disp sim.simstat.
      
   if available orderaccessory then 
      disp orderaccessory.productcode format "x(12)".
end.

       
       
       
   
