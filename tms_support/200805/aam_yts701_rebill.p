{Syst/testpaa.i}
katun = "ari".

def var lclist     as char no-undo.
def var lcerr      as char no-undo.
def var ldamt      as dec  no-undo.
def var lcextinvid as char no-undo.
def var lcline     as char no-undo.
def var liorder    as int  no-undo.
def var i          as int  no-undo.
def var j          as int  no-undo.
def var lioldnum   as int  no-undo.

def stream sread.
input stream sread from /apps/snet/200805/aam_yts701.log.

def stream slog.
output stream slog to /apps/snet/200805/aam_yts701_sfeedel.log append.

repeat:

   import stream sread unformatted lcline.
   
   assign liorder    = integer(entry(1,lcline,chr(9)))
          lioldnum   = integer(entry(2,lcline,chr(9)))
          lcextinvid = entry(3,lcline,chr(9))
          no-error.

   if error-status:error then next.
   
   find order where 
        order.brand   = "1" and
        order.orderid = liorder no-lock no-error.
   if not available order then next.
   
   if order.invnum = 0 and lioldnum = 0 then do:
      RUN Mc/cashfee.p (order.orderid,
                     1,
                     output lclist,
                     output ldamt,
                     output lcerr).
       if lcerr > "" then do:
           message "error in cashfee (new):" lcerr
           view-as alert-box.
       end.
       
       j = j + 1.
       
       next.
   end.
   
   
   find invoice where 
        invoice.invnum = order.invnum no-lock no-error.
   if not available invoice or invoice.extinvid ne lcextinvid or
      invoice.invnum ne lioldnum
   then next.
   
   i = i + 1.
   
   pause 0.
   display i j
           order.orderid
           order.cli
           invoice.chgstamp
           order.statuscode
           lioldnum format ">>>>>>>>9".
           

   RUN Inv/del_inv.p (invoice.invnum).
   

   for each singlefee exclusive-lock where
            singlefee.brand = "1" and
            singlefee.hosttable = "order" and
            singlefee.keyvalue = string(order.orderid):
            
      if singlefee.billed = false then do:      
         export stream slog singlefee.
         delete singlefee.      
      end.
      
      else do:
         message "singlefee:" 
             order.orderid singlefee.billcode singlefee.invnum
         view-as alert-box.
      end.
         
   end.

   RUN /apps/snet/200801/cashfee_rebill.p 
                 (order.orderid,
                  lcextinvid,
                  1,
                  output lclist,
                  output ldamt,
                  output lcerr).
   if lcerr > "" then do:
       message "error in cashfee (rebill):" lcerr
       view-as alert-box.
   end.
   
   disp order.invnum.
end.

disp i j.

