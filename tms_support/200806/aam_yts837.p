def stream sread.
input stream sread from /apps/snet/200806/mnp_requestid_corr.txt.

def var lcline as char no-undo.
def var lccli  as char no-undo.
def var lctms  as char no-undo.
def var lccorr as char no-undo.
def var i      as int  no-undo.
def var j      as int  no-undo.
def var k      as int  no-undo.
def var lcform as char no-undo.
def var liindex as int no-undo.

repeat:
   
   import stream sread unformatted lcline.
   
   if lcline = "" then next.
   
   assign lccli  = entry(1,lcline," ")
          lccorr = entry(2,lcline," ")
          lctms  = right-trim(entry(3,lcline," ")).
          
   find first order use-index cli where 
                    order.brand = "1" and 
                    order.cli = lccli and
                    order.crstamp < 20080620 and
                    order.statuscode ne "7"
                    exclusive-lock no-error.
   if not available order then next.                  
   
   i = i + 1.
   
   disp i    
        lccli format "x(12)"
        lctms format "x(15)"
        lccorr format "x(15)".

   /* to asol */
   for each mnpprocess exclusive-lock where
         mnpprocess.orderid = order.orderid:
         
      message mnpprocess.formrequest lctms lccorr 
          order.orderid order.statuscode order.mnpstatus
          mnpprocess.statuscode
          view-as alert-box.
          
      if mnpprocess.formrequest = lctms then 
      assign 
         mnpprocess.formrequest = lccorr
         mnpprocess.statuscode  = 2
         order.mnpstatus        = 3
         order.statuscode       = "12"
         j = j + 1.
          
   end.

   /*
   for first mnpprocess no-lock where
         mnpprocess.orderid = order.orderid,
   first mnpsub no-lock where
         mnpsub.mnpseq = mnpprocess.mnpseq,
    each mnpmessage no-lock where 
         mnpmessage.mnpseq = mnpprocess.mnpseq:

      if mnpprocess.formrequest = lctms then j = j + 1.
      else if mnpprocess.formrequest = lccorr then k = k + 1. 

      disp mnpmessage.sender
           mnpmessage.statuscode   
           mnpmessage.sentts
           MNPProcess.FormRequest format "x(15)"
           mnpprocess.statuscode
           order.orderid.

        liindex = index(mnpmessage.xmlmessage,"</formRequestCode>").
        lcform = substring(mnpmessage.xmlmessage,liindex - 11,11).
        
        disp lcform format "x(15)".
   end.      
   */
     
end.

disp i j k.


 
