{testpaa.i}
katun = "ari".
gcbrand = "1".
{cparam.i2}
{fmakemsreq.i}

def var lctermcontr as char no-undo.
def var lirequest   as int  no-undo.
def var lcerror     as char no-undo.
def var ldstamp     as dec  no-undo.
def var i           as int  no-undo.
def var lioth       as int  no-undo.
def var lipos       as int  no-undo.
def var lierr       as int  no-undo.

/* id of the periodical contract */
lcTermContr = fCParamC("PerContrTerminal").

IF lcTermContr = "" OR lcTermContr = ? THEN DO:
   message "Check lctermcont"
   view-as alert-box error.
   return.
end.
 
for each order no-lock where
         order.brand = "1" and
         order.paytype = false,
   first mobsub no-lock where
         mobsub.msseq = order.msseq and
         mobsub.paytype = false,
   first OrderAccessory OF Order NO-LOCK:

   /* pos-orders don't have a billing item, only imei code */
   IF (Order.OrderChannel = "pos" AND OrderAccessory.IMEI > "") OR
      (OrderAccessory.ProductCode > "" AND 
       CAN-FIND(FIRST BillItem WHERE
                      BillItem.Brand    = gcBrand AND
                      BillItem.BillCode = OrderAccessory.ProductCode AND
                      BillItem.BIGroup  = "7")) 
   THEN DO:                   
         
      i = i + 1. 
    
      if not can-find(first dccli where
                            dccli.msseq   = mobsub.msseq and
                            dccli.dcevent = lctermcontr)
      then do:
         if order.orderchannel = "pos" 
         then lipos = lipos + 1.
         else lioth = lioth + 1.
      
         ldstamp = MobSub.ActivationTS.
         if ldstamp = 0 then 
            ldstamp = fmake2dt(mobsub.activationdate,0).
         
         liRequest = fPCActionRequest(MobSub.MsSeq,
                                      lcTermContr,
                                      "act",
                                      ldstamp,
                                      FALSE,
                                      "",
                                      "",
                                      OUTPUT lcError).
                                       
         IF liRequest = 0 THEN DO:                              
            /* write possible error to an order memo */
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                             "Order",
                             STRING(Order.OrderID),
                             0,
                             "PERIODICAL CONTRACT CREATION FAILED",
                             lcError).
                             
            lierr = lierr + 1.                 
         END.
      end.
      
   END.   

   if i mod 1000 = 0 then do:
      pause 0.
      disp i lioth lipos lierr order.crstamp with 1 down.
   end.
    
END.

disp i lioth lipos lierr.



