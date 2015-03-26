output to /apps/snet/200802/order.d.
FOR EACH order where 
   order.brand = "1" and
   order.orderid < 58561 NO-LOCK:
   export order.
END.
output close.

output to /apps/snet/200802/ordercustomer.d.
FOR EACH ordercustomer where 
   ordercustomer.brand = "1" and
   ordercustomer.orderid < 58561 NO-LOCK:
   export ordercustomer.
END.
output close.

output to /apps/snet/200802/orderaccessory.d.
FOR EACH orderaccessory where 
   orderaccessory.brand = "1" and
   orderaccessory.orderid < 58561 NO-LOCK:
   export orderaccessory.
END.
output close.

output to /apps/snet/200802/orderpayment.d.
FOR EACH orderpayment where 
   orderpayment.brand = "1" and
   orderpayment.orderid < 58561 NO-LOCK:
   export orderpayment.
END.
output close.

output to /apps/snet/200802/orderservice.d.
FOR EACH orderservice where 
   orderservice.brand = "1" and
   orderservice.orderid < 58561 NO-LOCK:
   export orderservice.
END.
output close.

output to /apps/snet/200802/ordertopup.d.
FOR EACH ordertopup where 
   ordertopup.brand = "1" and
   ordertopup.orderid < 58561 NO-LOCK:
   export ordertopup.
END.
output close.

output to /apps/snet/200802/ordertimestamp.d.
FOR EACH ordertimestamp where 
   ordertimestamp.brand = "1" and
   ordertimestamp.orderid < 58561 NO-LOCK:
   export ordertimestamp.
END.
output close.
