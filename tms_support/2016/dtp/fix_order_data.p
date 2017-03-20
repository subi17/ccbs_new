def var lii as int no-undo.

for each order exclusive-lock use-index stamp where brand = "1" and crstamp > 20161130 and deliverysecure = 1 and deliverytype = 6:
   
   order.deliverysecure = 2.
   lii = lii + 1.
end.

message lii view-as alert-box.
