{Syst/tmsconst.i}
DEF STREAM sOutFile.

DEF BUFFER bCustomer FOR Customer.

OUTPUT STREAM sOutFile TO VALUE("delivery_type_changes.log") APPEND.

FOR EACH bCustomer WHERE
         bCustomer.Deltype NE {&INV_DEL_TYPE_NO_DELIVERY} OR
         bCustomer.Deltype NE {&INV_DEL_TYPE_SMS}:
   PUT STREAM sOutFile UNFORMATTED
      STRING(bCustomer.Custnum) + ";" +
      STRING(bCustomer.Deltype) + ";" + 
      STRING({&INV_DEL_TYPE_ESI}) SKIP.
   bCustomer.Deltype = {&INV_DEL_TYPE_ESI}.
END.
