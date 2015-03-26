def stream sread.
input stream sread from /apps/snet/200708/aam_yts93.txt.

def var lcline as char no-undo.
def var liorder as int no-undo.
def var lcorder as char no-undo.

def buffer border for order.

repeat:
    import stream sread unformatted lcline.

    liorder = integer(entry(1,lcline,chr(9))) no-error.

    find order where 
         order.brand = "1" and
         order.orderid = liorder no-lock.
                            /*
    disp order.orderid order.crstamp order.statuscode.
                            */
    for each border no-lock where
             border.cli = order.cli and
             recid(border) ne recid(order):
       if lookup(order.statuscode,"6,7") = 0 or 
          lookup(border.statuscode,"6,7") = 0 then       
       disp order.orderid
            order.crstamp 
            order.statuscode
            border.orderid
            border.crstamp
            border.statuscode.
    end.
    
end.
    
    
