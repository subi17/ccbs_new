DEFINE VARIABLE ttOrderId AS INT LABEL "OrderID" FORMAT "zzzzzzzz".

UPDATE ttOrderId.

FIND mnpprocess WHERE
     mnpprocess.orderid = ttOrderId EXCLUSIVE-LOCK.

     FIND order WHERE
        order.brAND = "1" AND
        order.orderid = mnpprocess.orderid EXCLUSIVE-LOCK.

        ASSIGN
            order.mnpstatus = 8
            order.statuscode = "7"
            mnpprocess.statuscode = 7
            mnpprocess.statusreason = "CANC_ABONA".

