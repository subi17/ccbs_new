DEFINE VARIABLE count AS INT NO-UNDO.
DEFINE VARIABLE ttNow AS CHAR NO-UNDO.
DEFINE VARIABLE daysSinceOrdered AS INT LABEL "Days old" NO-UNDO.
DEFINE VARIABLE ttSalesman AS CHAR INIT "selenium" LABEL "Salesman" NO-UNDO.

ttNow = SUBSTR(STRING(NOW), 7, 4) +
        SUBSTR(STRING(NOW), 4, 2) +
        SUBSTR(STRING(NOW), 1, 2).

UPDATE ttSalesman FORMAT "x(15)" WITH FRAME A.

FOR EACH order NO-LOCK WHERE
    OrderChannel = "pos" AND
    salesman = ttSalesman AND
    (INT(ttNow) - INT(CrStamp)) < 14:

    daysSinceOrdered = (INT(ttNow) - INT(CrStamp)).
    
    FIND FIRST customer WHERE
        order.custnum = customer.AgrCust
    NO-LOCK NO-ERROR.
    
    FIND FIRST sim WHERE
        sim.Stock = "CC" AND
        sim.SimStat = 1
    NO-LOCK NO-ERROR.

    
    DISPLAY daysSinceOrdered
            order.cli
            customer.CustIdType
            customer.orgid
            sim.icc
    WITH FRAME B DOWN.
END.

