DEFINE VARIABLE noGo AS LOGICAL INIT FALSE NO-UNDO.

FOR EACH mobsub NO-LOCK WHERE 
    msstatus = 4 AND
    (clitype = "tarj" OR
    clitype = "tarj4")
    BY activationDATE DESC:

    FIND FIRST customer WHERE
        customer.custnum = mobsub.AgrCust
    NO-LOCK NO-ERROR.
    
    IF NOT AVAIL customer THEN NEXT.
    IF Customer.CustIdType = "CIF" THEN NEXT.
    IF Customer.CustIdType = "PASSPORT" THEN NEXT.
    
    noGo = FALSE.

    FOR EACH msrequest NO-LOCK WHERE
        msrequest.msseq = mobsub.msseq AND
        customer.custnum = mobsub.custnum BY msrequest.reqtype DESC:

        IF msrequest.reqtype = 46 THEN noGo = TRUE.
    END.
    
    IF noGo = TRUE then
    DO:
    DISPLAY  
        mobsub.cli
        clitype
        custidtype
		customer.orgid	
		icc.
    END.
END.
