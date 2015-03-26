DEFINE VARIABLE noGo AS LOGICAL INIT FALSE NO-UNDO.

FOR EACH mobsub NO-LOCK WHERE 
    CliType = "CONT" OR 
    CliType = "CONT4" OR 
    CliType = "CONT5"
    BY activationDATE DESC:

    FIND FIRST customer WHERE
        customer.custnum = mobsub.AgrCust
    NO-LOCK NO-ERROR.
    
    IF NOT AVAIL customer THEN NEXT.
    IF Customer.CustIdType = "CIF" THEN NEXT.
    IF Customer.CustIdType = "PASSPORT" THEN NEXT.
    
    IF (DATE(NOW) - ActivationDATE) <= 180 THEN NEXT.
    
    FIND LAST callalarm WHERE
        mobsub.cli = callalarm.cli AND
        SUBSTR(delimsg,1,28) = "You can now enter Mi Yoigo,"
    NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE callalarm THEN NEXT.
    noGo = FALSE.

    FOR EACH msrequest NO-LOCK WHERE
        msrequest.msseq = mobsub.msseq AND
        customer.custnum = mobsub.custnum BY msrequest.reqtype DESC:

        IF msrequest.reqtype = 46 THEN noGo = TRUE.
    END.
    
    IF noGo = false then
    DO:
    DISPLAY  
        callalarm.cli 
        SUBSTR(delimsg,46,6) format "x(6)" LABEL "Password"
        customer.custidtype
        customer.orgid
        int(callalarm.ActStamp) format "zzzzzzzz" LABEL "Sent"
        (DATE(NOW) - ActivationDATE) LABEL "Since Act".
    END.
END.