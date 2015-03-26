DEFINE VARIABLE noGo AS LOGICAL INIT FALSE NO-UNDO.

FOR EACH mobsub NO-LOCK WHERE
    activationdate < 06/01/10 AND
    msstatus = 4 AND
    (LOOKUP("CONT",mobsub.clitype) > 0 OR
    LOOKUP("CONT4",mobsub.clitype) > 0 OR
    LOOKUP("CONT5",mobsub.clitype) > 0):

    FIND FIRST customer WHERE
        customer.custnum = mobsub.custnum AND
        (LOOKUP("NIF",customer.custidtype) > 0 OR
        LOOKUP("NIE",customer.custidtype) > 0)
    NO-LOCK NO-ERROR.

    IF NOT AVAIL customer THEN NEXT.

    noGo = FALSE.

    FOR EACH msrequest NO-LOCK WHERE
        msrequest.msseq = mobsub.msseq AND
        customer.custnum = mobsub.custnum BY msrequest.reqtype DESC:

        IF msrequest.reqtype = 46 THEN noGo = TRUE.
    END.
    IF noGo = false then
    DO:
        disp mobsub.cli mobsub.clitype mobsub.activationdate customer.orgid customer.custidtype.
    END.
END.
