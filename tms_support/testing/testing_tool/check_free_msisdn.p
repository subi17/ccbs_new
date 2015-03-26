{timestamp.i}

FOR EACH MSISDN NO-LOCK WHERE
         MSISDN.Brand = "1" AND
         MSISDN.ValidTo GE fMakeTS() AND
         MSISDN.StatusCode EQ 99 :
DISP MSISDN.CLI.
END.