DEFINE VARIABLE count AS INT NO-UNDO.

FOR EACH SIM NO-LOCK WHERE
        SIM.Stock = "CC" AND
        SIM.SimStat = 1:

        IF count = 5 THEN QUIT.

        DISPLAY ICC.
        count = count + 1.
END.

