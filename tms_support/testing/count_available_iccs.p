DEFINE VARIABLE count AS INT NO-UNDO.

FOR EACH SIM NO-LOCK WHERE
        SIM.Stock = "CC" AND
        SIM.SimStat = 1:

        count = count + 1.
END.

DISP count.
