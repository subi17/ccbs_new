FOR EACH SIM NO-LOCK WHERE
         SIM.Brand EQ "1" AND
        (SIM.Stock EQ "TESTING" OR
         SIM.Stock EQ "EMATESTING") AND
         SIM.SimStat EQ 1:
DISP SIM.ICC.
END.
