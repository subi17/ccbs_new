
DEFINE VARIABLE  icLog as char no-undo.

icLog = "rd_yot_62.output".

def stream slog.
output stream slog to value(icLog).

FOR EACH SIM NO-LOCK WHERE
         SIM.Brand = "1" AND
         SIM.Stock = "CC":
    put stream slog unformatted SIM.ICC  " " SIM.SimStat SKIP.
END.
output stream slog close.
