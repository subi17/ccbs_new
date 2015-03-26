FOR EACH slganalyse where
         slganalyse.brand = "1" and
         slganalyse.clitype = "CONT2" and
         validto > today EXCLUSIVE-LOCK:
    validto = today.
end.
