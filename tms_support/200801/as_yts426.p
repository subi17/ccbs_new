{testpaa.i}
katun = "cron".
gcbrand = "1".

def var ldtdate as date no-undo.
ldtDate = 1/13/2008.
RUN tapfilecr("",ldtDate - 1,ldtDate - 1,"",FALSE).

ldtDate = 1/14/2008.
RUN tapfilecr("",ldtDate - 1,ldtDate - 1,"",FALSE).

ldtDate = 1/15/2008.
RUN tapfilecr("",ldtDate - 1,ldtDate - 1,"",FALSE).
