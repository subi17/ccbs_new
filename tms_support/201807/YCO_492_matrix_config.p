
define buffer bmatrix for matrix.
define buffer bmxitem for MXItem.

FIND FIRST bmatrix NO-LOCK WHERE
           bmatrix.brand  eq "1"        and
           bmatrix.mxkey  eq "PERCONTR" and
           bmatrix.Prior  eq 23         and
           bmatrix.MXName eq "DSS2 and DSS2_UPSELL" NO-ERROR.

IF AVAIL bmatrix THEN DO:
   CREATE bMXItem.
   ASSIGN bMXItem.MXSeq   = bmatrix.MxSeq
          bMXItem.MXName  = "SubsTypeTo"
          bMXItem.MXValue = "CONTDSL48".

   CREATE bMXItem.
   ASSIGN bMXItem.MXSeq   = bmatrix.MxSeq
          bMXItem.MXName  = "SubsTypeTo"
          bMXItem.MXValue = "CONTFH48_50".

   CREATE bMXItem.
   ASSIGN bMXItem.MXSeq   = bmatrix.MxSeq
          bMXItem.MXName  = "SubsTypeTo"
          bMXItem.MXValue = "CONTFH58_300".

   CREATE bMXItem.
   ASSIGN bMXItem.MXSeq   = bmatrix.MxSeq
          bMXItem.MXName  = "SubsTypeTo"
          bMXItem.MXValue = "CONTFH76_1000".
END.
