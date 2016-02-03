{Syst/commpaa.i}
gcBrand = "1".
katun = "NewtonRPC".

DEF VAR lcErrFile AS CHAR NO-UNDO.

   RUN sendorderconf.p(14844910, "antti.savolainen@qvantel.com", OUTPUT lcErrFile).
/*   RUN sendorderreq.p(14844910, "antti.savolainen@qvantel.com", OUTPUT lcErrFile).. */
 
