{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "NewtonRPC".

DEF VAR lcErrFile AS CHAR NO-UNDO.

   RUN Mc/sendorderconf.p(14844910, "antti.savolainen@qvantel.com", OUTPUT lcErrFile).
/*   RUN Mc/sendorderreq.p(14844910, "antti.savolainen@qvantel.com", OUTPUT lcErrFile).. */
 
