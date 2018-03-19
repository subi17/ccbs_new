/*
   RES-885 NRTR National roaming traffic restrictions.
   Script for creating default service packages.
*/

/* Create with res_885_create_servpacs_input.p */

DEF VAR lcServpack AS CHAR NO-UNDO. 

FOR EACH clitype NO-LOCK where
         clitype.brand = "1":

   if clitype.clitype begins "cont" then lcServpack = "*1".
   else lcServpack = "*2".

   FIND FIRST ctservpac NO-LOCK where
              ctservpac.brand = clitype.brand and
              ctservpac.clitype = clitype.clitype and
              ctservpac.servpac = lcServpack no-error.
   IF NOT AVAIL ctservpac then next.

   FIND FIRST ctservel NO-LOCK where
              ctservel.clitype = ctservpac.clitype and
              ctservel.servcom = "nw" no-error.
   IF AVAIL ctservel then next.

   create ctservel.
   assign
      ctservel.Brand = "1"
      ctservel.ChgAllowed  = TRUE
      ctservel.CLIType  = ctservpac.clitype
      ctservel.CTServEl = next-value(CTServEl)
      ctservel.DefParam = ""
      ctservel.DefValue = 2
      ctservel.FromDate = today
      ctservel.ServCom  = "NW"
      ctservel.ServPac  = ctservpac.ServPac
      ctservel.ServType = 0.

   disp ctservel.clitype.
END.

