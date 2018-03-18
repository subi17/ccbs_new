/*
   RES-885 NRTR National roaming traffic restrictions.
   Script for creating default service packages.

*/

DEF VAR lcLine AS CHAR NO-UNDO.

find servpac NO-LOCK where
     servpac.brand = "1" and
     servpac.servpac = "NW".
find servel NO-LOCK where
     servel.brand = "1" and
     servel.servcom = "NW" and
     servel.servpac = servpac.servpac.

/* Create with res_885_create_servpacs_input.p */
input from active_CONT_clitypes.txt.

repeat trans:
   import unformatted lcLine.
   lcLine = trim(lcLine).
   FIND FIRST clitype NO-LOCK where
              clitype.brand = "1" and
              clitype.clitype = lcLine no-error.
   IF NOT AVAIL clitype then do:
      MESSAGE "Not available CliType " lcLine VIEW-AS ALERT-BOX.
      next.
   end.
   FIND FIRST ctservpac NO-LOCK where
              ctservpac.clitype = clitype.clitype and
              ctservpac.servpac = "NW" no-error.
   disp clitype.clitype avail(ctservpac).
   if avail ctservpac then next.
/* Not needed...
   create ctservpac.
   assign
      ctservpac.brand = "1"
      ctservpac.CLIType = clitype.clitype
      ctservpac.FromDate = 9/1/2014
      ctservpac.ServiceLimit = ""
      ctservpac.ServPac  = ServPac.ServPac
      ctservpac.ServType = 3
      ctservpac.ToDate = 12/31/2049.
*/
   /* display "create ctservel " ctservpac.clitype.*/
   create ctservel.
   assign
      ctservel.Brand = "1"
      ctservel.ChgAllowed   = TRUE
      ctservel.CLIType  = ctservpac.clitype
      ctservel.CTServEl = next-value(CTServEl)
      ctservel.DefParam = ""
      ctservel.DefValue = 1
      ctservel.FromDate = ctservpac.fromdate
      ctservel.ServCom  = "NW"
      ctservel.ServPac  = ctservpac.ServPac
      ctservel.ServType = 1.
END.

