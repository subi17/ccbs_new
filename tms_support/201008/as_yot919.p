{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/flimitreq.i}
{Syst/tmsconst.i}
{Func/fcustdata.i}

DEF VAR llMSLimitIsDefault AS LOG NO-UNDO.
DEF VAR pdMobsubLimit AS INT NO-UNDO.
DEF VAR lcLine AS CHAR NO-UNDO.
DEF VAR lcOrgId AS CHAR NO-UNDO.

def stream sout.
output stream sout to yot918.log append.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
input from as_yot919.input.
repeat:

   import unformatted lcline.

   i = i + 1.
   if i <= 10 then next.
/*   if i > 10 then leave. */
/*
   find mobsub where
        mobsub.cli = entry(1,lcline," ") NO-LOCK no-error.
   IF NOT AVAIL mobsub then do:
      put stream sout unformatted lcline "|mobsub not found" skip.
      next.
   end.
*/
   lcOrgId = entry(2, lcLine, " ").

   find customer where
        customer.brand = "1" and
        customer.orgid = lcOrgid and
        customer.roles ne "inactive" NO-LOCK no-error.
   IF NOT AVAIL customer then do:
      put stream sout unformatted lcline "|not found" skip.
      next.
   end.

   pdMobSubLimit = fGetMobSubLimit(Customer.Custnum,
                                   Customer.Category,
                            OUTPUT llMSLimitIsDefault).

   fGetLimit (Customer.Custnum, 0, {&LIMIT_TYPE_SUBQTY}, 0, 0, TODAY).

   do trans:
   IF AVAIL Limit THEN DO:
      IF Limit.LimitAmt NE 0 THEN DO:
         put stream sout unformatted lcline "|" limit.limitamt "|set" skip.
         fSetLimit(ROWID(Limit), 0, FALSE, TODAY, 12/31/2049). 
      END.
      ELSE put stream sout unformatted lcline "|skipped" skip.
   END.
   ELSE DO:
      fCreateLimit(Customer.Custnum,
                   0,
                   {&LIMIT_TYPE_SUBQTY},
                   0,
                   0,
                   0,
                   TODAY,
                   12/31/2049). 
       put stream sout unformatted lcline "|"
               pdMobsubLimit "|created" skip.
   END.
   end.

   release limit.

end.

