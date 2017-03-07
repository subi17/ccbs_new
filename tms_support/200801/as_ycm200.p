/* PART 1*/
/*
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO.

define stream out.
OUTPUT STREAM out TO "/home/anttis/msisdn.txt".

DO i = 622681440 TO 622701563:
  FIND FIRST msisdn where
   brand = "1" and
   cli = string(i) 
   use-index cli EXCLUSIVE-LOCK no-error.
   if avail msisdn then do:
      if msisdn.statuscode eq 1 then 
      msisdn.pos = "PREACTIVATED".
      j = j + 1.
      export stream out msisdn.
   end.
END.
output stream out close.
disp j.
*/

/*PART 2*/ 
/*
{Syst/testpaa.i}
katun = "anttis".
{Func/msisdn.i}
{Func/date.i}

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO.

define stream out.
OUTPUT STREAM out TO "/home/anttis/gold_msisdn.txt".

DO i = 622681440 TO 622701563:
  FIND FIRST msisdn where
   msisdn.brand = "1" and
   msisdn.cli = string(i) 
   use-index cli NO-LOCK no-error.
   if avail msisdn then do:
      find msisdnnumber where msisdnnumber.cli = msisdn.cli 
         and msisdnnumber.rank = 1 NO-LOCK no-error.
      if avail msisdnnumber then do:
         if msisdn.statuscode = 0 and msisdn.validto > fMakeTS() then do:
            j = j + 1.
         /*   MESSAGE msisdn.orderid VIEW-AS ALERT-BOX.*/
            find current msisdn exclusive-lock.
            export stream out msisdn.
            fMakeMsidnHistory(recid(msisdn)).
            msisdn.statuscode = 1.
            msisdn.pos = "PREACTIVATED".
         end.
      end.   
   end.
END.
output stream out close.
disp j.
*/
/* PART3 */
/*
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO.
{Func/date.i}
define stream out.
OUTPUT STREAM out TO "/home/anttis/order_msisdn2.txt".

DO i = 622681440 TO 622701563:
  FIND FIRST msisdn where
   msisdn.brand = "1" and
   msisdn.cli = string(i) 
   use-index cli NO-LOCK no-error.
   if avail msisdn then do:
      if /*msisdn.statuscode ne 0*/ msisdn.statuscode ne 1 
      and msisdn.validto > fMakeTS() then do:
         j = j + 1.
         put stream out unformatted msisdn.cli skip.
      end.
   end.
END.
output stream out close.
disp j.

/*PART 4*/
/*
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO.
{Syst/testpaa.i}
katun = "anttis".
{Func/msisdn.i}
{Func/date.i}
define stream out.
OUTPUT STREAM out TO "/home/anttis/newrange_msisdn.txt".

DO i = 622701564 TO 622737255:
  FIND FIRST msisdn where
   msisdn.brand = "1" and
   msisdn.cli = string(i) 
   use-index cli NO-LOCK no-error.
   if avail msisdn then do:
      /*if msisdn.statuscode ne 0 and msisdn.statuscode ne 1 */
      /*find first msisdnnumber where msisdnnumber.cli = msisdn.cli
      and (rank = 0) NO-LOCK no-error .
      IF AVAIL msisdnnumber then do:*/
      if msisdn.validto > fMakeTS() and 
         (msisdn.statuscode = 0 or msisdn.statuscode = 1) then do:
         j = j + 1.
         export stream out msisdn.
         find current msisdn exclusive-lock.
         fMakeMsidnHistory(recid(msisdn)).
         msisdn.statuscode = 1.
         msisdn.pos = "PREACTIVATED".
      end.
      /*end.*/
   end.
END.
output stream out close.
disp j.

create msrange.
assign
   msrange.CliFrom = "622701564"
   msrange.CliTo   = "622737255"
   msrange.custnum = 1001
   msrange.salesman = "PREACTIVATED"
   msrange.reservedate = TODAY
   msrange.expiredate  = 1/1/2052
   msrange.brand = "1".
      
  */ 
