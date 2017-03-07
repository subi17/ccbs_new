/* YBU-3361 */
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 
DEFINE VARIABLE k AS INTEGER NO-UNDO. 

DEFINE VARIABLE liSTCDay AS INTEGER NO-UNDO.

DEFINE VARIABLE ldaDate AS DATE NO-UNDO. 
DEFINE VARIABLE liTime AS INTEGER NO-UNDO. 
{Func/date.i}

liSTCDay = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + 1.
/* liSTCDay = 20130901. */

FOR EACH msrequest NO-LOCK where
         msrequest.brand = "1" and
         msrequest.reqtype = 0 and
         msrequest.reqstatus = 2 and
         msrequest.actstamp = liSTCDay and
         msrequest.reqcparam1 begins "cont":
   
   i = i + 1.
   if i mod 100 = 0 THEN DO:
      disp i label "Checked" 
           j label "Errors" 
           with frame b title "Duplicate MsOwner check after STC".
      pause 0.
   END.

   k = 0.
   FOR EACH msowner NO-LOCK where
            msowner.msseq = msrequest.msseq and
            msowner.tsend > liSTCDay and
            msowner.tsbeg <= liSTCDay:
      k = k  + 1.
   end.

   IF k > 1 then do:

      j = j + 1.

      fsplitts(msrequest.donestamp, output ldaDate, output liTime).

      FIND FIRST mobcdr NO-LOCK where
                 mobcdr.cli = msrequest.cli and
                 mobcdr.datest = ldaDate and
                 mobcdr.timestart < liTime and
                 mobcdr.billcode eq "14100001" no-error.
                 
      disp 
         msrequest.cli
         fts2hms(msrequest.donestamp) format "x(25)" label "STC done stamp"
         msrequest.reqcparam1 format "x(10)"
         msrequest.reqcparam2 format "x(10)"
         avail(mobcdr) label "GPRS found"
         with frame a 15 down.
    end.

end.
