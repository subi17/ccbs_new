/*output to as_yts2473_december.input2.*/

def buffer breq for msrequest.

FOR EACH msrequest where
         msrequest.brand = "1" and
         msrequest.reqtype = 0 and
         msrequest.reqstatus = 2 and
         msrequest.actstamp >= 20100101 NO-LOCK use-index reqtype:


   
   if index(reqcparam1,"rd") > 0 and index(reqcparam2,"rd") > 0 then do:

      if can-find(first breq NO-LOCK where
            breq.msseq = msrequest.msseq  and
            breq.reqtype = 0   and
            breq.reqstatus = 2 and
            breq.actstamp > msrequest.actstamp) then next.
/*   disp reqcparam1 reqcparam2. */
      if msseq = 2576084 then next.
      if can-find(first breq NO-LOCK where
            breq.msseq = msrequest.msseq  and
            breq.reqtype = 19   and
            breq.reqstatus = 2 and
            breq.actstamp < msrequest.actstamp)
        then MESSAGE msrequest.msseq "wtF" VIEW-AS ALERT-BOX.
/*      put unformatted msrequest.msseq skip. */

   end.

end.
