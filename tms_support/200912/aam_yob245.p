def var i as int no-undo.
def var listat as int no-undo.

def buffer breq for msrequest.

do listat = 0 to 8 by 8:

   for each msrequest no-lock where
            msrequest.brand = "1" and
            msrequest.reqtype = 0 and
            msrequest.reqstat = listat and
            msrequest.actstamp >= 20100101 and
            msrequest.actstamp < 20100101.09000:

      i = i + 1.
      pause 0.
      disp i with 1 down.
      
      find first breq where recid(breq) = recid(msrequest) exclusive-lock.
      breq.actstamp = 20100101.09000. 

      /*
      disp i msseq reqstat actstamp crestamp reqcparam1 format "x(8)"
           reqcparam2 format "x(8)".
      */
   end.

end.
