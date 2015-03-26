{commpaa.i}
katun = "anttis".
gcBrand = "1".
{msreqfunc.i}

input from /apps/snet/200905/as_yts1489.log.
output to /apps/snet/200905/as_yts1489_2.log append.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
repeat:
   import unformatted lcLine.
   find solog where
      solog.solog = int(entry(1,lcLine,"|")) NO-LOCK.

/*    if solog.response eq "ok" then do: */
    if solog.response ne "ok" then do:
      find msrequest where
         msrequest.msrequest = solog.msrequest and
         msrequest.reqstatus = 3 NO-LOCK.
         put unformatted msrequest.msrequest "|" msrequest.reqstatus skip.
         fReqStatus(2,"").
    end.

end.
