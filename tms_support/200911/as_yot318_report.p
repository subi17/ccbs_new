input from /apps/snet/200911/as_yot318.log.
output to /apps/snet/200911/as_yot318_report.log.
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liMsrequest AS INTEGER NO-UNDO. 
put unformatted "MSISDN|SUBSCRIPTION_ID|RESULT" skip.
repeat:
   import unformatted lcLine.
   liMsrequest = int(entry(6,lcLine," ")) no-error.
   if error-status:error then next. /* disp lcLine format "x(70)". */
   find msrequest where msrequest.msrequest = liMsrequest NO-LOCK.
   if msrequest.reqstatus ne 2 then 
   put unformatted msrequest.cli "|" msrequest.msseq "|error: " msrequest.memo skip.
   else
   put unformatted msrequest.cli "|" msrequest.msseq "|ok" skip.

end.
