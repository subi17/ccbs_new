{date.i}
find msrequest where
     msrequest.msrequest = 40638846.

ASSIGN MsRequest.ActStamp = fMakeTS()
       msrequest.sendsms = 0
       MsRequest.ReqDParam1 = fMakeTS()
       MsRequest.reqstatus = 6.


