find msrequest where
     msrequest.msrequest = 40638846.

ASSIGN MsRequest.ActStamp = Func.Common:mMakeTS()
       msrequest.sendsms = 0
       MsRequest.ReqDParam1 = Func.Common:mMakeTS()
       MsRequest.reqstatus = 6.


