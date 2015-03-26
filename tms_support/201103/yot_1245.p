FOR EACH mnpprocess where
         mnpprocess.orderid = 4793118 and
         mnpprocess.statuscode = 0 EXCLUSIVE-LOCK:
    if mnpprocess.formrequest eq "00502251443" then
      assign
         mnpprocess.statusreason = "AREC EXIST"
         mnpprocess.statuscode = 8.
    else
      assign
         mnpprocess.statusreason = "AREC DOMIN"
         mnpprocess.statuscode = 8.
END.
