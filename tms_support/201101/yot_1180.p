{Func/date.i}

find mnpprocess where
   mnpprocess.mnptype = 2 and
   mnpprocess.statuscode = 5 and
   mnpprocess.portrequest = "00100511100913180027795".

assign
   mnpprocess.statuscode = 6
   mnpprocess.updatets = fMakeTS().


