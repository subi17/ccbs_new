{Func/date.i}
find mnpprocess where
     mnpprocess.portrequest = "06400511100531083504057" and
     mnpprocess.statuscode = 2 EXCLUSIVE-LOCK.

assign
   mnpprocess.statuscode = 6
   mnpprocess.updatets = fMakeTS().
