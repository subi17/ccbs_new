{date.i}

find mnpprocess where
   mnpprocess.portrequest = "00400511100531083403959" EXCLUSIVE-LOCK.

assign
   mnpprocess.statuscode = 7
   mnpprocess.statusreason = "CANC_TECNI" 
   mnpprocess.updatets = fMakeTS().
