{date.i}

find mnpprocess where
   mnpprocess.portrequest = "00100511100531103812328" EXCLUSIVE-LOCK.

assign
   mnpprocess.statuscode = 7
   mnpprocess.statusreason = "CANC_TECNI" 
   mnpprocess.updatets = fMakeTS().
