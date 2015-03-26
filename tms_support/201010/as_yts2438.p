find mnpprocess where
     mnpprocess.portrequest = "00100511100930191320671" and
     mnpprocess.statusreason = "" and
     mnpprocess.statuscode = 2 and
     mnpprocess.mnptype = 2 EXCLUSIVE-LOCK.

assign
   mnpprocess.statuscode = 7.
   mnpprocess.statusreason = "CANC_TECNI".

disp mnpprocess.statuscode mnpprocess.statusreason.
