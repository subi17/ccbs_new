{Func/date.i}
find mnpprocess where
     mnpprocess.portrequest = "00400511101220081201847" and
     mnpprocess.statuscode = 2 EXCLUSIVE-LOCK. 

assign
   mnpprocess.statuscode = 7 
   mnpprocess.statusreason = "CANC_TECNI"
   mnpprocess.updatets = fmakets().

disp mnpprocess with 1 col.
