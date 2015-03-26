{date.i}
/*
find mnpprocess where
     mnpprocess.portrequest = "00300511110131191337899" and
     mnpprocess.statuscode = 2.

assign
   mnpprocess.statuscode = 7
   mnpprocess.updatets = fmakets()
   mnpprocess.statusreason = "CANC_TECNI".
*/
find mnpprocess where
     mnpprocess.portrequest = "00500411110131121424083" and
     mnpprocess.statuscode = 2.

assign
   mnpprocess.statuscode = 7
   mnpprocess.updatets = fmakets()
   mnpprocess.statusreason = "CANC_TECNI".
