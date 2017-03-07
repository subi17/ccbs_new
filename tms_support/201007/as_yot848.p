{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".

{Func/date.i}

find mnpprocess where
     mnpprocess.portrequest = "00400511100709113207078" and
     mnpprocess.mnptype = 2 EXCLUSIVE-LOCK.

assign
   mnpprocess.statuscode = 7
   mnpprocess.statusreason = "CANC_TECNI"
   mnpprocess.updatets = fMakeTS().
