{Syst/commpaa.i}
Syst.CUICommon:katun = "anttis".
Syst.CUICommon:gcBrand = "1".


find mnpprocess where
     mnpprocess.portrequest = "00400511100709113207078" and
     mnpprocess.mnptype = 2 EXCLUSIVE-LOCK.

assign
   mnpprocess.statuscode = 7
   mnpprocess.statusreason = "CANC_TECNI"
   mnpprocess.updatets = Func.Common:mMakeTS().
