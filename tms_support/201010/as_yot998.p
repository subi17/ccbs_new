{Func/date.i}
find mnpprocess where
     mnpprocess.portrequest = "00500311101020184024233" and
     mnpprocess.statuscode = 4 EXCLUSIVE-LOCK.

mnpprocess.statuscode = 8.
mnpprocess.updatets = fMakeTS().
