{Syst/commpaa.i}
Syst.CUICommon:katun = "anttis".
Syst.CUICommon:gcBrand = "1".

DEFINE VARIABLE oiCustomer AS INTEGER NO-UNDO. 

RUN /apps/snet/200904/createcustomer_ycm1436.p(INPUT 1896072,1,FALSE,output oiCustomer).
disp oiCustomer.
