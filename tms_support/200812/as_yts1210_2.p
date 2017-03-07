{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".

DEFINE VARIABLE msreqs AS CHARACTER NO-UNDO. 

msreqs = "6208085 6208953 6208954 6208955 6208956 6208957 6208958 6208959 6208960 6208961 6208962 6208963 6208964 6208965 6208966".

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

do i = 1 to num-entries(msreqs, " ") with frame a:
   find msrequest where 
        msrequest.msrequest = int(entry(i, msreqs, " ")) NO-LOCK NO-ERROR.
   RUN /apps/snet/200812/susprequest_yts1210.p(msrequest.msrequest).
end.
