def var ttdonestamp as int
    label "Since (yyyymmdd)"
    format "99999999"
no-undo.

update ttdonestamp.

for each msrequest no-lock where
    brand = "1" AND
    reqtype = 18 AND
    donestamp >= ttdonestamp:

    find first msowner where
        msowner.brand = "1" AND
        msowner.msseq = msrequest.msseq
    NO-LOCK NO-ERROR.

    disp msrequest.cli msowner.clitype donestamp.
end.

