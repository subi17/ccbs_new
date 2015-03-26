for each customer no-lock where
    brand = "1" AND
    orgid = "21524107V":

    for each mobsub no-lock where
        mobsub.brand = "1" AND
        (mobsub.msstatus = 4 OR
        mobsub.msstatus = 7 OR
        mobsub.msstatus = 8) AND
        mobsub.agrcust = customer.custnum:
        
        for each msrequest no-lock where 
            msrequest.brand = "1" AND
            msrequest.custnum = mobsub.agrcust:
            
            if msrequest.reqtype <> 35 THEN NEXT.
            
            disp msrequest.cli msrequest.reqtype.
        END.
    END.
end.