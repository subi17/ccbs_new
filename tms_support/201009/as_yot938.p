{timestamp.i}

/* ONLINE -> VIP */
find msisdn where
     msisdn.brand = "1" and
     msisdn.cli = "633160099" and
     msisdn.pos = "online" and
     msisdn.validto > fMakeTS() EXCLUSIVE-LOCK.

msisdn.pos = "VIP".

disp msisdn.cli msisdn.pos.
