FOR LAST msisdn where cli = "622087605" and statuscode = 0 EXCLUSIVE-LOCK:
   statuscode = 4.
   validto = 20070209.39026.
END.

FOR LAST msisdn where cli = "622517007" and statuscode = 0 EXCLUSIVE-LOCK. 
  statuscode = 4.
  validto = 20080206.46821.
END.

FOR LAST msisdn where cli = "656953028" and statuscode = 0 EXCLUSIVE-LOCK. 
  statuscode = 11.
  validto = 99999999.99999.
END.

FOR LAST msisdn where cli = "678182354" and statuscode = 0 EXCLUSIVE-LOCK. 
  statuscode = 11.
  validto = 99999999.99999.
END.

FOR EACH termmobsub where 
   cli = "678182354" or
   cli = "622517007" or
   cli = "656953028" or
   cli = "678182354" NO-LOCK:
   find first sim where sim.icc = termmobsub.icc EXCLUSIVE-LOCK.
   sim.simstat = 7.
END.
