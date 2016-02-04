RUN tms_support/utilities/rundynimporttablesofdir.p("/home/harrim/renewal2", FALSE).

FOR EACH Account NO-LOCK:
   DISP Account.
END.

