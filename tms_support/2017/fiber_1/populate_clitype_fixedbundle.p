DEF VAR lcFixedBundle AS CHAR NO-UNDO. 

DEF VAR llSimulate AS LOG NO-UNDO init false.

FOR EACH CLIType EXCLUSIVE-LOCK WHERE
   CLIType.FixedLineDownload > "" AND
   CLIType.FixedBundle EQ "":

   lcFixedBundle = "".

   IF INDEX(CLIType.CLIType,"1000") > 0 THEN lcFixedBundle = "CONTFH100".
   ELSE IF INDEX(CLIType.CLIType,"300") > 0 THEN lcFixedBundle = "CONTFH300".
   ELSE IF INDEX(CLIType.CliType,"50") > 0 THEN lcFixedBundle = "CONTFH50".
   ELSE lcFixedBundle = "CONTDSL".

   disp CliType.CliType lcFixedBundle format "x(30)".
   
   if not llSimulate then CliType.FixedBundle = lcFixedBundle.
      
END.
