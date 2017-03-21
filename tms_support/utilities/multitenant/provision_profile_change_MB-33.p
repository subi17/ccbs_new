/* Run using super tenant! */

FOR EACH CliType EXCLUSIVE-LOCK WHERE CliType.ServicePack = "12"
   TENANT-WHERE TENANT-ID() >= 0:
   
   IF BUFFER-TENANT-NAME(CliType) = "Default"
   THEN CliType.ServicePack = 42.
   ELSE CliType.ServicePack = 52.
   
END.

FOR EACH CliType EXCLUSIVE-LOCK WHERE CliType.ServicePack = "11"
   TENANT-WHERE TENANT-ID() >= 0:
   
   IF BUFFER-TENANT-NAME(CliType) = "Default"
   THEN CliType.ServicePack = 41.
   ELSE CliType.ServicePack = 51.
   
END.
