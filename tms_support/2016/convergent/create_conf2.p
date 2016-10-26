DEF VAR liModeCliType AS INT INIT 1.

IF liModeCliType > 0 THEN DO:
   FOR EACH CliType WHERE
            Clitype.brand EQ "1" AND
            Clitype.clitype BEGINS "CONTDSL":
      ASSIGN
      Clitype.fixedlinetype = 1
      Clitype.webstatuscode = 0
      Clitype.FixedLineDownload = "20M"
      Clitype.FixedLineUpload = "20M".
   END.

   FOR EACH CliType WHERE
            Clitype.brand EQ "1" AND
            Clitype.clitype MATCHES "CONTFH*50":
      ASSIGN
      Clitype.fixedlinetype = 2
      Clitype.webstatuscode = 0
      Clitype.FixedLineDownload = "50M"
      Clitype.FixedLineUpload = "5M".

   END.

   FOR EACH CliType WHERE
         Clitype.brand EQ "1" AND
         Clitype.clitype MATCHES "CONTFH*300":
      ASSIGN
      Clitype.fixedlinetype = 2
      Clitype.webstatuscode = 0
      Clitype.FixedLineDownload = "300M"
      Clitype.FixedLineUpload = "300M".

   END.
END.

FUNCTION fCreateTrans RETURNS LOG (INPUT icCode AS CHAR,
                                   INPUT iiLang AS INT,
                                   INPUT icText AS CHAR):
   FIND FIRST RepText WHERE
              Reptext.brand EQ "1" AND
              Reptext.texttype EQ 1 AND
              Reptext.linkcode EQ icCode AND
              Reptext.language EQ iiLang NO-ERROR.
              
      IF NOT AVAIL RepText THEN DO:        
         CREATE RepText.
         ASSIGN
            RepText.Brand    = "1"
            RepText.TextType = 1               /* Default value */
            RepText.LinkCode = icCode
            RepText.Language = iiLang
            RepText.FromDate = TODAY
            RepText.ToDate   = 12/31/49
            RepText.RepText  = icText  NO-ERROR.
       END.
   RETURN TRUE.
END.

fcreateTrans("51",1,"LLAMADAS FIJO").
fcreateTrans("51",2,"LLAMADAS FIJO").
fcreateTrans("51",3,"LLAMADAS FIJO").
fcreateTrans("51",5,"LLAMADAS FIJO").
fcreateTrans("53",1,"SERVICIOS PREMIUM FIJO").
fcreateTrans("53",2,"Catalan;SERVICIOS PREMIUM FIJO").
fcreateTrans("53",3,"SERVICIOS PREMIUM FIJO").
fcreateTrans("53",5,"SERVICIOS PREMIUM FIJO").

