def stream sout.
output stream sout to ypr_5973.log.

DEF VAR ldaNewFrom AS DATE NO-UNDO. 
DEF VAR ldaNewTo AS DATE NO-UNDO.
DEF VAR llSimulate AS LOG NO-UNDO. 

ASSIGN
   llSimulate  = false
   ldaNewFrom = 6/7/2017
   ldaNewTo = 6/6/2017.

put stream sout unformatted "NEW_TARIFFS:" skip.
FOR EACH tariff EXCLUSIVE-LOCK where
         tariff.validfrom eq 6/15/2017 and
         tariff.ccn = 90:
   put stream sout unformatted
      tariff.tariffnum ";"
      tariff.pricelist ";"
      tariff.ccn ";"
      tariff.bdest ";"
      tariff.validfrom ">" ldaNewFrom ";"
      tariff.validto skip.

   if not llSimulate then
      tariff.validfrom = ldaNewFrom.
end.

put stream sout unformatted skip(1) "OLD_TARIFFS:" skip.
FOR EACH tariff EXCLUSIVE-LOCK where
         tariff.validto = 6/14/2017 and
         tariff.ccn = 90:
   put stream sout unformatted
      tariff.tariffnum ";"
      tariff.pricelist ";"
      tariff.ccn ";"
      tariff.bdest ";"
      tariff.validfrom ";"
      tariff.validto ">" ldaNewTo skip.

   if not llSimulate then
      tariff.validto = ldaNewto.
end.

put stream sout unformatted skip(1) "NEW_SLGANALYSES:" skip.
FOR EACH slganalyse EXCLUSIVE-LOCK where
         slganalyse.brand = "1" and
         slganalyse.billcode = "14104019" and
         slganalyse.ccn = 90 and
         slganalyse.validfrom = 6/15/2017:
   put stream sout unformatted
      slganalyse.servicelimitgroup ";"
      slganalyse.clitype ";"
      slganalyse.billcode ";"
      slganalyse.validfrom ">" ldaNewFrom ";"
      slganalyse.validto skip. 
   
   if not llSimulate then
      slganalyse.validfrom = ldaNewfrom.
end.

put stream sout unformatted skip(1) "OLD_SLGANALYSES:" skip.
FOR EACH slganalyse EXCLUSIVE-LOCK where
         slganalyse.brand = "1" and
         slganalyse.servicelimitgroup eq "hspa_roam_eu" and
         slganalyse.ccn = 90 and
         slganalyse.validto = 6/14/2017:
   put stream sout unformatted
      slganalyse.servicelimitgroup ";"
      slganalyse.clitype ";"
      slganalyse.billcode ";"
      slganalyse.validfrom ";"
      slganalyse.validto ">" ldaNewTo skip.
   
   if not llSimulate then
      slganalyse.validto = ldaNewto.
end.

