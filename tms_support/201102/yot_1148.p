FOR EACH billitem where
         billitem.brand = "1" and
        lookup(billitem.billcode,"MDUB,MDUBMF,MDUBMFUPS,MDUB2,MDUB2MF,MDUB2MFUPS") > 0:
   costcentre = "SL".
   disp billcode costcentre.
END.

