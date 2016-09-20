DEF VAR ldaFrom AS DATE INIT 09/07/16.
DEF VAR limode AS INT INIT 0.

DEF TEMP-TABLE ttDaycampaign NO-UNDO LIKE Daycampaign.

FUNCTION fcreateDaycampaign RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                             INPUT icEvent AS CHAR,
                                             INPUT icname AS CHAR,
                                             INPUT icdctype AS CHAR,
                                             INPUT iiUpdateMode AS INT):
   FIND FIRST Daycampaign WHERE 
              Daycampaign.brand EQ "1" AND
              Daycampaign.dcevent EQ icBaseDCEvent NO-ERROR.
      CREATE ttDaycampaign.
      BUFFER-COPY daycampaign TO ttDaycampaign.
      ttDaycampaign.dctype = icDctype.
      ttDaycampaign.dcevent = icevent.
      ttDaycampaign.billcode = icevent + "MF".
      ttDaycampaign.feemodel = icevent + "MF".
      ttDaycampaign.dcname = icName.
      
      IF iiUpdateMode NE 0 THEN DO:
         CREATE Daycampaign.
         BUFFER-COPY ttDaycampaign TO Daycampaign.
         DELETE ttDaycampaign. /*ror safety reasons*/
      END.
      ELSE DISP ttDayCampaign.
   
END.

fcreateDaycampaign("CONTS2GB","CONTDSL45","La Infinita 2 GB + DSL","7",limode).
fcreateDaycampaign("CONTS10GB","CONTDSL55","La Infinita 10 GB + DSL","7",limode).
fcreateDaycampaign("CONTS2GB","CONTFH45_50","La Infinita 2 GB + FIBER 50M","7",limode).
fcreateDaycampaign("CONTS10GB","CONTFH55_50","La Infinita 10 GB + FIBER 50M","7",limode).
fcreateDaycampaign("CONTS2GB","CONTFH55_300","La Infinita 2 GB + FIBER 300M","7",limode).
fcreateDaycampaign("CONTS10GB","CONTFH65_300","La Infinita 10 GB + FIBER 300M","7",limode).
