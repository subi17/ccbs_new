{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "Qvantel".

DEF VAR ldReqAmt  AS DEC  NO-UNDO.
DEF VAR ldaDate   AS DATE NO-UNDO.
DEF VAR lcReqChar AS CHAR NO-UNDO.
DEF VAR llclosed  AS LOG  NO-UNDO.
DEF VAR ldAmt     AS DEC  NO-UNDO INIT 6.
DEF VAR liMsSeq   AS INT  NO-UNDO.
DEF VAR liCustNum AS INT  NO-UNDO.
DEF VAR ldaMobActDate AS DATE NO-UNDO.
DEF VAR llActive  AS LOG NO-UNDO.

ldaDate = 06/01/2014.


EACH_MOBSUB:
FOR FIRST DayCampaign WHERE
          DayCampaign.Brand = Syst.Var:gcBrand AND
          DayCampaign.DCEvent = "PAYTERM24_5" NO-LOCK,
    FIRST MobSub WHERE
          MobSub.MsSeq = 11092403 NO-LOCK,
    FIRST DCCLI WHERE
          DCCLI.MsSeq = MobSub.MsSeq AND
          DCCLI.DCEvent = DayCampaign.DCEvent NO-LOCK:


       ASSIGN liMsSeq = MobSub.MsSeq
              liCustNum = MobSub.CustNum
              ldaMobActDate = MobSub.ActivationDate
              llActive = TRUE.

   RUN Mc/creasfee.p(liCustNum,
                  liMsSeq,
                  ldaDate,
                  "FeeModel",
                  DayCampaign.FeeModel,
                  9,
                  ?,
                  DayCampaign.DCEvent + " created " + 
                  STRING(TODAY,"99.99.9999") +  /* memo */
                  "�" +  DayCampaign.DCEvent ,  /* calcobject */
                  FALSE,              /* no messages to screen */
                  Syst.Var:katun,
                  "",
                  0, /* order id */
                  "DCCLI",
                  STRING(DCCLI.PerContractId),
                  OUTPUT lcReqChar).

END.