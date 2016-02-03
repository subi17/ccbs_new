/* ----------------------------------------------------------------------
  MODULE .......: penfeerules.p 
  TASK .........: shows penalty fee rules for TERM18 contract
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 04/2008 
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Rate/daycampaign.i}
{Func/fcustpl.i}
{Func/penaltyfee.i}

DEF  INPUT PARAMETER iiDCCLIRecId  AS RECID NO-UNDO.
DEF VAR liPeriod   AS INT   NO-UNDO.
DEFINE VARIABLE idtFrom AS DATE NO-UNDO. 
DEFINE VARIABLE idtTo AS DATE NO-UNDO. 
/* change */

FIND FIRST DCCLI where 
   RECID(DCCLI) = iiDCCLIRecid NO-LOCK NO-ERROR.

DEF VAR icEvent  AS CHAR   NO-UNDO.
icEvent = DCCLI.DCEvent.

liperiod = YEAR(idtFrom) * 100 +
           MONTH(idtTo).

DEF VAR lcEvent    LIKE DCCounter.DCEvent    NO-UNDO.
DEF VAR lccli      LIKE dccli.cli            NO-UNDO.
DEF VAR lcCampaignEvent LIKE DayCampaign.dcName NO-UNDO.
DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 1.
DEF VAR ufkey      AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline    AS INT                    NO-UNDO  init 0.
DEF VAR ex-order   AS INT                    NO-UNDO.
DEF VAR memory     AS RECID                  NO-UNDO.
def var line       as int format "99"        NO-UNDO.
DEF VAR must-print AS LOG                    NO-UNDO.
DEF VAR must-add   AS LOG                    NO-UNDO.
DEF VAR fr-header  AS CHAR                   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24        NO-UNDO.
DEF VAR i          AS INT                    NO-UNDO.
DEF VAR iLoop      AS INT                    NO-UNDO.
def var ok         as log format "Yes/No"    NO-UNDO.
DEF VAR lcWeekDay  AS CHAR                   NO-UNDO.
DEF VAR lcCalcMethod AS CHAR                 NO-UNDO FORMAT "X(40)" .
DEF VAR lcUnit       AS CHAR                 NO-UNDO FORMAT "X(40)" .
DEF VAR lcTypeName AS CHAR                   NO-UNDO FORMAT "X(40)" .
DEF VAR ldeCurrPen  AS DEC                   NO-UNDO.
DEF VAR lcMaxCharge AS CHAR                  NO-UNDO.
DEF VAR ldaDCDateTo AS DATE                  NO-UNDO.
DEF VAR liRemPeriod AS INT                   NO-UNDO.
DEF VAR llActive AS LOG                      NO-UNDO.
DEF VAR ldePrice AS DEC                      NO-UNDO.
DEF VAR lcPriceList AS CHARACTER NO-UNDO.

DEF BUFFER xxDCCounter FOR DCCounter.

form
   " Contract status...:" llActive FORMAT "Active/Deactive" SKIP
   " Calculation method:" DayCampaign.CalcMethod lcCalcMethod FORMAT "x(30)" SKIP
   " Effective date....:" DCCLI.ValidFrom format 99-99-9999 SKIP
   " Expiration date...:" DCCLI.ValidTo   format 99-99-9999 SKIP
   " Original exp. date:" DCCLI.ValidToOrig format 99-99-9999 SKIP
" ---------------------------------------------------------------------------"
   SKIP  
   " Billing Event.....:" DayCampaign.TermFeeModel FeeModel.FeeName AT 35 SKIP
   " Price List........:" lcPriceList FORMAT "X(13)" PriceList.PLName AT 35 SKIP
   " Billing Item......:" DayCampaign.BillCode FORMAT "X(13)" 
                          BillItem.BIName AT 35     SKIP 
   " Price.............:" ldePrice SKIP 
" ---------------------------------------------------------------------------"
   " Remaining Period..:" liRemPeriod FORMAT ">>>9" SKIP
   " Current penalty ..:" ldeCurrPen SKIP
   " Create penalty Fee:" DCCLI.CreateFees SKIP
WITH OVERLAY ROW 2 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   " PENALTY FEE RULES FOR " + icEvent + " "
   WITH no-labels side-labels
   FRAME lis.


assign ufkey = TRUE ehto = 3.
RUN ufkey.

cfc = "lis". RUN ufcolor.

RUN LOCAL-UPDATE-RECORD.
       
PROCEDURE LOCAL-UPDATE-RECORD. 
   
   FIND FIRST DayCampaign WHERE
      DayCampaign.Brand = gcBrand AND
      DayCampaign.DCEvent = dccli.dcevent NO-LOCK NO-ERROR.

   IF AVAIL DayCampaign THEN DO:
      FIND FIRST TMSCodes WHERE
                 TMSCodes.Tablename    = "Daycampaign"   AND
                 TMSCodes.FieldName    = "InclUnit"      AND
                 TMSCodes.CodeGroup    = "Unit"          AND
                 TMSCodes.CodeValue    = STRING(Daycampaign.InclUnit)
      NO-LOCK NO-ERROR. 
      IF AVAIL TMSCodes THEN lcUnit = TMSCodes.CodeName. 

      FIND FIRST TMSCodes WHERE
                 TMSCodes.Tablename    = "DayCampaign"   AND
                 TMSCodes.FieldName    = "CalcMethod"    AND
                 TMSCodes.CodeGroup    = "DCCounter"     AND
                 TMSCodes.CodeValue    = STRING(DayCampaign.Calcmethod)
      NO-LOCK NO-ERROR.
      IF AVAIL TMSCodes THEN lcCalcMethod = TMSCodes.CodeName.
      
      FIND FIRST BillItem WHERE
                 BillItem.Brand    = gcBrand AND
                 BillItem.BillCode = DayCampaign.BillCode
      NO-LOCK NO-ERROR.
   
   END.

   ELSE DO:
      MESSAGE "Unknown contract"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   
   IF DayCampaign.TermFeeModel = "" THEN DO:
      MESSAGE "No fee defined for this contract"
      VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.
      
   FIND FIRST MobSub WHERE
      MobSub.MsSeq = DCCLI.MsSeq NO-LOCK NO-ERROR.
   
   lcPriceList = fFeeModelPriceList(MobSub.Custnum,
                                    MobSub.BillTarget,
                                    DayCampaign.TermFeeModel,
                                    TODAY).
   
   FIND FIRST FMItem NO-LOCK  WHERE
            FMItem.Brand     = gcBrand       AND
            FMItem.FeeModel  = DayCampaign.TermFeeModel AND
            FMItem.PriceList = lcPriceList AND
            FMItem.FromDate <= TODAY     AND
            FMItem.ToDate   >= TODAY NO-ERROR.
   
   IF AVAIL fmitem THEN 
   ldePrice = fmitem.amount.
   
   IF DCCLI.Amount NE ? THEN ldePrice = DCCLI.Amount.
   
   ASSIGN
      llActive    = DCCLI.TermDate EQ ?.
      liRemPeriod = DCCLI.ValidTo - TODAY.
   
   IF liRemPeriod < 0 THEN liRemPeriod = 0.

   /* calculate a factor for the fee (full / proportional) */
   ldeCurrPen = fCalculateFactor(DCCLI.ValidFrom,
                                 DCCLI.RenewalDate,
                                 DCCLI.ValidTo,
                                 DCCLI.ValidToOrig,
                                 TODAY,
                                 DayCampaign.TermFeeCalc).
   
   ldeCurrPen = TRUNCATE(ldeCurrPen * ldePrice,0).

   FIND FIRST FeeModel NO-LOCK WHERE
      FeeModel.Brand = gcBrand AND
      FeeModel.FeeModel = DayCampaign.TermFeeModel NO-ERROR.

   FIND FIRST PriceList NO-LOCK WHERE
      PriceList.Brand = gcBrand AND
      PriceList.PriceList = lcPriceList NO-ERROR.

   DISP
      llActive
      DCCLI.ValidFrom
      DCCLI.ValidTo
      DCCLI.ValidToOrig
      ldeCurrPen
      DayCampaign.CalcMethod lcCalcMethod
      DayCampaign.TermFeeModel
      FeeModel.FeeName WHEN AVAIL FeeModel
      DayCampaign.BillCode
      BillItem.BIName WHEN AVAIL BillItem
      lcPriceList
      PriceList.PLName WHEN AVAIL PriceList
      ldePrice
      liRemPeriod
      DCCLI.CreateFees 
   WITH FRAME lis.

   MESSAGE " - PRESS ENTER TO CONTINUE - " . PAUSE NO-MESSAGE.                               
   HIDE FRAME lis.
   
END.

