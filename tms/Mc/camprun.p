 /* ------------------------------------------------------
  MODULE .......: camprun.p
  FUNCTION .....: campaign run
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 18.02.04
  MODIFIED .....: 14.09.04/aam fCopyDefFatime()
                  22.09.04/aam iiEventPeriod
                  29.09.04/aam use MobSub.ActivationTs
                  29.11.04/aam new parameter to fcpfat-functions
                  13.10.05/aam take begin date from msowner if activationts=0
                  04.05.06/aam new logic for fat creation, use creafat.p
                  21.09.06/aam new parameter fo creafat.p (%)
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{camprundf.i}
{fcustpl.i}
{nncoit2.i}
{timestamp.i}
{fmakeservlimit.i}
{setfees.i}

DEF STREAM sLog.
OUTPUT STREAM sLog TO /apps/tms/snet/camrun.log append.

DEF INPUT  PARAMETER TABLE FOR ttCust.
DEF INPUT  PARAMETER icCampaign1   AS CHAR NO-UNDO. 
DEF INPUT  PARAMETER icCampaign2   AS CHAR NO-UNDO. 
DEF INPUT  PARAMETER iiEventPeriod AS DEC  NO-UNDO. 
DEF INPUT  PARAMETER ilMessages    AS LOG  NO-UNDO. 
DEF OUTPUT PARAMETER oiCount       AS INT  NO-UNDO.

DEF VAR llFound    AS LOG  NO-UNDO.
DEF VAR liCreated  AS INT  NO-UNDO.
DEF VAR liPer      AS INT  NO-UNDO. 
DEF VAR liDiff     AS INT  NO-UNDO. 
DEF VAR liCnt      AS INT  NO-UNDO. 
DEF VAR liEnd      AS INT  NO-UNDO. 
DEF VAR liFromPer  AS DEC  NO-UNDO.
DEF VAR liToPer    AS DEC  NO-UNDO. 
DEF VAR ldtBegDate AS DATE NO-UNDO.
DEF VAR liBegTime  AS INT  NO-UNDO. 
DEF VAR lcError    AS CHAR NO-UNDO.

DEF TEMP-TABLE ttCamp NO-UNDO
   FIELD Campaign  AS CHAR
   FIELD FromDate  AS DATE
   FIELD ToDate    AS DATE
   FIELD FromPer   AS DEC
   FIELD ToPer     AS DEC
   FIELD CRowType  AS INT
   FIELD CRowItem  AS CHAR
   FIELD CLIType   AS CHAR
   FIELD BillCode  AS CHAR.
 


/* valid campaigns */
FOR EACH Campaign NO-LOCK WHERE
         Campaign.Brand     = gcBrand     AND
         Campaign.Campaign >= icCampaign1 AND
         Campaign.Campaign <= icCampaign2,
    EACH CampRow OF Campaign NO-LOCK:

   CREATE ttCamp.
   ASSIGN ttCamp.Campaign = Campaign.Campaign
          ttCamp.FromDate = Campaign.FromDate
          ttCamp.ToDate   = Campaign.ToDate
          ttCamp.FromPer  = YEAR(Campaign.FromDate) * 10000 + 
                            MONTH(Campaign.FromDate) * 100  +
                            DAY(Campaign.FromDate)
          ttCamp.ToPer    = YEAR(Campaign.ToDate) * 10000 + 
                            MONTH(Campaign.ToDate) * 100  +
                            DAY(Campaign.ToDate)          +
                            0.86399
          ttCamp.CRowType = CampRow.CRowType
          ttCamp.CRowItem = CampRow.CRowItem
          ttCamp.CLIType  = CampRow.CLIType
          ttCamp.BillCode = CampRow.BillCode.
END.



/* customers and clis to which campaign can be used */
FOR EACH ttCust,
FIRST Customer NO-LOCK WHERE
      Customer.Brand   = gcBrand AND
      Customer.CustNum = ttCust.CustNum,
FIRST MSOwner NO-LOCK USE-INDEX MSSeq WHERE
      MSOwner.MSSeq   = ttCust.MSSeq AND
      MsOwner.CustNum = ttCust.CustNum:

   PUT STREAM sLog UNFORMATTED
      MsOwner.CLI     CHR(9)
      MsOwner.CLIType CHR(9)
      iiEventPeriod   CHR(9)
      "Find"        SKIP.
      
   /* event period given (e.g. order creation) */
   IF iiEventPeriod > 0 
   THEN ASSIGN liFromPer = iiEventPeriod
               liToPer   = iiEventPeriod.
   ELSE ASSIGN liFromPer = MsOwner.TSBeg
               liToPer   = MsOwner.TSEnd.
      
   FOR EACH ttCamp WHERE
            ttCamp.FromPer <= liToPer AND
            ttCamp.ToPer   >= liFromPer:
   
      IF ttCamp.CLIType > "" AND
         MsOwner.CLIType NE ttCamp.CLIType 
      THEN NEXT.
      
      /* already used for this cli */
      IF CAN-FIND(FIRST CampStat WHERE
                        CampStat.Brand    = gcBrand         AND
                        CampStat.Campaign = ttCamp.Campaign AND
                        CampStat.CLI      = MsOwner.CLI      AND
                        CampStat.CustNum  = MsOwner.CustNum)
      THEN NEXT.

      ASSIGN llFound = FALSE
             liEnd   = TRUNCATE(MsOwner.TSEnd / 100,0).

      /* try to use activation timestamp from mobsub, it not available
         (already killed) then use owner data 
         
         activation month is always the first, regardless of the
         activation date (even if it is e.g. 30th) 
      */
      ldtBegDate = ?.
      FIND MobSub WHERE MobSub.MsSeq = MsOwner.MsSeq NO-LOCK NO-ERROR.
      IF AVAILABLE MobSub AND
         MobSub.ActivationTS > 0
      THEN DO:
         liPer = TRUNCATE(MobSub.ActivationTS / 100,0).
             
         fSplitTS(MobSub.ActivationTS,
                  OUTPUT ldtBegDate,
                  OUTPUT liBegTime).
      END.
      
      IF ldtBegDate = ? THEN DO:
         liPer = TRUNCATE(MsOwner.TsBeg / 100,0).
             
         fSplitTS(MsOwner.TsBeg,
                  OUTPUT ldtBegDate,
                  OUTPUT liBegTime).
      END.
       
      CASE ttCamp.CRowType:
      WHEN 1 THEN DO:  /* pricelist */
             END.
      WHEN 2 THEN DO:  /* discplan */
             END.
      WHEN 3 THEN DO:  /* fatime */
      
                IF liPer > 0 THEN DO:
                   RUN creafat (Customer.CustNum,
                                MsOwner.MsSeq,
                                ttCamp.CRowItem,
                                0, /* amount */
                                0, /* % */
                                ?,
                                liPer,
                                liEnd,
                                OUTPUT lcError).
                   llFound = (lcError = "").

                   PUT STREAM sLog UNFORMATTED
                      MsOwner.CLI   CHR(9)
                      ldtBegDate    CHR(9)
                      "fat " 
                      string(llfound,"created/failed") SKIP.
                END. 

             END. 
             
      WHEN 4 THEN DO:  /* feemodel */
      
                IF ldtBegDate NE ? THEN
                liCreated = fMakeSetfees (ttCamp.CRowItem,
                                          MsOwner.CustNum,
                                          MsOwner.MSSeq,
                                          MsOwner.BillTarget,  
                                          ttCamp.Campaign,   
                                          "",
                                          liPer,
                                          ldtBegDate,
                                          FALSE,            /* interact */
                                          ?,         /* price from feemodel */
                                          "",               /* contract */
                                          TRUE,            /* active */
                                          katun,
                                          "",
                                          0,
                                          "",
                                          "").
                ELSE liCreated = 0.
                                          
                llFound = (liCreated > 0).
              
                PUT STREAM sLog UNFORMATTED
                   MsOwner.CLI   CHR(9)
                   ldtBegDate    CHR(9)
                   liCreated     CHR(9)
                   "created"     SKIP.
                                     
             END.
      END CASE.
      
      IF llFound THEN DO:
   
         oiCount = oiCount + 1.
   
         /* statistics from handled campaign events */ 
         CREATE CampStat.
         ASSIGN CampStat.Brand    = gcBrand
                CampStat.Campaign = ttCamp.Campaign
                CampStat.CustNum  = MsOwner.CustNum
                CampStat.CLI      = MsOwner.CLI
                CampStat.CampDate = TODAY.
                
         IF ilMessages AND
            (oiCount < 100 OR oiCount MOD 100 = 0)
         THEN DO:
            PAUSE 0.
            DISPLAY "Campaign events:" oiCount
            WITH NO-LABELS OVERLAY ROW 15 CENTERED TITLE " Setting "
            FRAME fQty1.
         END.
         
      END.
      
   END.
   
END.

OUTPUT STREAM sLog CLOSE.


