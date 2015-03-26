/* fcpfat.i       18.02.04/aam 

   copy fat rows to cli from default rows
   
   callers:  camprun.p
             cobal.p
             creafat.p
             readpaym.p

   changed:       29.11.04/aam icMemo
                  27.04.06/aam fCreateFatRow()
                  21.09.06/aam new input parameter idPerc to fCreateFatRow      
*/
{commali.i}
{create_eventlog.i}

DEF BUFFER bCPFatime   FOR Fatime.
DEF BUFFER bCPFatGroup FOR FatGroup.
DEF BUFFER bFatCust    FOR Customer.

DEF VAR liFatCreated AS INT NO-UNDO. 
DEF VAR liMemoCnt    AS INT NO-UNDO. 

DEF VAR lhTable AS HANDLE NO-UNDO.
lhTable = BUFFER FATime:HANDLE.

FUNCTION fFATLastPeriod RETURNS INTEGER
   (iiFromPeriod   AS INT,
    iiValidPeriods AS INT):
 
   DEF VAR liCountPer   AS INT NO-UNDO.
   DEF VAR liLastPeriod AS INT NO-UNDO.    
       
   IF iiValidPeriods = 0 THEN RETURN 0.
   
   liLastPeriod = iiFromPeriod.
   
   /* last period that this fatime can be used for */
   IF iiValidPeriods > 1 THEN 
   DO liCountPer = 2 TO iiValidPeriods:
      IF liLastPeriod MOD 100 < 12 
      THEN liLastPeriod = liLastPeriod + 1.
      ELSE liLastPeriod = (TRUNCATE(liLastPeriod / 100,0) + 1) * 100 + 1.
   END.      

   RETURN liLastPeriod.
     
END FUNCTION.

FUNCTION fCopyDefFatime RETURNS LOGICAL
   (icFatGrp   AS CHAR,
    iiCustNum  AS INT,
    iiMsSeq    AS INT,
    icCLI      AS CHAR,
    idAmt      AS DEC,
    iiFromPer  AS INT,
    iiToPer    AS INT,
    icMemo     AS CHAR).
    
   DEF VAR llDone     AS LOG NO-UNDO.
   DEF VAR liDiff     AS INT NO-UNDO. 
   DEF VAR liCnt      AS INT NO-UNDO. 
   DEF VAR ldAmtLimit AS DEC NO-UNDO.
    
   ASSIGN llDone       = FALSE
          liDiff       = ?
          liFatCreated = 0.         
               
   IF NOT AVAILABLE FatGroup OR
      FatGroup.FtGrp NE icFatGrp
   THEN DO:
      FIND FatGroup WHERE 
           FatGroup.Brand = gcBrand AND
           FatGroup.FtGrp = icFatGrp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FatGroup THEN RETURN FALSE.
   END.
 
   ldAmtLimit = IF FatGroup.AmtLimit > 0
                THEN FatGroup.AmtLimit
                ELSE 99999999.

   IF idAmt = 0 THEN idAmt = 99999999. /* not limited */
   
   /* get default rows */             
   FOR EACH bCPFatime NO-LOCK WHERE
            bCPFatime.Brand = gcBrand  AND
            bCPFatime.FTGrp = icFatGrp AND
            bCPFatime.CLI   = "Def"
   BY bCPFatime.Period:
           
      /* if cli has not been given then "calls"-type can not be used */
      IF bCPFatime.FatType = 0 AND icCLI = "" THEN NEXT.
      
      /* get the margin between first period to be created and first 
         default period */
      IF liDiff = ? THEN DO:
          IF bCPFatime.Period = iiFromPer THEN liDiff = 0.
                     
          IF bCPFatime.Period < iiFromPer 
          THEN liDiff = (TRUNCATE(iiFromPer / 100,0) - 1 - 
                        TRUNCATE(bCPFatime.Period / 100,0)) * 12 +
                        (iiFromPer MOD 100) +
                        (12 - bCPFatime.Period MOD 100).

          ELSE liDiff = (TRUNCATE(iiFromPer / 100,0) + 1 -  
                        TRUNCATE(bCPFatime.Period / 100,0)) * 12 -
                        (12 - iiFromPer MOD 100) -
                        (bCPFatime.Period MOD 100).
                        
          /* if amount has been limited then calculate previous events */
          IF FatGroup.AmtLimit > 0 THEN 
          FOR EACH Fatime NO-LOCK WHERE
                   Fatime.Brand   = gcBrand   AND
                   Fatime.FtGrp   = icFatGrp  AND
                   Fatime.CustNum = iiCustNum AND
                   Fatime.MsSeq   = iiMsSeq:
             ldAmtLimit = ldAmtLimit - FATime.Amt 
                          /* - FATime.Used - FATime.TransQty) */.
          END.
          
      END.
        
      IF ldAmtLimit <= 0 OR idAmt <= 0 THEN DO:
         /* mark so that fCreateFatime() will not be run */
         llDone = TRUE.
         LEAVE. 
      END.
      
      /* copy fatime row to target */             
      CREATE Fatime.
      BUFFER-COPY bCPFatime EXCEPT FatNum FatId TO Fatime.
      ASSIGN Fatime.FatNum    = NEXT-VALUE(ftseq)
             Fatime.Fatid     = NEXT-VALUE(ft-seq)
             Fatime.CustNum   = iiCustNum
             Fatime.CLI       = icCLI WHEN bCPFatime.FatType = 0
             Fatime.MsSeq     = iiMsSeq
             /* check amount limit */   
             Fatime.Amt       = MIN(Fatime.Amt,ldAmtLimit)
             /* and given amount */
             Fatime.Amt       = MIN(Fatime.Amt,idAmt)
             Fatime.PayerType = PROGRAM-NAME(2)
             Fatime.InvNum    = 0
             Fatime.TransQty  = 0
             Fatime.Used      = 0 
             ldAmtLimit       = ldAmtLimit - Fatime.Amt
             idAmt            = idAmt - Fatime.Amt
             liFatCreated     = liFatCreated + 1.

      IF icMemo > "" THEN DO liMemoCnt = 1 TO 5:
         IF Fatime.Memo[liMemoCnt] = "" THEN DO:
            Fatime.Memo[liMemoCnt] = icMemo.
            LEAVE.
         END.
      END.
      
      /* modify default row's period so that it is sensible for target */
        
      /* margin is within current year */
      IF bCPFatime.Period MOD 100 + liDiff <= 12 AND
         bCPFatime.Period MOD 100 + liDiff > 0 
      THEN Fatime.Period = bCPFatime.Period + liDiff.

      /* margin exceeds new year */
      ELSE IF liDiff > 0 THEN DO:
         ASSIGN Fatime.Period = bCPFatime.Period.
                liCnt = liDiff - (13 - Fatime.Period MOD 100).
                       
         DO WHILE TRUE:
            Fatime.Period = (TRUNCATE(Fatime.Period / 100,0) + 1)
                            * 100 + 1.

            IF liCnt < 12 THEN DO:
               Fatime.Period = Fatime.Period + liCnt.
               LEAVE.
            END.
            ELSE liCnt = liCnt - 12.              
         END.
      END.
                   
      /* margin is negative (should not be common) */
      ELSE DO:
         ASSIGN Fatime.Period = bCPFatime.Period
                liCnt = liDiff + Fatime.Period MOD 100.
                       
         DO WHILE TRUE:
            Fatime.Period = (TRUNCATE(Fatime.Period / 100,0) - 1) 
                            * 100 + 12.

            IF ABS(liCnt) < 12 THEN DO:
               Fatime.Period = Fatime.Period + liCnt.
               LEAVE.
            END.
            ELSE liCnt = liCnt + 12.              
         END.
      END.
               
      /* end period exceeded */ 
      IF Fatime.Period > iiToPer THEN DO:
         DELETE Fatime.
         LEAVE.
      END.

      /* last period that this fatime can be used for */
      IF FatGroup.ValidPeriods > 0 THEN
         FATime.LastPeriod = fFATLastPeriod(FATime.Period,
                                            FatGroup.ValidPeriods).
 
      fMakeCreateEvent(lhTable,"FtGrp,FatNum",katun,"").
            
      RELEASE Fatime.
      
      llDone = TRUE.
                   
   END. /* each bCPFatime */

   RETURN llDone.
                   
END FUNCTION.
                   
/* create single fatime from scratch */
FUNCTION fCreateFatime RETURNS LOGICAL
   (icFatGrp   AS CHAR,
    iiCustNum  AS INT,
    iiMsSeq    AS INT,
    icCLI      AS CHAR,
    idAmt      AS DEC,
    iiPeriod   AS INT,
    icMemo     AS CHAR).
    
   DEF VAR ldAmtLimit AS DEC NO-UNDO.
 
   IF NOT AVAILABLE FatGroup OR
      FatGroup.FtGrp NE icFatGrp
   THEN DO:
      FIND FatGroup WHERE 
           FatGroup.Brand = gcBrand AND
           FatGroup.FtGrp = icFatGrp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FatGroup THEN RETURN FALSE.
   END.
  
   IF FatGroup.AmtLimit > 0 THEN DO:
      ldAmtLimit = FatGroup.AmtLimit.
      FOR EACH Fatime NO-LOCK WHERE
               Fatime.Brand   = gcBrand   AND
               Fatime.FtGrp   = icFatGrp  AND
               Fatime.CustNum = iiCustNum AND
               Fatime.MsSeq   = iiMsSeq:
         ldAmtLimit = ldAmtLimit - Fatime.Amt.
      END.   
   END.
   ELSE ldAmtLimit = 999999999. 
   
  
   /* if group already has members then take defaults from one */
   FIND FIRST bCPFatime OF FatGroup NO-LOCK NO-ERROR.

   /* cli is mandatory for "calls" -type */
   IF icCLI = "" AND 
      AVAILABLE bCPFatime AND 
      bCPFatime.FatType = 0 
   THEN RETURN FALSE.
 
   CREATE FATime.
   ASSIGN FATime.FatNum    = NEXT-VALUE(ftseq)
          Fatime.Fatid     = NEXT-VALUE(ft-seq)
          Fatime.FtGrp     = FatGroup.FtGrp 
          Fatime.Period    = iiPeriod
          Fatime.CustNum   = iiCustNum
          Fatime.CLI       = icCLI
          Fatime.MSSeq     = iiMsSeq
          /* check amount limit */
          Fatime.Amt       = MIN(idAmt,ldAmtLimit)
          Fatime.Brand     = gcBrand
          Fatime.PayerType = PROGRAM-NAME(2).
          
   IF icMemo > "" THEN DO liMemoCnt = 1 TO 5:
      IF Fatime.Memo[liMemoCnt] = "" THEN DO:
         Fatime.Memo[liMemoCnt] = icMemo.
         LEAVE.
      END.
   END.
           
   /* if this is the first one then make a general fat */
   IF NOT AVAILABLE bCPFatime THEN ASSIGN 
          Fatime.QtyUnit  = "Amt"
          Fatime.FatType  = 2       /* all */
          Fatime.FatClass = TRUE 
          Fatime.Transfer = TRUE. 

   /* otherwise take values from previous one */       
   ELSE ASSIGN
          Fatime.QtyUnit  = bCPFatime.QtyUnit
          Fatime.FatType  = bCPFatime.FatType
          Fatime.FatClass = bCPFatime.FatClass
          Fatime.Transfer = bCPFatime.Transfer. 

   /* last period that this fatime can be used for */
   IF FatGroup.ValidPeriods > 0 THEN
      FATime.LastPeriod = fFATLastPeriod(FATime.Period,
                                         FatGroup.ValidPeriods).
    
   fMakeCreateEvent(lhTable,"FtGrp,FatNum",katun,"").

   RELEASE Fatime.
   
   RETURN TRUE.
   
END FUNCTION.
 
/* create fatime using defaults from FatGroup */
FUNCTION fCreateFatRow RETURNS CHARACTER
   (icFatGrp    AS CHAR,
    iiCustNum   AS INT,
    iiMsSeq     AS INT,
    icCLI       AS CHAR,
    icHostTable AS CHAR,
    icKeyValue  AS CHAR,
    idAmt       AS DEC,
    idPerc      AS DEC,
    ilVatIncl   AS LOG,
    iiFromPer   AS INT,
    iiToPer     AS INT,
    icMemo      AS CHAR).
    
   DEF VAR ldAmtLimit   AS DEC  NO-UNDO.
   DEF VAR liPeriodCnt  AS INT  NO-UNDO.
   DEF VAR liCrePeriod  AS INT  NO-UNDO.
   DEF VAR liNextPer    AS INT  NO-UNDO.
   DEF VAR lcChkValue   AS CHAR NO-UNDO.
   DEF VAR lcSkipPeriod AS CHAR NO-UNDO.
 
   DEF BUFFER bFatOwner FOR MsOwner.
    
   IF NOT AVAILABLE FatGroup OR
      FatGroup.FtGrp NE icFatGrp
   THEN DO:
      FIND FatGroup WHERE 
           FatGroup.Brand = gcBrand AND
           FatGroup.FtGrp = icFatGrp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FatGroup THEN RETURN "Unknown FAT group".
   END.
  
   IF FatGroup.AmtLimit > 0 THEN DO:
      ldAmtLimit = FatGroup.AmtLimit.
      FOR EACH Fatime NO-LOCK WHERE
               Fatime.Brand   = gcBrand   AND
               Fatime.FtGrp   = icFatGrp  AND
               Fatime.CustNum = iiCustNum AND
               Fatime.MsSeq   = iiMsSeq   AND
               Fatime.OrigFat = 0:
         ldAmtLimit = ldAmtLimit - Fatime.Amt.
      END.   
   END.
   ELSE ldAmtLimit = 999999999. 
   
   IF ldAmtLimit <= 0 THEN RETURN "FATime limit exceeded". 
  
   /* cli is mandatory for "calls" -type */
   IF icCLI = "" AND FatGroup.FatType = 0 THEN RETURN "MSISDN is mandatory".
 
   IF icCLI > "" AND iiMsSeq = 0 THEN 
   FOR FIRST bFatOwner NO-LOCK WHERE
             bFatOwner.CLI     = icCLI AND
             bFatOwner.CustNum = iiCustNum:
      iiMsSeq = bFatOwner.MsSeq.
   END.
   
   /* are there clitypes that should be excluded */
   IF iiMsSeq > 0 THEN DO:
      IF CAN-FIND(FIRST FATConfig WHERE
                        FATConfig.Brand      = gcBrand  AND
                        FATConfig.FTGrp      = icFatGrp AND
                        FATConfig.ConfType   = 4        AND
                        FATConfig.ConfTarget = "CLIType" AND
                        FATConfig.ValidFrom <= TODAY    AND
                        FATConfig.ValidTo   >= TODAY)
      THEN DO:
         lcChkValue = "".

         FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
         IF AVAILABLE MobSub THEN lcChkValue = MobSub.CLIType.
         ELSE DO:
            FIND FIRST bFatOwner WHERE bFatOwner.MsSeq = iiMsSeq 
               NO-LOCK NO-ERROR.
            IF AVAILABLE bFatOwner THEN lcChkValue = bFatOwner.CLIType.
         END.
         
         IF lcChkValue > "" THEN 
         FOR FIRST FATConfig NO-LOCK WHERE
                   FATConfig.Brand      = gcBrand   AND
                   FATConfig.FTGrp      = icFatGrp  AND
                   FATConfig.ConfType   = 4         AND
                   FATConfig.ConfTarget = "CLITYPE" AND
                   FATConfig.ValidFrom <= TODAY     AND
                   FATConfig.ValidTo   >= TODAY     AND
                   FATConfig.ConfRule1  = lcChkValue:
            RETURN "Not allowed for subscription's CLI type".        
         END.
      END.
   END.

   lcSkipPeriod = "".
   /* are there other groups that are mutually excluded */
   FOR EACH FATConfig NO-LOCK WHERE
            FATConfig.Brand      = gcBrand   AND
            FATConfig.FTGrp      = icFatGrp  AND
            FATConfig.ConfType   = 1         AND
            FATConfig.ValidFrom <= TODAY     AND
            FATConfig.ValidTo   >= TODAY:
          
       IF FATConfig.ConfTarget = "FATGroup" THEN 
       FOR EACH FATime NO-LOCK WHERE
                FATime.CustNum  = iiCustNum AND
                FATime.CLI      = icCLI     AND
                FATime.FTGrp    = FATConfig.ConfRule1 AND
                FATime.Period  >= iiFromPer:
          lcSkipPeriod = lcSkipPeriod + 
                         (IF lcSkipPeriod > "" THEN "," ELSE "") + 
                         STRING(FATime.Period).      
       END.

       ELSE IF FATConfig.ConfTarget = "BillItem" THEN 
       FOR EACH FATime NO-LOCK WHERE
                FATime.CustNum  = iiCustNum AND
                FATime.CLI      = icCLI     AND
                FATime.Period  >= iiFromPer,
          FIRST bCPFatGroup NO-LOCK WHERE
                bCpFatGroup.Brand    = gcBrand      AND
                bCPFatGroup.FTGrp    = FATime.FTGrp AND
                bCpFatGroup.BillCode = FATConfig.ConfRule1:
          lcSkipPeriod = lcSkipPeriod + 
                         (IF lcSkipPeriod > "" THEN "," ELSE "") + 
                         STRING(FATime.Period).      
       END.
                          
   END.
            
   ASSIGN liCrePeriod  = iiFromPer
          liFatCreated = 0.
   
   /* use amount from group if not given here */
   IF (idAmt = ? OR idAmt = 0) AND
      (idPerc = ? OR idPerc = 0) 
   THEN ASSIGN idAmt  = FatGroup.Amount
               idPerc = FatGroup.FatPerc.
   
   /* nothing to do */
   IF idAmt = 0 AND idPerc = 0 THEN RETURN "Credit amount is zero".
   
   IF ilVatIncl = ? THEN DO:
      FIND bFatCust WHERE bFatCust.CustNum = iiCustNum NO-LOCK.
      ilVatIncl = bFatCust.VatIncl.
   END.
   
   /* create as many fatime rows as has been defined to group */
   DO liPeriodCnt = 1 TO FatGroup.PeriodQty:

      /* another fat already exists on period */
      IF LOOKUP(STRING(liCrePeriod,"999999"),lcSkipPeriod) > 0 THEN DO:
         liPeriodCnt = liPeriodCnt - 1.
      END.
      
      ELSE DO:
      
         IF liCrePeriod > iiToPer THEN LEAVE.  

         CREATE FATime.
         ASSIGN FATime.FatNum    = NEXT-VALUE(ftseq)
                Fatime.Fatid     = NEXT-VALUE(ft-seq)
                Fatime.FtGrp     = FatGroup.FtGrp 
                Fatime.Period    = liCrePeriod
                Fatime.CustNum   = iiCustNum
                Fatime.CLI       = icCLI
                Fatime.MSSeq     = iiMsSeq
                Fatime.HostTable = icHostTable
                Fatime.KeyValue  = icKeyValue
                Fatime.VatIncl   = ilVatIncl
                /* check amount limit */
                Fatime.Amt       = MIN(idAmt,ldAmtLimit)
                ldAmtLimit       = ldAmtLimit - Fatime.Amt
                Fatime.FatPerc   = idPerc
                Fatime.QtyUnit   = FatGroup.QtyUnit
                Fatime.FatType   = FatGroup.FatType
                Fatime.Transfer  = FatGroup.Transfer
                Fatime.Interval  = FatGroup.Interval
                Fatime.Brand     = gcBrand
                Fatime.PayerType = PROGRAM-NAME(2)
                liFatCreated     = liFatCreated + 1.
          
         DO liMemoCnt = 1 TO 5:
            Fatime.Memo[liMemoCnt] = FatGroup.InvMemo[liMemoCnt].
         END.

         /* is there a rule for default value that overrides group default */
         IF liPeriodCnt = 1 THEN 
         FOR FIRST FATConfig NO-LOCK WHERE
                   FATConfig.Brand      = gcBrand    AND
                   FATConfig.FTGrp      = icFatGrp   AND
                   FATConfig.ConfType   = 2          AND
                   FATConfig.ConfTarget = "Transfer" AND
                   FATConfig.ValidFrom <= TODAY      AND
                   FATConfig.ValidTo   >= TODAY      AND
                   FATConfig.ConfRule1  = "First":
            FATime.Transfer = (FATConfig.ConfRule2 = "yes").       
         END.

         /* last period that this fatime can be used for */
         IF FatGroup.ValidPeriods > 0 THEN
            FATime.LastPeriod = fFATLastPeriod(FATime.Period,
                                               FatGroup.ValidPeriods).
           
         fMakeCreateEvent(lhTable,"FtGrp,FatNum",katun,"").
         
         RELEASE Fatime.
      END.
      
      IF ldAmtLimit <= 0 THEN LEAVE.
        
      /* next period */
      DO liNextPer = 1 TO FatGroup.Interval:
         IF liCrePeriod MOD 100 < 12 
         THEN liCrePeriod = liCrePeriod + 1.
         ELSE liCrePeriod = (TRUNCATE(liCrePeriod / 100,0) + 1) * 100 + 1.
      END.   

   END.
   
   RETURN STRING(liFatCreated > 0,"/No rows created").
   
END FUNCTION.
  
FUNCTION fCloseFat RETURNS LOGICAL
   (icFatGrp AS CHAR,
    iiMsSeq AS INT,
    iiLastPeriod AS INT):

   DEF BUFFER FATime FOR FATime.
   
   FOR EACH FATime EXCLUSIVE-LOCK WHERE
            FATime.Brand = gcBrand AND
            FATime.MsSeq = iiMsSeq AND
            FATime.FTGrp = icFatGrp AND
            FATime.InvNum = 0 AND
            (IF FATime.LastPeriod > 0 THEN
             FATime.LastPeriod > iiLastPeriod
             ELSE TRUE) USE-INDEX MobSub:
      Fatime.LastPeriod = iiLastPeriod.
   END.

   RETURN TRUE.
END.

FUNCTION fFatExists RETURNS LOGICAL (icFatGrp     AS CHAR,
                                     iiMsSeq      AS INT,
                                     iiCustnum    AS INT,
                                     iiLastPeriod AS INT):

   DEF BUFFER FATime FOR FATime.
   DEF VAR llExists  AS  LOG  NO-UNDO.
   
   FOR EACH FATime NO-LOCK WHERE
            FATime.Brand = gcBrand AND
            FATime.MsSeq = iiMsSeq AND
            Fatime.Custnum = iiCustnum AND
            FATime.FTGrp = icFatGrp AND
            FATime.InvNum = 0 AND
            (IF FATime.LastPeriod > 0 THEN
             FATime.LastPeriod >= iiLastPeriod
             ELSE TRUE) USE-INDEX MobSub:
      llExists = TRUE.
      LEAVE.
   END.

   RETURN llExists.
END.
