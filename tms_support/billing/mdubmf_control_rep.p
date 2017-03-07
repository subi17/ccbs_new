
/* MDUB Control Report */

{Func/timestamp.i}

FUNCTION fGetMobSubInfo RETURNS LOGICAL 
         (INPUT piMsSeq AS INT,
          OUTPUT ocCLI AS CHAR,
          OUTPUT oiInvCust AS INT):
    FIND MobSub WHERE 
         MobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
    IF NOT AVAIL MobSub THEN DO:
            FIND TermMobSub WHERE
                 TermMobSub.MSSeq = piMsSeq NO-LOCK NO-ERROR.
            IF AVAIL TermMobSub THEN
               ASSIGN ocCLI = TermMobSub.CLI
                      oiInvCust = TermMobSub.InvCust.
         END.
    ELSE ASSIGN ocCLI = MobSub.CLI 
                oiInvCust = MobSub.InvCust. 
                
 RETURN TRUE.
END.


DEFINE VARIABLE lcSLCodeList AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldaInvDate AS DATE NO-UNDO.
DEFINE VARIABLE ldaInvStart AS DATE NO-UNDO.
DEFINE VARIABLE ldaInvEnd AS DATE NO-UNDO.
DEFINE VARIABLE ldaBegin AS DATE NO-UNDO.
DEFINE VARIABLE ldaEnd AS DATE NO-UNDO.
DEFINE VARIABLE ldBeginTS AS DECIMAL NO-UNDO.
DEFINE VARIABLE ldEndTS AS DECIMAL NO-UNDO.
DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liInvCust AS INTEGER NO-UNDO.  
DEFINE VARIABLE gcBrand AS CHARACTER NO-UNDO INITIAL "1".
DEFINE VARIABLE liQty AS INTEGER NO-UNDO. 
DEFINE VARIABLE llInvoiced AS LOGICAL NO-UNDO. 
DEFINE VARIABLE liCount AS INTEGER NO-UNDO. 
DEFINE VARIABLE llActivePer AS LOGICAL NO-UNDO.
DEFINE VARIABLE lcSumFile AS CHAR NO-UNDO.
DEFINE VARIABLE lcList1File AS CHAR NO-UNDO.
DEFINE VARIABLE lcList2File AS CHAR NO-UNDO.

DEF STREAM sLogSum.
DEF STREAM sLogList1.
DEF STREAM sLogList2.

/* report variables */
/* list 1 */
DEF VAR liPerContr AS INT NO-UNDO. 
DEF VAR liFeePerContr AS INT NO-UNDO.
/* list 2 */
DEF VAR liFeeCurrPerInvContr AS INT NO-UNDO.
DEF VAR liFeeCurrPerNotInv AS INT NO-UNDO.
DEF VAR liFeeCurrPerInvNotContr AS INT NO-UNDO.
DEF VAR liFeePrevPerInv AS INT NO-UNDO.

UPDATE 
   ldaInvDate FORMAT "99-99-9999" LABEL "Invoice Date " SKIP(1)
   lcSumFile  FORMAT "X(50)" LABEL "Summary File" SKIP(1)
   lcList1File FORMAT "X(50)" LABEL "List1 File" SKIP(1)
   lcList2File FORMAT "X(50)" LABEL "List2 File"
with overlay side-labels 1 column row 4 centered title "MDUB Control Report"
frame fcontrolrep.
hide frame fcontrolrep no-pause.

IF ldaInvDate = ? OR 
   lcSumFile = "" OR
   lcList1File = "" OR
   lcList2File = "" THEN RETURN.

pause 0.
disp "Reading Data ...."
with overlay row 10 centered side-labels frame freading.

lcSLCodeList = "MDUB,MDUBEND".
/* period */
ldaEnd = DATE(MONTH(ldaInvDate),1,YEAR(ldaInvDate)) - 1.
ldaBegin = DATE(MONTH(ldaEnd),1,YEAR(ldaEnd)).
/* invoice */
ldaInvStart = ldaInvDate.

IF MONTH(ldaInvDate) = 12 THEN
   ldaInvEnd = DATE(12,31,YEAR(ldaInvDate)).
ELSE
   ldaInvEnd =  DATE(MONTH(ldaInvDate) + 1 ,1,YEAR(ldaInvDate)) - 1 .

ldBeginTS = fHMS2TS(ldaBegin,"00:00:00").
ldEndTS   =  fHMS2TS(ldaEnd,"23:59:59").


OUTPUT STREAM sLogSum TO VALUE(lcSumFile).
OUTPUT STREAM sLogList1 TO VALUE(lcList1File).
OUTPUT STREAM sLogList2 TO VALUE(lcList2File).

/* periodical contract - list 1 */
FOR EACH ServiceLimit NO-LOCK WHERE
         LOOKUP(ServiceLimit.SLCode,lcSLCodeList) > 0,
    EACH MServiceLimit NO-LOCK WHERE 
         MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
         MServiceLimit.FromTS <= ldEndTS AND
         MServiceLimit.EndTS >= ldBeginTS AND
         MServiceLimit.EndTS >= MServiceLimit.FromTS:
 
        /* periodical contract actives */
        liPerContr = liPerContr + 1 .
         fGetMobSubInfo(MServiceLimit.MsSeq,
                        OUTPUT lcCLI,
                        OUTPUT liInvCust).
        liCount = 0 .
        FOR EACH FixedFee NO-LOCK USE-INDEX HostTable WHERE
                 FixedFee.Brand     = gcBrand   AND 
                 FixedFee.HostTable = "MobSub"  AND
                 FixedFee.KeyValue  = STRING(MServiceLimit.MsSeq),
            EACH FFItem OF FixedFee NO-LOCK WHERE 
                 FFItem.BillPeriod = (YEAR(ldaBegin) * 100 + MONTH(ldaBegin)) AND
                 FFItem.BillCode = "MDUBMF":
                
                /* fee in contract active on period */
                liCount  =  liCount + 1 .  
        END.

        IF liCount = 0 THEN DO:
            PUT STREAM sLogList1 UNFORMATTED
                lcCLI CHR(9)
                STRING(MServiceLimit.Msseq) CHR(9)
                STRING(liInvCust) CHR(9)
                ServiceLimit.SLCode SKIP.

        END.
        ELSE liFeePerContr =  liFeePerContr + 1 .
        
END.

OUTPUT STREAM sLogList1 CLOSE.     
PUT STREAM sLogSum UNFORMATTED
  "Invoice date" CHR(9) ldaInvDate SKIP
  "BillPeriod" CHR(9)  STRING(YEAR(ldaBegin) * 100 + MONTH(ldaBegin)) SKIP 
  "Periodical Contract active on given billing period" CHR(9) STRING(liPerContr) SKIP
  "Fees on Periodical Contract active on given billing period " CHR(9) STRING(liFeePerContr) SKIP
  "No Fees on Periodical Contract active on given billing period "  CHR(9) STRING( liPerContr - liFeePerContr) SKIP.

/* fixed fee - list2 ------------------------------- */
FOR EACH FixedFee NO-LOCK USE-INDEX BillCode WHERE
         FixedFee.Brand = gcBrand AND 
         FixedFee.BillCode = "MDUBMF",
    EACH FFItem OF FixedFee NO-LOCK WHERE
         FFItem.BillPeriod <= (YEAR(ldaBegin) * 100 + MONTH(ldaBegin)):

    /* get mobsub details */
    lcCLI = "".
    liInvCust = 0.
    fGetMobSubInfo(INT(FixedFee.KeyValue),
                   OUTPUT lcCLI,
                   OUTPUT liInvCust).

    /* check if has been invoiced in given period*/
     FIND Invoice WHERE
          Invoice.Brand = gcBrand AND 
          Invoice.InvNum = FFItem.InvNum AND 
          Invoice.InvDate >= ldaInvStart AND
          Invoice.InvDate <= ldaInvEnd NO-LOCK NO-ERROR. 

    /* check active per contract */
    llActivePer = FALSE.  
    FOR EACH ServiceLimit NO-LOCK WHERE
          LOOKUP(ServiceLimit.SLCode,lcSLCodeList) > 0,
        EACH MServiceLimit NO-LOCK WHERE
             MServiceLimit.MsSeq = INT(FixedFee.KeyValue) AND 
             MServiceLimit.DialType = 7 AND 
             MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
             MServiceLimit.FromTS <= ldEndTS AND
             MServiceLimit.EndTS >= ldBeginTS:
        llActivePer = TRUE.
    END.

    /* give period */
     IF FFItem.BillPeriod = (YEAR(ldaBegin) * 100 + MONTH(ldaBegin)) THEN DO:
        /* invoiced */
        IF AVAIL Invoice THEN DO: 
             /* without active per contract */
             IF NOT llActivePer THEN DO:
                liFeeCurrPerInvNotContr = liFeeCurrPerInvNotContr + 1 .
                PUT STREAM sLogList2 UNFORMATTED
                    lcCLI CHR(9)
                    FixedFee.KeyValue CHR(9)
                    STRING(liInvCust) CHR(9)
                    Invoice.ExtInvID  SKIP.

             END.
             ELSE liFeeCurrPerInvContr  = liFeeCurrPerInvContr + 1 .

        END.
        ELSE  liFeeCurrPerNotInv  =  liFeeCurrPerNotInv  +  1. 
     END.
     /* previous periods invoiced */
     ELSE IF AVAIL Invoice THEN   liFeePrevPerInv =  liFeePrevPerInv + 1 .

END.

PUT STREAM sLogSum UNFORMATTED
  "Fees on given billing period invoiced and active contract " STRING(liFeeCurrPerInvContr) SKIP
  "Fees on given billing period invoiced but no active contract " STRING(liFeeCurrPerInvNotContr) SKIP
  "Fees on given billing period not invoiced "  STRING(liFeeCurrPerNotInv) SKIP
  "Fees on previous billing periods invoiced " STRING(liFeePrevPerInv) SKIP.


hide frame freading no-pause.

OUTPUT STREAM sLogList2 CLOSE.
OUTPUT STREAM sLogSum  CLOSE.
