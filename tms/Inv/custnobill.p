/* ----------------------------------------------------------------------
  MODULE .......: custnobill
  TASK .........: list customers whose invoice creation failed
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 10.05.05
  CHANGED ......: 16.12.05/aam period given -> pick multiple files
                  15.09.06/aam optionally only active customers,
                               don't check for newer invoice (after period)
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{email.i}

DEF INPUT  PARAMETER idtDate1 AS DATE NO-UNDO.
DEF INPUT  PARAMETER idtDate2 AS DATE NO-UNDO.
DEF INPUT  PARAMETER icUser   AS CHAR NO-UNDO. 
DEF INPUT  PARAMETER ilActive AS LOG  NO-UNDO.
DEF INPUT  PARAMETER icFile   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER ilMail   AS LOG  NO-UNDO. 
DEF OUTPUT PARAMETER oiCnt    AS INT  NO-UNDO.

DEF VAR lcDate      AS CHAR NO-UNDO.
DEF VAR lcEventDir  AS CHAR NO-UNDO. 
DEF VAR lcConfDir   AS CHAR NO-UNDO.
DEF VAR lcFile      AS CHAR NO-UNDO.
DEF VAR lcLine      AS CHAR NO-UNDO.
DEF VAR liCust      AS INT  NO-UNDO.
DEF VAR lcText      AS CHAR NO-UNDO. 
DEF VAR liCustPos   AS INT  NO-UNDO. 
DEF VAR liInvPos    AS INT  NO-UNDO. 
DEF VAR lcUser      AS CHAR NO-UNDO. 
DEF VAR lcMailFile  AS CHAR NO-UNDO. 
DEF VAR llFound     AS LOG  NO-UNDO. 
DEF VAR liQty       AS INT  NO-UNDO. 
DEF VAR liChk       AS INT  NO-UNDO. 
DEF VAR ldtFileDate AS DATE NO-UNDO.
DEF VAR liTotal     AS INT  NO-UNDO.
DEF VAR lcTotal     AS CHAR NO-UNDO.
DEF VAR liPeriodEnd AS INT  NO-UNDO.
DEF VAR ldSubPeriod AS DEC  NO-UNDO.

DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum   AS INT
   FIELD RejReason AS CHAR
   INDEX CustNum CustNum.
   
DEF BUFFER bEventCust FOR Customer.

DEF STREAM sRead.
DEF STREAM sLog.

FORM
   ldtFileDate COLON 10 LABEL "Date"    FORMAT "99-99-99" SKIP 
   liQty       COLON 10 LABEL "Collect" FORMAT ">>>>>>>>9" SKIP
   liChk       COLON 10 LABEL "Check"   FORMAT ">>>>>>>>9" 
      lcTotal NO-LABEL FORMAT "X(13)" SKIP
   oiCnt       COLON 10 LABEL "Print"   FORMAT ">>>>>>>>9" SKIP   
   WITH OVERLAY SIDE-LABELS ROW 9 CENTERED TITLE " Processing " FRAME fQty.
 
 
OUTPUT STREAM sLog TO VALUE(icFile).

DO ldtFileDate = idtDate1 TO idtDate2:

   ASSIGN lcDate     = STRING(ldtFileDate,"99999999")
          lcEventDir = fCParamC("EventLogDir")
          lcConfDir  = fCParamC("RepConfDir")
          lcFile     = lcEventDir + "/" + lcDate + ".log".

   IF SEARCH(lcFile) = ? THEN NEXT.
   
   INPUT STREAM sRead FROM VALUE(lcFile).

   REPEAT:

      IMPORT STREAM sRead UNFORMATTED lcLine.
   
      ASSIGN liInvPos  = INDEX(lcLine,"invoice:")
             liCustPos = INDEX(lcLine,"customer:")
             lcUser    = ENTRY(1,lcLine).

      /* events of one user requested */
      IF icUser > "" AND lcUser NE icUser THEN NEXT. 
   
      /* both invoice and customer must appear in line */
      IF liInvPos = 0 OR liCustPos = 0 THEN NEXT.

      ASSIGN lcText = SUBSTRING(lcLine,liCustPos)
             liCust = INTEGER(ENTRY(2,lcText,":")) NO-ERROR.
      IF ERROR-STATUS:ERROR OR liCust = 0 THEN NEXT.

      liQty = liQty + 1.
      IF NOT SESSION:BATCH THEN DO:
         IF liQty < 100 OR liQty MOD 1000 = 0 THEN DO:
            PAUSE 0.
            DISP ldtFileDate liQty WITH FRAME fQty.
         END.
      END.
   
      FIND FIRST ttCust WHERE 
                 ttCust.CustNum = liCust NO-ERROR.
          
      /* creation started */
      IF INDEX(lcLine,"started") > 0 THEN DO:
      
         /* there may be several attempts on same customer, 
            report only latest */
         IF AVAILABLE ttCust THEN ttCust.RejReason = "".
      
         ELSE DO:
            CREATE ttCust.
            ttCust.CustNum = liCust.
         END. 
      END.
   
      /* creation succeeded */
      ELSE IF INDEX(lcLine,"created") > 0 THEN DO:
         IF AVAILABLE ttCust THEN DELETE ttCust.
      END.
   
      /* creation failed on known reason */
      ELSE DO:
         IF NOT AVAILABLE ttCust THEN DO:
            CREATE ttCust.
            ttCust.CustNum = liCust.
         END.
      
         ttCust.RejReason = SUBSTRING(lcLine,liInvPos + 8,
                                      liCustPos - liInvPos - 8).
         IF SUBSTRING(ttCust.RejReason,LENGTH(ttCust.RejReason),1) = ":"
         THEN ttCust.RejReason = SUBSTRING(ttCust.RejReason,1,
                                           LENGTH(ttCust.RejReason) - 1).
                                   
      END. 

      /* if only "started" line exists, then ttCust ends upd on the list with 
         empty reason */

   END.

   INPUT STREAM sRead CLOSE.
END.


PUT STREAM sLog UNFORMATTED 
   "Customer"     CHR(9)
   "Name"         CHR(9)
   "Reason"       CHR(9)
   "Last Invoice" SKIP.
   
DISP "" @ ldtFileDate WITH FRAME fQty.

FOR EACH ttCust:
   liTotal = liTotal + 1.
END.

ASSIGN lcTotal     = "/ " + TRIM(STRING(liTotal,">>>>>>>>9"))
       liPeriodEnd = YEAR(idtDate2 + 1) * 10000 + 
                     MONTH(idtDate2 + 1) * 100  +
                     DAY(idtDate2 + 1)
       ldSubPeriod = YEAR(idtDate1) * 10000 + 
                     MONTH(idtDate1) * 100  +
                     DAY(idtDate1).

FOR EACH ttCust:

   liChk = liChk + 1.
   IF NOT SESSION:BATCH THEN DO:
      IF liChk < 100 OR liChk MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP liChk lcTotal oiCnt WITH FRAME fQty.
      END.
   END.
 
   FIND Customer WHERE Customer.CustNum = ttCust.CustNum NO-LOCK NO-ERROR.

   /* should customer have had a live subscription */
   IF ilActive AND
      NOT CAN-FIND(FIRST MsOwner WHERE 
                         MsOwner.InvCust = Customer.CustNum AND
                         /* active in the beginning of the period
                            (events are billed beforehand) */
                         MsOwner.TsEnd  >= ldSubPeriod      AND
                         MsOwner.TsBeg  <= ldSubPeriod)
   THEN NEXT. 
   
   /* should there be something to bill */
   IF ttCust.RejReason = "" THEN DO:
      llFound = CAN-FIND(FIRST InvSeq WHERE 
                               InvSeq.CustNum = Customer.CustNum AND
                               InvSeq.Billed  = FALSE).
                               
      IF NOT llFound THEN 
      FOR EACH bEventCust NO-LOCK WHERE
               bEventCust.InvCust = Customer.CustNum:
               
         llFound = CAN-FIND(FIRST SingleFee WHERE
                                  SingleFee.Brand   = gcBrand            AND
                                  SingleFee.CustNum = bEventCust.CustNum AND
                                  SingleFee.Active  = TRUE               AND
                                  SingleFee.Billed  = FALSE).
                                 
         IF NOT llFound
         THEN FOR EACH FixedFee NO-LOCK WHERE
                       FixedFee.Brand   = gcBrand            AND
                       FixedFee.CustNum = bEventCust.CustNum AND
                       FixedFee.InUse   = TRUE,
                 FIRST FFItem OF FixedFee NO-LOCK WHERE
                       FFItem.Billed = FALSE:
            llFound = TRUE.
            LEAVE.
         END. 
         
         IF llFound THEN LEAVE.
         
      END. 
                           
      /* probably a closed customer */                         
      IF NOT llFound THEN NEXT.                          
   END. 
   
   lcDate = "".
   FOR EACH Invoice NO-LOCK WHERE
            Invoice.Brand   = gcBrand          AND
            Invoice.CustNum = Customer.CustNum AND
            Invoice.InvType < 3
   BY Invoice.InvDate DESC:
      lcDate = STRING(Invoice.InvDate,"99-99-99").
      LEAVE.
   END.
              
   PUT STREAM sLog UNFORMATTED
      ttCust.CustNum   CHR(9)
      (IF AVAILABLE Customer 
       THEN Customer.CustName
       ELSE "")        CHR(9)
      ttCust.RejReason CHR(9)
      lcDate           SKIP.
      
   oiCnt = oiCnt + 1.   
END.

OUTPUT STREAM sLog CLOSE.

IF NOT SESSION:BATCH THEN 
HIDE FRAME fQty NO-PAUSE.

/* send report via mail */
IF ilMail THEN DO:

   ASSIGN lcMailFile = "/tmp/custnobil_" +
                       STRING(YEAR(TODAY),"9999") +
                       STRING(MONTH(TODAY),"99")  +
                       STRING(DAY(TODAY),"99")    + 
                       "_" + STRING(TIME) + ".txt".
                                     
    OUTPUT STREAM sLog TO VALUE(lcMailFile).
    PUT STREAM sLog UNFORMATTED
       "Invoice creation failed for listed customers on " 
       STRING(idtDate1,"99.99.9999") " - " 
       STRING(idtDate2,"99.99.9999")
       SKIP.
    OUTPUT STREAM sLog CLOSE. 

    /* mail recipients */
    GetRecipients(lcConfDir + 
                  "custnobill.email").
                  
    IF xMailAddr > "" THEN 
    SendMail(lcMailFile,
             icFile).

END.
 



