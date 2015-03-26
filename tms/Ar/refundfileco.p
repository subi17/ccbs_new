/* ----------------------------------------------------------------------
  MODULE .......: refundfileco.p
  TASK .........: Collect payments (actually requests) to a refund file 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 05.09.07
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}

DEFINE INPUT  PARAMETER icInvGrp       AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iiCustNum1     AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiCustNum2     AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER idtAccDate     AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER icFile         AS CHAR NO-UNDO. 
DEFINE INPUT  PARAMETER ilEmptyFile    AS LOG  NO-UNDO. 
DEFINE OUTPUT PARAMETER oiInvCount     AS INT  NO-UNDO.
DEFINE OUTPUT PARAMETER oiFileCount    AS INT  NO-UNDO.
DEFINE OUTPUT PARAMETER ocError        AS CHAR NO-UNDO. 

DEF VAR lcDate      AS CHAR NO-UNDO.
DEF VAR lcFile      AS CHAR NO-UNDO. 
DEF VAR liPaymCount AS INT  NO-UNDO.
DEF VAR liPicked    AS INT  NO-UNDO. 

{refundfilett.i}

DEF TEMP-TABLE ttAccDate NO-UNDO
   FIELD AccDate AS DATE
   INDEX AccDate AccDate.


FUNCTION fMakeTemp RETURNS LOGICAL.

    /* already taken to bank */
    IF MsRequest.ReqCParam3 > "" THEN RETURN FALSE. 
    
    /* no negative amounts */
    IF MsRequest.ReqDParam1 <= 0 THEN RETURN FALSE. 
    
    CREATE ttRequest.
    ASSIGN ttRequest.MsRequest = MsRequest.MsRequest
           ttRequest.AccDate   = MsRequest.ReqDtParam1
           liPicked            = liPicked + 1. 

    IF Customer.IDelName > "" 
    THEN ttRequest.ZipCode = Customer.IDelZipCode.
    ELSE ttRequest.ZipCode = Customer.ZipCode.

    IF NOT CAN-FIND(FIRST ttAccDate WHERE 
                          ttAccDate.AccDate = ttRequest.AccDate)
    THEN DO:
       CREATE ttAccDate.
       ttAccDate.AccDate = ttRequest.AccDate.
    END.

    RETURN TRUE.
    
END FUNCTION.

FOR EACH MsRequest NO-LOCK USE-INDEX ReqType WHERE    
         MsRequest.Brand       = gcBrand      AND
         MsRequest.ReqType     = 23           AND
         MsRequest.ReqStat     = 16           AND
         MsRequest.CustNum    >= iiCustNum1   AND
         MsRequest.CustNum    <= iiCustNum2,
   FIRST Customer OF MsRequest NO-LOCK WHERE 
         (IF icInvGrp NE "" 
          THEN Customer.InvGroup = icInvGrp 
          ELSE TRUE):

   IF idtAccDate NE ? AND MsRequest.ReqDtParam1 NE idtAccDate
   THEN NEXT. 

   fMakeTemp().
        
   IF liPicked MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY liPicked FORMAT ">>>>>>>9"
      WITH NO-LABELS OVERLAY ROW 10 CENTERED
           TITLE " Collecting " FRAME fColl.
   END.
   
END. 

/* should an empty file be made */
IF NOT CAN-FIND(FIRST ttAccDate) AND ilEmptyFile THEN DO:
   CREATE ttAccDate.
   ttAccDate.AccDate = TODAY.
END.


/* print, one file per posting date  */
FOR EACH ttAccDate:

   /* invgroup to file name */
   IF icInvGrp > "" 
   THEN lcFile = REPLACE(icFile,"#IGRP",icInvGrp).
   ELSE lcFile = REPLACE(icFile,"#IGRP","ALL").
   
   /* due date to file name */   
   lcDate = DYNAMIC-FUNCTION("fDateFmt" IN ghFunc1,
                             ttAccDate.AccDate,
                             "yyyymmdd").
   lcFile = REPLACE(lcFile,"#DATE",lcDate).

   RUN refundfile (INPUT-OUTPUT TABLE ttRequest,  
                   ttAccDate.AccDate,
                   liPicked,
                   lcFile,
                   ilEmptyFile,
                   OUTPUT liPaymCount). 

   ASSIGN 
      ocError     = ocError + (IF ocError > "" THEN ", " ELSE "") + 
                    RETURN-VALUE
      oiFileCount = oiFileCount + INTEGER(liPaymCount > 0)
      oiInvCount  = oiInvCount + liPaymCount.
END.

 
