/* ----------------------------------------------------------------------------
  MODULE .......: fmitemprice.p
  FUNCTION .....: feemodel item price changes 
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 21.09.07
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{Func/msreqfunc.i}

{Syst/eventval.i} 

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}
END.


DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 27 THEN RETURN "ERROR".

lcReqType = "fmprice".

RUN pFMItemPrice.

/* eventlog temp-tables */
fCleanEventObjects().

RETURN RETURN-VALUE.


PROCEDURE pFMItemPrice:

   DEF VAR ldtFromDate   AS DATE NO-UNDO.
   DEF VAR liTime        AS INT  NO-UNDO.
   DEF VAR lcFeeModel    AS CHAR NO-UNDO EXTENT 6.
   DEF VAR lcPayType     AS CHAR NO-UNDO EXTENT 2.
   DEF VAR lcMSISDNType  AS CHAR NO-UNDO EXTENT 3.
   DEF VAR liFound       AS INT  NO-UNDO.
   
   DEF BUFFER bFMItem FOR FMItem.
                                                  
   IF llDoEvent THEN DO:
      DEFINE VARIABLE lhFMItem AS HANDLE NO-UNDO.
      lhFMItem = BUFFER FMItem:HANDLE.
      RUN StarEventInitialize(lhFMItem).
      
      DEFINE VARIABLE lhbFMItem AS HANDLE NO-UNDO.
      lhbFMItem = BUFFER bFMItem:HANDLE.
      RUN StarEventInitialize(lhbFMItem).
      
   END.
   
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   /* starting date for new billing event row */
   ldtFromDate = MsRequest.ReqDtParam1.
   IF ldtFromDate = ? THEN DO: 
      fSplitTS(MsRequest.ActStamp,
               OUTPUT ldtFromDate,
               OUTPUT liTime).
   END.
   
   /* price change cannot be dated to past (causes problems in order handling)
   */
   IF ldtFromDate < TODAY THEN DO:
      fReqError("Price change cannot be dated to past").
      RETURN.
   END.

   IF NOT CAN-FIND(BillItem WHERE
                   BillItem.Brand    = gcBrand AND
                   BillItem.BillCode = MsRequest.ReqCParam1)
   THEN DO:
      fReqError("Unknown billing item").
      RETURN.
   END.
   
   IF MsRequest.ReqIParam1 > 2 THEN DO:
      fReqError("Invalid payment type value").
      RETURN.
   END.

   IF MsRequest.ReqIParam2 > 2 THEN DO:
      fReqError("Invalid MSISDN type value").
      RETURN.
   END.

   CASE MsRequest.ReqIParam1:
   WHEN 0 THEN ASSIGN
      lcPayType[1] = "PRE"
      lcPayType[2] = "POS".
   WHEN 1 THEN 
      lcPayType[1] = "PRE".
   WHEN 2 THEN    
      lcPayType[2] = "POS".
   END CASE.
      
   CASE MsRequest.ReqIParam2:
   WHEN 0 THEN ASSIGN
      lcMSISDNType[1] = "MNP"
      lcMSISDNType[2] = "NEW"
      lcMSISDNType[3] = "REN".
   WHEN 1 THEN 
      lcMSISDNType[1] = "MNP".
   WHEN 2 THEN ASSIGN   
      lcMSISDNType[2] = "NEW"
      lcMSISDNType[3] = "REN".
   END CASE.
   
   /* form codes of fee models which should be changed */  
   DO liReqCnt = 1 TO 2:
      IF lcPayType[liReqCnt] = "" THEN NEXT.
      
      IF lcMSISDNType[1] > "" THEN 
         lcFeeModel[liReqCnt] = lcPayType[liReqCnt] + lcMSISDNType[1].
      IF lcMSISDNType[2] > "" THEN  
         lcFeeModel[liReqCnt + 2] = lcPayType[liReqCnt] + lcMSISDNType[2].
      IF lcMSISDNType[3] > "" THEN  
         lcFeeModel[liReqCnt + 4] = lcPayType[liReqCnt] + lcMSISDNType[3].
   END.
      
   DO liReqCnt = 1 TO 6:
   
      IF lcFeeModel[liReqCnt] = "" THEN NEXT.
      
      FOR EACH FeeModel NO-LOCK WHERE
               FeeModel.Brand   = gcBrand      AND
               FeeModel.FeeModel BEGINS lcFeeModel[liReqCnt],
         FIRST FMItem EXCLUSIVE-LOCK WHERE
               FMItem.Brand     = gcBrand              AND
               FMITem.FeeModel  = FeeModel.FeeModel    AND
               FMItem.BillCode  = MsRequest.ReqCParam1 AND
               FMItem.FromDate <= ldtFromDate          AND
               FMItem.ToDate   >= ldtFromDate:
         
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFMItem).

         /* update to an already changed price */
         IF FMItem.FromDate = ldtFromDate THEN DO:
            liTime = 1.
 
            REPEAT:
               IF NOT CAN-FIND(FIRST bFMItem WHERE
                                     bFMItem.Brand     = FMItem.Brand     AND
                                     bFMITem.FeeModel  = FMITem.FeeModel  AND
                                     bFMITem.PriceList = FMITem.PriceList AND
                                     bFMItem.BillCode  = FMItem.BillCode  AND
                                     bFMItem.FromDate  = ldtFromDate - liTime)
               THEN LEAVE.                      

               liTime = liTime + 1.
            END.
         
            ASSIGN 
               FMItem.FromDate = ldtFromDate - liTime
               /* make sure that this price will not be used */
               FMItem.ToDate   = FMItem.FromDate - 1.
         END.
         
         /* end current price */
         ELSE FMItem.ToDate = ldtFromDate - 1.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFMItem).
         
         /* create new row for changed price */   
         CREATE bFMItem.
         BUFFER-COPY FMItem EXCEPT FromDate ToDate TO bFMItem.
         ASSIGN 
            bFMItem.FromDate = ldtFromDate
            bFMItem.ToDate   = 12/31/2050
            bFMItem.Amount   = MsRequest.ReqDParam1
            liFound          = liFound + 1.
            
         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhbFMItem).

         RELEASE bFMItem.      
      END.
   END.
    
   IF liFound > 0 THEN DO:
      FIND CURRENT MsRequest EXCLUSIVE-LOCK.
      MsRequest.Memo = MsRequest.Memo + 
                       (IF MsRequest.Memo > "" THEN " " ELSE "") + 
                       STRING(liFound) + " prices were changed".
                       
      /* request handled succesfully */   
      fReqStatus(2,""). 
   END.
   
   ELSE DO:
      fReqError("No prices were found with given criteria").
   END.
   
END PROCEDURE.

