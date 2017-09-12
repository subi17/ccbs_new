{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/log.i}
{Func/date.i}
{Func/memo.i}
{Func/cparam2.i}
{Func/orderfunc.i}
{Mc/orderfusion.i}

DEFINE TEMP-TABLE ttCustomer NO-UNDO
    FIELD CustomerId   AS CHAR
    FIELD CustName     AS CHAR
    FIELD Email        AS CHAR
    FIELD Region       AS CHAR
    FIELD Product      AS CHAR
    FIELD SerialNbr    AS CHAR
    FIELD MessageRowId AS ROWID
    INDEX IdxCustomerId IS UNIQUE PRIMARY CustomerId.

DEFINE STREAM str_err.

DEF VAR liLoop AS INTE NO-UNDO.
DEF VAR ldeNow AS DECI NO-UNDO.

DO ON ERROR UNDO, THROW:

    RUN pProcessRequests.

    RUN pWriteFile.

END.

PROCEDURE pProcessRequests:
    DEF VAR lcCustomerId    AS CHAR NO-UNDO.
    DEF VAR ldeActCreatedTS AS DECI NO-UNDO DECIMALS 5.
    DEF VAR liAgrCust       AS INTE NO-UNDO.
    DEF VAR liSubscriptionAgrCust AS INTE NO-UNDO.
    
    DEFINE BUFFER AgreeCustomer           FOR Customer.
    DEFINE BUFFER bf_TPService_Activation FOR TPService.

    ASSIGN ldeNow = fMakeTS().

    MESSAGE_LOOP:
    FOR EACH TPService WHERE TPService.MsSeq > 0 AND TPService.Operation = {&TYPE_DEACTIVATION} AND TPService.ServStatus = {&WAITING_FOR_STB_DEACTIVATION} NO-LOCK 
        BY TPService.CreatedTS:
       
       IF TPService.CreatedTS > ldeNow THEN 
          NEXT MESSAGE_LOOP.

       ASSIGN ldeActCreatedTS = 0.
          
       FIND FIRST MobSub WHERE MobSub.MsSeq = TPService.MsSeq NO-LOCK NO-ERROR.
       IF NOT AVAIL MobSub THEN 
       DO:
          FIND FIRST TermMobSub WHERE TermMobSub.MsSeq = TPService.MsSeq NO-LOCK NO-ERROR.
          IF NOT AVAIL TermMobSub THEN
              RETURN fTPServiceError(BUFFER TPService,"Contract not found").
          ELSE 
              ASSIGN liSubscriptionAgrCust = TermMobSub.AgrCust.        
       END.
       ELSE 
          ASSIGN liSubscriptionAgrCust = MobSub.AgrCust.

       FIND FIRST bf_TPService_Activation WHERE bf_TPService_Activation.MsSeq      = TPService.MsSeq    AND 
                                                bf_TPService_Activation.Operation  = {&TYPE_ACTIVATION} AND 
                                                bf_TPService_Activation.ServType   = "Television"       AND 
                                                bf_TPService_Activation.ServStatus = {&STATUS_HANDLED}  NO-LOCK NO-ERROR.
       IF AVAIL bf_TPService_Activation THEN                                              
           ASSIGN ldeActCreatedTS = bf_TPService_Activation.CreatedTS.

       IF ldeActCreatedTS > 0 THEN 
       DO:
           FIND FIRST MsOwner WHERE MsOwner.MsSeq    = TPService.MsSeq AND 
                                    MsOwner.TSBegin <= ldeActCreatedTS AND 
                                    MsOwner.TSEnd   >= ldeActCreatedTS NO-LOCK NO-ERROR. 
           IF AVAIL MsOwner THEN 
               ASSIGN liAgrCust = MsOwner.AgrCust.
           ELSE     
               ASSIGN liAgrCust = liSubscriptionAgrCust.
       END.
       ELSE 
          ASSIGN liAgrCust = liSubscriptionAgrCust.

       FIND FIRST AgreeCustomer WHERE AgreeCustomer.Brand   = gcBrand   AND 
                                      AgreeCustomer.CustNum = liAgrCust NO-LOCK NO-ERROR.
       IF NOT AVAIL AgreeCustomer THEN 
           RETURN fTPServiceError(BUFFER TPService,"Agreement customer not found").

       ASSIGN lcCustomerId = AgreeCustomer.OrgId.

       FIND FIRST ttCustomer WHERE ttCustomer.CustomerId = lcCustomerId NO-LOCK NO-ERROR.
       IF NOT AVAIL ttCustomer THEN 
       DO:
          CREATE ttCustomer.
          ASSIGN
              ttCustomer.CustomerId   = lcCustomerId
              ttCustomer.CustName     = REPLACE(DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1, BUFFER AgreeCustomer),"|","")
              ttCustomer.Email        = AgreeCustomer.Email
              ttCustomer.Region       = AgreeCustomer.Region
              ttCustomer.Product      = TPService.Product
              ttCustomer.SerialNbr    = TPService.SerialNbr
              ttCustomer.MessageRowId = ROWID(TPService).
       END.

    END.

    RETURN "".

END PROCEDURE.

PROCEDURE pWriteFile:
    DEF VAR liCnt            AS INTE NO-UNDO.
    DEF VAR lcSep            AS CHAR NO-UNDO INIT ",". 
    DEF VAR lcDateTime       AS CHAR NO-UNDO.
    DEF VAR lcMessageId      AS CHAR NO-UNDO.
    DEF VAR lcLogFileName    AS CHAR NO-UNDO.
    DEF VAR lcOutputFileName AS CHAR NO-UNDO.

    ASSIGN 
        lcDateTime       = REPLACE(ISO-DATE(TODAY),"-","") + REPLACE(STRING(TIME,"HH:MM:SS"),":","")
        lcLogFileName    = fCParamC('TVServiceDeActLogFileName')
        lcOutputFileName = fCParamC('TVServiceDeActOutgoingFileName')
        lcLogFileName    = REPLACE(lcLogFileName   ,"#DATETIME",lcDateTime)
        lcOutputFileName = REPLACE(lcOutputFileName,"#DATETIME",lcDateTime).

    OUTPUT TO VALUE(lcOutputFileName).
    CUSTOMERLOOP:
    FOR EACH ttCustomer:

        FIND TPService WHERE ROWID(TPService) = ttCustomer.MessageRowId EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF LOCKED TPService OR NOT AVAIL TPService THEN 
        DO:
            OUTPUT STREAM str_err TO VALUE(lcLogFileName) APPEND.
            PUT STREAM str_err UNFORMATTED "Customer: '" + ttCustomer.CustomerId + "' Device: '" + ttCustomer.SerialNbr + "' locked from update." SKIP.
            OUTPUT STREAM str_err CLOSE.

            NEXT CUSTOMERLOOP.
        END.    

        ASSIGN 
            lcMessageId = ""
            lcMessageId = lcDateTime + "_" + STRING(liCnt,"99999").

        PUT UNFORMATTED "2"                   + lcSep +
                        ttCustomer.CustomerId + lcSep +
                        ttCustomer.CustName   + lcSep +
                        "MM_bossID"           + lcSep +  
                        lcMessageId           + lcSep +
                        ttCustomer.SerialNbr  SKIP.

        ASSIGN
            liCnt               = liCnt + 1
            TPService.MessageId = lcMessageId.

        fCreateTPServiceMessage(TPService.MsSeq, TPService.ServSeq, {&SOURCE_TMS}, {&WAITING_FOR_STB_DEACTIVATION_CONFIRMATION}).
    
    END.
    OUTPUT CLOSE.

    RETURN "".

END PROCEDURE.

FINALLY:
   EMPTY TEMP-TABLE ttCustomer.
END.


