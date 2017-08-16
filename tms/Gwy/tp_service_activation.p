{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/log.i}
{Func/date.i}
{Func/memo.i}
{Func/cparam2.i}
{Mc/orderfusion.i}

DEFINE TEMP-TABLE ttCustomer NO-UNDO
    FIELD CustomerId AS CHAR
    FIELD CustName   AS CHAR
    FIELD Email      AS CHAR
    FIELD Region     AS CHAR
    FIELD Product    AS CHAR
    FIELD SerialNbr  AS CHAR
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
    DEF VAR lcCustomerId AS CHAR NO-UNDO.

    DEFINE BUFFER AgreeCustomer FOR Customer.

    ASSIGN ldeNow = fMakeTS().
    
    MESSAGE_LOOP:
    FOR EACH TPService WHERE TPService.MsSeq > 0 AND TPService.Operation = {&TYPE_ACTIVATION} AND TPService.ServStatus = {&WAITING_FOR_STB_ACTIVATION} NO-LOCK 
        BY TPService.CreatedTS:
       
       IF TPService.CreatedTS > ldeNow THEN 
           NEXT MESSAGE_LOOP.

       FIND FIRST MobSub WHERE MobSub.MsSeq = TPService.MsSeq NO-LOCK NO-ERROR.
       IF NOT AVAIL MobSub THEN 
          RETURN fTPServiceError(BUFFER TPService,"Contract not found").

       FIND FIRST AgreeCustomer WHERE AgreeCustomer.Brand   = MobSub.Brand   AND 
                                      AgreeCustomer.CustNum = MobSub.AgrCust NO-LOCK NO-ERROR.
       IF NOT AVAIL AgreeCustomer THEN 
           RETURN fTPServiceError(BUFFER TPService,"Agreement customer not found").

       CASE TPService.ServType:
           WHEN "Television" THEN 
           DO:
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
           OTHERWISE
           DO:
           END.  
       END CASE.    

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
        lcLogFileName    = fCParamC('TVServiceActLogFileName')
        lcOutputFileName = fCParamC('TVServiceActOutgoingFileName')
        lcLogFileName    = REPLACE(lcLogFileName   ,"#DATETIME",lcDateTime) 
        lcOutputFileName = REPLACE(lcOutputFileName,"#DATETIME",lcDateTime).
 
    OUTPUT TO VALUE(lcOutputFileName).
    CUSTOMERLOOP:
    FOR EACH ttCustomer:

        FIND TPService WHERE ROWID(TPService) = ttCustomer.MessageRowId EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF LOCKED TPService THEN 
        DO:
            OUTPUT STREAM str_err TO VALUE(lcLogFileName) APPEND.
            PUT STREAM str_err UNFORMATTED "Customer: '" + ttCustomer.CustomerId + "' Device: '" + ttCustomer.SerialNbr + "' locked from update." SKIP.
            OUTPUT STREAM str_err CLOSE.

            NEXT CUSTOMERLOOP.
        END.    

        ASSIGN 
            lcMessageId = ""
            lcMessageId = lcDateTime + "_" + STRING(liCnt,"99999").

        PUT UNFORMATTED "1"                   + lcSep +
                        ttCustomer.CustomerId + lcSep +
                        ttCustomer.CustName   + lcSep +
                        ttCustomer.Region     + lcSep +
                        ttCustomer.Email      + lcSep +
                        "QtelBOSS"            + lcSep +
                        lcMessageId           + lcSep +
                        ttCustomer.Product    + lcSep +
                        ttCustomer.SerialNbr  SKIP.
          
        ASSIGN 
            liCnt               = liCnt + 1
            TPService.MessageId = lcMessageId.

        fCreateTPServiceMessage(TPService.MsSeq, TPService.ServSeq, {&SOURCE_TMS}, {&WAITING_FOR_STB_ACTIVATION_CONFIRMATION}).

    END.
    OUTPUT CLOSE.

    RELEASE TPService.
    RELEASE TPServiceMessage.
    
    RETURN "".

END PROCEDURE.

FINALLY:
   EMPTY TEMP-TABLE ttCustomer.
END.


