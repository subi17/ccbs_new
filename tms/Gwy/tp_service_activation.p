{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/log.i}
{Func/date.i}
{Func/memo.i}
{Func/cparam2.i}

DEFINE TEMP-TABLE ttCustomer NO-UNDO
    FIELD CustomerId AS CHAR
    FIELD Email      AS CHAR
    FIELD Region     AS CHAR
    FIELD Product    AS CHAR
    FIELD SerialNbr  AS CHAR
    FIELD MessageRowId AS ROWID
    INDEX IdxCustomerId IS UNIQUE PRIMARY CustomerId.

DEFINE STREAM str_err.

DO ON ERROR UNDO, THROW:

    RUN pProcessRequests.

    RUN pWriteFile.

END.

PROCEDURE pProcessRequests:
    DEF VAR lcCustomerId AS CHAR NO-UNDO.

    DEFINE BUFFER AgreeCustomer FOR OrderCustomer.

    MESSAGE_LOOP:
    FOR EACH TPServiceMessage WHERE TPServiceMessage.Source        EQ {&SOURCE_TMS}                    AND
                                    TPServiceMessage.MessageStatus EQ {&WAITING_FOR_VENDOR_ACTIVATION} AND
                                    TPServiceMessage.MessageType   EQ {&ACTIVATION}                    NO-LOCK BY TPServiceMessage.CreatedTS 
        liLoop = 1 TO 10:
       
       IF TPServiceMessage.CreatedTS > ldeNow THEN 
           NEXT MESSAGE_LOOP.

       FIND FIRST TPService WHERE TPService.ServSeq = TPServiceMessage.ServSeq NO-LOCK NO-ERROR.
       IF NOT AVAIL TPService THEN
          RETURN fTPServiceMessageError(BUFFER TPServiceMessage,"Service request not found").

       FIND FIRST MobSub WHERE MobSub.MsSeq = TPService.MsSeq NO-LOCK NO-ERROR.
       IF NOT AVAIL MobSub THEN 
          RETURN fTPServiceMessageError(BUFFER TPServiceMessage,"Contract not found").

       FIND FIRST AgreeCustomer WHERE AgreeCustomer.Brand   = MobSub.Brand   AND 
                                      AgreeCustomer.OrderId = MobSub.AgrCust NO-LOCK NO-ERROR.
       IF NOT AVAIL AgreeCustomer THEN 
           RETURN fTPServiceMessageError(BUFFER FusionMessage,"Agreement customer not found").

       ASSIGN lcCustomerId = AgreeCustomer.CustId + "-" + AgreeCustomer.OrgId.

       FIND FIRST ttCustomer WHERE ttCustomer.CustomerId = lcCustomerId NO-LOCK NO-ERROR.
       IF NOT AVAIL ttCustomer THEN 
       DO:
          CREATE ttCustomer.
          ASSIGN
              ttCustomer.CustomerId   = lcCustomerId
              ttCustomer.CustName     = REPLACE(fPrintCustName(BUFFER AgreeCustomer),"|","")
              ttCustomer.Email        = AgreeCustomer.Email
              ttCustomer.Region       = AgreeCustomer.Region
              ttCustomer.Product      = TPService.Product
              ttCustomer.SerialNbr    = TPService.SerialNbr
              ttCustomer.MessageRowId = ROWID(TPServiceMessage).
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
        lcLogFileName    = fCParamC('TVServiceActLogFileName')
        lcOutputFileName = fCParamC('TVServiceActOutgoingFileName')
        lcLogFileName    = REPLACE(lcLogFileName   ,"#DATETIME",lcDateTime) 
        lcOutputFileName = REPLACE(lcOutputFileName,"#DATETIME",lcDateTime).
 
    OUTPUT TO VALUE(lcOutputFileName).
    CUSTOMERLOOP:
    FOR EACH ttCustomer:

        FIND TPServiceMessage WHERE ROWID(TPServiceMessage) = ttCustomer.MessageRowId EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF LOCKED TPServiceMessage THEN 
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
            liCnt                          = liCnt + 1
            TPServiceMessage.MessageId     = lcMessageId
            TPServiceMessage.UpdateTS      = fMakeTS()
            TPServiceMessage.MessageStatus = {&PENDING_ACTIVATION_CONFIRMATION}.
    END.
    OUTPUT CLOSE.

    RETURN "".

END PROCEDURE.

FINALLY:
   EMPTY TEMP-TABLE ttCustomer.
   xmlrpc_finalize().
END.


