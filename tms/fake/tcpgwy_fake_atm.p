/* Simulate the Air Response XML */


{commali.i}
{cparam2.i}
gcBrand = "1".

{xmlfunction.i}
{msbalance.i}
{date.i}

FUNCTION fGenRespCode RETURN CHARACTER
         (INPUT pcRespCodeList AS CHARACTER, INPUT-OUTPUT phSAXWriter AS HANDLE):

       DEFINE VARIABLE lcGenRespCode AS CHARACTER NO-UNDO INITIAL "0".

       /* only less than 25 %  select possible errors */
       IF RANDOM(1,4) EQ 1 THEN
          lcGenRespCode =  ENTRY( RANDOM(1,NUM-ENTRIES(pcRespCodeList)) , pcRespCodeList).

        /* add responseCode */
        phSAXWriter:START-ELEMENT("member").
        phSAXWriter:WRITE-DATA-ELEMENT("name","responseCode").
        phSAXWriter:START-ELEMENT("value").
        phSAXWriter:WRITE-DATA-ELEMENT("int",lcGenRespCode).
        phSAXWriter:END-ELEMENT("value").
        phSAXWriter:END-ELEMENT("member").

        RETURN lcGenRespCode.
END FUNCTION.

FUNCTION fBalanceEnquiryTResponse  RETURN LOGICAL
         (INPUT-OUTPUT phSAXWriter AS HANDLE,
          INPUT pcCLI AS CHARACTER):

         DEFINE VARIABLE lcRespCodeList AS CHARACTER
                         INITIAL "0,100,102,123,124,125,126" NO-UNDO.
         DEFINE VARIABLE lcRespCode AS CHARACTER NO-UNDO.

        /*  lcRespCode = fGenRespCode(lcRespCodeList,phSAXWriter). */

         /* calling method is always expect no error response
            should be change in the future
         */
         lcRespCode = "0".
         phSAXWriter:START-ELEMENT("member").
         phSAXWriter:WRITE-DATA-ELEMENT("name","responseCode").
         phSAXWriter:START-ELEMENT("value").
         phSAXWriter:WRITE-DATA-ELEMENT("int",lcRespCode).
         phSAXWriter:END-ELEMENT("value").
         phSAXWriter:END-ELEMENT("member").


         IF lcRespCode EQ "0" THEN DO:

             /* pick up balance */
             DEFINE VARIABLE liMsSeq  AS INTEGER NO-UNDO.
             DEFINE VARIABLE liCustNum AS INTEGER NO-UNDO.
             FIND MobSub WHERE MobSub.CLI = pcCLI NO-LOCK NO-ERROR.
             IF AVAILABLE MobSub THEN DO:
                FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK NO-ERROR.
                liMsSeq = MobSub.MsSeq.
             END.
             ELSE DO:
                FIND FIRST MsOwner NO-LOCK WHERE
                           MsOwner.Brand = gcBrand AND
                           MsOwner.CLI   = pcCLI NO-ERROR.
                IF AVAILABLE MsOwner THEN
                     FIND Customer WHERE Customer.CustNum = MsOwner.InvCust NO-LOCK NO-ERROR.
                ELSE RETURN FALSE.
                liMsSeq = MsOwner.MsSeq.
             END.

             IF NOT AVAILABLE Customer THEN RETURN FALSE.
             ELSE liCustNum = Customer.CustNum.

             DEFINE VARIABLE ldBalance AS DECIMAL NO-UNDO.
             ldBalance = fGetMsBalance(liMsSeq,liCustNum,"TOP") + /* ATM Recharge  */
                         fGetMsBalance(liMsSeq,liCustNum,"CAM") + /* Campain       */
                         fGetMsBalance(liMsSeq,liCustNum,"COM") + /* Compensation  */
                         fGetMsBalance(liMsSeq,liCustNum,"IT").   /* Initial Topup */

             ldBalance = ldBalance * 100.

             /* add accountValue1*/
             phSAXWriter:START-ELEMENT("member").
             phSAXWriter:WRITE-DATA-ELEMENT("name","accountValue1").
             phSAXWriter:START-ELEMENT("value").
             phSAXWriter:WRITE-DATA-ELEMENT("string",STRING(ldBalance)).
             phSAXWriter:END-ELEMENT("value").
             phSAXWriter:END-ELEMENT("member").

         END.

         RETURN TRUE.

END FUNCTION.

FUNCTION fGetAccountDetails  RETURN LOGICAL
         (INPUT-OUTPUT phSAXWriter AS HANDLE,
          INPUT pcCLI AS CHARACTER):

         DEFINE VARIABLE lcRespCode AS CHARACTER NO-UNDO.
         DEF VAR lcServiceClass AS CHAR NO-UNDO. 

        /*  lcRespCode = fGenRespCode(lcRespCodeList,phSAXWriter). */

         /* calling method is always expect no error response
            should be change in the future
         */
         lcRespCode = "0".
         phSAXWriter:START-ELEMENT("member").
         phSAXWriter:WRITE-DATA-ELEMENT("name","responseCode").
         phSAXWriter:START-ELEMENT("value").
         phSAXWriter:WRITE-DATA-ELEMENT("int",lcRespCode).
         phSAXWriter:END-ELEMENT("value").
         phSAXWriter:END-ELEMENT("member").

         FIND FIRST MobSub NO-LOCK WHERE
                    MobSub.CLI = pcCLI NO-ERROR.
         IF AVAIL MobSub AND 
                  MobSub.CLiType EQ "TARJ7" THEN lcServiceClass = "3".
         ELSE lcServiceClass = "6".

         IF lcRespCode EQ "0" THEN DO:

             /* add accountValue1*/
             phSAXWriter:START-ELEMENT("member").
             phSAXWriter:WRITE-DATA-ELEMENT("name","serviceClassCurrent").
             phSAXWriter:START-ELEMENT("value").
             phSAXWriter:WRITE-DATA-ELEMENT("int",lcServiceClass).
             phSAXWriter:END-ELEMENT("value").
             phSAXWriter:END-ELEMENT("member").

         END.

         RETURN TRUE.

END FUNCTION.

FUNCTION fUpdateServiceClass  RETURN LOGICAL
         (INPUT-OUTPUT phSAXWriter AS HANDLE,
          INPUT pcCLI AS CHARACTER):

         DEFINE VARIABLE lcRespCode AS CHARACTER NO-UNDO.

        /*  lcRespCode = fGenRespCode(lcRespCodeList,phSAXWriter). */

         /* calling method is always expect no error response
            should be change in the future
         */
         lcRespCode = "0".
         phSAXWriter:START-ELEMENT("member").
         phSAXWriter:WRITE-DATA-ELEMENT("name","responseCode").
         phSAXWriter:START-ELEMENT("value").
         phSAXWriter:WRITE-DATA-ELEMENT("int",lcRespCode).
         phSAXWriter:END-ELEMENT("value").
         phSAXWriter:END-ELEMENT("member").

         RETURN TRUE.

END FUNCTION.


FUNCTION fReFillTResponse  RETURN LOGICAL
         (INPUT-OUTPUT phSAXWriter AS HANDLE):

         DEFINE VARIABLE lcRespCodeList AS CHARACTER
                         INITIAL "0,1,2,100,102,104,120,123,125,126" NO-UNDO.
         DEFINE VARIABLE lcRespCode AS CHARACTER NO-UNDO.

         lcRespCode = fGenRespCode(lcRespCodeList, phSAXWriter).

END FUNCTION.

FUNCTION fAdjustmentTResponse RETURN LOGICAL
         (INPUT-OUTPUT phSAXWriter AS HANDLE):

         DEFINE VARIABLE lcRespCodeList AS CHARACTER
                         INITIAL "0,100,102,125,126,136" NO-UNDO.
         DEFINE VARIABLE lcRespCode AS CHARACTER NO-UNDO.

         lcRespCode = fGenRespCode(lcRespCodeList,phSAXWriter).

END FUNCTION.


/* main */

DEFINE INPUT PARAMETER pcRequest AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcMethodRequest AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLI           AS CHARACTER NO-UNDO.
lcMethodRequest = fGetNodeValue(pcRequest,"methodName") NO-ERROR.
lcCLI = fGetRPCNodeValue(pcRequest,"subscriberNumber") NO-ERROR.

/* generate an error XML response - probability 1/100 */
/* IF RANDOM(1,100) EQ 1 THEN RETURN "ERR". */

DEFINE VARIABLE lhSAXWriter  AS HANDLE    NO-UNDO.
DEFINE VARIABLE llOK         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lmXML        AS MEMPTR    NO-UNDO.
DEFINE VARIABLE lcRoot       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStruct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcXML        AS CHARACTER NO-UNDO.

ASSIGN
    lcRoot   = "methodResponse"
    lcStruct = "params,param,value,struct".


CREATE SAX-WRITER lhSAXWriter.

lhSAXWriter:FORMATTED = FALSE.

llOK = lhSAXWriter:SET-OUTPUT-DESTINATION("memptr",lmXML).

llOK = lhSAXWriter:START-DOCUMENT().
llOK = lhSAXWriter:START-ELEMENT(lcRoot).

fRPCStruct("Start",lcStruct,lhSAXWriter).

def stream slog.
output stream slog to /tmp/air_queries.log append.
put stream slog unformatted lcCLI ":" fts2hms(fmakets()) ":" lcMethodRequest skip
   pcRequest skip(2).
output stream slog close.

/* select request type */
CASE lcMethodRequest :
     WHEN "ReFill" THEN fReFillTResponse(INPUT-OUTPUT lhSAXWriter).
     WHEN "AdjustmentTRequest" THEN fAdjustmentTResponse(INPUT-OUTPUT lhSAXWriter).
     WHEN "BalanceEnquiryTRequest" THEN fBalanceEnquiryTResponse(INPUT-OUTPUT lhSAXWriter, lcCLI).
     WHEN "GetAccountDetails" THEN DO:
      fGetAccountDetails(INPUT-OUTPUT lhSAXWriter, lcCLI).
     END.
     WHEN "UpdateServiceClass" THEN DO:
      fUpdateServiceClass(INPUT-OUTPUT lhSAXWriter, lcCLI).
     END.
END CASE.

fRPCStruct("End",lcStruct,lhSAXWriter).

llOK = lhSAXWriter:END-ELEMENT(lcRoot).

llOK = lhSAXWriter:END-DOCUMENT().

DELETE OBJECT lhSAXWriter.

lcXML = GET-STRING(lmXML,1).

SET-SIZE(lmXML) = 0.

/*IF lcMethodRequest EQ "GetAccountDetails" THEN RETURN "ERROR:TEST ERROR".
ELSE */ RETURN lcXML.





