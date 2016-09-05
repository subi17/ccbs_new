
{Syst/commpaa.i}
katun = "rafaeldv".
gcBrand = "1".

{Func/timestamp.i}
{Func/xmlfunction.i}

DEFINE VARIABLE cOutputFile AS CHARACTER NO-UNDO INITIAL "ytc_121_4.output".
DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.  
DEFINE STREAM sout.
OUTPUT STREAM sout TO VALUE(cOutputFile).



DEFINE VARIABLE ldate AS DATE NO-UNDO. 
DEFINE VARIABLE ldate2 AS DATE NO-UNDO.

DEFINE VARIABLE lcTimeInit AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTimeEnd AS CHARACTER NO-UNDO. 

DEFINE VARIABLE ldBegin AS DECIMAL NO-UNDO.
DEFINE VARIABLE ldEnd AS DECIMAL NO-UNDO.

DEFINE VARIABLE lcXML AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liRespCode AS INTEGER NO-UNDO. 
DEFINE BUFFER bufPP FOR PrePaidRequest.

ldate = DATE(07,21,2009).
ldate2 = DATE(07,22,2009).

lcTimeInit = "00:00:00". /*"09:54:00". */
lcTimeEnd =  "24:59:59". /*"15:56:00". */

ldBegin = fHMS2TS(ldate,lcTimeInit).
ldEnd = fHMS2TS(ldate2,lcTimeEnd).

 
FOR EACH PrepaidRequest NO-LOCK WHERE 
          PrepaidRequest.Brand = "1" AND
          PrepaidRequest.TSRequest > ldBegin AND
          PrepaidRequest.TSRequest < ldEnd AND
          PrepaidRequest.Response BEGINS "ERR:Lost connection" AND
        /*  PrepaidRequest.Source = "Web Order" AND
          PrepaidRequest.UserCode = "CreSub":
        */
         PrepaidRequest.PPStatus = 2 :
       /*
       RUN Gwy/balancequery.p (PrepaidRequest.CLI).
       */

        RUN Gwy/pp_platform.p(gcBrand,PrePaidRequest.PPRequest).
        lcXML = RETURN-VALUE.
        liRespCode = INT(fGetRPCNodeValue(lcXML,"responseCode")) NO-ERROR.
        
        DO FOR bufPP:
           FIND FIRST bufPP WHERE RECID(bufPP) = RECID(PrePaidRequest) EXCLUSIVE-LOCK.
           ASSIGN
                bufPP.TSResponse = fMakeTS()
                bufPP.Response   = lcXML
                bufPP.RespCode   = liRespCode.
        END.

         cLine = 
               STRING(PrepaidRequest.PPRequest) + " " +
               STRING(PrepaidRequest.CLI) +  " " +
               STRING(PrepaidRequest.TSResponse) +  " " +
               STRING(PrePaidRequest.Request) +  " " + 
               STRING(PrePaidRequest.TopUpAmt) + " " +
               STRING(PrePaidRequest.RespCode) +  " " +
               STRING(PrepaidRequest.PPStatus). 

       PUT STREAM sout UNFORMATTED cLine  SKIP.

END.

OUTPUT STREAM sout CLOSE.

IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
