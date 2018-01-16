DEFINE TEMP-TABLE gescon
	FIELD dummy AS CHAR.

DEFINE TEMP-TABLE generaldata
	FIELD gescalCode AS CHAR.

DEFINE TEMP-TABLE feasibilities
	FIELD addressId      AS CHAR
	FIELD technology 	 AS CHAR
	FIELD subtechnology  AS CHAR
	FIELD modality 		 AS CHAR
	FIELD territoryOwner AS CHAR
	FIELD speedProfile   AS CHAR
	FIELD speedEstimated AS CHAR
	FIELD source 		 AS CHAR
	FIELD priority 		 AS CHAR
	FIELD typology 		 AS CHAR
	FIELD dateActivation AS CHAR.

DEFINE DATASET GesconDataSet FOR generaldata, feasibilities.

PROCEDURE pSpeedCheck_GESCAL:
	DEFINE INPUT PARAMETER icGescal   AS CHAR NO-UNDO.
	DEFINE INPUT PARAMETER icNewSpeed AS CHAR NO-UNDO.

    DEFINE VARIABLE lcURL       	AS CHARACTER NO-UNDO.
	DEFINE VARIABLE lcHost      	AS CHARACTER NO-UNDO. 
	DEFINE VARIABLE lcAddress   	AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRespCode      AS CHAR      NO-UNDO.
    DEFINE VARIABLE liContentLength AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcResponseData  AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcHttpHead      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE llReturn        AS LOGICAL   NO-UNDO.

    ASSIGN
    	lcUrl     = fCParam("GESCON","SendHost") /* -H 10.100.7.43 -S 99 */
    	lcAddress = fCParam("GESCON","SendURL")  /* /api/gescon/1/feasibility?codeGescal=#GESCAL&marca=YOIGO&segmento=RES */
    	lcAddress = REPLACE(lcAddress,"#GESCAL", icGescal)
    	lcHost    = ENTRY(2,lcUrl," ") + ":" + ENTRY(4, lcUrl, " ").

    lcHttpHead = SUBST("POST &1 HTTP/1.0~r~n" +
                    "User-Agent: TMSRPC/&2~r~n" +
                    "Host: &3~r~n" +
                    "Connection: &4~r~n" +
                    "Content-Type: application/json~r~n" +
                    "Content-length: &5~r~n~r~n",
                    lcAddress,
                    "0.1",
                    lcHost,
                    STRING(false, "keep-alive/close"),
                    liContentLength).

   etime(true).
   
   RUN Gwy/tcpgwy_large.p(lcHttpHead,lcURL,60,1,"", output lcResponseData).

   IF lcResponseData <> "" THEN 
   DO:
       ASSIGN lcResponseData = '~{"GesconDataSet":~{"gescon":' + lcResponseData + '~}~}'.	

       ASSIGN llReturn = iParser:READ-JSON("LONGCHAR", lcResponseData, "EMPTY") NO-ERROR.

	   IF llReturn THEN
	       RETURN ERROR "Error".

	   FIND FIRST feasibilities NO-ERROR.
	   IF AVAIL feasibilities THEN  
	   DO:
	   		/* Add Comparision logic */
	   		RETURN TRUE.  
	   END.		
   END.

   RETURN FALSE.

END PROCEDURE.