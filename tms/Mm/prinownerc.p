/* ----------------------------------------------------------------------------
  MODULI .......: PRINOWNERC.P
  TEHTAVA ......: Contract letter for owner change 
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 21.02.06
  MUUTOSPVM ....: 
  VERSIO .......: M15
---------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/utumaa.i new}
{Func/cparam2.i}
{Inv/edefine.i new}
{Func/finvtxt.i}
{Func/fcustdata.i}
{Func/msreqfunc.i}

DEF INPUT  PARAMETER iiRequest   AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiPrintType AS INT  NO-UNDO. /* 1=epl,2=local */
DEF OUTPUT PARAMETER ocError     AS CHAR NO-UNDO. 

DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR liITNum       AS INT  NO-UNDO EXTENT 4.
DEF VAR lcCustRole    AS CHAR NO-UNDO.
DEF VAR lcTxtType     AS CHAR NO-UNDO.
DEF VAR liPrintCnt    AS INT  NO-UNDO.
DEF VAR lcError       AS CHAR NO-UNDO.
DEF VAR liPrintType   AS INT  NO-UNDO.

ASSIGN tuni1 = "prinowne"
       tuni2 = "".

IF iiPrintType NE 2 THEN DO:
   ocError = "Only local printing allowed".
   RETURN.
END. 

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest THEN DO:
   ocError = "Unknown request".
   RETURN.
END.

IF MsRequest.ReqStat NE 0 AND MsRequest.ReqStat NE 11 THEN DO:
   ocError = "Request is not in a valid state for contract letter".
   RETURN.
END. 

       /* text for letters, first sheet cover  */
ASSIGN liITNum[1] = fGetInvTextID("General",
                                  "OwnerChgLetter",
                                   1,   /* only one language used */
                                   TODAY)
       /* actual letter */
       liITNum[2] = fGetInvTextID("General",
                                  "OwnerChgContr",
                                   1,   /* only one language used */
                                   TODAY)
       liITNum[3] = liITNum[2]
       
       /* attachment, reference explanations */
       liITNum[4] = fGetInvTextID("General",
                                  "OwnerChgRef",
                                   1,   /* only one language used */
                                   TODAY).
                                   
 
IF liITNum[1] = 0 OR liItNum[2] = 0 THEN DO:
   ocError = "Text for owner contract letter has not been defined".
   RETURN.
END.

/* letter is sent to new agreement customer -> get address from request */
ASSIGN lcEPLRName    = ENTRY(1,MsRequest.ReqCParam1,";") + " " +
                       ENTRY(2,MsRequest.ReqCParam1,";")
       lcEPLRLast    = ENTRY(1,MsRequest.ReqCParam1,";")
       lcEPLRFirst   = ENTRY(2,MsRequest.ReqCParam1,";")
       lcEPLRCoName  = ENTRY(3,MsRequest.ReqCParam1,";")
       lcEPLRAddr    = ENTRY(4,MsRequest.ReqCParam1,";")
       lcEPLRZipCode = ENTRY(5,MsRequest.ReqCParam1,";")
       lcEPLRPost    = lcEPLRZipCode + " " + 
                       ENTRY(6,MsRequest.ReqCParam1,";")
       lcEPLRCountry = ENTRY(7,MsRequest.ReqCParam1,";").


/* local print */
IF iiPrintType = 2 THEN DO:
   tila = TRUE.
   {Syst/utuloste.i "return"}
END.

DO liPrintCnt = 1 TO 4:

    IF liItNum[liPrintCnt] = 0 THEN NEXT. 
    
    /* print last letter as attachment */
    IF liPrintCnt = 4 
    THEN liPrintType = 5.
    ELSE IF iiPrintType <= 1
         THEN liPrintType = 1.
         ELSE liPrintType = 2.  /* local */
    
    RUN printxt (MsRequest.CustNum,
                 MsRequest.MsSeq, 
                 MsRequest.CLI,
                 1,  /* 1=invtext */
                 7,  /* address set here */
                 "",
                 "",
                 liITNum[liPrintCnt],
                 liPrintType,
                 0,  /* letterclass from invtext */
                 OUTPUT lcError).
                 
   ocError = ocError + lcError.              
END.

/* local print */
IF iiPrintType = 2 THEN DO:
   tila = FALSE.
   {Syst/utuloste.i}
END.

/* if printing succeeded then update new status for request */
IF ocError = "" THEN DO:
   fReqStatus(11,""). 
END.
      

 
