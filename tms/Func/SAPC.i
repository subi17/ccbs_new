
/*------------------------------------------------------------------------
    File        : SAPC.i
    Purpose     : General functions and procedures usefull for SAPC project 

    Syntax      :

    Description : This include needs the below in the program where it is 
                  inserted:
                    {syst/tmsconst.i}
                    
                  These statements are not included here because several .i 
                  files could use them and, including them in all the .i files, 
                  will mean the statements are duplicated, triplicated, ... if 
                  two or more of the .i files are included in the same program
                  
    Author(s)   : Diego Pastrana
    Created     : Tue Jul 24 12:04:43 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetDSSDataLimit RETURNS DECIMAL 
	(INPUT iiMsRequest LIKE MsRequest.msrequest) FORWARD.

FUNCTION fIsFunctionAvailInSAPC RETURNS LOGICAL 
	(INPUT iiMsRequest LIKE MsRequest.msrequest) FORWARD.


/* ***************************  Main Block  *************************** */

/* ************************  Function Implementations ***************** */


FUNCTION fGetDSSDataLimit RETURNS DECIMAL 
	(INPUT iiMsRequest LIKE MsRequest.MsRequest):
/*------------------------------------------------------------------------------
 Purpose: Return the data amount for a DSS group so this limit can be sent 
          to the network
 Notes:
------------------------------------------------------------------------------*/	
   DEF VAR liDSSMsSeq          AS INT   NO-UNDO.
   DEF VAR ldeCurrentDSSLimit  AS DEC   NO-UNDO.
   DEF VAR liDSSLimit          AS INT64 NO-UNDO.
   DEF VAR lcDSSBundleId       AS CHAR  NO-UNDO.
   
   DEF BUFFER MsRequest  FOR MsRequest.
   DEF BUFFER ShaperConf FOR ShaperConf.
   
   FIND FIRST MsRequest WHERE 
              MsRequest.Msrequest = iiMsRequest NO-LOCK NO-ERROR.
   IF NOT AVAIL MsRequest THEN 
      RETURN 0.

   /* Find DSS limits */
   fGetDSSMsSeqLimit(INPUT MsRequest.CustNum,
                     INPUT (IF MsRequest.ActStamp > Func.Common:mMakeTS() THEN
                               MsRequest.ActStamp ELSE Func.Common:mMakeTS()),
                     OUTPUT liDSSMsSeq,
                     OUTPUT ldeCurrentDSSLimit,
                     OUTPUT lcDSSBundleId).

   /* Special behaviour for DSS4 */
   IF MSRequest.ReqCparam3 EQ {&DSS4} THEN
   DO: 
      FIND FIRST ShaperConf NO-LOCK WHERE
                 ShaperConf.Brand        EQ Syst.Var:gcBrand AND
                 ShaperConf.ShaperConfID EQ {&DSS4SHAPERID}  NO-ERROR.
      IF NOT AVAIL ShaperConf THEN
         RETURN 0.
   END.

   /* Business logic inherited from fMakeDSSCommLine.
      It looks like in "MODIFY" DSS group requests, the data in the request 
      is updated */
   IF ldeCurrentDSSLimit > 0          AND 
      MsRequest.ReqCparam1 = "MODIFY" AND
     (   MsRequest.ReqCparam2 = "" 
      OR INDEX(MsRequest.ReqCparam2,"LIMIT") > 0) THEN
   DO:
   
      blk:
      DO TRANSACTION ON ERROR UNDO, RETURN 0
                     ON STOP UNDO, RETURN 0:
                        
         ASSIGN liDSSLimit = (ldeCurrentDSSLimit * 1024 * 1024).
         
         FIND CURRENT MsRequest EXCLUSIVE-LOCK.

         IF MSRequest.ReqCparam3 EQ {&DSS4} THEN
            ASSIGN  
               MSRequest.ReqCparam2 = "DSS-ACCOUNT="    + STRING(MSRequest.CustNum)        + "," +
                                      "TEMPLATE="       + ShaperConf.Template              + "," +
                                      "TARIFF_TYPE="    + ShaperConf.TariffType            + "," +
                                      "TARIFF="         + MSRequest.ReqCparam3             + "," +
                                      "LIMIT_UNSHAPED=" + STRING(ShaperConf.LimitUnshaped) + "," +
                                      "LIMIT_SHAPED="   + STRING(ShaperConf.LimitShaped).
         ELSE    
            ASSIGN 
               MSRequest.ReqCparam2 = "DSS-ACCOUNT=" + STRING(MSRequest.CustNum)  + "," +
                                      "TEMPLATE=DSS_MONTHLY"                      + "," +
                                      "TARIFF_TYPE=DSS"                           + "," +
                                      "TARIFF="          + MsRequest.ReqCparam3   + "," +
                                      "LIMIT_UNSHAPED="  + STRING(liDSSLimit)     + "," +
                                      "LIMIT_SHAPED="    + STRING({&PL_LIMIT_SHAPED}).

         RELEASE MSRequest.
      END. /* blk */
      
   END.

   /* Adjusting limit for DSS4. In Mb */
   IF MSRequest.ReqCparam3 EQ {&DSS4} THEN
      ASSIGN 
         ldeCurrentDSSLimit = ((ShaperConf.LimitUnshaped / 1024) / 1024).
   
   RETURN ldeCurrentDSSLimit.
		
END FUNCTION.

FUNCTION fIsFunctionAvailInSAPC RETURNS LOGICAL 
	(INPUT iiMsRequest LIKE MsRequest.msrequest):
/*------------------------------------------------------------------------------
 Purpose: Checks the request can be managed by SAPC logic
 Notes:
------------------------------------------------------------------------------*/	
   DEFINE BUFFER MsRequest  FOR MsRequest. 
   DEFINE BUFFER bMsRequest FOR MsRequest. 

   FIND MsRequest WHERE MsRequest.MsRequest = iiMsRequest NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MsRequest THEN /* This should never happen */ 
      RETURN FALSE. 
   
   /* Checking "create DSS group" request */
   IF MsRequest.ReqType = {&REQTYPE_DSS} AND
      MsRequest.ReqCparam1 = "CREATE"    AND 
      LOOKUP(MsRequest.ReqCparam3,{&DSS_BUNDLES} ) > 0 THEN
      RETURN TRUE.
   
   /* Checking "adding MSISDN to DSS group" request */
   IF MsRequest.ReqType = {&REQTYPE_DSS} AND
      MsRequest.ReqCparam1 = "ADD"       AND 
      LOOKUP(MsRequest.ReqCparam3,{&DSS_BUNDLES} ) > 0 THEN
      RETURN TRUE.

   /* Checking "removing MSISDN to DSS group" request */
   IF MsRequest.ReqType = {&REQTYPE_DSS} AND
      MsRequest.ReqCparam1 = "REMOVE"       AND 
      LOOKUP(MsRequest.ReqCparam3,{&DSS_BUNDLES} ) > 0 THEN
      RETURN TRUE.

   /* Checking create upsell or add/delete dataplan */
   IF MsRequest.ReqCParam1 = "SHAPER" AND
      MsRequest.OrigRequest > 0 THEN 
   DO:
      FIND FIRST bMsRequest WHERE 
                 bMsRequest.MsRequest = MsRequest.OrigRequest
                 NO-LOCK NO-ERROR.
      IF AVAILABLE bMsRequest THEN 
      DO:
         IF bMsRequest.ReqCparam3 MATCHES "*_UPSELL" THEN
            RETURN TRUE.
         ELSE
         IF bMsRequest.ReqType = 8 OR   /* ADD - Per. contract activation */
            bMsRequest.ReqType = 9 THEN /* DELETE - Per. contract termination*/
            RETURN TRUE.            
      END.
   END.   
    
   /* If not check is successful */       
   RETURN FALSE.

END FUNCTION.

