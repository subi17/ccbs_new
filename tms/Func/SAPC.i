
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


FUNCTION fIsFunctionAvailInSAPC RETURNS LOGICAL 
	(INPUT iiMsRequest LIKE MsRequest.msrequest) FORWARD.


/* ***************************  Main Block  *************************** */

/* ************************  Function Implementations ***************** */


FUNCTION fIsFunctionAvailInSAPC RETURNS LOGICAL 
	(INPUT iiMsRequest LIKE MsRequest.msrequest):
/*------------------------------------------------------------------------------
 Purpose: Checks the request can be managed by SAPC logic
 Notes:
------------------------------------------------------------------------------*/	
   DEFINE BUFFER bMsRequest FOR MsRequest. 

   FIND MsRequest WHERE MsRequest.MsRequest = iiMsRequest NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MsRequest THEN /* This should never happen */ 
      RETURN FALSE. 
   
   /* Checking "create DSS group" request */
   IF MsRequest.ReqType = {&REQTYPE_DSS} AND
      MsRequest.ReqCparam1 = "CREATE"    AND 
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

