/* ----------------------------------------------------------------------
  MODULE .......: bob_discount.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Saktibalan
  CREATED ......: 08.05.18
  Version ......: Yoigo
----------------------------------------------------------------------- */

/* ***************************  Definitions  ************************** */
{Syst/commpaa.i}
Syst.Var:katun   EQ "Cron".
Syst.Var:gcBrand EQ "1".

{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/coinv.i}
{Func/fsendsms.i}
{Mc/dpmember.i}


DEFINE VARIABLE lcLine   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSep    AS CHARACTER NO-UNDO INITIAL ";".
DEFINE VARIABLE liNumOK  AS INTEGER   NO-UNDO.
DEFINE VARIABLE liNumErr AS INTEGER   NO-UNDO.

/* files and dirs */
DEFINE VARIABLE lcLogFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIncDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessDir    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInputFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcDir       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldtConValue     AS DATE      NO-UNDO.
DEFINE VARIABLE liLastDtValue   AS INTEGER   NO-UNDO.
DEFINE VARIABLE liKeyValue      AS INTEGER   NO-UNDO.

/* field variables */
DEFINE VARIABLE liOrderID    AS INTEGER   NO-UNDO.
DEFINE VARIABLE liActivFlag  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcStoreID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLeadFlag   AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldaValidFrom AS DATE      NO-UNDO.
DEFINE VARIABLE ldaValidTo   AS DATE      NO-UNDO.

/* streams */
DEFINE STREAM sin.
DEFINE STREAM sFile.
DEFINE STREAM sLog.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN
   lcIncDir     EQ fCParam("Charges","IncDir")
   lcProcessDir EQ fCParam("Charges","IncProcessDir")
   lcProcDir    EQ fCParam("Charges","IncProcDir")
   lcSpoolDir   EQ fCParam("Charges","OutSpoolDir")
   lcOutDir     EQ fCParam("Charges","OutDir").

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine  "|"
      icMessage SKIP.

END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).

   liNumErr EQ liNumErr + 1.

END FUNCTION.

FUNCTION fUpdateCreditCardStatus RETURNS INTEGER
   (INPUT icBrand  AS CHAR,
    INPUT icEvent  AS CHAR,
    INPUT iiMsSeq  AS INT,
    INPUT idToDate AS DATE):
   
   DEFINE BUFFER lbDCCLi FOR DCCLi.
   
   DEFINE VARIABLE liReturnValue AS INTEGER NO-UNDO.
   
   FIND FIRST lbDCCLi EXCLUSIVE-LOCK WHERE 
              lbDCCLi.Brand   EQ icBrand  AND
              lbDCCLi.DCEvent EQ icEvent  AND 
              lbDCCLi.MsSeq   EQ iiMsSeq  AND 
	          lbDCCLi.ValidTo EQ idToDate NO-ERROR.                    
				                       
   IF NOT AVAILABLE lbDCCli THEN DO:
      fError("Inactive Periodical Contract").
      NEXT.
   END.			   
   lbDCCLi.ServiceStatus EQ {&YOICARD_STATUS_ACTIVADA}.
   liReturnValue EQ lbDCCLi.ServiceStatus.
   
   RETURN liReturnValue.
   
END FUNCTION.

FUNCTION fDiscPlanValidation RETURNS LOGIC
         (INPUT icBrand      AS CHAR,
          INPUT icDiscPlanID AS CHAR):
          
         FIND FIRST DiscountPlan NO-LOCK WHERE
                    DiscountPlan.Brand    EQ icBrand      AND
                    DiscountPlan.DPRuleID EQ icDiscPlanID NO-ERROR.
         IF NOT AVAILABLE DiscountPlan THEN DO:
            fError("Unknown discount plan").
            NEXT.
         END.
         ASSIGN 
            ldaValidFrom = /* DATE */
            ldaValidTo = /* DATE */ NO-ERROR.
            
         IF ERROR-STATUS:ERROR OR ldaValidFrom = ? THEN DO:
            fError("Invalid period").
            NEXT.
         END.
         IF ldaValidTo = ? THEN ldaValidTo = 12/31/2049.

         IF ldaValidTo < ldaValidFrom THEN DO:
            fError("End date is earlier than begin date").
            NEXT.
         END.                  
END FUNCTION.

INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.

   lcInputFile EQ lcIncDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:
      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.

      lcProcessFile EQ fMove2TransDir(lcInputFile, "", lcProcessDir).

      IF lcProcessFile EQ "" THEN NEXT.

      IF NOT SEARCH(lcProcessFile) EQ ? THEN
         INPUT STREAM sin FROM VALUE(lcProcessFile).
      ELSE NEXT.

   END.
   ELSE NEXT.

   ASSIGN
      liNumOk  EQ 0
      liNumErr EQ 0.

   fBatchLog("START", lcProcessFile).

   lcLogFile EQ lcSpoolDir + lcFileName + ".log".

   OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.

   RUN pCreateDiscountForCreditCard.

   PUT STREAM sLog UNFORMATTED
       "input: "   STRING(liNumOK + liNumErr) ", "
       "updated: " STRING(liNumOK) ", "
       "errors: "  STRING(liNumErr) SKIP.

   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   lcReportFileOut EQ fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile EQ fMove2TransDir(lcProcessFile, "", lcProcDir).

   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
END. /* REPEAT */

INPUT STREAM sFile CLOSE.

PROCEDURE pCreateDiscountForCreditCard:       
      
   REPEAT TRANSACTION:
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine EQ "" THEN NEXT.
	  
	  IF NUM-ENTRIES(lcLine,lcSep) < 4 THEN DO:
         fError("Invalid line format").
         NEXT.
      END.

      /*[Order ID];[ActivationFlag];[Store ID];[Lead Flag]*/
	  ASSIGN
         liOrderID   EQ ENTRY(1,lcReadLine,lcSep)
         liActivFlag EQ INT(ENTRY(2,lcReadLine,lcSep))
         lcStoreID   EQ ENTRY(3,lcReadLine,lcSep)
         liLeadFlag  EQ INT(ENTRY(4,lcReadLine,lcSep) NO-ERROR.

	  FIND FIRST Order NO-LOCK WHERE
                 Order.Brand   EQ Syst.Var:gcBrand AND  
	             Order.OrderId EQ liOrderID        NO-ERROR.				 
	  
	  IF NOT AVAILABLE Order THEN DO:
         fError("Invalid Order").
         NEXT.
      END.
	  
	  IF LOOKUP(Order.Statuscode, {&ORDER_INACTIVE_STATUSES}) EQ 0 THEN DO:
	     fError("Ongoing Order").
         NEXT.
	  END.	  
	  
	  IF LOOKUP(Order.Statuscode, {&ORDER_INACTIVE_STATUSES}) > 0 THEN DO: 
	     fError("Canceled Order").
         NEXT.
	  END.
	  
	  FIND FIRST MobSub NO-LOCK WHERE 
	             MobSub.MsSeq EQ Order.MsSeq NO-ERROR.					
	  IF NOT AVAILABLE MobSub THEN DO:	  
	     FIND FIRST TermMobSub NO-LOCK WHERE
                    TermMobSub.MsSeq EQ Order.MsSeq NO-ERROR.					   
         IF AVAILABLE TermMobSub THEN DO:
		    ferror("Inactive Subscription").
		    NEXT.
         END.
         fError("Invalid Mobile Subscription").
		 NEXT.
      END.
	  
	  CASE liActivFlag:
		 WHEN 0 THEN
		   
	     END.
			
		 WHEN 1 THEN DO:     /* Only update credit card status to active */	            
            fUpdateCreditCardStatus(Syst.Var:gcBrand,
                                    "YOICARD",
                                    MobSub.MsSeq,
                                    /*ValidTo date */).	         			   
	     END.
	  
	     WHEN 2 THEN DO: /* update credit card satus to active and apply use discount */
		    fUpdateCreditCardStatus(Syst.Var:gcBrand,
                                    "YOICARD",
                                    MobSub.MsSeq,
                                    /*ValidTo date */). 
                                    
            fDiscPlanValidation(Syst.Var:gcBrand,
                                {&YOIGO_USE_DISCOUNT}).
                                 
            fAddDiscountPlanMember(MobSub.MsSeq,
                                   DiscountPlan.DPRuleID,
                                   5.00,
                                   ldaValidFrom,
                                   ldaValidTo,
                                   ?,
                                   liOrderID).        
              			   
            /* logic to capture change credit card status yet to code */			
	     END.
	  
	     WHEN 3 THEN DO: /* only apply use discount */
	        fDiscPlanValidation(Syst.Var:gcBrand,
                                   {&YOIGO_USE_DISCOUNT}).
                                   
               fAddDiscountPlanMember(MobSub.MsSeq,
                                      DiscountPlan.DPRuleID,
                                      5.00,
                                      ldaValidFrom,
                                      ldaValidTo,
                                      ?,
                                      liOrderID).           
         END.
	  
	     WHEN 4 THEN DO: 
			fUpdateCreditCardStatus(Syst.Var:gcBrand,
                                    "YOICARD",
                                     MobSub.MsSeq,
                                     /*ValidTo date */).	
									
            fDiscPlanValidation(Syst.Var:gcBrand,
                                   {&YOIGO_ACTIVATION_DISCOUNT}).
                                   
            fAddDiscountPlanMember(MobSub.MsSeq,
                                   DiscountPlan.DPRuleID,
                                   25.00,
                                   ldaValidFrom,
                                   ldaValidTo,
                                   ?,
                                   liOrderID).       
               		
			/* logic to capture change credit card status yet to code */
	     END.  
	      
   END. /* REPEAT TRANSACTION */ 
     	 
END PROCEDURE.

/* to send SMS to MM
   RUN pSendSMS(INPUT MsOwner.MsSeq, 
                INPUT 0, 
				INPUT lcSMSName,
                INPUT 10, 
				INPUT {&UPSELL_SMS_SENDER}, 
				INPUT "").
*/					  