/* ----------------------------------------------------------------------
  MODULE .......: custfunc.i
  TASK .........: Functions for handling Yoigo PRO customer functionality
                  Reference: YPRO-project
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......:
  CREATED ......: 24.5.2017
  CHANGED ......:
  ------------------------------------------------------------------------*/
 
&IF "{&YOIGOCUSTFUNC_I}" NE "YES"
&THEN
&GLOBAL-DEFINE YOIGOCUSTFUNC_I YES

/* Find correct customer segment */
FUNCTION fgetCustSegment RETURNS CHAR
   (INPUT icIdType AS CHAR,
    INPUT ilSelfemployed AS LOG,
    INPUT ilProCust AS LOG,
    INPUT icCustId AS CHARACTER,  /* YDR-2621 */
    OUTPUT ocCategory AS CHAR):

   DEF VAR lcSegment AS CHAR NO-UNDO.

   DEF BUFFER CustCat FOR CustCat.

   IF ilProCust THEN DO:
      FIND FIRST CustCat NO-LOCK WHERE
                 Custcat.brand EQ Syst.Var:gcBrand AND
                 Custcat.custidtype EQ icIdType AND
                 CustCat.selfemployed EQ ilSelfEmployed AND
                 CustCat.pro EQ ilProCust NO-ERROR.
      IF AVAIL CustCat THEN DO:
         lcSegment = CustCat.Segment.
         ocCategory = CustCat.category.
      END.
   END.
   ELSE DO:
      IF icIDType EQ "CIF" THEN DO:
         IF icCustID BEGINS "V00" THEN
            ocCategory = "30".  /* YDR-2621 */
         ELSE
            ocCategory = "23".
      END.
      ELSE IF icIDType EQ "NIF" AND ilSelfEmployed THEN 
         ocCategory = "44".         
      ELSE IF icIDType EQ "NIF" AND NOT ilSelfEmployed THEN
         ocCategory = "10". 
      ELSE IF icIDType EQ "NIE" AND ilSelfEmployed THEN
         ocCategory = "45".         
      ELSE IF icIDType EQ "NIE" AND NOT ilSelfEmployed THEN
         ocCategory = "11". 
      IF ocCategory > "" THEN DO:
         FIND FIRST CustCat NO-LOCK WHERE
                    Custcat.brand EQ Syst.Var:gcBrand AND
                    Custcat.category EQ ocCategory NO-ERROR.

         IF AVAIL CustCat THEN
            lcSegment = CustCat.Segment.
      END.
      ELSE DO:
         FIND FIRST CustCat NO-LOCK WHERE
                    Custcat.brand EQ Syst.Var:gcBrand AND
                    CustCat.custidtype EQ icIdType AND
                    CustCat.selfemployed EQ ilSelfemployed AND
                    CustCat.pro EQ ilProCust NO-ERROR.
         IF AVAIL CustCat THEN DO:
            lcSegment = CustCat.Segment.
            ocCategory = CustCat.category.
         END.
      END.
   END.
   RETURN lcSegment.
END.

FUNCTION fhasTVService RETURNS LOGICAL (INPUT iiMsseq AS INT):

   DEF BUFFER TPService FOR TPService.
   DEF BUFFER bTpService FOR TPService.

   FIND FIRST TPService WHERE 
              TPService.MsSeq EQ iiMsSeq AND
              TPService.Operation EQ "ACTIVATION" AND
              TPService.ServType  EQ "Television" AND
              TPService.servStatus NE "ERROR" NO-LOCK NO-ERROR.
   IF NOT AVAIL TPService THEN RETURN FALSE. /* No active TV Service */

   FIND LAST bTPService WHERE 
             bTPService.MsSeq EQ iiMsSeq AND
             bTPService.createdts GT TPService.createdts AND
             bTPService.ServStatus EQ "HANDLED" AND 
             bTPService.Operation = "DEACTIVATION" AND
             bTPService.ServType  = "Television"
             NO-LOCK NO-ERROR.

   IF AVAIL bTPService THEN RETURN FALSE. /* Deactivated */
   ELSE RETURN TRUE. /* Still active */
END FUNCTION.

/* YCO-712.                                                  */
/* Returns activation status for a subscription's TV service */          
/* Returned values:                                          */
/* 0 -> Inactive                                             */
/* 1 -> Active                                               */
/* 2 -> Pending activation                                   */
/* 3 -> Pending deactivation                                 */
FUNCTION fSubsTVServiceStatus RETURNS INTEGER  
   (INPUT iiMsSeq   AS INTEGER, 
    INPUT icDCEvent AS CHARACTER /* TV bundle */ ):
      
   DEFINE VARIABLE liServStatus AS INTEGER NO-UNDO.
   
   DEFINE BUFFER bTPService_Activation   FOR TPService.
   DEFINE BUFFER bTPService_Deactivation FOR TPService.    
      
   FIND LAST bTPService_Activation NO-LOCK WHERE 
             bTPService_Activation.MsSeq      EQ iiMsSeq             AND 
             bTPService_Activation.Operation  EQ {&TYPE_ACTIVATION}  AND 
             bTPService_Activation.ServType   EQ "Television"        AND 
             bTPService_Activation.ServStatus > ""                   AND 
             bTPService_Activation.Product    EQ icDCEvent 
             USE-INDEX MsSeqTypeStatus NO-ERROR.
   IF NOT AVAILABLE bTPService_Activation THEN 
      liServStatus =  0. /* Inactive */
   ELSE 
   DO:    
      IF bTPService_Activation.ServStatus = {&STATUS_HANDLED} THEN 
         ASSIGN liServStatus = 1.  /* Activation Done */ 
      ELSE IF LOOKUP(bTPService_Activation.ServStatus, {&STATUS_CANCELED} + "," + {&STATUS_ERROR}) > 0 THEN
         ASSIGN liServStatus = 0. /* Inactive */         
      ELSE     
         ASSIGN liServStatus = 2.  /* Pending activation */
      FIND LAST bTPService_Deactivation NO-LOCK WHERE 
                bTPService_Deactivation.MsSeq      EQ iiMsSeq                          AND 
                bTPService_Deactivation.Operation  EQ {&TYPE_DEACTIVATION}             AND 
                bTPService_Deactivation.ServType   EQ "Television"                     AND 
                bTPService_Deactivation.ServStatus > ""                                AND 
                bTPService_Deactivation.CreatedTS  > bTPService_Activation.CreatedTS   AND 
                bTPService_Deactivation.Product    EQ icDCEvent 
                USE-INDEX MsSeqTypeStatus NO-ERROR.
      IF AVAILABLE bTPService_Deactivation THEN DO:
         IF bTPService_Deactivation.ServStatus = {&STATUS_HANDLED} THEN 
            ASSIGN liServStatus = 0. /* Inactive */
         ELSE IF LOOKUP(bTPService_Deactivation.ServStatus, {&STATUS_CANCELED} + "," + {&STATUS_ERROR}) > 0 THEN
            ASSIGN liServStatus = 1. /* Deactivation error or canceled, so service is still 'Active' */
         ELSE 
            ASSIGN liServStatus = 3. /* Pending deactivation */
      END.
   END.         
      
   RETURN  liServStatus.  
      
END FUNCTION. 

/* YCO-712.                                                    */
/* Returns TRUE if the subscription has any TV servide active. */
FUNCTION fIsTVServiceActive RETURNS LOGICAL
   (iiMsSeq AS INTEGER):
   
   DEFINE BUFFER bDayCampaign FOR DayCampaign.
   
   FOR EACH bDayCampaign NO-LOCK WHERE
            bDayCampaign.BundleTarget = {&TELEVISION_BUNDLE}:
      IF fSubsTVServiceStatus(iiMsSeq, bDayCampaign.DCEvent) EQ 1 /* Active */ THEN 
         RETURN TRUE.
   END.
   
   RETURN FALSE.

END FUNCTION.


&ENDIF


