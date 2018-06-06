
/*Barrengine is used to create barring commands/requests.
Output parameters:
ocResult:
   Number value, not 0: Request is created successfully.
   Text value         : Error or information text. This is given if
                        request creation is failed.
*/

{Syst/commali.i}
{Func/barr_request.i}
{Func/barrfunc.i}

DEFINE INPUT PARAMETER iiMsSeq    AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER icBarringCommands AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icSource   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icCreator  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER idActStamp AS DECIMAL   NO-UNDO.
DEFINE INPUT PARAMETER icSMSText  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER ocStatus  AS CHARACTER NO-UNDO.

DEFINE VARIABLE liNewRequest  AS INTEGER NO-UNDO.
DEFINE VARIABLE liUnbarrRequest AS INT NO-UNDO. 
DEFINE VARIABLE ldtAction     AS DATE    NO-UNDO.
DEFINE VARIABLE liTime        AS INT     NO-UNDO.
DEFINE VARIABLE lcStatus      AS CHAR    NO-UNDO.

/*Check that mobsub exists*/

FIND MobSub NO-LOCK WHERE
     MobSub.MsSeq = iiMsSeq NO-ERROR.

IF NOT AVAIL MobSub THEN RETURN. 

/*YPR-4774*/
/*(De)Activation is not allowed if fixed line provisioning is pending*/
/*This should be checked before coming to barring setting. 
This is an additional checkpoint*/
IF MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} /*16*/ OR
   MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE} /*17*/ THEN DO:
   ocStatus = "No active mobile line prevents setting".
   RETURN.
END.

Func.Common:mSplitTS(idActStamp,
         OUTPUT ldtAction,
         OUTPUT liTime).

lcStatus = fValidateBarrRequest(iiMsSeq, icBarringCommands).
IF lcStatus NE "" THEN DO:
   ocStatus = lcStatus. 
   RETURN.
END.

DEF VAR lcBarringCommand AS CHAR NO-UNDO. 
DEF VAR lcBarring AS CHAR NO-UNDO.
DEF VAR liLoop AS INT NO-UNDO. 
DEF VAR llDebtBarring AS LOG NO-UNDO. 
DEF VAR lcDebtBarrings AS CHAR NO-UNDO. 
DEF VAR lcActiveBarrings AS CHAR NO-UNDO. 
DEF VAR lcUnBarring AS CHAR NO-UNDO. 

lcDebtBarrings = fGetBarringsInGroup("Collections").

IF fISInList(icBarringCommands,lcDebtBarrings) EQ TRUE THEN DO:
  
   IF NUM-ENTRIES(icBarringCommands) > 1 THEN DO:
     ocStatus = "ERROR:Collection level barring cannot be combined with other barring commands".
     RETURN.
   END.

   lcActiveBarrings = Func.BarrMethod:mGetActiveBarrings(iiMsSeq).

   IF icBarringCommands EQ "Debt_HOTL=1" AND
      LOOKUP("Debt_HOTLP",lcActiveBarrings) > 0 THEN DO:
      ocStatus = "ERROR:Debt_HOTL cannot be set over Debt_HOTLP".
      RETURN.
   END.

   DO liLoop = 1 TO NUM-ENTRIES(lcActiveBarrings):

      IF ENTRY(1,icBarringCommands,"=") EQ ENTRY(liLoop,lcActiveBarrings)
         THEN NEXT.

      IF LOOKUP(ENTRY(liLoop,lcActiveBarrings),lcDebtBarrings) > 0 THEN
         lcUnbarring = "," + lcUnbarring + 
                       ENTRY(liLoop,lcActiveBarrings) + "=0".
   END.
   lcUnbarring = LEFT-TRIM(lcUnbarring,",").
   
   IF lcUnbarring NE "" AND
      icCreator NE "Collection" THEN DO:
      ocStatus = "ERROR:Cannot activate Collection level barring over existing Collection level barring ".
      RETURN.
   END.

END.

DO TRANSACTION:

   IF lcUnbarring > "" THEN DO:
      fCreateBP(lcUnbarring,
                MobSub.InvCust,       /*iiInvCust*/
                MobSub.Cli,           /*icCli*/
                MobSub.MsSeq,         /*iiMsSeq*/
                "",                   /*icCLB*/
                0,                    /*mandatory request*/
                "5",                  /*icSource*/
                icCreator,            /*icCreator*/
                idActStamp,           /*idActStamp*/
                "",                   /*icSMSText*/
                "",                   /*icPrevBarr*/
                OUTPUT liUnbarrRequest). /*oiCrReqId*/
      ocStatus = STRING(liUnbarrRequest).
      IF NOT liUnbarrRequest > 0 THEN UNDO, RETURN.
   END.

   /*Create request*/
   fCreateBP(icBarringCommands,
             MobSub.InvCust,       /*iiInvCust*/
             MobSub.Cli,           /*icCli*/
             MobSub.MsSeq,         /*iiMsSeq*/
             "",                   /*icCLB*/
             liUnbarrRequest,      /*mandatory request*/
             icSource,             /*icSource*/
             icCreator,            /*icCreator*/
             idActStamp,           /*idActStamp*/
             icSMSText,            /*icSMSText*/
             "",                   /*icPrevBarr*/
             OUTPUT liNewRequest). /*oiCrReqId*/

   ocStatus = STRING(liNewRequest).
   IF NOT liNewRequest > 0 THEN UNDO, RETURN.
END.
