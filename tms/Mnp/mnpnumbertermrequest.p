/* ----------------------------------------------------------------------
  MODULE .......: mnpnumbertermrequest.p
  TASK .........: Create new mnp number termination request
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 08/2009
  Version ......: xfera
----------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{mnpmessages.i}
{mnp.i}
{tmsconst.i}
{msisdn.i}

DEFINE INPUT PARAMETER icCLI    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iiMsSeq  AS INTEGER   NO-UNDO.

DEF VAR lcXML AS CHAR NO-UNDO. /* xml should be < 32000 chars */
DEF VAR liSeq AS INTEGER NO-UNDO.
DEF VAR lcFormRequest AS CHARACTER NO-UNDO.
DEF VAR ldeNow AS DECIMAL NO-UNDO. 
DEF VAR lcStatuses AS CHARACTER NO-UNDO. 

ldeNow = fMakeTS().
   
FIND msisdn where
     msisdn.brand = gcBrand and
     msisdn.cli = icCLI AND
     msisdn.statuscode = {&MSISDN_ST_WAITING_RETURN} and
     msisdn.validto > ldeNow NO-LOCK NO-ERROR.
IF NOT AVAIL msisdn THEN DO:
   RETURN "ERROR:MSISDN was not found or it is in wrong status". 
END.

lcStatuses = SUBST("&1,&2,&3,&4,&5,&6",
   {&MNP_ST_APOR},{&MNP_ST_AREC},{&MNP_ST_ACAN},
   {&MNP_ST_AREC_CLOSED},{&MNP_ST_BCAN},{&MNP_ST_BDEF}).

FOR EACH MNPSub WHERE
         MNPSub.CLI = icCLI NO-LOCK:
   
   FOR EACH MNPProcess WHERE
            MNPProcess.MNPSeq = MNPSub.MNPSeq AND
           (MNPProcess.MNPType = {&MNP_TYPE_TERMINATION} OR
            MNPProcess.MNPType = {&MNP_TYPE_OUT}) NO-LOCK:
      IF LOOKUP(STRING(MNPProcess.StatusCode),lcStatuses) = 0 THEN
         RETURN "ERROR:Active MNP Process was found".
   END.
END.

ASSIGN
   liSeq         = NEXT-VALUE(M2MSeq)
   lcFormRequest = "005" + STRING(liSeq,"99999999"). 

DO TRANS:

   CREATE MNPProcess.
   ASSIGN 
      MNPProcess.CreatedTS   = fMakeTS()
      MNPProcess.MNPSeq      = next-value(m2mrequest)
      MNPProcess.FormRequest = lcFormRequest 
      MNPProcess.StatusCode  = {&MNP_ST_NEW}
      MNPProcess.Brand       = gcBrand
      MNPProcess.MNPType     = {&MNP_TYPE_TERMINATION}
      MNPProcess.UserCode    = katun
      MNPProcess.UpdateTS    = MNPProcess.CreatedTS.

   CREATE MNPSub.
   ASSIGN
      MNPSub.MNPSeq = MNPProcess.MNPSeq
      MNPSub.CLI    = icCLI
      MNPSub.MsSeq  = iiMsSeq.

   IF NOT fSendNumberingTerminationRequest(icCLI) THEN
      UNDO, RETURN "ERROR:Number termination request failed".

END.

RETURN "Number termination process was created successfully".
