/* ----------------------------------------------------------------------
  MODULE .......: mnpmigrationrequest.p
  TASK .........: Create new mnp migration process
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 10/2009
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/testpaa.i}
katun = "anttis".
{Func/timestamp.i}
{Mnp/mnpmessages.i}
{Mnp/mnp.i}

DEFINE VARIABLE lcXML AS CHAR NO-UNDO. /* xml should be < 32000 chars */
DEFINE VARIABLE liSeq         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcFormRequest AS CHARACTER NO-UNDO.
     
ehto = 10.
RUN Syst/ufkey.p.

CREATE ttMigrationRequest.

UPDATE ttMigrationRequest 
WITH frame a 1 col
overlay centered row 5 title "Migration Request".

if not lookup(keylabel(LASTKEY),"f1,return") > 0 then do:
   MESSAGE "Request creation aborted" VIEW-AS ALERT-BOX.
   UNDO.
end.

ASSIGN
   liSeq         = NEXT-VALUE(M2MSeq)
   lcFormRequest = "005" + STRING(liSeq,"99999999").

CREATE MNPProcess.
ASSIGN 
   MNPProcess.CreatedTS   = fMakeTS()
   MNPProcess.MNPSeq      = next-value(m2mrequest)
   MNPProcess.FormRequest = lcFormRequest
   MNPProcess.OrderId     = 0
   MNPProcess.StatusCode  = 0
   MNPProcess.Brand       = gcBrand
   MNPProcess.MNPType     = 5
   MNPProcess.UserCode    = katun
   MNPProcess.UpdateTS    = MNPProcess.CreatedTS.

IF fSendMigrationRequest(INPUT TABLE ttMigrationRequest BY-REFERENCE) THEN

   MESSAGE "Migration request was created" VIEW-AS ALERT-BOX.
   ELSE MESSAGE "Migration request unsuccessful" VIEW-AS ALERT-BOX.


RETURN "".
