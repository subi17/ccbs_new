/**
 * Creates a termination request.
 *
 * @input    termdata;struct;mandatory;contains input data
 * @termdata msseq;int;mandatory;key of mobsub
             salesman;string;mandatory;user who initiated action
             orderer;int;mandatory;termination orderer/reason
             killts;datetime;mandatory;timestamp of mobsub termination
             simstat;int;optional;a new sim status after termination (optional)
             msisdnstat;int;optional;a new msisdn status after termination (optional)
             quartime;int;optional;quarantine time in days (optional)
             opcode;int;optional;operator code (required when orderer = 2)
             termination_type;string;optional;full (default) or partial for convergent mobile part

 * @output  success;boolean
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i} 

{Syst/commpaa.i}
gcBrand = "1".
{Func/fsubstermreq.i}
{Mm/fbundle.i}
{Func/msisdn_prefix.i}
{Func/main_add_lines.i}

/* Input parameters */
DEF VAR piMsSeq    AS INT  NO-UNDO.
DEF VAR pcSalesman AS CHAR NO-UNDO.
DEF VAR piOrderer  AS INT  NO-UNDO.
DEF VAR pdeKillTS  AS DEC  NO-UNDO.
DEF VAR piSimStat  AS INT NO-UNDO.
DEF VAR piMSISDNStat AS INT NO-UNDO.
DEF VAR piQuarTime AS INT NO-UNDO.
DEF VAR piOpCode   AS INT NO-UNDO.
DEF VAR pcTermType AS CHAR NO-UNDO.

DEF VAR pcTermStruct AS CHAR NO-UNDO.
DEF VAR lcTermStruct AS CHAR NO-UNDO.

/* Local variables */
DEF VAR ocResult         AS CHAR      NO-UNDO.
DEF VAR llPenalty        AS LOG       NO-UNDO.
DEF VAR liError          AS INT       NO-UNDO.
DEF VAR llYoigoCLI       AS LOG       NO-UNDO.
DEF VAR llMasmovilCLI    AS LOG       NO-UNDO.
DEF VAR lcKillTS         AS CHAR      NO-UNDO.
DEF VAR liReq            AS INT       NO-UNDO.
DEF VAR lcOpCode         AS CHARACTER NO-UNDO. 
DEF VAR ldaTermDate      AS DATE      NO-UNDO. 
DEF VAR lcTenant         AS CHAR      NO-UNDO.
DEF VAR llYoigoTenant    AS LOG       NO-UNDO INIT FALSE.
DEF VAR llMasmovilTenant AS LOG       NO-UNDO INIT FALSE.

/* Output parameters */
DEF VAR result AS LOGICAL.

pcTermType = {&TERMINATION_TYPE_FULL}. /* Default value */

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.
pcTermStruct = get_struct(param_toplevel_id, "0").

lcTermStruct = validate_request(pcTermStruct,
        "salesman!,msseq!,orderer!,killts!,simstat,msisdnstat,quartime,opcode,termination_type").
IF lcTermStruct EQ ? THEN RETURN.

/* required params */
piMsSeq     = get_pos_int(pcTermStruct, "msseq").
katun       = "VISTA_" + get_string(pcTermStruct, "salesman").
piOrderer   = get_pos_int(pcTermStruct, "orderer").
pdeKillTS   = get_timestamp(pcTermStruct, "killts").
IF LOOKUP("termination_type", lcTermStruct) GT 0 THEN
   pcTermType  = get_string(pcTermStruct, "termination_type").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(katun) EQ "VISTA_" THEN DO:
   RETURN appl_err("username is empty").
END.

{newton/src/findtenant.i NO ordercanal MobSub MsSeq piMsSeq}
IF NOT (pcTermType EQ {&TERMINATION_TYPE_PARTIAL} OR
        pcTermType EQ {&TERMINATION_TYPE_FULL}) THEN
      RETURN appl_err("Incorrect termination type").

/* Check that mobsub is available */
FIND MobSub WHERE
     MobSub.MsSeq = piMsSeq
NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN DO:
   RETURN appl_err("System Error ! Mobile Subscription not available").
END.

IF pcTermType EQ {&TERMINATION_TYPE_PARTIAL} AND 
   MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE} THEN DO:
   RETURN appl_err("System Error ! Partial termination not allowed").
END.

/* Yoigo MSISDN? */
ASSIGN 
    lcTenant         = BUFFER-TENANT-NAME(MobSub)
    llYoigoCLI       = fIsYoigoCLI(MobSub.CLI)
    llMasmovilCLI    = fIsMasmovilCLI(MobSub.CLI)
    llYoigoTenant    = (IF lcTenant = {&TENANT_YOIGO}    THEN TRUE ELSE FALSE)  
    llMasmovilTenant = (IF lcTenant = {&TENANT_MASMOVIL} THEN TRUE ELSE FALSE).

liError = fCheckOrderer(piOrderer, llYoigoCLI, llMasmovilCLI, ocResult).            
IF liError NE 0 THEN DO:
   RETURN appl_err(ocResult).
END.

liError = fCheckBillingPermission(piMsSeq, output ocResult).
IF liError EQ 1 THEN DO:
   RETURN appl_err(ocResult).
END.

fInitialiseValues(
   INPUT piOrderer,
   INPUT llYoigoCLi,
   INPUT llMasmovilCLI,
   OUTPUT piMsisdnStat,
   OUTPUT piSimStat,
   OUTPUT piQuarTime).

IF piOrderer = 2 THEN DO:
   piOpCode = get_int(pcTermStruct, "opcode").
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   liError = fCheckOpCode(piOpCode, OUTPUT ocResult).
   IF liError NE 0 THEN DO:
      RETURN appl_err(ocResult).
   END.
END.

/* optional params */

/* Validate optional parameters */
IF piOrderer EQ 5 THEN DO:
   
   IF LOOKUP("simstat", lcTermStruct) GT 0 THEN DO:
      piSimStat   = get_pos_int(pcTermStruct, "simstat").
      liError = fCheckSimStat(piSimStat, OUTPUT ocResult).
      IF liError NE 0 THEN DO:
         RETURN appl_err(ocResult).
      END.
   END.
   
   IF LOOKUP("msisdnstat", lcTermStruct) GT 0 THEN DO:
      IF ((llYoigoCLI AND llYoigoTenant) OR (llMasmovilCLI AND llMasmovilTenant)) THEN DO:
         piMSISDNStat = get_pos_int(pcTermStruct, "msisdnstat").
         liError = fCheckMsisdnStat(piMSISDNStat, OUTPUT ocResult).
         IF liError NE 0 THEN DO:
            RETURN appl_err(ocResult).
         END.
      END.
      ELSE IF get_pos_int(pcTermStruct, "msisdnstat") NE piMsisdnStat THEN DO:
         RETURN appl_err("Incorrect or missing MSISDN status value").
      END.
   END.

   IF LOOKUP("quartime", lcTermStruct) GT 0 THEN DO: 
      IF ((llYoigoCLI AND llYoigoTenant) OR (llMasmovilCLI AND llMasmovilTenant)) AND piMSISDNStat EQ 4 THEN DO:
         piQuarTime  = get_pos_int(pcTermStruct, "quartime").
         IF piQuarTime < 1 OR piQuarTime > 90 THEN DO:
            RETURN appl_err("Value must be between 1 and 90!").
         END.
      END.
      ELSE IF get_pos_int(pcTermStruct, "quartime") NE piQuarTime THEN DO:
         RETURN appl_err("Incorrect or missing quarantine time").
      END.
   END.

   IF gi_xmlrpc_error NE 0 THEN RETURN.

END.

liError = fDeleteMsValidation(piMsSeq, piOrderer, ocResult).
IF liError EQ 3 THEN DO:
   RETURN appl_err("Ongoing termination requests"). 
END.
IF liError NE 0 THEN DO:
   RETURN appl_err(ocResult). 
END.

liError = fCheckKillTS(piOrderer,pdeKillTS, OUTPUT ocResult).
IF liError NE 0 THEN DO:
   RETURN appl_err(ocResult).
END.

llPenalty = fIsPenalty(piOrderer,piMsSeq).

IF piOpCode NE 0 THEN lcOpCode = STRING(piOpCode).
ELSE lcOpCode = "".

liReq = fTerminationRequest(
   piMSSeq,
   pdeKillTS,    /* when request should be handled */
   piMsisdnStat,
   piSimStat,
   piQuarTime,
   INT(llPenalty),
   lcOpCode,
   STRING(piOrderer),
   ({&REQUEST_SOURCE_NEWTON}),
   "",
   0,
   pcTermType,
   OUTPUT ocResult).

IF liReq > 0 THEN DO:

   Func.Common:mTS2Date(pdeKillTS, OUTPUT ldaTermDate).

   fAdditionalLineSTC(liReq,
                      Func.Common:mMake2DT(ldaTermDate + 1, 0),
                      "DELETE").
   
   add_boolean(response_toplevel_id, "", true).
END.
ELSE
   add_boolean(response_toplevel_id, "", false).
   
FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
