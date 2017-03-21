/* ----------------------------------------------------------------------
  Module .......: tmsrpc/rpcmethods/newton__add_reactivation_request.p
  Task .........: Subscription Reactivation Request
  Application ..: RPCMETHOD
  Author .......: Vikas
  Created ......: 03.10.11
  Version ......: Yoigo

  Input ........: 1) reacdata;struct;mandatory;contains input data
                    a) String;mandatory;person who requests the reactivation (salesman)
                    b) Int;mandatory;MsSeq of TermMobSub (Subscription Id)
                    c) Timestamp;optional;when request to be handled (Action DateTime Stamp)
                  2) struct;mandatory;memo
                    a) title;string;mandatory
                    b) content;string;mandatory
  Output .......: success;boolean
---------------------------------------------------------------------- */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Func/cparam2.i}
{Func/fcreatereq.i}
{Mnp/mnp.i}
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/freacmobsub.i}

DEFINE VARIABLE liMsSeq           AS INTEGER     NO-UNDO.
DEFINE VARIABLE ldActStamp        AS DECIMAL     NO-UNDO.

DEFINE VARIABLE liMsReq           AS INTEGER     NO-UNDO.
DEFINE VARIABLE lcResult          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pcReacStruct      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pcMemoStruct      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcStruct          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcMemoTitle       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcMemoContent     AS CHARACTER   NO-UNDO.

DEFINE BUFFER lbMobSub            FOR MobSub.
DEFINE BUFFER bTermMobSub         FOR TermMobSub.

IF validate_request(param_toplevel_id, "struct,struct") EQ ? THEN RETURN.

pcReacStruct = get_struct(param_toplevel_id, "0").
pcMemoStruct = get_struct(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcReacStruct,"salesman!,msseq!,ActStamp").
IF lcStruct EQ ? THEN RETURN.

lcStruct = validate_request(pcMemoStruct,"title!,content!").
IF lcStruct EQ ? THEN RETURN.

/* Required Params */
liMsSeq  = get_pos_int(pcReacStruct, "msseq").
katun    = "VISTA_" + get_string(pcReacStruct, "salesman").

lcMemoTitle = get_string(pcMemoStruct, "title").
lcMemoContent = get_string(pcMemoStruct, "content").

IF TRIM(katun) EQ "VISTA_" THEN
   RETURN appl_err("username is empty").

IF LOOKUP("ActStamp", pcReacStruct) GT 0 THEN
   ldActStamp = get_timestamp(pcReacStruct, "ActStamp").

/* Set Reactivation time */
IF ldActStamp = 0 OR ldActStamp = ? THEN
   ldActStamp = fMakeTS().

{newton/src/findtenant.i NO ordercanal TermMobSub MsSeq liMsSeq}

lcResult = freacprecheck(INPUT liMsSeq, INPUT katun, INPUT FALSE).
IF lcResult > "" THEN DO:
   lcResult = REPLACE(lcResult, CHR(10), " ").
   RETURN appl_err(lcResult).
END. /* IF lcResult > "" THEN DO: */

FIND FIRST bTermMobSub WHERE
           bTermMobSub.MsSeq = liMsSeq NO-LOCK NO-ERROR.
IF AVAIL bTermMobSub AND bTermMobSub.MultiSIMId > 0 AND
   bTermMobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} THEN DO:
   FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
              lbMobSub.Brand  = gcBrand AND
              lbMobSub.MultiSimID = bTermMobSub.MultiSimID AND
              lbMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
              lbMobSub.Custnum = bTermMobSub.Custnum NO-ERROR.
   IF NOT AVAIL lbMobSub THEN
      RETURN appl_err("Reactivation is not allowed since primary line " +
                      "of multi SIM is not active").
END. /* IF AVAIL bTermMobSub THEN DO: */

/* Create Reactivation Request */
liMsReq = fReactivationRequest(INPUT liMsSeq,
                               INPUT 0,
                               INPUT ldActStamp,
                               INPUT katun,
                               INPUT {&REQUEST_SOURCE_NEWTON},
                               OUTPUT lcResult).
IF liMsReq > 0 THEN
   add_boolean(response_toplevel_id, "", true).
ELSE
   RETURN appl_err(lcResult).

IF lcMemoTitle > "" AND liMsReq > 0 THEN DO:
   FIND FIRST TermMobSub WHERE TermMobSub.MsSeq = liMsSeq NO-LOCK NO-ERROR.
   CREATE Memo.
   ASSIGN
       Memo.CreStamp  = {&nowTS}
       Memo.Brand     = gcBrand
       Memo.HostTable = "MobSub"
       Memo.KeyValue  = STRING(liMsSeq)
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = katun
       Memo.MemoTitle = lcMemoTitle
       Memo.MemoText  = lcMemoContent
       Memo.CustNum   = (IF AVAILABLE TermMobSub THEN TermMobSub.CustNum ELSE 0).
END. /* IF lcMemoTitle > "" THEN DO: */

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.


