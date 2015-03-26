/**
 * Checks if customer has any additional lines if the last main line is removed. Additional line is excluded if it has a pending termination, STC or MNP OUT request. Also checks if the subscription has legacy bonos. Additional line check is done before bono check.
 *
 * @input  int;mandatory;subscription ID (main line msseq)
 * @output string;mandatory;HAS_ADDITIONAL_LINES, OK
 */
{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".
{cparam2.i}
{tmsconst.i}
{main_add_lines.i}

DEF VAR piMsSeq AS INT NO-UNDO. 
DEF VAR llAdditionalLines AS LOG NO-UNDO. 

DEF BUFFER lbMobSub FOR mobsub.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piMsSeq = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND mobsub NO-LOCK WHERE
     mobsub.msseq = piMsSeq NO-ERROR.
IF NOT AVAILABLE mobsub THEN
   RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                  CLIType.Brand = gcBrand AND
                  CLIType.CLIType = MobSub.TariffBundle AND
                  CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN DO:
   MOBSUB_LOOP:
   FOR EACH lbMobSub NO-LOCK WHERE
            lbMobSub.Brand   = gcBrand AND
            lbMobSub.InvCust = Mobsub.CustNum AND
            lbMobSub.Paytype = FALSE,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = gcBrand AND
            CLIType.CLIType = (IF lbMobSub.TariffBundle > ""
                               THEN lbMobSub.TariffBundle
                               ELSE lbMobSub.CLIType) AND
            CLIType.LineType > 0:

      IF lbMobSub.MsSeq = Mobsub.MsSeq THEN NEXT.
      
      IF fHasPendingRequests(
         lbMobSub.MsSeq,
         lbMobSub.CLI,
         CLIType.LineType) THEN NEXT.

      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} THEN DO:
         llAdditionalLines = FALSE.
         LEAVE MOBSUB_LOOP.
      END.

      llAdditionalLines = TRUE.
   END.
END.

IF llAdditionalLines THEN
   add_string(response_toplevel_id, "", "HAS_ADDITIONAL_LINES").
ELSE 
   add_string(response_toplevel_id, "", "OK").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
