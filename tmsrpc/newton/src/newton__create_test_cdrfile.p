/**
 * RPC for creating test CDRs
 * Authors: ilkkasav & kariaika.
 *
 * @input   lcCLI;char;mandatory;msisdn for the own subscriber
            lcSecCLI;char;mandatory;Call destination number (B number)
            lcCDRFile;char;mandatory;base file for CDR data
            ldaDateTime;datetime;mandatory;Date&time for generated data
            liCount;int;mandatory;number of CDRs
            liMeas;int;mandatory;Duration or data amount
 * @output success;boolean
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:katun = "Newton".
Syst.Var:gcBrand = "1".

/*New input parameters*/
DEFINE VARIABLE pcStruct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcTenant    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLI       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSecCLI    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCDRFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldtDateTime AS DATETIME NO-UNDO.
DEFINE VARIABLE lcType      AS CHAR NO-UNDO INIT "Voice".

/*TEMP ENDS*/

/*DEFINE VARIABLE lcType AS CHAR NO-UNDO./* INIT "Voice".*/ */
DEFINE VARIABLE liCount AS INT NO-UNDO.
DEFINE VARIABLE liMeas AS INT NO-UNDO.

/*Local variables*/
DEF VAR lcRootDir AS CHAR NO-UNDO.
DEF VAR lcRateDir AS CHAR NO-UNDO.
DEF VAR lcCDRDir AS CHAR NO-UNDO.
DEF VAR lcImsi AS CHAR NO-UNDO.
DEF VAR lcc AS CHAR NO-UNDO.
DEF VAR lcError AS CHARACTER NO-UNDO. 

/*Search correct directory*/
lcRootDir = SEARCH("testing/donotremove_testdir.txt").
lcRootDir = REPLACE(lcrootDir, "donotremove_testdir.txt", "").
lcRateDir = "Rate/".

IF validate_request(param_toplevel_id, "string,string,string,string,datetime,int,int") EQ ? THEN RETURN.

pcTenant    = get_string(param_toplevel_id, "0").
lcCLI       = get_string(param_toplevel_id, "1").
lcSecCLI    = get_string(param_toplevel_id, "2").
lcCDRFile   = get_string(param_toplevel_id, "3").
ldtDateTime = get_datetime(param_toplevel_id, "4").
liCount     = get_int(param_toplevel_id, "5").
liMeas      = get_int(param_toplevel_id, "6").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

/*Check that customer exists*/
IF lcCli BEGINS "8" OR 
   lcCli BEGINS "9" THEN DO:
   FIND mobsub NO-LOCK
   WHERE mobsub.brand = "1" and
         MobSub.FixedNumber = lcCLI NO-ERROR.
   IF NOT AVAILABLE mobsub THEN
      RETURN appl_err(SUBST("MobSub entry &1 not found", lcCLI)).
END.
ELSE DO:
   FIND mobsub NO-LOCK
   WHERE mobsub.brand = "1" and 
         MobSub.CLI = lcCLI NO-ERROR.
   IF NOT AVAILABLE mobsub THEN
      RETURN appl_err(SUBST("MobSub entry &1 not found", lcCLI)).
END.
ASSIGN
lcImsi = mobsub.imsi.

lcCDRDir = lcRootDir + "cdrfiles/".
/*Generate CDR files*/
/*RUN VALUE(lcRootDir + "create_test_cdrfile.p")*/
RUN testing/create_test_cdrfile.p (
          lcCli,
          lcSecCLI,
          LCiMSI,
          ldtDateTime,
          liMeas,
          (lcCDRDir + lcCDRFile),
          (lcRootDir + "cdrfiles/temp/" + lcCDRFile + ".test"),
          liCount ).


/*Insert generated CDRs to TMS*/
RUN VALUE(lcRateDir + "onlinereader.p")(
         true,lcRootDir + "cdrfiles/temp/" + lcCDRFile + ".test",
         true,0).


add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   END.
