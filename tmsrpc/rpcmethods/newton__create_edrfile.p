/**
 * RPC for creating test EDRs
 * Authors: ilkkasav & kariaika.
 *
 * @input   lcCLI;char;mandatory;msisdn for the own subscriber
            lcEDRFile;char;mandatory;base file for EDR data
            ldaDateTime;datetime;mandatory;Date&time for generated data
 * @output success;boolean
 */
{xmlrpc/xmlrpc_access.i}
{Mnp/mnpoutchk.i}
{Syst/commpaa.i}
katun = "Newton".
gcBrand = "1".

/*Input parameters*/
DEFINE VARIABLE pcStruct AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcEDRFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldtDateTime AS DATETIME NO-UNDO.


/*Local variables*/
DEF VAR lcRootDir AS CHAR NO-UNDO.
DEF VAR lcRateDir AS CHAR NO-UNDO.
DEF VAR lcEDRDir AS CHAR NO-UNDO.
DEF VAR lcImsi AS CHAR NO-UNDO.

/*FILE-INFO:FILE-NAME = ".".
lcRootDir = FILE-INFO:FULL-PATHNAME + "/".
lcRootdir = REPLACE (lcRootDir,"tmsrpc/","yoigo/tms_support/testing/").*/


/*Search correct directory*/
lcRootDir = SEARCH("testing/donotremove_testdir.txt").
lcRootDir = REPLACE(lcrootDir, "donotremove_testdir.txt", "").
lcRateDir = "Rate/".
/*lcRootDir = "/apps/yoigo/tms_support/testing/".*/

IF validate_request(param_toplevel_id, "string,string,datetime") EQ ? THEN RETURN.

lcCLI       = get_string(param_toplevel_id, "0").
lcEDRFile   = get_string(param_toplevel_id, "1").
ldtDateTime = get_datetime(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

/*Check that customer exists*/
FIND mobsub NO-LOCK
WHERE mobsub.brand = "1" and 
      MobSub.CLI = lcCLI NO-ERROR.
IF NOT AVAILABLE mobsub THEN
   RETURN appl_err(SUBST("MobSub entry &1 not found", lcCLI)).

ASSIGN
lcImsi = mobsub.imsi.

lcEDRDir = lcRootDir + "edrfiles/".
/*Generate EDR files*/
RUN "testing/create_edrfile.p" (
      lcCli,
      ldtDateTime,
      (lcEDRDir + lcEDRFile),
      (lcRootDir + "edrfiles/temp/" + lcEDRFile + ".test")).


/*Insert generated EDRs to TMS*/
RUN VALUE(lcRateDir + "edr_reader.p")(
     lcRootDir + "edrfiles/temp/" + lcEDRFile + ".test",
     0).


add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
