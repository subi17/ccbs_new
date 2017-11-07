/**
 * Change segmentation according to MSISDN.
 *
 * @input  msisdn;string;mandatory;user msisdn
           segmentation;string;mandatory;selected segmentation (OFA-OFF)
 * @output status;int;Change status (0=failure, 1=Success)
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Func/tmsparam4.i}
{Syst/commpaa.i}
Syst.Var:katun = "Newton".
Syst.Var:gcBrand = "1".
{Mc/provmaint.i}

DEF VAR pcTenant       AS CHAR NO-UNDO.
DEF VAR pcMsisdn       AS CHAR NO-UNDO. 
DEF VAR pcSegmentation AS CHAR NO-UNDO. 
DEF VAR piStatus       AS INT  NO-UNDO INIT 0. 

/* validate 1st and 2nd parameter */
IF validate_request(param_toplevel_id, "string,string,string") EQ ? THEN RETURN.
pcTenant = get_string(param_toplevel_id, "0").
pcMsisdn = get_string(param_toplevel_id, "1").
pcSegmentation = get_string(param_toplevel_id, "2").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF NOT( pcSegmentation = "OFA" OR 
        pcSegmentation = "OFB" OR
        pcSegmentation = "OFC" OR
        pcSegmentation = "OFD" OR
        pcSegmentation = "OFE" OR
        pcSegmentation = "OFF" ) THEN DO:
   RETURN appl_err(SUBST("Incorrect segmentation value &1", pcSegmentation)).
END.

{newton/src/settenant.i pcTenant}

FIND FIRST MobSub NO-LOCK WHERE
           MobSub.Brand = Syst.Var:gcBrand AND
           MobSub.CLI   = pcMsisdn NO-WAIT NO-ERROR. /* Find with MSISDN */
IF NOT AVAILABLE MobSub THEN
   RETURN appl_err(SUBST("MSISDN not found &1", pcMsisdn)).

   /* parameter validation ends*/
FIND FIRST Segmentation EXCLUSIVE-LOCK WHERE
           Segmentation.MsSeq = MobSub.MsSeq.
IF NOT AVAILABLE Segmentation THEN
   RETURN appl_err(SUBST("Segmentation not found &1", pcMsisdn)).

   /* Change the segmentation offer */
Assign  Segmentation.SegmentOffer = pcSegmentation
        piStatus = 1.
RELEASE Segmentation.

   /*  Response status  */
add_int(response_toplevel_id, "",  piStatus). 

FINALLY:
   END.
