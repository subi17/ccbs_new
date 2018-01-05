/**
 * Get bundle details.
 *
 * @input  id;array of string;mandatory; tmsrelation ids
 * @output compat;array of struct;discount compatibility data
 * @compat        id;string;bundle id
                  old_value;string;existing discountplan id value
                  new_value;string;new discountplan id value
                  role;string;the dominant discountplan id
                  validfrom;date;valid from date
                  validto;date;valid to date
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/fprepaidfee.i}

DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR pcId AS CHAR NO-UNDO. 
DEF VAR pcIdArray AS CHAR NO-UNDO. 
DEF VAR liCounter AS INTEGER NO-UNDO. 
DEFINE VARIABLE resp_array AS CHARACTER NO-UNDO.

DEFINE VARIABLE liParamCount   AS INTEGER   NO-UNDO.
DEFINE VARIABLE pcBrand        AS CHARACTER NO-UNDO.
DEFINE VARIABLE objTMSRelation AS CLASS Syst.TMSRelation     NO-UNDO.
DEFINE VARIABLE objObject      AS CLASS Progress.Lang.Object NO-UNDO.

IF validate_request(param_toplevel_id, "string,array") = ? THEN RETURN.

pcBrand   = get_string(param_toplevel_id, "0").
pcIDArray = get_array(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

DO ON ERROR UNDO, THROW:
   
   multitenancy.TenantInformation:mSetEffectiveBrand(pcBrand).

   CATCH errorobj AS Progress.Lang.AppError:
      RETURN appl_err(errorobj:GetMessage(1)).
   END.
END.

resp_array = add_array(response_toplevel_id, "").

liParamCount = get_paramcount(pcIDArray).

objTMSRelation = NEW Syst.TMSRelation("DiscountPlan", "Compatibility").

objTMSRelation:mSetSerializeNames("id",
                                  "old_value",
                                  "new_value",
                                  "role",
                                  "validfrom",
                                  "validto",
                                  NO).

/* We could call 
   objTMSRelation:mSetTimeRange(NOW, NOW)
   To get the data currently active but this is the default so no need
*/

/* Lets populate currently active "DiscountPlan" and "Compatibility" data */  
objTMSRelation:mPopulateData().

/* If the counts doesn't match we need to mark the data which were available */
IF liParamCount NE objTMSRelation:RecordCount
THEN DO:
   /* There aren't the same data, we need to mark the missing data */
   DO liCounter = 0 TO liParamCount - 1:

      pcID = get_string(pcIDArray, STRING(liCounter)).

      IF gi_xmlrpc_error NE 0 THEN RETURN.

      DO ON ERROR UNDO, THROW:
         
         objTMSRelation:mMark(INT64(pcID)).
      
         CATCH errobj AS Progress.Lang.ProError:
            RETURN appl_err(errobj:GetMessage(1)).
         END.

      END.
   END.
END.

/* NOTE: If we get same amount of id's then we assume that we can return
   the data without doing any checking (this is potentially dangerous
   but in this case this should be the case as there should be always
   the same list here what newton__disccompat_list sent earlier. */

/* Serialize the data available */
objObject = objTMSRelation:mSerialize().

add_json_as_object(resp_array, "", objObject).

FINALLY:
   IF VALID-OBJECT(objObject)
   THEN DELETE OBJECT objObject.
    
   IF VALID-OBJECT(objTMSRelation)
   THEN DELETE OBJECT objTMSRelation.
END FINALLY.