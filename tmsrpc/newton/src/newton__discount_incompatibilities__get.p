/**
 * Get bundle details.
 *
 * @input  brand;string;mandatory;
           id;array of numbers;mandatory; tmsrelation ids
 * @output compat;array of struct;discount compatibility data
 * @compat        id;string;bundle id
                  old_value;string;existing discountplan id value
                  new_value;string;new discountplan id value
                  role;string;the dominant discountplan id
                  validfrom;date;valid from date
                  validto;date;valid to date
 */

/* NOTE! It would be a lot better to implement getall method
         which would return data for one tenant without needing
         to use the list method at all */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".

DEFINE TEMP-TABLE ttBrandId NO-UNDO
   FIELD brand      AS CHARACTER
   FIELD id         AS INT64
   FIELD order      AS INTEGER
   INDEX brand IS PRIMARY UNIQUE brand id.

DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR pcId AS CHAR NO-UNDO. 
DEF VAR pcIdArray AS CHAR NO-UNDO. 
DEF VAR liCounter AS INTEGER NO-UNDO.

DEFINE VARIABLE liParamCount   AS INTEGER   NO-UNDO.
DEFINE VARIABLE pcBrand        AS CHARACTER NO-UNDO.
DEFINE VARIABLE liOrder        AS INTEGER   NO-UNDO.
DEFINE VARIABLE objTMSRelation AS CLASS Syst.TMSRelation     NO-UNDO.

IF validate_request(param_toplevel_id, "array") = ? THEN RETURN.

pcIDArray = get_array(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

liParamCount = get_paramcount(pcIDArray).

objTMSRelation = NEW Syst.TMSRelation("DiscountPlan", "Compatibility").

objTMSRelation:mSetSerializeNames("id",
                                  "old_value",
                                  "new_value",
                                  "rule",
                                  "valid_from",
                                  "valid_to",
                                  NO,
                                  YES).

/* We could call 
   objTMSRelation:mSetTimeRange(NOW, NOW)
   To get the data currently active but this is the default so no need
*/

DO liCounter = 0 TO liParamCount - 1:

   pcID = get_string(pcIDArray, STRING(liCounter)).

   IF gi_xmlrpc_error NE 0 THEN RETURN.

   IF NUM-ENTRIES(pcID,"|") > 1 THEN
       ASSIGN
           pcBrand  = ENTRY(2, pcID, "|")
           pcID     = ENTRY(1, pcID, "|").
   ELSE
       RETURN appl_err("Invalid tenant information").

   CREATE ttBrandId.
   ASSIGN
      liOrder         = liOrder + 1
      ttBrandId.brand = pcBrand
      ttBrandId.id    = INT64(pcID)
      ttBrandId.order = liOrder.
END.

FOR EACH ttBrandId
   BREAK BY ttBrandId.brand:

   IF FIRST-OF(ttBrandId.brand)
   THEN DO ON ERROR UNDO, THROW:
      /* Delete redundant data from the temp-table */
      objTMSRelation:mDeleteNotMarked().

      multitenancy.TenantInformation:mSetEffectiveBrand(ttBrandId.brand).
      /* Lets populate currently active "DiscountPlan" and "Compatibility" data
         for the brand */
      objTMSRelation:mPopulateBrand(ttBrandId.brand).
      CATCH errorobj AS Progress.Lang.AppError:
         RETURN appl_err(errorobj:GetMessage(1)).
      END.
   END.

   DO ON ERROR UNDO, THROW:
      /* Lets mark the data which we got*/
      objTMSRelation:mMark(ttBrandId.id, ttBrandId.order).

      CATCH errobj AS Progress.Lang.ProError:
         RETURN appl_err(errobj:GetMessage(1)).
      END.

   END.
END.

/* Serialize the data available. Note: The fcgi_library will take care
   the deletion of the object which mSerialize creates */
add_json_as_object(response_toplevel_id, "", objTMSRelation:mSerialize()).

FINALLY:
   EMPTY TEMP-TABLE ttBrandId.
   IF VALID-OBJECT(objTMSRelation)
   THEN DELETE OBJECT objTMSRelation.
END FINALLY.