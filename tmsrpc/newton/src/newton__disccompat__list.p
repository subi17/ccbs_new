/**
 * Get discount compatibility list
 *
 * @input conditions;struct;mandatory;brand
 * @output struct;array of TMSRelation ids
*/

{newton/src/flistrpc.i}

DEF VAR pcBrand AS CHARACTER NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!").

pcBrand = get_string(pcStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

DO ON ERROR UNDO, THROW:
   
   multitenancy.TenantInformation:mSetEffectiveBrand(pcBrand).

   CATCH errorobj AS Progress.Lang.AppError:
      RETURN appl_err(errorobj:GetMessage(1)).
   END.
END.

DEF VAR lcQuery AS CHARACTER NO-UNDO.

lcQuery = SUBSTITUTE('FOR EACH &1 NO-LOCK WHERE ' +
                              '&1.TableName = "DiscountPlan"  AND ' +
                              '&1.KeyType   = "Compatibility" AND ' +
                              '&1.ToTime   >= &2',
                     'TMSRelation',
                     NOW).

fMakeListUsingQuery(NO, "TMSRelation",lcQuery,"TMSRelationID").