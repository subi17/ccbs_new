{test_xmlrpc_includes.i}
{unit/checkutils.i}

gcFixtures = "CustCat". 

DEFINE VARIABLE gcRetStruct   AS CHARACTER NO-UNDO.


FUNCTION fCallCustomerCategoriesGet RETURN LOGICAL:
   run_rpc_method("newton.customer_categories__get").
   RETURN TRUE.
END.



FUNCTION fCheckCustCat RETURN LOGICAL 
  (INPUT pcFixName AS CHARACTER, INPUT pcRetStruct AS CHARACTER):
   fetch_fixture(pcFixName, BUFFER CustCat:HANDLE).
   assert(get_paramcount(pcRetStruct) EQ 4, 
      "There was not exactly 3 fields returned in category struct").
   checkChar("CustCat.Category", CustCat.CateGory, 
      get_string(pcRetStruct, "id")).
   checkChar("CustCat.CatName", CustCat.CatName,
      get_string(pcRetStruct, "name")).
   checkInt("CustCat.MobSubLimit", CustCat.MobSubLimit, 
      get_int(pcRetStruct, "limit")).

   RETURN TRUE.
END.




PROCEDURE test_get_existing_category:
    def var cArray as char no-undo.
    def var cGetArray as char no-undo.

    cArray = add_array("", "").
    add_string(cArray, "", "cat1").
    fCallCustomerCategoriesGet().
    assert_success().
    assert(get_paramcount("") EQ 1, 
       "There was not exactly one array in the root of the return").
    cGetArray = get_array("", "").

    gcRetStruct = get_struct(cGetArray, "0").
    fCheckCustCat("CustCat1", gcRetStruct).
END.


PROCEDURE test_get_not_existing_category:
    def var cArray as char no-undo.
    def var cGetArray as char no-undo.

    cArray = add_array("", "").
    add_string(cArray, "", "cat6").
    fCallCustomerCategoriesGet().
    assert_fault({&APPLICATION_ERROR}, 
        "Category was not found with category cat6").
END.
