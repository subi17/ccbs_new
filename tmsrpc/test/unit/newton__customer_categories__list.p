{test_xmlrpc_includes.i}
{unit/checkutils.i}

gcFixtures = "CustCat". 

DEFINE VARIABLE gcRetArray    AS CHARACTER NO-UNDO. 

FUNCTION fCallCustomerCategoryList RETURN LOGICAL:
   add_struct("", "").
   run_rpc_method("newton.customer_categories__list").
   RETURN TRUE.
END.


PROCEDURE test_get_categories:
    fCallCustomerCategoryList().
    assert_success().
    gcRetArray = get_array("", "").
    assert(get_paramcount(gcRetArray) = 4, "There was not exactly 4 categories returned").
    checkChar("CustCat.Category, limit5 category", "41", get_string(gcRetArray, "0")).
    checkChar("CustCat.Category, first category", "Cat1", get_string(gcRetArray, "1")).
    checkChar("CustCat.Category, second category", "Cat2", get_string(gcRetArray, "2")).
    checkChar("CustCat.Category, third category", "Cat3", get_string(gcRetArray, "3")).
END.

