/** 
 * RPC to return list of all InvText EMAILs.
 *
 * @input empty;
 * @output keyvalue_st;array;containing list of all emails
 * @keyvalue_st keyvalue;string;Unique value for identifying
                process;string;process name
                emails_st;array;array of all translations
 * @emails_st description;string;title of EMAIL
              language;int;translation of EMAIL
              emailtext;string;content of EMAIL
*/

{xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".


DEFINE VARIABLE resp_array AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcResultStruct AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcEmailArray AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLangItemStruct AS CHARACTER NO-UNDO.


resp_array = add_array(response_toplevel_id, "").

FOR EACH InvText NO-LOCK WHERE
         InvText.Brand     = gcBrand AND
         InvText.Target    = "EMAIL" AND
         InvText.ToDate   >= TODAY AND
         InvText.FromDate <= TODAY BREAK BY InvText.KeyValue:

      IF FIRST-OF(InvText.KeyValue) THEN DO:
         lcResultStruct = add_struct(resp_array, "").
         add_string(lcResultStruct, "keyvalue", InvText.KeyValue).
         add_string(lcResultStruct, "process", InvText.Category).
         lcEmailArray = add_array(lcResultStruct, "langitems").
      END.
      lcLangItemStruct = add_struct(lcEmailArray, "").
      add_string(lcLangItemStruct, "description", InvText.TxtTitle).
      add_int(lcLangItemStruct, "language", InvText.Language).
      add_string(lcLangItemStruct, "emailtext", InvText.InvText).
END.
FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.

