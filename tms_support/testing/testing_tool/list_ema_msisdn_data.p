/* ----------------------------------------------------------------------
  Module .......: list_ema_msisdn_data.p
  Task .........: List EMA MSISDN data with selected record field
  Application ..: TMS
  Author .......: Pasi Hautaniemi
  Created ......: 26.05.2015
  Version ......: Yoigo
---------------------------------------------------------------------- */

{timestamp.i}

DEF VAR lcFieldSelection AS CHAR NO-UNDO FORMAT "X(10)".
DEF VAR lcFieldvalue     AS CHAR NO-UNDO FORMAT "X(10)".
DEF VAR liFieldvalue     AS INT NO-UNDO.

MAIN_LOOP:
DO WHILE TRUE
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN LEAVE.
   lcFieldSelection = "".
   lcFieldvalue     = "".

FORM
   SKIP
   " List all with enter or                             " SKIP
   " give MSISDN record field and value to search.      " SKIP
   " CLI, CustNum, OrderId, StatusCode " lcFieldSelection SKIP
   " Enter value                       " lcFieldvalue     SKIP
   WITH OVERLAY CENTERED ROW 5 TITLE " List all EMA MSISDN " NO-LABELS
   FRAME fclean.

UPDATE lcFieldSelection lcFieldvalue with FRAME fclean.

   CASE lcFieldSelection:
      WHEN "" THEN DO:
         FOR EACH MSISDN NO-LOCK WHERE
          ((MSISDN.CLI >= "633993500" AND MSISDN.CLI <= "633993620") OR
           (MSISDN.CLI >= "633993700" AND MSISDN.CLI <= "633993750")).
            DISP MSISDN.CLI CustNum OrderId ValidTo StatusCode.
         END.
      END.
      WHEN "CLI" THEN DO:
         FOR EACH MSISDN NO-LOCK WHERE
          ((MSISDN.CLI >= "633993500" AND MSISDN.CLI <= "633993620") OR
           (MSISDN.CLI >= "633993700" AND MSISDN.CLI <= "633993750")) AND
            MSISDN.CLI = lcFieldvalue.
            DISP MSISDN.CLI CustNum OrderId ValidTo StatusCode.
         END.
      END.
      WHEN "CustNum" THEN DO:
         liFieldvalue = INT(lcFieldvalue) NO-ERROR.
         FOR EACH MSISDN NO-LOCK WHERE
          ((MSISDN.CLI >= "633993500" AND MSISDN.CLI <= "633993620") OR
           (MSISDN.CLI >= "633993700" AND MSISDN.CLI <= "633993750")) AND
            MSISDN.CustNum = liFieldvalue.
            DISP MSISDN.CLI CustNum OrderId ValidTo StatusCode.
         END.
      END.
      WHEN "OrderId" THEN DO:
         liFieldvalue = INT(lcFieldvalue) NO-ERROR.
         FOR EACH MSISDN NO-LOCK WHERE
          ((MSISDN.CLI >= "633993500" AND MSISDN.CLI <= "633993620") OR
           (MSISDN.CLI >= "633993700" AND MSISDN.CLI <= "633993750")) AND
            MSISDN.OrderId = liFieldvalue.
            DISP MSISDN.CLI CustNum OrderId ValidTo StatusCode.
         END.
      END.
      WHEN "StatusCode" THEN DO:
         liFieldvalue = INT(lcFieldvalue) NO-ERROR.
         FOR EACH MSISDN NO-LOCK WHERE
          ((MSISDN.CLI >= "633993500" AND MSISDN.CLI <= "633993620") OR
           (MSISDN.CLI >= "633993700" AND MSISDN.CLI <= "633993750")) AND
            MSISDN.StatusCode = liFieldvalue.
            DISP MSISDN.CLI CustNum OrderId ValidTo StatusCode.
         END.
      END.
   END CASE.
END. /* DO WHILE TRUE: */
