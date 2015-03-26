/* PROCUDT INFORMATION FOR PaymFile PRINTING */

{Country.i}

FUNCTION fBillCodeCode RETURNS CHAR
   (INPUT BillCode AS CHAR).

   def var ret as c no-undo init "-?-".

   case {&Country}:

      when 1 THEN DO:
         case BillCode:
            when "internat"  then ret = "U".
            when "freephone" then ret = "F".
            when "intfree"   then ret = "FI".
            when "mobile"    then ret = "M".
            when "merlin"    then ret = "RB".
         END.
      END.

      when 2 THEN DO:
         case BillCode:
            when "internat"  then ret = "U".
            when "freephone" then ret = "G".
            when "mobile"    then ret = "M".
         END.
      END.

      when 3 THEN DO:
         case BillCode:
            when "internat"  then ret = "I".
            when "freephone" then ret = "F".
            when "mobile"    then ret = "NM".
         END.
      END.

   END.

   RETURN ret.

END.

