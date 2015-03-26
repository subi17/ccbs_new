/* ageanal.i        20.08.03/aam

   definitions for age analysis
   callers:     nnikaj.p
                ageanal.p


*/


DEF TEMP-TABLE TCustGroup NO-UNDO
   FIELD CustGroup LIKE CustGroup.CustGroup
   INDEX CustGroup CustGroup.

DEF TEMP-TABLE ttCriter NO-UNDO
   FIELD Day11    AS INT 
   FIELD Day12    AS INT 
   FIELD Day21    AS INT 
   FIELD Day22    AS INT 
   FIELD Day31    AS INT 
   FIELD Day32    AS INT 
   FIELD Day41    AS INT 
   FIELD Day42    AS INT 
   FIELD Day51    AS INT 
   FIELD Day52    AS INT 
   FIELD DayOver  AS INT
   FIELD InvGroup AS CHAR
   FIELD ToFile   AS CHAR
   FIELD RepDate  AS DATE
   FIELD OnlySum  AS LOG
   FIELD SortBy   AS INT.

def var valikko as char format "x(30)" no-undo extent 2
   init ["BY CUST. NUMBER","BY CUST. NAME"].
def var jar     as char format "x(30)" no-undo.



