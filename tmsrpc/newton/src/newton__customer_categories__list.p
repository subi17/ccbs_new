/**
 * Get customer categories.
 *
 * @input struct;no contents
 * @output struct;array of customer categories
*/

{newton/src/flistrpc.i}

DEF VAR lcQuery  AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH CustCat NO-LOCK WHERE CustCat.Brand = "1"'.

fListQuery(
   "CustCat",
   lcQuery,
   "Category").
