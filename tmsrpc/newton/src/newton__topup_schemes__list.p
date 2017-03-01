/**
 * Get topup scheme ids.
 *
 * @input conditions;struct;mandatory;empty
 * @output struct;array of topup scheme ids
*/

{newton/src/flistrpc.i}

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH TopUpScheme NO-LOCK WHERE TopUpScheme.Brand = "1"'.

fListQuery(
   "TopUpScheme",
   lcQuery,
   "TopupScheme").
