/**
 * Get FAT scheme ids.
 *
 * @input conditions;struct;mandatory;empty
 * @output struct;array of fat scheme ids
*/

{newton/src/flistrpc.i}

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH FatGroup NO-LOCK WHERE FatGroup.Brand = "1"'.

fListQuery(
   "FatGroup",
   lcQuery,
   "FtGrp").
