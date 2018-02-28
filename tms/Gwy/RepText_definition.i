/* Gwy/RepText_definition.i
*/

DEF TEMP-TABLE ttRepText SERIALIZE-NAME "commercial_name"
   FIELD parent_id AS RECID
   FIELD language AS CHAR
   FIELD comm_name AS CHAR
   FIELD valid_from AS CHAR
   FIELD valid_to AS CHAR
   FIELD rt_validfrom AS DATE
   FIELD rt_validto AS DATE
   FIELD rt_language AS INT.
 