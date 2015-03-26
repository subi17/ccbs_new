/* ----------------------------------------------------------------------
  MODULE .......: dumpall.i
  TASK .........: dump all fields for tables
  APPLICATION ..: tms
  AUTHOR .......: tk
  CREATED ......: 26.05.04
  CHANGED ......: 24.08.07/aam replace line feeds (chr10) with spaces,
                               new delimiter for OrderCustomer
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

def var j as i no-undo.
def var lcFields as c no-undo.
def var lcDelim as c no-undo.

DEF VAR lcDump  AS CHAR NO-UNDO.

lcDelim = " \"|\" ".
if "{2}" = "order" or "{2}" = "msrequest" then lcDelim = " \"\\\":\\\"\" ".

   find first {1}._file where
              {1}._file._file-name = "{2}"
   no-lock no-error.

   for each {1}._field of {1}._file no-lock
   by _field._order: 
      j = j + 1.

      IF _field._data-type = "character" AND _field._extent = 0 THEN 
         lcDump = 'REPLACE(' + _field._field-name + ',CHR(10),\" \")'.
      ELSE lcDump = _field._field-name.   
      
      if j = 1 then lcFields = lcDump.
      else lcFields = lcFields + lcDelim + lcDump. 
   end.

   run dumpall2.i "{2}" lcFields.
