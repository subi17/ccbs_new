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

SESSION:NUMERIC-FORMAT = "AMERICAN".

def var j as i no-undo.
def var lcFields as c no-undo.
def var lcDelim as c no-undo.

DEF VAR lcDump  AS CHAR NO-UNDO.

lcDelim = " \"|\" ".

   find first mcdr._file where
              mcdr._file._file-name = "mobcdr"
   no-lock no-error.

   for each mcdr._field of mcdr._file no-lock
   by _field._order: 
      j = j + 1.

      IF _field._data-type = "character" AND _field._extent = 0 THEN 
         lcDump = 'REPLACE(' + 
                  _file._file-name + "." + _field._field-name + 
                  ',CHR(10),\" \")'.
      ELSE lcDump = _file._file-name + "." + _field._field-name.   
      
      if j = 1 then lcFields = lcDump.
      else lcFields = lcFields + lcDelim + lcDump. 
   end.

   RUN /apps/snet/200709/aam_yoi414.i lcFields.