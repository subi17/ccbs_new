DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE lhTable AS HANDLE NO-UNDO.
DEFINE VARIABLE lhQuery AS HANDLE NO-UNDO.

CREATE QUERY lhQuery.

def stream sout.
output stream sout to {1}_tablecount.txt.

FOR EACH {1}._File
   WHERE _File-number > 0 AND
   _Owner = "PUB" AND
   _File-num < 32767 NO-LOCK
   BY _File-name:
   CREATE BUFFER lhTable FOR TABLE _File._File-name.
   lhQuery:SET-BUFFERS(lhTable:HANDLE).
   lhQuery:QUERY-PREPARE("FOR EACH " + _File._File-name + " NO-LOCK ").
   lhQuery:QUERY-OPEN.

   i = 0.
   DUMP-LOOP:
   DO WHILE TRUE:
      lhQuery:GET-NEXT(NO-LOCK).
      IF lhQuery:QUERY-OFF-END THEN LEAVE DUMP-LOOP.
      i = i + 1.
      if i > 4000 then leave.
   end.

   disp "{1}" _File._File-name  i.
   pause 0 .
   put stream sout unformatted _File._File-name ":" i skip.
END.

delete object lhTable no-error.
delete object lhQuery no-error.
