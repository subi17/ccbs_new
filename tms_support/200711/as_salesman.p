def stream sread.
input stream sread from /apps/snet/200711/users.txt.
def stream slog.
output stream slog to /apps/snet/200711/as_salesman.log.
def var lcline    as char no-undo.


DEFINE VARIABLE lcSalesman AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcName AS CHARACTER NO-UNDO.

repeat:
   import stream sread unformatted lcline.
    
    lcSalesman = trim(entry(2,lcLine,chr(9))).
    lcName = trim(entry(3,lcLine,chr(9))) + " " + trim(entry(4,lcLine,chr(9))).
    
    if num-entries(lcLine,chr(9)) = 5 then
    lcName = " " + trim(entry(5,lcLine,chr(9))).
    
    put stream slog unformatted
      "salesman: "  lcSalesman  skip
      "name: "      lcName.

    if num-entries(lcLine,chr(9)) = 5 then
    put stream slog  " " + entry(5,lcLine,chr(9)).
   
   put stream slog skip(2).

   create salesman.
   assign
      salesman.brand    = "1"
      salesman.salesman = lcSalesman
      salesman.smname   = lcName.

end.

input stream sread close.
output stream slog close.
