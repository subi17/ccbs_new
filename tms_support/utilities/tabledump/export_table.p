def stream sout.
output stream sout to value("{2}/{1}.d").

def var i as int no-undo.

i = 0 .

disp 'Exporting {1}' with frame a.
pause 0.
for each {1} no-lock:
   export stream sout {1}.
end.

output stream sout close.
