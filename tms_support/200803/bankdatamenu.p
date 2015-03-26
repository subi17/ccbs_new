for first menutree exclusive-lock where
          menutree.level = "161" and
          menutree.position = 3:
     delete menutree.
end.

for each menutree exclusive-lock where
         menutree.level = "1613":
     delete menutree.
end.


def stream sread.
input stream sread from bankdatamenu.d.

repeat:
   create menutree.
   import stream sread menutree.
end.

input stream sread close.


