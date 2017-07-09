def stream sout.
output stream sout to yts_10755.log.

FOR EACH clitype EXCLUSIVE-LOCK where
         clitype.linetype > 0 and
         clitype.FixedLineDownload > "":
   put stream sout unformatted
      clitype.clitype ";"
      clitype.linetype skip
   clitype.linetype = 0.

end.

