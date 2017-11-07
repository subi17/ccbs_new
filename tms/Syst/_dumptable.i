
for each {1}._file no-lock where
         {1}._file._hidden = FALSE:

   disp
      {1}._file._file-name format "x(20)" 
      pdbname("dictdb") format "x(40)".
   
   pause 0.

   RUN Syst/_dumpall.i {1}._file._file-name.
   
end.
