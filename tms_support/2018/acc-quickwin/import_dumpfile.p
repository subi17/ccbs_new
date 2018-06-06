def stream sin.
def stream sin2.
def stream sin3.

DEF VAR lcSourceFolder AS CHAR NO-UNDO. 

lcSourceFolder = "../tms_support/2018/acc-quickwin/".

input stream sin from value(lcSourceFolder + "dumpfile.d").
input stream sin2 from value(lcSourceFolder + "dumphpd.d").
input stream sin3 from value(lcSourceFolder + "dffield.d").

DEF TEMP-TABLE ttDumpfile LIKE DumpFile.
DEF TEMP-TABLE ttDumpHPD LIKE DumpHPD.
DEF TEMP-TABLE ttDFFIeld LIKE dffield.

do trans:
   repeat :
      create ttDumpfile.
      import stream sin ttDumpfile.
   end.
   repeat :
      create ttDumpHPD.
      import stream sin2 ttDumpHPD.
   end.
   repeat :
      create ttDFFIeld.
      import stream sin3 ttdffield.
   end.
end.

DEF VAR i AS INT NO-UNDO. 
FIND LAST DumpFile USE-INDEX DumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile
THEN i = DumpFile.DumpID + 1.
ELSE i = 1.

do trans:
FOR EACH ttdumpfile:
   
   disp "Importing " dumpfile.dumpname.

   FOR EACH ttdumphpd where
            ttdumphpd.dumpid = ttdumpfile.dumpid :
       ttdumphpd.dumpid = i.
       create dumphpd.
       buffer-copy ttdumphpd to dumphpd.
   end.
   
   FOR EACH ttdffield where
            ttdffield.dumpid = ttdumpfile.dumpid :
       ttdffield.dumpid = i.
       create dffield.
       buffer-copy ttdffield to dffield.
   end.

   create dumpfile.
   assign dumpfile.dumpid = i.
   buffer-copy ttdumpfile except ttdumpfile.dumpid to dumpfile.

   i = i + 1.
end.
end.

