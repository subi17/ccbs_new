{Syst/testpaa.i}
katun = "ari".

def var ldadate as date no-undo.
def var lcfile as char no-undo.
def var oidone as int no-undo.
def var olbreak as log no-undo. 

find first dumpfile where dumpfile.dumpid = 21 no-lock.

ldadate = 10/4/9.

run pfilename(ldadate).

run /apps/snet/200910/aam_yot240_dump.p (ldadate,   
                                         dumpfile.dumpid,
                                         lcfile,
                                         "full",
                                         0,
                                         "",
                                         "",
                                         output oidone,
                                         output olbreak).

ldadate = 10/11/9.

run pfilename(ldadate).

run /apps/snet/200910/aam_yot240_dump.p (ldadate,   
                                         dumpfile.dumpid,
                                         lcfile,
                                         "full",
                                         0,
                                         "",
                                         "",
                                         output oidone,
                                         output olbreak).
                                          
procedure pfilename:

   def input parameter idadate as date no-undo.
   
   /* output file name */
   ASSIGN
      lcFile = DumpFile.FileName
      lcFile = REPLACE(lcFile,"#DATE",STRING(YEAR(idadate),"9999") + 
                                      STRING(MONTH(idadate),"99") + 
                                      STRING(DAY(idadate),"99"))
      lcFile = REPLACE(lcFile,"#CAT",DumpFile.FileCategory)
      lcFile = REPLACE(lcFile,"#RUN","")
   
      lcFile = DumpFile.SpoolDir + "/" + lcFile.
 
end procedure.

                                         
