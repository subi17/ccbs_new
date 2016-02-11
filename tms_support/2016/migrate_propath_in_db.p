DEF VAR lcModule AS CHAR NO-UNDO. 
DEF VAR lcModuleWithPath AS CHAR NO-UNDO. 
DEF VAR lcTMSRoot AS CHAR NO-UNDO.
DEF VAR llSimulate AS LOG NO-UNDO. 
DEF VAR lcEntry AS CHAR NO-UNDO. 

lcTMSRoot = "/apps/xfera/ansavola/tms/".
llSimulate = yes.

def stream sout.
output stream sout to migrate_propath_in_db.txt.

def stream serr.
output stream serr to migrate_propath_in_db_errors.txt.

FOR EACH menutree EXCLUSIVE-LOCK where
   menutree.module > "":
   {_migrate_propath_in_db.i menutree.module lcEntry}  
end.

FOR EACH requesttype EXCLUSIVE-LOCK where
         requesttype.program > "":
   {_migrate_propath_in_db.i requesttype.program lcEntry}  
end.

FOR EACH requeststatus EXCLUSIVE-LOCK where
         requeststatus.program > "":
   {_migrate_propath_in_db.i requeststatus.program lcEntry} 
end.

FOR EACH dumpfile EXCLUSIVE-LOCK where
         dumpfile.fullcollmodule > "":
   {_migrate_propath_in_db.i dumpfile.fullcollmodule lcEntry} 
end.

FOR EACH dumpfile EXCLUSIVE-LOCK where
         dumpfile.ModCollModule > "":
   {_migrate_propath_in_db.i dumpfile.ModCollModule lcEntry} 
end.

FOR EACH dumpfile EXCLUSIVE-LOCK where
         dumpfile.logicmodule > "":
   {_migrate_propath_in_db.i dumpfile.logicmodule lcEntry} 
end.

FOR EACH funcrunconfig EXCLUSIVE-LOCK where
         funcrunconfig.runcommand > "":
    lcEntry = entry(3,funcrunconfig.runcommand,' ').
    {_migrate_propath_in_db.i 
         funcrunconfig.runcommand lcEntry}
end.

FOR EACH orderfunction EXCLUSIVE-LOCK where
  orderfunction.ofmodule  > "":  
    
   if index(orderfunction.ofmodule,",") > 0 then
      lcEntry = entry(1,orderfunction.ofmodule,",").
   else lcEntry = "".
    {_migrate_propath_in_db.i 
         orderfunction.ofmodule lcEntry}
end. 
