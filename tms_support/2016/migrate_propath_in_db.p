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


DEFINE VARIABLE liLoop AS INTEGER NO-UNDO.
lcEntry = "".
FOR EACH MsReqStatFunc NO-LOCK:

   DO liLoop = 2 TO NUM-ENTRIES(MsReqStatFunc.FuncGroup,","):
      FIND MsReqFuncItem EXCLUSIVE-LOCK WHERE
           MsReqFuncItem.ItemId = ENTRY(liLoop,MsReqStatFunc.FuncGroup,",")
      NO-ERROR.

      IF AVAILABLE MsReqFuncItem
      THEN DO:         
         {_migrate_propath_in_db.i 
         MsReqFuncItem.Module lcEntry}
      END.
   END.

END.

FIND FIRST TMSParam EXCLUSIVE-LOCK WHERE 
   TMSParam.Brand = "1" AND 
   TMSParam.ParamCode  =  "TCPModule"
NO-ERROR.

IF AVAIL TMSParam
THEN ASSIGN
        TMSParam.CharVal = REPLACE(TMSParam.CharVal,"Gwy/","")
        TMSParam.CharVal = REPLACE(TMSParam.CharVal,".p","")
        TMSParam.CharVal = "Gwy/" + TMSParam.CharVal + ".p"
        .

FOR EACH TMSCodes EXCLUSIVE-LOCK WHERE
         TMSCodes.CodeGroup = "UI_Maint" AND
         TMSCodes.CodeValue = "Top":

   /* Module provmaint */
   ASSIGN
      TMSCodes.FieldName = REPLACE(TMSCodes.FieldName,"Mc/","")
      TMSCodes.FieldName = REPLACE(TMSCodes.FieldName,".p","")
      TMSCodes.FieldName = "Mc/" + TMSCodes.FieldName + ".p"
      .
END.