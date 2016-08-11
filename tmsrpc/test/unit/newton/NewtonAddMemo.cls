// Depends = rpcmethods/order__create.p

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS unit.tmsrpc.NewtonAddMemo INHERITS gearbox.unit.ProcTmsrpcTest:

   METHOD PUBLIC OVERRIDE CHAR getInstanceName():
      RETURN "newton".
   END METHOD.

   METHOD PUBLIC VOID testForUser ():
      DEF VAR lcStruct AS CHAR NO-UNDO.
      lcStruct = addStruct().
      addString(lcStruct, "Reference", "refnum").
      addString(lcStruct, "Type", "type").
      addInt(lcStruct, "Custnum", 1).
      addString(lcStruct, "Creator", "Bob").
      addString(lcStruct, "Title", "Title").
      addString(lcStruct, "Content", "Content").
      addInt(lcStruct, "Priority", 1).

      runRpcMethod("newton__add_memo").

      FIND FIRST Memo NO-LOCK NO-ERROR.

      assert(AVAILABLE Memo, "Memo is not created").
      
   END METHOD.

END CLASS. 
