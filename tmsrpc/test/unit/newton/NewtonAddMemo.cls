/*
   Depends = newton/src/newton__add_memo.p,Func/timestamp.i,fcgi_agent/xmlrpc/xmlrpc_access.i
*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS unit.newton.NewtonAddMemo INHERITS gearbox.unit.ProcTmsrpcTest:

   METHOD PUBLIC OVERRIDE CHAR getInstanceName():
      RETURN "newton".
   END METHOD.

   METHOD PUBLIC VOID testMemoCreate ():

      addString("Reference", "refnum").
      addString("Type", "type").
      addInt("Custnum", 1).
      addString("Creator", "Bob").
      addString("Title", "Title").
      addString("Content", "Content").
      addInt("Priority", 1).

      runRpcMethod("newton.add_memo").

      assertSuccess().

      FIND FIRST Memo NO-LOCK NO-ERROR.

      assert(AVAILABLE Memo, "Memo is not created").

   END METHOD.

END CLASS.
