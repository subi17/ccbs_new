ROUTINE-LEVEL ON ERROR UNDO, THROW.

INTERFACE gearbox.daemons.Monitor:

   METHOD PUBLIC VOID sendHeartbeat ().

   METHOD PUBLIC VOID updateStats ( iExecutionTimeMilliSeconds AS INT,
                                    iBatchIterations AS INT64,
                                    iTotalIterations AS INT64 ).

END INTERFACE.
