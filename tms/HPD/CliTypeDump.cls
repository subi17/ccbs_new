/*------------------------------------------------------------------------
    File        : CliTypeDump
    Purpose     : Dump program for CliType table 
    Syntax      : 
    Description : 
    Author(s)   : diego.pastrana
    Created     : Tue Apr 24 12:15:00 EET 2018
    Notes       : Program created for APIBSS-108
  ----------------------------------------------------------------------*/
  
USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.
USING HPD.DumpMQ.
USING HPD.DumpSomeWhere.

{HPD/HPDConst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.CliTypeDump FINAL INHERITS DumpHandler:
  
   DEFINE PRIVATE BUFFER lbCliType FOR CliType.
  
   CONSTRUCTOR PUBLIC CliTypeDump
      (iiDumpID AS INTEGER):

      lhMainTable = BUFFER lbCliType:HANDLE.
      
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC CliTypeDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      IF icFile = "{&FileIDForMQ}"
      THEN objDumpBase = NEW DumpMQ(iiDumpID, THIS-OBJECT).
      ELSE objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).

      objDumpBase:mUseTable(BUFFER lbCliType:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessFullDump(INPUT CAST(objDumpBase, DumpSomeWhere)).
      
      /* If there has been problems we won't be here... */
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.

   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      DEFINE VARIABLE lcAllowedDSS2SubsType AS CHARACTER NO-UNDO.
      
      CASE icField:
         WHEN "DSS2Compatible"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE DO:
                   FIND FIRST TMSParam where
                              TMSParam.Brand = Syst.Var:gcBrand AND 
                              TMSParam.ParamCode  = "DSS2_SUBS_TYPE"
                        NO-LOCK NO-ERROR.
                   IF AVAIL TMSParam THEN lcAllowedDSS2SubsType = TMSParam.CharVal.
                                     ELSE lcAllowedDSS2SubsType = ?.
                      
                   RETURN STRING(lbCliType.CLIType = "CONT15" OR LOOKUP(lbCliType.CLIType, lcAllowedDSS2SubsType) > 0, "yes/no"). /* CONT15 is always DSS2 compatible */
              END.
              
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.

   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpSomeWhere AS CLASS DumpSomeWhere):

      FOR
         EACH lbCliType NO-LOCK WHERE
            lbCliType.Brand    = Syst.Var:gcBrand
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.

         liEvents = liEvents + 1.

         objDumpSomeWhere:mWriteALine().

         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "CliType Counter" 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.      
         
      END.

      IF NOT SESSION:BATCH THEN
         HIDE FRAME fQty NO-PAUSE.

   END METHOD.

END CLASS.