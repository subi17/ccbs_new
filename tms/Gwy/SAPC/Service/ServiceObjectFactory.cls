 
 /*------------------------------------------------------------------------
    File        : ServiceFactory
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Lang.AppError.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.
USING Gwy.SAPC.ProCommandOrder.
USING Gwy.SAPC.Service.IService.
USING Gwy.SAPC.Service.ServiceObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.Service.ServiceObjectFactory IMPLEMENTS IService: 

   DEFINE PROTECTED TEMP-TABLE ttObject NO-UNDO
      FIELD ordernbr AS INTEGER
      FIELD obj AS Progress.Lang.Object
      INDEX ordernbr IS PRIMARY UNIQUE ordernbr.
   
   DESTRUCTOR ServiceObjectFactory():
      FOR EACH ttObject:
         IF VALID-OBJECT(ttObject.obj)
         THEN DELETE OBJECT ttObject.obj.
      END.
   END DESTRUCTOR.

   METHOD PRIVATE INTEGER mNextOrderNbr():
      FIND LAST ttObject USE-INDEX ordernbr NO-ERROR.
      IF AVAILABLE ttObject
      THEN RETURN ttObject.ordernbr + 1.
      RETURN 1.
   END METHOD.

   METHOD PROTECTED VOID mAddObject
      ( ioServiceObject AS CLASS ServiceObject ):

      DEFINE VARIABLE liNextNbr AS INTEGER NO-UNDO.
      liNextNbr = mNextOrderNbr().
      CREATE ttObject.
      ASSIGN
         ttObject.ordernbr = liNextNbr
         ttObject.obj      = ioServiceObject.

   END METHOD.

   METHOD PUBLIC VOID mCreateServices
      ( ioJsonObject AS CLASS JsonObject ):

      IF NOT CAN-FIND(FIRST ttObject)
      THEN UNDO, THROW NEW AppError("No service object available").

      DEFINE VARIABLE loJsonArray  AS CLASS JsonArray  NO-UNDO.
      loJsonArray = NEW JsonArray().
      
      FOR EACH ttObject:
         IF NOT TYPE-OF(ttObject.obj, ServiceObject)
         THEN UNDO, THROW NEW AppError("Service object must be ServiceObject class type").
         loJsonArray:Add(CAST(ttObject.obj, ServiceObject):aoJsonObject).
      END.

      ioJsonObject:Add("Services", loJsonArray).
      
   END METHOD.

END CLASS.