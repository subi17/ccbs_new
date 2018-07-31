 
 /*------------------------------------------------------------------------
    File        : RESTError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Mar 26 17:13:40 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Net.HTTP.HttpRequestError.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.RESTError INHERITS AppError: 

   DEFINE PUBLIC PROPERTY HTTPStatus   AS INTEGER NO-UNDO
      GET.
      PRIVATE SET.
   
   DEFINE PUBLIC PROPERTY ErrorMessage AS LONGCHAR NO-UNDO
      GET.
      PRIVATE SET.

   CONSTRUCTOR RESTError
      ( icReturnValue AS CHARACTER ):

      SUPER(icReturnValue).

      Class.Logger:writeLog(1,
                            icReturnValue,
                            "ERROR").

   END CONSTRUCTOR.


   CONSTRUCTOR RESTError
      ( ioHttpRequestError AS CLASS HttpRequestError ):

      ErrorMessage = ioHttpRequestError:GetErrorMessage().
      Class.Logger:writeLog(1,
                            ErrorMessage,
                            "ERROR").

   END CONSTRUCTOR.

   CONSTRUCTOR RESTError
      ( iiHTTPStatus AS INTEGER,
        icHTTPStatusName AS CHARACTER,
        ioObject AS CLASS Progress.Lang.Object ):

      Class.Logger:writeLog(1,
                            SUBSTITUTE("&1 &2",
                                       iiHTTPStatus,
                                       icHTTPStatusName),
                            "ERROR").
      
      HTTPStatus = iiHTTPStatus.
      
      IF NOT VALID-OBJECT(ioObject)
      THEN RETURN.

      ErrorMessage = Class.Logger:objToLongChar(ioObject).

      Class.Logger:writeLog(1,
                            ErrorMessage,
                            "ERROR").

   END CONSTRUCTOR.
   
   METHOD PUBLIC OVERRIDE VOID AddMessage
      (icMessage AS CHARACTER,
       iiMesNum  AS INTEGER):
   
      SUPER:AddMessage(icMessage, iiMesNum).
      
      Class.Logger:writeLog(1,
                            icMessage,
                            "ERROR").
   END METHOD.


   METHOD PUBLIC LONGCHAR mGetErrorData():

      DEFINE VARIABLE lcReturnValue AS LONGCHAR NO-UNDO.
      DEFINE VARIABLE lii           AS INTEGER  NO-UNDO.

      IF THIS-OBJECT:ReturnValue > ""
      THEN lcReturnValue = CHR(10) + ReturnValue.

      DO lii = 1 TO THIS-OBJECT:NumMessages:
         lcReturnValue = lcReturnValue + CHR(10) + THIS-OBJECT:GetMessage(lii).
      END.

      RETURN SUBSTRING(lcReturnValue,2).

   END METHOD.

END CLASS.