
/*------------------------------------------------------------------------
    File        : IObjectStorage
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Tue Apr 24 20:44:55 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.JsonObject.

INTERFACE bss.cls.IObjectStorage:  

   METHOD PUBLIC VOID mCreateData(ioJsonObject AS CLASS JsonObject).

   METHOD PUBLIC VOID mStoreData():

END INTERFACE.