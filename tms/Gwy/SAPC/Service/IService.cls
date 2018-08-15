/*------------------------------------------------------------------------
    File        : IService
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Thu Aug 09 15:51:21 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Json.ObjectModel.JsonObject.
USING Gwy.SAPC.ProCommandOrder.

INTERFACE Gwy.SAPC.Service.IService:  

   METHOD PUBLIC VOID mCreateServices
      (ioJsonObject AS CLASS JsonObject).

END INTERFACE.