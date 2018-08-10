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
USING Gwy.ProCommandSubscription.

INTERFACE Gwy.SAPC.Service.IService:  

   METHOD PUBLIC VOID mCreateServices
      (ioJsonObject AS CLASS JsonObject,
       ioPCS AS CLASS ProCommandSubscription).

END INTERFACE.