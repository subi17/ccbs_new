 
 /*------------------------------------------------------------------------
    File        : VerbEnum
    Purpose     : List of currently supported ProCommandVerbs
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Fri Aug 10 11:35:02 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

ENUM Gwy.SAPC.VerbEnum:

   DEFINE ENUM
      POST
      GET
      PATCH
      PUT
      DELETE
      .

END ENUM.