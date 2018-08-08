 
 /*------------------------------------------------------------------------
    File        : ProCommandOrderEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Thu Mar 15 14:04:47 EET 2018
    Notes       : 
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

ENUM Gwy.ProCommandOrderEnum:

   DEFINE ENUM
      CREATION
      TERMINATION
      PORTABILITY
      .
      
END ENUM.
