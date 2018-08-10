 
 /*------------------------------------------------------------------------
    File        : TargetEnum
    Purpose     : List of currently supported ProCommandTargets
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Fri Aug 10 11:35:02 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

ENUM Gwy.SAPC.TargetEnum:

   DEFINE ENUM
      NB_AS
      NB_CH
      BPM
      .
END ENUM.