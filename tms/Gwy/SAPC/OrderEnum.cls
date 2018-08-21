 
 /*------------------------------------------------------------------------
    File        : OrderEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Thu Mar 15 14:04:47 EET 2018
    Notes       : 
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

ENUM Gwy.SAPC.OrderEnum:

   DEFINE ENUM
      SUBSCRIPTION_CREATE
      SUBSCRIPTION_TERMINATE
      SUBSCRIPTION_REACTIVATE
      DATAPLAN_ADD
      DATAPLAN_DELETE
      DATAPLAN_MODIFY
      CHANGE_SIM
      UPSELL_ADD
      .
      
END ENUM.
