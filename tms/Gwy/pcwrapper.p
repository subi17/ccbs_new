
/*------------------------------------------------------------------------
    File        : pcwrapper.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue May 15 11:29:52 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEF VAR oPCObj AS PCListener NO-UNDO.

oPCObj = NEW PCListener().

oPCObj:processOne().

delete Object oPCObj.
