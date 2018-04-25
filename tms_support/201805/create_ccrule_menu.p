
/*------------------------------------------------------------------------
    File        : create_ccrule_menu.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Apr 23 11:53:58 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

CREATE MenuText.
ASSIGN 
    MenuText.MenuNum  = 9857
    MenuText.MenuText = "ACCOUNT.RULES"
    .

CREATE MenuTree.
ASSIGN 
    MenuTree.MenuNum   = MenuText.MenuNum
    MenuTree.Level     = "1112"
    MenuTree.Position  = 7
    MenuTree.MenuType  = "1"
    MenuTree.Module    = "Mc/ccrule_run.p"
    MenuTree.MenuID    = "acc-rule"
    MenuTree.MenuTitle = "Accounting Rules"
    MenuTree.MenuClass = 9999
    MenuTree.TokenCode = "MAIN"
    .
       
       
       
       
       
       
       
       
       
 
       