
/* ----------------------------------------------------------------------
  MODULE .......: menu2luna
  TASK .........: Render the TMS menutree as luna documentation page
  AUTHOR .......: mikko 
  CREATED ......: 16.03.09
  CHANGED ......: fabian; adapted for ccbs
----------------------------------------------------------------------- */

{docgen/menutree/confluence.i}
{docgen/menutree/menu2luna.i}

DEFINE VARIABLE lcModule AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcModulePart AS CHARACTER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE k AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 
DEFINE VARIABLE llSkipLevel AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lcBaseSourceUrl AS CHARACTER NO-UNDO. 

lcBaseSourceUrl = ENTRY(1, SESSION:PARAMETER).
OUTPUT TO VALUE(ENTRY(2, SESSION:PARAMETER)).


PUT UNFORMATTED 
fGetCompany() + "TMS Menu structure~n~n"
"Page generated at " 
STRING(TODAY,"99.99.9999") " " STRING(TIME,"hh:mm") 
" on host *"  OS-GETENV("HOST") "*  ~n~n"
"||Warning Symbol||Description||~n"
"|(-)|Usage denied|~n"
"|(?)|Module source code missing|~n"
"|(!)|Menutree configuration missing|~n~n"

"~{toc}~n".

FOR EACH menutree NO-LOCK 
   BREAK BY menutree.level
         BY menutree.position:

   IF NOT level BEGINS "1" THEN NEXT.


   IF FIRST-OF(level) THEN DO:
      IF fGetMenuTitle(level) EQ ? THEN DO:
         llSkipLevel = TRUE.
         NEXT.
      END.
      ELSE 
         llSkipLevel = FALSE.
     
      IF LENGTH(level) GT 1 THEN 
         /* submenus */
         PUT UNFORMATTED
            "~{anchor:" + level + "}~n" +
            "~nh" + STRING(LENGTH(level) - 1) + ". " + 
            fGetMenuTitle(level) + "~n".
      ELSE
         /* main level menus */
         PUT UNFORMATTED
         "~n~{anchor:" + level + "}~n" +
         "~nh1. " + fGetMenuTitle(level) " ~n".

      /* start a section */
      PUT UNFORMATTED
         "~{section}~n".
         
      i = 1.
   END.
   
   IF llSkipLevel THEN NEXT.

   IF menutree.position > i THEN DO:
      j = i.
      /* next item's level was greater than current position */
      DO k = j TO menutree.position - 1:
         /* empty menuitems */
         PUT UNFORMATTED 
            fWrapColumn(
               fWrapInfo("(!) F" + STRING(i),
                  fWrapMenuText(?,?))) + "~n".
         i = i + 1.
      END.
   END.
   
   IF menutree.state[1] NE YES THEN DO:
      /* this is an active button */
      /* if it's a program button */
      /* try to search the module linked to program */
      lcModule = "".

      IF menutree.menutype EQ {&MENUTYPE_PROGRAM} THEN
         lcModule = SEARCH(Menutree.Module + ".p").

      IF lcModule EQ ? THEN DO:
         /* module not found among sources */
         lcModule = fWrapRed(Menutree.Module).
         IF TRIM(lcModule) NE "" THEN lcModulePart = "*Module:* (?) " + 
            lcModule.
      END.
      ELSE IF TRIM(lcModule) NE "" THEN DO:
         /* module was found among sources */
         lcModulePart = "*Module:* " + 
         fLinkToSourceFile(lcModule,lcBaseSourceUrl).
      END.
      ELSE DO:
         /* menutree.module EQ "" */
         lcModulePart = "".
      END.

      PUT UNFORMATTED 
         fWrapColumn(fCreateWikiButton(menutree.level,menutree.position) + 
            "*Desc:* " + Menutree.MenuTitle + {&BREAKLINE} +
            "*Token:* " + MenuTree.token + {&BREAKLINE} +
            lcModulePart) + "~n".
      i = i + 1.
   END.
   ELSE DO:
      /* menuitem disabled; deny usage State[1] = YES */
      /* so add empty menuitem */
      PUT UNFORMATTED 
         fWrapColumn(
            fWrapInfo("(-) F" + STRING(i),
               fWrapMenuText(?,?))) + "~n".
      i = i + 1.

   END.

   IF LAST-OF(level) THEN DO:
      /* end the section */
      PUT UNFORMATTED 
         "~{section}~n".
   END.
END.         

