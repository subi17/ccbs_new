&IF "{&MENU_I}" NE "YES" &THEN
&GLOBAL-DEFINE MENU_I YES

DEFINE BUFFER x FOR menutree.

&GLOBAL-DEFINE MENUTYPE_PROGRAM 1
&GLOBAL-DEFINE MENUTYPE_MENU 2
&GLOBAL-DEFINE MENUTYPE_RETURN 3

{docgen/menutree/confluence.i}

FUNCTION fGetCompany RETURN CHAR():

   FIND FIRST company NO-LOCK NO-ERROR.
   IF AVAIL company THEN 
   RETURN company.compname + " ".
   ELSE RETURN "".
END.

FUNCTION fGetMenuTitle RETURN CHAR
(INPUT icLevel AS CHAR):
   DEFINE VARIABLE lcLevel AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liPos AS INT NO-UNDO.
              

   IF icLevel EQ "0" THEN RETURN "Functions accessed directly".
   IF icLevel EQ "1" THEN RETURN fGetCompany() + "Main Menu".
  
   /* take last char of level */
   liPos = INT(SUBSTRING(icLevel,LENGTH(TRIM(icLevel)))).

   /* take level part of level */
   lcLevel = SUBSTRING(icLevel, 1, LENGTH(TRIM(icLevel)) - 1).
   FIND FIRST x where 
              x.level = lcLevel AND
              x.position = liPos NO-ERROR.

   IF AVAIL x THEN 
      RETURN "(" + fLevelToString(SUBSTRING(icLevel,2)) + ") " + x.menutitle.
   ELSE RETURN ?.


END.


FUNCTION fGetMenuText RETURN CHAR
(INPUT icLevel AS CHAR,
 INPUT iiPosition AS INT):

   DEFINE VARIABLE lcMenutext AS CHARACTER NO-UNDO. 

   IF icLevel EQ ? THEN 
      RETURN "". 

   FIND FIRST x WHERE
              x.level = icLevel AND
              x.position = iiPosition NO-ERROR.

   IF NOT AVAIL x THEN
      RETURN "". 

   FIND FIRST menutext NO-LOCK WHERE   
              menutext.menunum = x.Menunum NO-ERROR.

   IF NOT AVAIL menutext THEN lcMenutext = "NO-TEXT".
   ELSE lcMenutext = menutext.menutext.
   
   RETURN lcMenutext.
      
END.

FUNCTION fGetPreviousLevel RETURN CHAR
(INPUT icLevel AS CHAR):

   DEFINE VARIABLE lcPrev AS CHARACTER NO-UNDO. 
   IF LENGTH(icLevel) <= 1 THEN RETURN icLevel.
   lcPrev = SUBSTRING(icLevel,1,LENGTH(icLevel) - 1).
   RETURN lcPrev.

END.

FUNCTION fLinkToSourceFile RETURN CHAR
(INPUT icModule AS CHAR,
 INPUT icUrlBase AS CHAR):

   DEFINE VARIABLE lcUrl AS CHARACTER NO-UNDO. 
   lcUrl = icUrlBase + TRIM(icModule,".").

   RETURN fWrapExternalLink(icModule,lcUrl).
END.

FUNCTION fCreateWikiButton RETURN CHAR
(INPUT icLevel AS CHAR,
 INPUT iiPosition AS INT):
   FIND FIRST x WHERE
              x.level = icLevel AND
              x.position = iiPosition NO-ERROR.

   IF NOT AVAIL x THEN RETURN "".

   CASE x.menutype:
      
      WHEN {&MENUTYPE_RETURN} THEN DO:
         
         RETURN fWrapNote("F" + STRING(x.position) + " ",
            fWrapMenuText(fGetMenuText(icLevel,iiPosition),
               fGetPreviousLevel(icLevel))) + " ".
      END.

      WHEN {&MENUTYPE_MENU} THEN DO:
         RETURN fWrapInfo("F" + STRING(x.position) + " ",
            fWrapMenuText(fGetMenuText(icLevel,iiPosition),
               icLevel + STRING(iiPosition))) 
            + " ".
      END.

      WHEN {&MENUTYPE_PROGRAM} THEN DO:
         RETURN fWrapTip("F" + STRING(x.position) + " ",
            fWrapMenuText(fGetMenuText(icLevel,iiPosition),?)) + " ".
      END.
   END CASE.


END.

&ENDIF
