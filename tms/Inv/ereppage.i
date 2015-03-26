/* ereppage.i     07.12.2001/aam out from eletterinv.p 
                  16.01.2002/aam Jippii forms
                  24.01.2002/aam use only channel 1,
                                 made as a function
                  10.02.2003/aam use also backside of invoice sheet 
                  12.02.2004/aam use cover sheet if a new letter started 
                  16.06.2004/aam fSpecGenHeader
                    
*/

DEF VAR lcSpecGenHead1 AS CHAR NO-UNDO.
DEF VAR lcSpecGenHead2 AS CHAR NO-UNDO.

FUNCTION fSpecGenHeader RETURNS LOGICAL
   (iiCustNum  AS INT,
    iiInvNum   AS INT,
    iiLanguage AS INT).
   
   lcSpecGenHead1 = STRING(fTeksti(140,iiLanguage),"X(15)") + STRING(iiCustNum).
   
   IF iiInvNum > 0 
   THEN lcSpecGenHead2 = STRING(fTeksti(141,iiLanguage),"X(15)") + 
                         STRING(iiInvNum).
   ELSE lcSpecGenHead2 = "".
   
END FUNCTION.


FUNCTION fNewPage RETURNS LOGICAL
    (iAddLine AS INT).

    IF Licalask + iAddLine le xInvLines - 1
    THEN RETURN FALSE. 

    /* backside of the actual invoice sheet */
    IF etusivuerit THEN ASSIGN 
        lckutsu     = REPLACE(lckutsu,"EPL5","EPL6")
        xInvLines   = liInvPage2
        etusivuerit = FALSE.

    ELSE DO:
        lckutsu = "EPL" + lcSpecForm.
        
        IF Llcakanto = true  
        THEN assign 
            /* backside of report sheet */
            lckutsu     = REPLACE(lckutsu,"EPL5","EPL6")
            Llcakanto   = false.
        ELSE assign 
            /* front page of report sheet */
            lckutsu   = REPLACE(lckutsu,"EPL6","EPL5")
            Llcakanto = true
            liSheets  = liSheets + 1.
            
        ASSIGN xInvLines = liRepPage.
    END.

    lcRepFLine =   
        STRING(lcEPLRepHead,"X(30)") +        /* CALL SPECIFICATION TEXT  */
        STRING(fTeksti(28,LIkieli) + " " + 
               STRING(IF LLcasivu < 0 THEN 1 ELSE llCaSivu),"X(8)")
         + "¤" + 
        lcSpecGenHead1
        + "¤" + 
        lcSpecGenHead2.
    
    PUT STREAM ekirje UNFORMATTED
              "1I"
              SPACE(40)
              ENTRY(1,lcRepFLine,"¤")
              MY-NL
              lcKutsu                        
              MY-NL
              "0I"
              SPACE(40)
              ENTRY(2,lcRepFLine,"¤")
              MY-NL
              " I"
              SPACE(40)
              ENTRY(3,lcRepFLine,"¤")
              MY-NL.
        
    PUT STREAM ekirje UNFORMATTED
        "0I" 
        MY-NL.
        
    ASSIGN 
    LLcasivu = LLcasivu + 1            /* PAGE COUNTER                */
    licalask = 6. 

    RETURN TRUE.
    
END FUNCTION. 

