/* nnasse.i: Customer lisäys & muutos 

        25.01.02/aam OrgId NOT mandatory
        11.09.02/aam CustDiscProd removed
        16.09.02/aam PriceList and RateCust removed
        09.10.02/jr  Suggest SearchName
        09.10.02/jr  Language validate
        09.10.02/jr  Country validate and name display
        20.11.02 lp  AccGrp validate and name display
        28.02.03/aam DirMark added
        24.03.03/aam Country and Salesman not mandatory
        01.10.03/aam Salesman and Reseller removed
        07.10.03/aam PaymTerm default from CustCat
        16.01.04/aam AgrCust added, AccGrp removed, help for RepCodes
        06.02.04/aam brand was missing from custcat-find
        12.02.04/aam PaymCust by default same as AgrCust
        27.04.04/aam reportcode 4 not allowed
        07.12.05/aam -> trel2
        14.11.06/aam yoigo fields and layout
        16.05.07/aam normal id types cannot be changed into passport 
*/

           EDITING:
              READKEY. nap = keylabel(LASTKEY).

              if lookup(nap,"F4") > 0 THEN UNDO, LEAVE.

              ELSE IF KEYLABEL(LASTKEY) = "F9" AND 
                   LOOKUP(FRAME-FIELD,"CustIDType,HonTitle,lcCustZipCode") > 0
              THEN DO:
                  
                  IF FRAME-FIELD = "CustIDType" THEN DO:           
                     RUN Help/h-tmscodes.p(INPUT "Customer",    /* TableName */
                                          "CustIDType",  /* FieldName */
                                          "CustCare",  /* GroupCode */
                                    OUTPUT lcCode).

                     IF lcCode ne "" AND lcCode NE ? THEN DO:
                        DISPLAY lcCode @ Customer.CustIDType
                        WITH FRAME lis.   
                     END.   
                  END.
                  
                  ELSE IF FRAME-FIELD = "HonTitle" THEN DO:
                     RUN Help/h-tmscodes.p(INPUT "Customer",    /* TableName */
                                          "Title",      /* FieldName */
                                          "CustCare",   /* GroupCode */
                                    OUTPUT lcCode).

                     IF lcCode ne "" AND lcCode NE ? THEN DO:
                        DISPLAY lcCode @ Customer.HonTitle
                        WITH FRAME lis.   
                     END.   
                  END. 

                  ELSE IF FRAME-FIELD = "lcCustZipCode" THEN DO:
                     ASSIGN si-recid = ?
                            siirto   = "".
                     RUN Help/h-postcode.
                     /* several rows with same zipcode */
                     IF si-recid NE ? THEN DO:
                        DISPLAY siirto @ lcCustZipCode WITH FRAME lis.
                        FIND PostCode WHERE RECID(PostCode) = si-recid
                           NO-LOCK NO-ERROR.
                        IF AVAILABLE PostCode THEN DO:
                           DISPLAY PostCode.PostOffice @ lcCustPostOffice
                                   PostCode.Region     @ lcCustRegion
                                   PostCode.Country    @ lcCustCountry
                           WITH FRAME lis.

                           fRegion(PostCode.Region).
                           DISPLAY lcRegion.
                           DISP fDefInvGroup(PostCode.Region) @ 
                              Customer.InvGroup.

                           
                           fCountry(PostCode.Country).
                           DISPLAY lcCountry.
                           
                        END.
                     END.      
                  END.
                  
                  ehto = 9.
                  RUN Syst/ufkey.
                  NEXT. 
              END.

              ELSE IF lookup(nap,poisnap) > 0 THEN DO WITH FRAME lis:
                 HIDE MESSAGE no-pause.

                
                 if frame-field = "language" THEN DO:
                    
                    FIND Language where Language.Language =
                         INPUT Customer.language no-lock no-error.
                    IF NOT AVAIL language THEN DO:
                       bell. 
                       message "Unknown Language !". 
                       NEXT. 
                    END.
                    lcLang = Language.LangName.
                    DISPLAY lcLang WITH FRAME lis.
                 END.

                 ELSE IF FRAME-FIELD = "lcCustCountry" THEN DO:
                       
                    IF INPUT FRAME lis Customer.CustIDType = "NIF" AND
                       INPUT lcCustCountry NE lcDefCountry
                    THEN DO:
                       MESSAGE "Country should be" lcDefCountry
                               "when ID type is NIF"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.
                       
                    IF NOT fCountry(INPUT INPUT lcCustCountry) THEN DO:
                       MESSAGE "Unknown country" VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.
                    DISP lcCountry. 
                          
                    IF INPUT lcCustCountry > "" AND 
                       INPUT Customer.Nationality = ""
                    THEN DISPLAY INPUT lcCustCountry @ 
                                 Customer.Nationality WITH FRAME lis.
                 END.
                    
                 ELSE IF FRAME-FIELD = "lcCustAddress" THEN DO:
                 
                    IF INPUT lcCustAddress = "" AND
                       LOOKUP(INPUT Customer.CustIDType,"N/A") = 0
                    THEN DO:
                       MESSAGE "Address is mandatory" 
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.
                 END.
 
                 ELSE IF FRAME-FIELD = "lcCustZipCode" THEN DO:

                    IF INPUT lcCustZipCode > "" THEN DO:
                       /* there are several rows with same zipcode so don't
                          take the name from there if it has already been 
                          assigned */
                       IF INPUT FRAME lis lcCustPostOffice = "" THEN DO:     
                          IF INPUT FRAME lis lcCustCountry > "" THEN  
                             FIND FIRST PostCode WHERE
                                  PostCode.Country = INPUT lcCustCountry AND
                                  PostCode.ZipCode = INPUT lcCustZipCode
                             NO-LOCK NO-ERROR.
                          ELSE 
                             FIND FIRST PostCode WHERE
                                  PostCode.Country = lcDefCountry AND
                                  PostCode.ZipCode = INPUT lcCustZipCode
                             NO-LOCK NO-ERROR.

                          IF INPUT lcCustZipCode = "" OR
                             NOT AVAILABLE PostCode 
                          THEN DISPLAY SUBSTRING(INPUT lcCustZipCode,1,2) @
                                       lcCustRegion WITH FRAME lis.
                          ELSE DISPLAY 
                             PostCode.Region     @ lcCustRegion
                             PostCode.PostOffice @ lcCustPostOffice
                             PostCode.Country    @ lcCustCountry
                             WITH FRAME lis.

                          fRegion(INPUT INPUT lcCustRegion).
                          fCountry(INPUT INPUT lcCustCountry).
                          
                          DISP fDefInvGroup(INPUT INPUT lcCustRegion) 
                           @ Customer.InvGroup.
                         
                          DISPLAY lcRegion
                                  lcCountry.
                       END.
                    END. 
                    
                    ELSE IF LOOKUP(INPUT Customer.CustIDType,"N/A") = 0
                    THEN DO:
                       MESSAGE "Zip code is mandatory" 
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.
                 END.

                 ELSE IF FRAME-FIELD = "lcCustPostOffice" THEN DO:
                 
                    IF INPUT lcCustPostOffice = "" AND
                       LOOKUP(INPUT Customer.CustIDType,"N/A") = 0
                    THEN DO:
                       MESSAGE "City is mandatory" 
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.
                 END.
                 
                 ELSE IF FRAME-FIELD = "Nationality" THEN DO:

                    IF NOT fNationality(INPUT INPUT Customer.Nationality)
                    THEN DO:
                       MESSAGE "Unknown nationality"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.
                    
                    DISP lcNationality. 
                    
                    IF INPUT FRAME lis lcCustCountry = "" THEN 
                       DISPLAY INPUT Customer.Nationality @ lcCustCountry.
                 
                 END.
                 ELSE IF FRAME-FIELD = "lcCustRegion" THEN DO:

                    IF NOT fRegion(INPUT INPUT lcCustRegion) THEN DO:
                       MESSAGE "Unknown region"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.

                    DISPLAY lcRegion
                            fDefInvGroup(INPUT INPUT lcCustRegion)
                              @ Customer.InvGroup.
                 END.

                 ELSE IF frame-field  = "OrgId" THEN DO:

                    IF Customer.CustNum > 1000 AND 
                       NOT fChkCustID(INPUT INPUT Customer.CustIDType,
                                      INPUT INPUT Customer.OrgId)
                    THEN DO:
                       MESSAGE "Invalid customer ID"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.

                    IF INPUT FRAME lis Customer.CustIDType = "CIF" THEN DO:
                       NEXT-PROMPT Customer.Nationality.
                       NEXT.
                    END.
                 END.

                 ELSE IF FRAME-FIELD = "Birthday" THEN DO:
                    IF INPUT Customer.Birthday = ? AND
                       LOOKUP(INPUT Customer.CustIDType,"NIF,NIE,Passport") > 0
                    THEN DO:
                       MESSAGE "Birthday is mandatory"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.
                 END.
                 
                 else if frame-field = "Category" THEN DO:
                    FIND FIRST CustCat where 
                               CustCat.Brand    = gcBrand AND
                               CustCat.Category = INPUT FRAME lis Customer.Category 
                    no-lock no-error.
                    IF NOT AVAIL CustCat THEN DO:
                       MESSAGE "Unknown category" VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END. 
                    ELSE DO:
                       
                       ASSIGN dkatnimi = CustCat.CatName.
                       DISPLAY dkatnimi.
                    END.
                 END.


                 else if frame-field = "CustName" THEN DO:

                    if input Customer.CustName = "" AND
                       LOOKUP(INPUT FRAME lis Customer.CustIDType,"CIF,N/A") 
                            = 0 
                    THEN DO:
                       MESSAGE "Surname is mandatory"
                       VIEW-AS ALERT-BOX.
                       NEXT.
                    END.

                 END. 

                 ELSE IF FRAME-FIELD = "FirstName" AND Customer.CustNum > 1000
                 THEN DO:
                    IF INPUT FRAME lis Customer.FirstName = "" AND
                       LOOKUP(INPUT FRAME lis Customer.CustIDType,"CIF,N/A") 
                            = 0 
                    THEN DO:
                       MESSAGE "First name is mandatory"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.
                 END.

                 ELSE IF FRAME-FIELD = "SurName2" THEN DO:
                    IF INPUT FRAME lis Customer.CustIDType NE "N/A" THEN DO:
                       NEXT-PROMPT Customer.Language.
                       NEXT.
                    END.
                 END.
                 
                 ELSE IF FRAME-FIELD = "HonTitle" THEN DO:
                 
                    IF INPUT FRAME lis Customer.HonTitle > "" AND 
                       NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                            "Customer",
                                            "Title",
                                            INPUT INPUT FRAME lis
                                                  Customer.HonTitle)
                    THEN DO:                              
                       MESSAGE "Unknown title"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.
                 END.
                 
                 ELSE IF FRAME-FIELD = "CustIDType" THEN DO:
                 
                    lcIDType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                                "Customer",
                                                "CustIDType",
                                                INPUT INPUT FRAME lis
                                                      Customer.CustIDType).
                    IF lcIDType = "" AND Customer.CustNum > 1000 THEN DO:
                       MESSAGE "Unknown ID type"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.

                    /* passport cannot always be used */
                    IF LOOKUP(Customer.CustIDType,"NIE,NIF,CIF") > 0 AND
                       INPUT FRAME lis Customer.CustIDType = "Passport" 
                    THEN DO:
                       MESSAGE "Normal ID type cannot be changed into passport"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.

                    IF INPUT FRAME lis Customer.CustIDType = "NIF" THEN DO:
                       DISPLAY lcDefCountry @ lcCustCountry.
                    END.
                    IF Customer.CustIDType <> INPUT FRAME lis Customer.CustIDType THEN DO:
                       FOR EACH CustCat NO-LOCK WHERE
                                CustCat.Brand = gcBrand:
                           IF LOOKUP(INPUT FRAME lis Customer.CustIDType,
                                 CustCat.CustIDType) > 0 THEN DO:
                          DISPLAY 
                                  CustCat.Category @ Customer.Category
                                  CustCat.CatName @ dkatnimi WITH FRAME lis.
                          LEAVE.   
                         END.
                       END.
                    END.
                    /* all other fields are not updateable if type is 
                       unknown */
                    IF INPUT FRAME lis Customer.CustIDType = "N/A" THEN DO:
                         ASSIGN lcInvGroup = fDefInvGroup("")
                                lcRegion          = "".
 
                          DISPLAY  Customer.OrgID
                                   Customer.FirstName
                                   Customer.HonTitle
                                   Customer.CustName
                                   Customer.SurName2
                                   Customer.CompanyName
                                   lcCustZipCode
                                   lcCustPostOffice
                                   lcCustAddress
                                   lcCustRegion
                                   lcRegion
                                   lcInvGroup @  Customer.InvGroup.
                          LEAVE. 
                      
                    END.
                 
                 END.
                 
              END.
              
              ELSE IF FRAME-FIELD = "DataProtected" THEN DO:
                 FIND FIRST Mobsub WHERE 
                    Mobsub.Brand   = gcBrand AND
                    Mobsub.Custnum = Customer.CustNum NO-LOCK NO-ERROR.
                 IF AVAIL Mobsub THEN DO:
                    MESSAGE "The Function is not allowed." SKIP
                        "This customer has active subscription(s)..."
                    VIEW-AS ALERT-BOX ERROR.
                    NEXT.
                 END.   
              END.
    
              /* fields in which only F9 and enter are allowed */    
              ELSE DO:
                 IF LOOKUP(FRAME-FIELD,"lcCustZipCode") > 0 THEN NEXT. 
              END.
              
              APPLY LASTKEY.

           END. /* EDITING */

