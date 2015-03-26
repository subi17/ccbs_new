/* fnumberinq.i       07.09.05/aam 

   functions for handling number inquiry service 
   fctserval.i, fsubser.i, fmakemsreq.i needed
*/

DEF VAR lcNumberInq AS CHAR NO-UNDO INIT "NUMBERINQ".
DEF VAR lcSecretPar AS CHAR NO-UNDO INIT "AES".
DEF VAR lcSerDA     AS CHAR NO-UNDO.
DEF VAR lcSerPR     AS CHAR NO-UNDO.
DEF VAR lcSerCD     AS CHAR NO-UNDO. 
 

/* default values for attributes when service opened */
FUNCTION fNumberInqOpened RETURNS CHARACTER
   (INPUT  icCLIType AS CHAR,
    OUTPUT ocAddress AS CHAR,
    OUTPUT ocSex     AS CHAR).

   DEF VAR lcSerInq     AS CHAR NO-UNDO.
   DEF VAR llAllowed    AS LOG  NO-UNDO. 
  
   
   ASSIGN ocAddress = fServAttrValue(icCLIType,
                                     lcNumberInq,
                                     "ADDRESS",
                                     OUTPUT llAllowed)
          ocSex     = fServAttrValue(icCLIType,
                                     lcNumberInq,
                                     "SEX",
                                     OUTPUT llAllowed)
          lcSerDA   = fServAttrValue(icCLIType,
                                     lcNumberInq,
                                     "DA",
                                     OUTPUT llAllowed)
          lcSerPR   = fServAttrValue(icCLIType,
                                     lcNumberInq,
                                     "PR",
                                     OUTPUT llAllowed)
          lcSerCD   = fServAttrValue(icCLIType,
                                     lcNumberInq,
                                     "CD",
                                     OUTPUT llAllowed).

   IF LOOKUP(lcSerDA,"0,1") = 0 THEN lcSerDa = "0".
   IF LOOKUP(lcSerPR,"0,1") = 0 THEN lcSerDa = "0".
   IF LOOKUP(lcSerCD,"0,1") = 0 THEN lcSerDa = "0".
   
   lcSerInq = STRING(INTEGER(lcSerDA),"9") +
              STRING(INTEGER(lcSerDA),"9") +  /* this is for ED */
              STRING(INTEGER(lcSerPR),"9") +
              STRING(INTEGER(lcSerCD),"9") +
              FILL("0",4). 

   RETURN lcSerInq.
   
END FUNCTION.

/* default values for attributes when service closed */
FUNCTION fNumberInqClosed RETURNS CHARACTER
   (OUTPUT ocAddress AS CHAR,
    OUTPUT ocSex     AS CHAR).

   ASSIGN ocAddress = "S"
          ocSex     = "0".

   RETURN FILL("0",7) + "1".       

END FUNCTION.

/* get subscription's current values */
FUNCTION fNumberInqValues RETURNS INTEGER
   (INPUT iiMsSeq AS INT,
    OUTPUT ocInquiry AS CHAR,
    OUTPUT ocAddress AS CHAR,
    OUTPUT ocSex     AS CHAR).
   
   DEF VAR liInqSer AS INT  NO-UNDO.
   
   ASSIGN liInqSer  = fCurrSubSer(iiMsSeq,
                                  lcNumberInq)

          ocAddress = fCurrSubSerPara(iiMsSeq,
                                      lcNumberInq,
                                      "ADDRESS")
          ocSex     = fCurrSubSerPara(iiMsSeq,
                                      lcNumberInq,
                                      "SEX")
          /* ED is always the same as DA */                            
          lcSerDA   = fCurrSubSerPara(iiMsSeq,
                                      lcNumberInq,
                                      "DA")
          lcSerPR   = fCurrSubSerPara(iiMsSeq,
                                      lcNumberInq,
                                      "PR")
          lcSerCD   = fCurrSubSerPara(iiMsSeq,
                                      lcNumberInq,
                                      "CD").
   
   /* make sure that both parameters are off */
   IF liInqSer = 0 THEN liInqSer = fCurrSubSer(iiMsSeq,
                                               lcSecretPar).
   IF liInqSer > 1 THEN liInqSer = 1.
                                    
   IF LOOKUP(ocSex,"0,1")   = 0 THEN ocSex   = "0".
   IF LOOKUP(lcSerDA,"0,1") = 0 THEN lcSerDa = "0".
   IF LOOKUP(lcSerPR,"0,1") = 0 THEN lcSerDa = "0".
   IF LOOKUP(lcSerCD,"0,1") = 0 THEN lcSerDa = "0".
 
   ocInquiry = STRING(INTEGER(lcSerDA),"9") +
               STRING(INTEGER(lcSerDA),"9") +  /* this is for ED */
               STRING(INTEGER(lcSerPR),"9") +
               STRING(INTEGER(lcSerCD),"9") +
               FILL("0",3)                  +
               STRING(liInqSer,"9"). 

   RETURN liInqSer.
   
END FUNCTION.

/* make service change request for attribute change */
FUNCTION fNumberInqAttrReq RETURNS LOGIC
   (iiMsSeq     AS INT, 
    idChgStamp  AS DEC,
    icAttribute AS CHAR,
    icNewValue  AS CHAR).
 
   DEF VAR lcResult AS CHAR NO-UNDO.
       
   fServAttrRequest(iiMsSeq,
                    lcNumberInq,
                    icAttribute,
                    icNewValue,
                    idChgStamp,
                    TRUE,      /* fees */
                    FALSE,     /* sms */    
                    "",
                    "",
                    0,
                    FALSE,
                    OUTPUT lcResult).
END FUNCTION.


/* check if service change requests should be made from changes */
FUNCTION fNumberInqAttrChanged RETURNS LOGICAL
   (iiMsSeq      AS INT,
    idChgStamp   AS DEC,
    icOldInq     AS CHAR,
    icOldAddress AS CHAR,
    icOldSex     AS CHAR,
    icNewInq     AS CHAR,
    icNewAddress AS CHAR,
    icNewSex     AS CHAR).
 
   ASSIGN lcSerDA = SUBSTRING(icNewInq,1,1)
          lcSerPR = SUBSTRING(icNewInq,3,1)
          lcSerCD = SUBSTRING(icNewInq,4,1).
      
   IF lcSerDA NE SUBSTRING(icOldInq,1,1) THEN DO:
      fNumberInqAttrReq (iiMsSeq,
                         idChgStamp,
                         "DA",   
                         lcSerDA). 
      /* ED is always the same as DA */              
      fNumberInqAttrReq (iiMsSeq,
                         idChgStamp,
                         "ED",   
                         lcSerDA). 
   END.

   IF lcSerPR NE SUBSTRING(icOldInq,3,1) THEN DO:
      fNumberInqAttrReq (iiMsSeq,
                         idChgStamp,
                         "PR",   
                         lcSerPR). 
   END.

   IF lcSerCD NE SUBSTRING(icOldInq,4,1) THEN DO:
      fNumberInqAttrReq (iiMsSeq,
                         idChgStamp,
                         "CD",   
                         lcSerCD). 
   END.

   IF icNewAddress NE icOldAddress THEN DO:
      fNumberInqAttrReq (iiMsSeq,
                         idChgStamp,
                         "ADDRESS",   
                         icNewAddress). 
   END.
     
   IF icNewSex NE icOldSex THEN DO:
      fNumberInqAttrReq (iiMsSeq,
                         idChgStamp,
                         "SEX",   
                         icNewSex). 
   END.

END FUNCTION.


