/* edefine.i    07.12.2001/aam out from eletterinv.p, make all vars shared 
                11.02.2003/aam more lines to invoice sheet's backside
                28.03.2003/aam boxes after product lines -> 22 lines available
                21.05.2003/aam routines from eplfile.p into functions
                21.08.2003/aam accept foreign countries
                09.10.2003/aam testflag can be E
                18.12.2003/aam ttDomHead
                17.02.2004/aam cover sheet, printhouse
                20.04.2004/aam divide icFirstLine from "¤"
                27.01.2006/aam country code in front of zipcode if not FI
                06.03.2006/aam fEplSeqFileName()
                07.09.2006/aam nearly all variables as 'shared'

callers: eplfile.p
         climepl.p
*/


DEFINE {1} SHARED STREAM ekirje.

DEFINE {1} SHARED VAR LIpituus    AS INTEGER   NO-UNDO.

DEFINE {1} SHARED VAR lcastunnus  AS CHARACTER NO-UNDO  INIT "112635". 
DEFINE {1} SHARED VAR lcsala      AS CHARACTER NO-UNDO  INIT "2298".
DEFINE {1} SHARED VAR LCviitenro  AS CHARACTER NO-UNDO  FORMAT "x(22)".
DEFINE {1} SHARED VAR LIerrivi    AS INTEGER   NO-UNDO.
DEFINE {1} SHARED VAR LIkieli     AS INTEGER   NO-UNDO.

DEFINE {1} SHARED VAR LIcaamt     AS INTEGER   NO-UNDO.
DEFINE {1} SHARED VAR LCcadur     AS INTEGER   NO-UNDO.
DEFINE {1} SHARED VAR LDcabrut    AS DECIMAL   NO-UNDO.
DEFINE {1} SHARED VAR LIcalask    AS INTEGER   NO-UNDO.
DEFINE {1} SHARED VAR LLcakanto   AS LOG       NO-UNDO  INIT FALSE.
DEFINE {1} SHARED VAR LLcasivu    AS INTEGER   NO-UNDO.

DEFINE {1} SHARED VAR LLfirst     AS LOGICAL   NO-UNDO  INIT TRUE 
           EXTENT 3.
DEFINE {1} SHARED VAR lisaerit    AS LOGICAL   NO-UNDO  INIT FALSE.
DEFINE {1} SHARED VAR etusivuerit AS LOGICAL   NO-UNDO  INIT TRUE.
DEFINE {1} SHARED VAR kutsuerit   AS CHARACTER NO-UNDO.

DEFINE {1} SHARED VAR eritlaskuri  AS INTEGER   NO-UNDO.
DEFINE {1} SHARED VAR alaosa       AS LOGICAL   NO-UNDO  INITIAL FALSE.
DEFINE {1} SHARED VAR lckutsu      AS CHAR      NO-UNDO.     

DEFINE {1} SHARED VAR lcPrintHouse AS CHAR      NO-UNDO.     
DEFINE {1} SHARED VAR lcSpecForm   AS CHAR      NO-UNDO.     
DEFINE {1} SHARED VAR lcAttach1    AS CHAR      NO-UNDO.     
DEFINE {1} SHARED VAR lcAttach2    AS CHAR      NO-UNDO.     
DEFINE {1} SHARED VAR lcAttach3    AS CHAR      NO-UNDO.     
DEFINE {1} SHARED VAR lcCoverSheet AS CHAR      NO-UNDO.     
DEFINE {1} SHARED VAR lcCoverTitle AS CHAR      NO-UNDO.     
DEFINE {1} SHARED VAR lcCoverTxt   AS CHAR      NO-UNDO.     
DEFINE {1} SHARED VAR liSheets     AS INT       NO-UNDO.

/* letter receiver */
DEF {1} SHARED VAR lcEPLRName    AS CHAR NO-UNDO.
DEF {1} SHARED VAR lcEPLRLast    AS CHAR NO-UNDO.
DEF {1} SHARED VAR lcEPLRFirst   AS CHAR NO-UNDO.
DEF {1} SHARED VAR lcEPLRCoName  AS CHAR NO-UNDO.
DEF {1} SHARED VAR lcEPLRAddr    AS CHAR NO-UNDO.
DEF {1} SHARED VAR lcEPLRZipCode AS CHAR NO-UNDO.
DEF {1} SHARED VAR lcEPLRPost    AS CHAR NO-UNDO.
DEF {1} SHARED VAR lcEPLRCountry AS CHAR NO-UNDO.
DEF {1} SHARED VAR lcEPLACountry AS CHAR NO-UNDO.

/* lines for specification form */
DEFINE {1} SHARED VAR liRepPage    AS INT  NO-UNDO INIT 78.
/* lines for invoice form, front page */
DEFINE {1} SHARED VAR liInvPage1   AS INT  NO-UNDO INIT 22.
/* back page */
DEFINE {1} SHARED VAR liInvPage2   AS INT  NO-UNDO INIT 45.
/* cover sheet, front and back */
DEFINE {1} SHARED VAR liCoverPage1 AS INT  NO-UNDO INIT 39.
DEFINE {1} SHARED VAR liCoverPage2 AS INT  NO-UNDO INIT 53.

DEF {1} SHARED VAR xInvLines    AS INT  NO-UNDO.
DEF {1} SHARED VAR xEplForm     AS CHAR NO-UNDO.
DEF {1} SHARED VAR xEplTest     AS CHAR NO-UNDO. 
DEF {1} SHARED VAR xBuAdd       AS CHAR NO-UNDO.
DEF {1} SHARED VAR xTransDir    AS CHAR NO-UNDO.
DEF {1} SHARED VAR xPSTransDir  AS CHAR NO-UNDO. 
DEF {1} SHARED VAR xFileExt     AS CHAR NO-UNDO.
DEF {1} SHARED VAR xErrFile     AS CHAR NO-UNDO. 
DEF {1} SHARED VAR xConfDir     AS CHAR NO-UNDO. 
DEF {1} SHARED VAR lcEPLContact AS CHAR NO-UNDO. 
DEF {1} SHARED VAR lcEPLRepHead AS CHAR NO-UNDO. 

DEF VAR lcRepFLine   AS CHAR NO-UNDO. 
DEF VAR liCnt        AS INT  NO-UNDO.
DEF VAR tab          AS CHAR NO-UNDO.
DEF VAR my-nl        AS CHAR NO-UNDO.

ASSIGN my-nl = CHR(13) + CHR(10)
       tab   = CHR(9).

FUNCTION fEPLInit RETURNS LOGICAL.

   ASSIGN xTransDir     = fCParamC("EPLTransDir")
          xPSTransDir   = fCParamC("PSTransDir")
          xEPLTest      = fCParamC("EPLTest") 
          xFileExt      = ".temppi"
          xErrFile      = fCParamC("EPLErrorFile")
          xConfDir      = fCParamC("RepConfDir")
          lcSpecForm    = fCParamC("EPLSpecForm").

   /* text for special work replaces contact info */
   IF xEPLTest = "E" 
   THEN lcEPLContact = fCParamC("EPLSpecialWork").
   ELSE lcEPLContact = fCParamC("EPLContact").
   
    /* make sure that values are valid */
   IF LOOKUP(xEPLTest,"T,E") = 0 THEN xEPLTest = "".
   IF lcEPLContact = ? THEN lcEPLContact = "".
  
END FUNCTION.

FUNCTION fEPLFileName RETURNS CHARACTER
   (icFile AS CHAR).

   ASSIGN liCnt  = 0
          xBuAdd = xFileExt.

   /* check that the file doesn't exist */        
   REPEAT:                                                  
      IF SEARCH(icFile + xBuAdd) = ? THEN LEAVE.
      ASSIGN liCnt  = liCnt + 1
             xBuAdd = "_" + string(liCnt) + xFileExt.
   END.

   icFile = icFile + xBuAdd.

   RETURN icFile.

END FUNCTION.

FUNCTION fEPLSeqFileName RETURNS CHARACTER
   (icFile AS CHAR).

   /* check that the file doesn't exist */        
   REPEAT:                                                  
      xBuAdd = "_" + STRING(NEXT-VALUE(invfile)) + xFileExt.

      IF SEARCH(icFile + xBuAdd) = ? THEN LEAVE.
   END.

   icFile = icFile + xBuAdd.

   RETURN icFile.

END FUNCTION.

FUNCTION fEPLCheckAddr RETURNS CHARACTER
   (iiLetterClass AS INT,
    icName        AS CHAR,
    icZipCode     AS CHAR,
    icCountry     AS CHAR,
    OUTPUT ocACountry AS CHAR).

   DEF VAR lcErrMess AS CHAR NO-UNDO.
   
   ASSIGN lcErrMess    = ""        
          ocACountry = "".
          
   icCountry = RIGHT-TRIM(icCountry).
   
   /* check that name is valid */
   IF icName = "" 
   THEN lcErrMess = "Name empty". 

   /* foreign country */
   ELSE IF LOOKUP(icCountry,"FI,FIN,FINLAND,") = 0 
   THEN DO:
      IF LENGTH(icCountry) = 2 
      THEN FIND Country WHERE Country.Country = icCountry
           NO-LOCK NO-ERROR.
      ELSE FIND FIRST Country WHERE Country.CoName = icCountry 
           NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Country OR 
         LENGTH(Country.Country) NE 2
      THEN lcErrMess = "Invalid country " + icCountry.
      ELSE DO:
         ocACountry = Country.Country.
         IF LENGTH(lcEPLRCountry) = 2 
         THEN lcEPLRCountry = Country.CoName.
      END. 
      
      /* foreign letters always in 1-class */
      IF lcErrMess = "" AND iiLetterClass NE 1 
      THEN lcErrMess = "Foreign letters should use 1 class".
                       
   END.        
       
   /* finnish postcode */
   ELSE DO:
       liCnt = INTEGER(icZipCode) NO-ERROR.

       IF ERROR-STATUS:ERROR OR liCnt = 0 OR 
          LENGTH(TRIM(icZipCode)) NE 5 
       THEN lcErrMess = "Invalid postcode " + icZipCode. 
       ocACountry = "FI".
   END.

   RETURN lcErrMess. 

END FUNCTION.


/* main header for EPL-file; sender ids etc. */
FUNCTION fEPLMainHeader RETURNS LOGICAL
   (iiLetterClass AS INT).

   PUT STREAM ekirje UNFORMATTED
      "EPL1"                                  /* ELETTER CODE          */
      lcastunnus       FORMAT "xxxxxx"        /* CUSTOMER SIGN         */
      lcsala           FORMAT "xxxx"          /* PASSWORD              */
      "0"                                     /* ONE ADDRES LETTER     */
      xEPLTest         FORMAT "X"             /* SERVICES:             
                                                 T     = TESTDATA
                                                 E     = special work
                                                 EMPTY = REALDATA      */
      "T"                                     /* T=only print 
                                                 B=print AND archive   */
      "0"                                     /* NORMAL CODE           */
      "0"                                     /* OPTICAL CONTROLCODE   */
      iiLetterClass    FORMAT "9"             /* LETTERCLASS           */
      "S"                                     /* ENVELOPE 
                                                (S = ELETTER STANDARD)*/
      " "                                     /* SECTION               */
      " "                                     /* EMPTY                 */
      "0"                                     /* PAPER 
                                               (0 = ELETTER STANDARD)   */
      FILL(" ",8)                             /* 8 EMPTY SPACE         */
      FILL(" ",8)                             /* DEFAULT FORM (EMPTY)  */
      lcEPLContact    FORMAT "X(40)"          /* SENDER CONTACT        */
      MY-NL.
    
END FUNCTION.   

FUNCTION fEPLReceiver RETURNS LOGICAL
   (icFirstLine AS CHAR).
   
   DEF VAR lcAddLine AS CHAR NO-UNDO.
   
   liCnt = INDEX(icFirstLine,"¤").
   IF liCnt > 0 
   THEN ASSIGN lcAddLine   = SUBSTRING(icFirstLine,liCnt + 1)
               icFirstLine = ENTRY(1,icFirstLine,"¤").
   ELSE lcAddLine = "".            
               
   PUT STREAM ekirje UNFORMATTED
      "1H"                                   /* SENDER FIRST ROW             */
      icFirstLine
      MY-NL

      xEplForm
      MY-NL.
      
   IF lcAddLine > "" THEN DO liCnt = 1 TO NUM-ENTRIES(lcAddLine,"¤"):
   
      PUT STREAM ekirje UNFORMATTED
        (IF liCnt = 1 THEN "0" ELSE " ")
        "J" 
        FILL(" ",10)
        ENTRY(liCnt,lcAddLine,"¤")
        MY-NL.
   END.
   

   PUT STREAM ekirje UNFORMATTED
      "2H"                                   /* RECEIVER FIRST ROW          */
      lcEPLRName                             /* RECEIVER NAME               */
      MY-NL

      " H"
      lcEPLRCoName
      MY-NL

      " H"                                   /* RECEIVER EXTRA ROW          */
      lcEPLRAddr                             /* RECEIVER ADDRESS             */
      MY-NL

      " H"                                   /* RECEIVER EXTRA ROW          */
      (IF LOOKUP(lcEPLACountry,",FI") = 0 AND
       lcEPLRCountry > "" 
       THEN lcEPLACountry + "-"
       ELSE "") 
      lcEPLRPost                             /* RECEIVER POSTAL ADDR        */
      MY-NL.

   if LOOKUP(lcEPLRCountry,",FI,FIN,FINLAND") = 0 THEN 
   PUT STREAM ekirje UNFORMATTED
      " H"                                   /* RECEIVER EXTRA ROW           */
      lcEPLRCountry 
      MY-NL.

END FUNCTION.

/* customer (receiver) data */
FUNCTION fEPLCustHeader RETURNS LOGICAL
    (icFirstLine AS CHAR).

   /* zip code without country prefix */
   IF lcEPLRZipCode BEGINS lcEPLACountry
   THEN lcEPLRZipCode = SUBSTRING(lcEPLRZipCode,3). 
   
   PUT STREAM ekirje UNFORMATTED
      "EPLK"                                 /* NEW LETTER/RECEIVER          */
      STRING(lcEPLACountry,"X(2)")           /* COUNTRYCODE (FI = FINLAND)   */
      STRING(lcEPLRZipCode,"X(8)")           /* RECEIVER POSTALNUMBER        */
      "1"                                    /* NUMBER OF COPIES             */
      "00"                                   /* STANDARD                     */
      STRING(lcAttach1,"X")                  /* attachment 1                 */
      STRING(lcAttach2,"X")                  /* attachment 2                 */
      STRING(lcAttach3,"X")                  /* attachment 3                 */
      MY-NL.
      
   fEPLReceiver(icFirstLine).

   liSheets = 1.
   
END FUNCTION.
   
FUNCTION fCoverSheet RETURNS LOGIC
   (icCoverType AS CHAR,
    iiLanguage  AS INT):

   ASSIGN lcCoverTitle = ""
          lcCoverTxt   = ""
          lcCoverSheet = "".
          
   /* no need to save this into e.g. a temp-table, because cover sheet
      is used only for single printouts (i.e. for one customer at a time) */
   FOR FIRST InvText NO-LOCK WHERE
             InvText.Brand     = gcBrand     AND
             InvText.Target    = "General"   AND
             InvText.KeyValue  = icCoverType AND
             InvText.FromDate <= TODAY       AND
             InvText.ToDate   >= TODAY       AND
             InvText.Language  = iiLanguage:

      ASSIGN lcCoverSheet = IF InvText.EPLForm > ""
                            THEN "EPL" + InvText.EPLForm
                            ELSE ""
             lcCoverTitle = InvText.TxtTitle
             lcCoverTxt   = InvText.InvText.
   END.   

END.



