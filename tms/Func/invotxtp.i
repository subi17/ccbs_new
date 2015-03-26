/************************************************************
 MODULE .......: invotxtp.i
 FUNCTION .....: FUNCTIONS FOR GETTING TEXTS FOR INVOICE
 APPLICATION ..: MASTER
 CREATED ......: 17.05.02 aam
 CHANGED ......: 25.09.02 jr  Added Language limit to fGetInvoTxt
                 25.09.02 jr  fSeparateInvoTxt returns now chr(9)
                              separated string instead of printing
                 17.03.03 aam new functions fCollInvTxt & fCollRowTexts,
                              fGetInvoTxt creates ttInvoTxt,
                              EPLForm
                 28.03.03 aam fSeparateInvoTxt corrected 
                 05.09.03 aam brand,
                              "General"
                 22.10.03 aam fCollRemTxt             
                 16.02.04 aam fSeparateInvoTxt into own include fdivtxt.i
                 02.03.04 aam texts for deposit invoices
                 07.05.04 aam MainTitle
                 22.03.05 aam use bFeeCust for SingleFee

*************************************************************/

DEF VAR liITOrd  AS INT  NO-UNDO. 

DEF BUFFER bFeeCust FOR Customer.

DEF TEMP-TABLE ttInvoTxt NO-UNDO
   FIELD ITPos   AS INT
   FIELD ITKey   AS CHAR
   FIELD ITOrd   AS INT
   FIELD ITTarg  AS CHAR
   FIELD ITTxt   AS CHAR
   FIELD ITNum   AS INT 
   FIELD ITLevel AS INT
   FIELD ITMain  AS CHAR
   FIELD ITTitle AS CHAR
   FIELD ITForm  AS CHAR
   INDEX ITPos ITPos
   INDEX ITNum ITNum. 

/* get all valid texts for one target */ 
FUNCTION fGetInvoTxt RETURNS LOGICAL
    (iTarget    AS CHAR,
     iKey       AS CHAR,
     iReport    AS INT,
     idLevel    AS DEC,
     iDate      AS Date,
     iLang      AS INT,
     iITKey     AS CHAR).

    DEF VAR lcLevel AS CHAR NO-UNDO.
    
    FOR EACH InvText NO-LOCK WHERE
             InvText.Brand     = gcBrand AND
             InvText.Target    = iTarget AND
             InvText.KeyValue  = iKey    AND
             InvText.FromDate <= iDate   AND
             InvText.ToDate   >= iDate   AND
             InvText.Language  = iLang   AND
             InvText.Report    = iReport:

       IF CAN-FIND(FIRST ttInvoTxt WHERE 
                         ttInvoTxt.ITNum = InvText.ITNum)
       THEN NEXT. 

       /* reminders */
       IF iReport = 1 THEN DO:
       
          lcLevel = REPLACE(STRING(idLevel),",",".").
          
          /* reminder level */
          IF LOOKUP(lcLevel,InvText.CStateList) = 0 THEN NEXT.
          
          
       END.   
       
       /* text */
       IF InvText.InvText > "" OR InvText.TxtTitle > "" THEN DO:
          CREATE ttInvoTxt.
          ASSIGN ttInvoTxt.ITPos   = IF InvText.InfoType = "CS"
                                     THEN -1
                                     ELSE InvText.Position
                 ttInvoTxt.ITOrd   = liITOrd
                 liITOrd           = liITOrd + 1
                 ttInvoTxt.ITKey   = iITKey
                 ttInvoTxt.ITNum   = InvText.ITNum 
                 ttInvoTxt.ITTarg  = InvText.Target
                 ttInvoTxt.ITLevel = InvText.RemLevel
                 ttInvoTxt.ITForm  = InvText.EPLForm
                 ttInvoTxt.ITMain  = InvText.MainTitle
                 ttInvoTxt.ITTitle = InvText.TxtTitle
                 ttInvoTxt.ITTxt   = InvText.InvText. 
       END. 
    END.

END. 

/* get all valid texts for one target */ 
FUNCTION fGetReminderTxt RETURNS LOGICAL
    (idLevel    AS DEC,
     iDate      AS DATE,
     iLang      AS INT).

    DEF VAR lcLevel AS CHAR NO-UNDO.

    /* reminder level */
    lcLevel = REPLACE(STRING(idLevel),",",".").

    FOR EACH InvText NO-LOCK WHERE
             InvText.Brand     = gcBrand    AND
             InvText.Target    = "general"  AND
             InvText.KeyValue  BEGINS "rem" AND
             InvText.FromDate <= iDate      AND
             InvText.ToDate   >= iDate      AND
             InvText.Language  = iLang      AND
             InvText.Report    = 1:

       IF CAN-FIND(FIRST ttInvoTxt WHERE 
                         ttInvoTxt.ITNum = InvText.ITNum)
       THEN NEXT. 

       /* reminder level */
       IF LOOKUP(lcLevel,InvText.CStateList) = 0 THEN NEXT.
          
       /* text */
       IF InvText.InvText > "" OR InvText.TxtTitle > "" THEN DO:
          CREATE ttInvoTxt.
          ASSIGN ttInvoTxt.ITPos   = IF InvText.InfoType = "CS"
                                     THEN -1
                                     ELSE InvText.Position
                 ttInvoTxt.ITOrd   = liITOrd
                 liITOrd           = liITOrd + 1
                 ttInvoTxt.ITNum   = InvText.ITNum 
                 ttInvoTxt.ITTarg  = InvText.Target
                 ttInvoTxt.ITLevel = InvText.RemLevel
                 ttInvoTxt.ITForm  = InvText.EPLForm
                 ttInvoTxt.ITMain  = InvText.MainTitle
                 ttInvoTxt.ITTitle = InvText.TxtTitle
                 ttInvoTxt.ITTxt   = InvText.InvText. 
       END. 
    END.

END. 



/* go through relevant targets and collect texts to a temp-table */
FUNCTION fCollInfoTxt RETURNS LOGICAL
   (iiReport AS INT,
    idLevel  AS DEC,
    icKey    AS CHAR,
    idtDate  AS DATE).

   DEF VAR liCollCnt AS INT NO-UNDO.
   
   EMPTY TEMP-TABLE ttInvoTxt. 
   liITOrd = 0.

   /* deposit invoices have texts of their own */
   IF (Invoice.InvType = 3 OR Invoice.InvType = 4) AND 
      iiReport = 0 THEN DO:

      /* text for cover sheet */
      icKey = "".
      FOR FIRST SingleFee NO-LOCK WHERE
               SingleFee.InvNum = Invoice.InvNum:
         /* result of credit rating is stored in calcobj */       
         icKey = fCParamC((IF Invoice.InvType = 3 THEN "Depo" ELSE "AdvPaym") +
                          "Txt" + STRING(SingleFee.CalcObj)).
      END.    
        
      IF icKey = ? THEN icKey = "".
      
      if icKey > "" then icKey = icKey + ",".
      
      /* text for actual invoice */
      icKey = icKey + (IF Invoice.InvType = 3 THEN "Depo" ELSE "AdvPaym") + 
              "Fee".
      
   END.
   
   ELSE IF iiReport = 1 AND idLevel = 2 AND INDEX(icKey,"PP") > 0
   THEN icKey = icKey + ",PaymPlan".   

   IF icKey > "" THEN DO liCollCnt = 1 TO NUM-ENTRIES(icKey):
      fGetInvoTxt("General",
                  ENTRY(liCollCnt,icKey),
                  iiReport,
                  idLevel,
                  idtDate,
                  Customer.Language,
                  "").
   END.
   ELSE DO:
      fGetInvoTxt("General",
                  "",
                  iiReport,
                  idLevel,
                  idtDate,
                  Customer.Language,
                  "").
   END.
    
   IF Invoice.InvType = 3 OR Invoice.InvType = 4 
   THEN RETURN TRUE.
   
   fGetInvoTxt("Invgroup",
                Customer.InvGroup,
                iiReport,
                idLevel,
                idtDate,
                Customer.Language,
                "").

   /* check all groups that customer belongs to */
   FOR EACH CGMember NO-LOCK WHERE
            CGMember.CustNum = Customer.CustNum:

        fGetInvoTxt("Custgroup",
                     CGMember.CustGroup,
                     iiReport,
                     idLevel,
                     idtDate,
                     Customer.Language,
                     "").
   END.

   fGetInvoTxt("Customer",
                STRING(Customer.CustNum),
                iiReport,
                idLevel,
                idtDate,
                Customer.Language,
                "").

   fGetInvoTxt("Salesman",
                Customer.Salesman,
                iiReport,
                idLevel,
                idtDate,
                Customer.Language,
                "").

END FUNCTION.

/* get invoice related texts */
FUNCTION fCollInvTxt RETURNS LOGICAL.

    fCollInfoTxt(0,     
                 0.0,
                 "",
                 Invoice.InvDate).
    
END FUNCTION.

/* get reminder related texts */
FUNCTION fCollRemTxt RETURNS LOGICAL
   (idtDate AS DATE,
    idLevel AS DEC,
    icKey   AS CHAR).

   EMPTY TEMP-TABLE ttInvoTxt. 
   liITOrd = 0.

   fGetReminderTxt(idLevel,
                   idtDate,
                   Customer.Language).
    
END FUNCTION.


FUNCTION fCollRowTxt RETURNS LOGICAL.

   fGetInvoTxt("BillItem",
               InvRow.BillCode,
               0,
               0.0,
               Invoice.InvDate,
               Customer.Language,
               STRING(InvRow.InvRowNum)).

END FUNCTION.

