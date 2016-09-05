/* -----------------------------------------------------
  MODULE .......: cpbrand.p
  FUNCTION .....: copy data to a new brand from an existing one
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 14.10.03
  MODIFIED .....: 
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF INPUT  PARAMETER icSourceBrand AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icTargetBrand AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER olCopied      AS LOG  NO-UNDO.

DEF VAR lcTable    AS CHAR NO-UNDO.
DEF VAR lcExpl     AS CHAR NO-UNDO.
DEF VAR lcProg     AS CHAR NO-UNDO. 
DEF VAR lcSystem   AS CHAR NO-UNDO. 
DEF VAR lcSystExpl AS CHAR NO-UNDO. 
DEF VAR lcSystProg AS CHAR NO-UNDO. 
DEF VAR liCount    AS INT  NO-UNDO. 
DEF VAR liProc     AS INT  NO-UNDO. 
DEF VAR liPos      AS INT  NO-UNDO. 
DEF VAR llCopy     AS LOG  NO-UNDO. 
DEF VAR lcCopy     AS CHAR NO-UNDO. 
DEF VAR lcToCopy   AS CHAR NO-UNDO. 
DEF VAR lcRun      AS CHAR NO-UNDO. 
DEF VAR lcCond     AS CHAR NO-UNDO. 
DEF VAR liCond     AS INT  NO-UNDO.
DEF VAR lcWhere    AS CHAR NO-UNDO. 

DEF BUFFER bSource FOR Brand.
DEF BUFFER bConf   FOR DpConf.
DEF BUFFER bBasis  FOR DpBasis.

FUNCTION fConfirmCopy RETURNS LOGICAL
   (icTable AS CHAR).

   DEF VAR llConf AS LOG  NO-UNDO.
   DEF VAR lcName AS CHAR NO-UNDO. 

   lcName = ENTRY(LOOKUP(icTable,lcTable),lcExpl).

   IF lcName = "" THEN RETURN TRUE.
   
   REPEAT:
   
      llConf = TRUE.
      
      MESSAGE SKIP
              "Copy all" lcName "from brand" icSourceBrand "?" SKIP(1)
              "Note: Copying process can be stopped with 'Cancel'-choice."
      VIEW-AS ALERT-BOX
      QUESTION
      BUTTONS YES-NO-CANCEL
      TITLE " Copy Data to Brand " + icTargetBrand + " " 
      SET llConf.
   
      IF llConf = ? THEN DO:

         llConf = FALSE.
         
         MESSAGE "Entire copying process will be cancelled, and all already"
                 SKIP
                 "copied data will be deleted from new brand" icTargetBrand
                 "." SKIP(1)
                 "Continue with cancellation ?"
         VIEW-AS ALERT-BOX
         QUESTION
         BUTTONS YES-NO
         TITLE " Cancellation "
         SET llConf.
   
         IF NOT llConf THEN NEXT.
         ELSE llConf = ?.
      END.
      
      LEAVE.
   END.
   
   RETURN llConf.
      
END FUNCTION.

FUNCTION fConfirmUpd RETURNS LOGICAL
   (icTable AS CHAR,
    icName  AS CHAR).

   DEF VAR llConf AS LOG  NO-UNDO.

   llConf = TRUE.
   
   IF icName = "" THEN RETURN FALSE.
   
   MESSAGE icName "have been copied to brand"  icTargetBrand "." SKIP
           "Do You want to modify the copied data ?"
   VIEW-AS ALERT-BOX
   QUESTION
   BUTTONS YES-NO
   TITLE " Update Copied Data "
   SET llConf.
   
   RETURN llConf.
      
END FUNCTION.

FUNCTION fWhereCond RETURNS CHARACTER
   (icTable AS CHAR).

   DEF VAR lcWhereCond AS CHAR NO-UNDO.
    
   liCond = LOOKUP(icTable,lcCond).
   IF liCond > 0
   THEN lcWhereCond = ENTRY(liCond + 1,lcCond).
   ELSE lcWhereCond = "".
   
   RETURN lcWhereCond.
   
END.

ASSIGN olCopied   = FALSE
       
       lcSystem   = "TMSParam,HdrText,MsClass,CLIType"
       lcSystExpl = "System Parameters,Header Texts,,"
       lcSystProg = "Syst/cparam.p,Mc/nnteyp.p,,"
      
       /* tables, that are copied as a group, are joined with "¤" */ 
       lcTable    = "Account,Interest," + 
                    "BillItem¤BItemGroup,InvSect," +
                    "FeeModel¤FMItem,CCN,BDest¤BNet," +
                    "PriceList¤RatePref,RateCCN,RatePlan¤PListConf," +
                    "DiscPlan,PNPGroup," + 
                    "Reseller¤Salesman,InvGroup,CustCat,CustClass," +
                    "DMarketing,Service¤ServPac¤ServEl¤ServCom" 
       lcExpl     = "Accounts,Interest Percents," + 
                    "Billing Items and Item Groups,Invoice Sections," +
                    "Fee Models,CCNs,B-Destinations," + 
                    "Price Lists,Rating CCNs for BDestinations,Rating Plans," +
                    "Discount Plans,PNP Groups," + 
                    "Resellers and Salesmen,Invoicing Groups," +
                    "Customer Categories,Customer Classes," +
                    "Direct Marketing Codes,Mobile Service Parameters"
       lcProg     = "Mc/nnacyp.p,Ar/otint.p," +
                    "Mc/nntuyp.p¤Mc/nnpgyp.p,Mc/invsect.p," +
                    "Mc/bevent.p,Mc/nnmayp.p,Mc/nnbtyp.p¤Mm/bnet.p," + 
                    "Mc/nnplyp.p,rccn,Mc/rateplan.p,Mc/discplan.p,Mc/pnpgroup.p," +
                    "Mc/nnrsyp.p,Mc/nnigyp.p,Mc/nnkayp.p,Mc/custclass.p," +
                    "Mc/dirmark.p,Mm/service.p¤Mm/servpac.p"
       lcCond     = "".

FIND bSource WHERE bSource.Brand = icSourceBrand NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSource THEN DO:
   MESSAGE "Source brand not available."
   VIEW-AS ALERT-BOX.
   RETURN.
END.

FIND Brand WHERE Brand.Brand = icTargetBrand NO-LOCK NO-ERROR.
IF NOT AVAILABLE Brand THEN DO:
   MESSAGE "Target brand not available."
   VIEW-AS ALERT-BOX.
   RETURN.
END.

IF CAN-FIND(FIRST TMSParam WHERE TMSParam.Brand = icTargetBrand) OR
   CAN-FIND(FIRST Company  WHERE Company.Brand  = icTargetBrand)
THEN DO:
   MESSAGE "Target brand already has data."
   VIEW-AS ALERT-BOX.
   RETURN.
END.

/* set default brand for update modules */
ASSIGN gcBrand = icTargetBrand
       ynimi   = Brand.BRName.

/* do all in one transaction (max. few thousand records to copy) 
   -> possibility to undo all */

CopyBrand:
REPEAT TRANSACTION:

   /* tables that will be created with empty data */
   CREATE Company.
   ASSIGN Company.Brand    = icTargetBrand
          Company.CompName = Brand.BRName.
    
   CREATE PaymVouch.
   ASSIGN PaymVouch.Brand  = icTargetBrand.

   /* system tables which will always be copied (copy these first, because
      some update modules may need them) */
   DO liCount = 1 TO NUM-ENTRIES(lcSystem):   

      lcCopy = ENTRY(liCount,lcSystem).
 
      /* copy */
      DO liProc = 1 TO NUM-ENTRIES(lcCopy,"¤"):

         ASSIGN lcToCopy = ENTRY(liProc,lcCopy,"¤").
                lcWhere  = fWhereCond(lcToCopy).
                
         RUN pCopyData(lcToCopy,
                       lcWhere).
      END.
      
      /* modify copied data */
      IF fConfirmUpd(lcCopy,ENTRY(LOOKUP(lcCopy,lcSystem),lcSystExpl))
      THEN DO:

         lcRun = ENTRY(liCount,lcSystProg).
            
         DO liProc = 1 TO NUM-ENTRIES(lcRun,"¤"):
            RUN VALUE(ENTRY(liProc,lcRun,"¤")).
         END.
         
         ehto = 4.
         RUN Syst/ufkey.p.
            
      END.
      
   END.
 
   /* tables which will be copied only if user wants them */  
   DO liCount = 1 TO NUM-ENTRIES(lcTable):

      lcCopy = ENTRY(liCount,lcTable).
      
      /* first ask user what to do */
      llCopy = fConfirmCopy(lcCopy).
      
      /* cancel the process */
      IF llCopy = ? THEN UNDO CopyBrand, LEAVE CopyBrand.
      
      /* skip copying */
      ELSE IF NOT llCopy THEN NEXT.
      
      ELSE DO:
      
         /* copy */
         DO liProc = 1 TO NUM-ENTRIES(lcCopy,"¤"):
            
            ASSIGN lcToCopy = ENTRY(liProc,lcCopy,"¤")
                   lcWhere  = fWhereCond(lcToCopy).

            RUN pCopyData(lcToCopy,
                          lcWhere).

            /* reptexts must be copied if father table has been copied */
            liPos = LOOKUP(lcToCopy,"BillItem,BDest,CCN").
            IF liPos > 0 THEN 
            RUN pCopyData("RepText",
                          "AND RepText.TextType = " + STRING(liPos)).
                                   
            /* discount configuration must be copied with this way;
               dpconf has a unique sequence key and
               dpbasis hasn't got brand as a field 
            */
            IF lcToCopy = "discplan" THEN DO:
            
               FOR EACH DiscPlan NO-LOCK WHERE
                        DiscPlan.Brand = icSourceBrand,
                   EACH DpConf OF DiscPlan NO-LOCK:
                   
                  CREATE bConf.
                  BUFFER-COPY DpConf EXCEPT Brand DpConfNum TO bConf.
                  ASSIGN bConf.Brand     = icTargetBrand
                         bConf.DpConfNum = NEXT-VALUE(DpConfNum).
                          
                  FOR EACH DpBasis OF DpConf NO-LOCK:
                     CREATE bBasis.
                     BUFFER-COPY DpBasis EXCEPT DpConfNum TO bBasis.
                     bBasis.DpConfNum = bConf.DpConfNum.
                  END.
               END.
             
            END.
                                                            
         END.
         
         /* modify copied data */
         IF fConfirmUpd(lcCopy,ENTRY(LOOKUP(lcCopy,lcTable),lcExpl)) 
         THEN DO:
         
            lcRun = ENTRY(liCount,lcProg).
            
            DO liProc = 1 TO NUM-ENTRIES(lcRun,"¤"):
               RUN VALUE(ENTRY(liProc,lcRun,"¤")).
            END.
            
            ehto = 4.
            RUN Syst/ufkey.p.
            
         END.
      END.
      
   END.

   LEAVE CopyBrand.
   
END.  /* copybrand */


olCopied = CAN-FIND(FIRST TMSParam WHERE TMSParam.Brand = icTargetBrand).
   

PROCEDURE pCopyData:
    
    DEF INPUT PARAMETER icTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER icWhere AS CHAR NO-UNDO. 

    DEF VAR lhSource AS HANDLE NO-UNDO.
    DEF VAR lhTarget AS HANDLE NO-UNDO.
    DEF VAR lhFind   AS HANDLE NO-UNDO.
    DEF VAR lhField  AS HANDLE NO-UNDO.
    DEF VAR liField  AS INT    NO-UNDO. 

    CREATE BUFFER lhSource FOR TABLE icTable.
    CREATE BUFFER lhTarget FOR TABLE icTable.
    
    CREATE QUERY lhFind.
    lhFind:SET-BUFFERS(lhSource).
    lhFind:QUERY-PREPARE('FOR EACH ' + ictable + ' NO-LOCK WHERE ' +
                          ictable + '.Brand = "' + icSourceBrand + '" ' +
                          icWhere).
    lhFind:QUERY-OPEN.

    /* find brand-field */
    DO liField = 1 TO 100:
       lhField = lhSource:BUFFER-FIELD(liField).
       IF lhField:NAME = "brand" THEN LEAVE.
    END.

    REPEAT:
        lhFind:GET-NEXT().

        IF lhFind:QUERY-OFF-END THEN LEAVE.
        
        DO TRANS:      
        
           /* create record for target brand and copy data from source */
           lhTarget:BUFFER-CREATE().
           
           lhTarget:BUFFER-COPY(lhSource,"brand").

           lhField = lhTarget:BUFFER-FIELD(liField).

           /* set target brand for copied data */
           lhField:BUFFER-VALUE = icTargetBrand.

        END.
        
    END.

    lhFind:QUERY-CLOSE().
    
    DELETE OBJECT lhFind.
    DELETE OBJECT lhSource. 
    DELETE OBJECT lhTarget.
    
END PROCEDURE.

