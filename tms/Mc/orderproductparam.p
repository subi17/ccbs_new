/* ----------------------------------------------------------------------
  MODULE .......: orderproductparam.P
  TASK .........: ORDER PRODUCT PARAMS
  APPLICATION ..: TMS
  AUTHOR .......: srvuddan
  CREATED ......: 23-07-2018
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i} 

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhOrderProductParam AS HANDLE NO-UNDO.
   lhOrderProductParam = BUFFER OrderProductParam:HANDLE.
   RUN StarEventInitialize(lhOrderProductParam).
               
ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhOrderProductParam).
   END.
END.

DEF INPUT PARAMETER iiOrderID AS INT  NO-UNDO.
DEF INPUT PARAMETER iiOrderProductID AS INT  NO-UNDO.

DEF /* NEW */ shared VAR siirto       AS CHAR.

DEF VAR OrderProductParam LIKE OrderProductParam.OrderProductID NO-UNDO.
DEF VAR xrecid       AS RECID init ?.
DEF VAR FIRSTrow     AS INT   NO-UNDO init 0.
DEF VAR FrmRow       AS INT   NO-UNDO init 1.
DEF VAR FrmDown      AS INT   NO-UNDO init 15.
DEF VAR order        AS INT   NO-UNDO init 1.
DEF VAR orders       AS CHAR  NO-UNDO.
DEF VAR maxOrder     AS INT   NO-UNDO init 2.
DEF VAR ufkey        AS LOG   NO-UNDO init TRUE.
DEF VAR delrow       AS INT   NO-UNDO init 0.
DEF VAR pr-order     AS INT   NO-UNDO.
DEF VAR Memory       AS RECID NO-UNDO.
DEF VAR RowNo        AS INT   NO-UNDO.
DEF VAR must-print   AS LOG   NO-UNDO.
DEF VAR must-add     AS LOG   NO-UNDO.
DEF VAR ac-hdr       AS CHAR  NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24 NO-UNDO.
DEF VAR i            AS INT   NO-UNDO.
DEF VAR ok           AS log   format "Yes/No" NO-UNDO.

DEF VAR lcHeader     AS CHAR  NO-UNDO.

DEF TEMP-TABLE ttOrdProductParam NO-UNDO LIKE OrderProductParam
   FIELD DbRec       AS INT
   FIELD ParamValue  AS CHAR
   INDEX OrderProductID OrderProductID
   INDEX OrderID OrderID.
   
DEF BUFFER bttOrdProductParam FOR ttOrdProductParam.   

FORM
    ttOrdProductParam.ParamName   FORMAT "x(24)"   
    ttOrdProductParam.ParamValue  FORMAT "x(24)"   
    WITH OVERLAY ROW FrmRow WIDTH 80 CENTERED SCROLL 1 FrmDown DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " PRODUCT PARAMS OF ORDER PRODUCTID:" + STRING(iiOrderProductID) + " " FRAME sel.

FORM
    ttOrdProductParam.OrderID        COLON 20
    ttOrdProductParam.OrderProductID COLON 20 FORMAT ">>>>>>>9" COLUMN-LABEL "ProductID"
    ttOrdProductParam.ParamName      COLON 20 FORMAT "x(24)"       
    ttOrdProductParam.ValueType      COLON 20       
    ttOrdProductParam.CharValue      COLON 20
    ttOrdProductParam.DateValue      COLON 20
    ttOrdProductParam.DecValue       COLON 20
    ttOrdProductParam.IntValue       COLON 20    
    WITH  OVERLAY ROW 4 CENTERED
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

IF iiOrderID > 0 THEN ASSIGN 
   order = 1
   maxorder = 1
   FrmRow = 3
   FrmDown = 10.
ELSE IF iiOrderProductID > 0 THEN ASSIGN
   order = 2
   maxorder = 1
   FrmRow = 3
   FrmDown = 10.

IF iiOrderID > 0 AND iiOrderProductID > 0 THEN DO:
   FOR EACH OrderProductParam WHERE
            OrderProductParam.orderid = iiOrderID AND
            OrderProductParam.OrderProductID = iiOrderProductID
            NO-LOCK:
       CREATE ttOrdProductParam.
       BUFFER-COPY OrderProductParam to ttOrdProductParam.
       ttOrdProductParam.DbRec = RECID(OrderProductParam).
       CASE ttOrdProductParam.ValueType:
          WHEN "CHAR" THEN ASSIGN ttOrdProductParam.ParamValue = STRING(ttOrdProductParam.CharValue).
          WHEN "INT" THEN ASSIGN ttOrdProductParam.ParamValue = STRING(ttOrdProductParam.IntValue).
          WHEN "DEC" THEN ASSIGN ttOrdProductParam.ParamValue = STRING(ttOrdProductParam.DecValue).
          WHEN "DATE" THEN ASSIGN ttOrdProductParam.ParamValue = STRING(ttOrdProductParam.DateValue).
       END CASE.  
   END. 
END.  

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. 
ASSIGN 
    Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

RUN local-find-first.
IF AVAILABLE ttOrdProductParam THEN 
   ASSIGN
      Memory     = recid(ttOrdProductParam)
      must-print = TRUE
      must-add   = FALSE.
ELSE DO:
   MESSAGE "No product params available !" VIEW-AS ALERT-BOX.
   ASSIGN
      Memory     = ?
      must-print = FALSE
      must-add   = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:
   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.

   IF must-add THEN 
   DO:  /* Add a OrderProductParam  */
       ASSIGN 
          Syst.Var:cfc = "lis" 
          ufkey        = true 
          ac-hdr       = " ADD " 
          must-add     = FALSE.
       RUN Syst/ufcolor.p.
       ADD-ROW:
       REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
           
          PAUSE 0 NO-MESSAGE.
          Syst.Var:ehto = 9. RUN Syst/ufkey.p.
            
          REPEAT TRANSACTION WITH FRAME lis
          ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW:
            
             CLEAR FRAME lis NO-PAUSE.

             PROMPT-FOR ttOrdProductParam.OrderProductID
                VALIDATE(ttOrdProductParam.OrderProductID NOT ENTERED OR
                NOT CAN-FIND(ttOrdProductParam WHERE
                   ttOrdProductParam.OrderProductID = INPUT FRAME lis ttOrdProductParam.OrderProductID ),
                   "OrderProductParams " + string(INPUT ttOrdProductParam.OrderProductID) +
                   " already exists !").
             IF INPUT ttOrdProductParam.OrderProductID NOT ENTERED THEN 
             LEAVE add-row.
               
             CREATE ttOrdProductParam.
             ASSIGN
                ttOrdProductParam.OrderProductID = INPUT FRAME lis 
                                    ttOrdProductParam.OrderProductID.

             RUN local-UPDATE-record.

             IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
                UNDO add-row, LEAVE add-row.

             IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhOrderProductParam).
                ASSIGN
                   Memory = recid(ttOrdProductParam)
                   xrecid = Memory.
             LEAVE.
          END.
       END.  /* ADD-ROW */
        
       HIDE FRAME lis NO-PAUSE.
       ASSIGN 
          must-print = TRUE.

       /* is there ANY record ? */
       FIND FIRST ttOrdProductParam WHERE 
                  ttOrdProductParam.OrderProductID = iiOrderProductID
                  NO-LOCK NO-ERROR.
       IF NOT AVAILABLE ttOrdProductParam THEN LEAVE LOOP.
       NEXT LOOP.
    END.
    
PrintPage:
   DO:
      IF must-print THEN DO:
         UP FRAME-LINE - 1.
         FIND ttOrdProductParam WHERE recid(ttOrdProductParam) = Memory NO-LOCK NO-ERROR.

         /* DISPLAY one page beginning the record 
         whose RECID is saved into 'Memory'.
         starting from ROW 'delrow' */

         /* IF a ROW was recently DELETEd ... */
         IF delrow > 0 THEN DOWN delrow - 1.

         REPEAT WITH FRAME sel:
            IF AVAILABLE ttOrdProductParam THEN DO:
               RUN local-disp-row.
               rtab[FRAME-LINE] = recid(ttOrdProductParam).
               RUN local-find-NEXT.
            END.
            ELSE DO:
               CLEAR NO-PAUSE.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
         up FRAME-LINE - 1.
         DOWN FIRSTrow.
         ASSIGN 
            FIRSTrow   = 0
            must-print = FALSE.
         PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */
    
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN 
      delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
            Syst.Var:ufk[1]= 0 Syst.Var:ufk[2]= 0 Syst.Var:ufk[3]= 0  Syst.Var:ufk[4]= 0
            Syst.Var:ufk[5]= 0 Syst.Var:ufk[6]= 0 Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
            Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.
      
      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttOrdProductParam.ParamName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) ttOrdProductParam.ParamName WITH FRAME sel.
      END. 
     /* ELSE IF order = 2 THEN DO:
         CHOOSE ROW ttOrdProductParam.ParamName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) ttOrdProductParam.ParamName WITH FRAME sel.
      END. */ 

      Syst.Var:nap = keylabel(LASTKEY).
    
      IF rtab[FRAME-LINE] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.   
      END. 

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 AND maxorder > 1 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 AND maxorder > 1 THEN DO:
         order = order - 1. IF order = 0 THEN order = maxOrder.
      END.
      
      IF order <> pr-order AND MaxOrder > 1 THEN DO:
         ASSIGN 
            FIRSTrow = 0 
            Memory   = rtab[FRAME-LINE].
         FIND ttOrdProductParam WHERE recid(ttOrdProductParam) = Memory NO-LOCK.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-PREV.
            IF AVAILABLE ttOrdProductParam THEN
               ASSIGN FIRSTrow = i Memory   = recid(ttOrdProductParam).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.  
      
      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            RUN local-find-this(FALSE).           
            RUN local-find-PREV.
            IF NOT AVAILABLE ttOrdProductParam THEN DO:
               MESSAGE "YOU ARE ON THE FIRST ROW !".
               BELL. PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
              /* PREVious was found */
               SCROLL DOWN.
               RUN local-disp-row.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
                  rtab[1] = recid(ttOrdProductParam)
                  Memory  = rtab[1].
            END.
        END.
         ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND ttOrdProductParam WHERE RECID(ttOrdProductParam) = rtab[FRAME-DOWN] NO-LOCK .
            RUN local-find-NEXT.
            IF NOT AVAILABLE ttOrdProductParam THEN DO:
               MESSAGE "YOU ARE ON THE LAST ROW !".
               BELL. PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
              /* NEXT ROW was found */
               SCROLL UP.
               RUN local-disp-row.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(ttOrdProductParam).
               /* save RECID of uppermost ROW */
               Memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT ROW */
      
      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
         Memory = rtab[1].
         FIND ttOrdProductParam WHERE recid(ttOrdProductParam) = Memory NO-LOCK NO-ERROR.
         RUN local-find-PREV.
         IF AVAILABLE ttOrdProductParam THEN DO:
            Memory = recid(ttOrdProductParam).

           /* reverse 1 page */
            DO RowNo = 1 TO (FRAME-DOWN - 1):
               RUN local-find-PREV.
               IF AVAILABLE ttOrdProductParam THEN Memory = recid(ttOrdProductParam).
               ELSE RowNo = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
           /* is this the very FIRST record of the table ?  */
            MESSAGE "YOU ARE ON THE FIRST PAGE !".
            BELL. PAUSE 1 NO-MESSAGE.
         END.
      END. /* PREVious page */
      
      /* NEXT page */
      ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
         /* PUT Cursor on downmost ROW */
         IF rtab[FRAME-DOWN] = ? THEN DO:
            MESSAGE "YOU ARE ON THE LAST PAGE !".
            BELL. PAUSE 1 NO-MESSAGE.
         END.
         ELSE DO: /* downmost ROW was NOT empty*/
            Memory = rtab[FRAME-DOWN].
            FIND ttOrdProductParam WHERE recid(ttOrdProductParam) = Memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
         END.
      END. /* NEXT page */
      
      ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
         REPEAT WITH FRAME lis TRANSACTION
         ON ENDKEY UNDO, LEAVE:
         
         /* change */
         {Syst/uright2.i}
         RUN local-find-this(FALSE).
         ASSIGN ac-hdr = " VIEW " ufkey = TRUE Syst.Var:ehto = 9.
         Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
         RUN local-disp-lis.
        
         IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
         KEYLABEL(LASTKEY) = "F4" THEN UNDO, LEAVE.
        
         RUN local-disp-row.
          
         xrecid = recid(OrderProductParam).
         LEAVE.
         
      END.
      
      ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
         RUN local-find-FIRST.
         ASSIGN Memory = recid(ttOrdProductParam) must-print = TRUE.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
         RUN local-find-LAST.
         ASSIGN Memory = recid(ttOrdProductParam) must-print = TRUE.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

PROCEDURE local-find-this:
   DEF INPUT PARAMETER exlock AS lo NO-UNDO.

   IF exlock THEN
      FIND ttOrdProductParam WHERE recid(ttOrdProductParam) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK NO-ERROR.
    ELSE
       FIND ttOrdProductParam WHERE recid(ttOrdProductParam) = rtab[frame-line(sel)] 
       NO-LOCK NO-ERROR.
END PROCEDURE.
   
PROCEDURE local-find-FIRST:
   IF order = 1 THEN DO:
      IF iiOrderID > 0 AND iiOrderProductID > 0 THEN 
         FIND FIRST ttOrdProductParam WHERE 
                    ttOrdProductParam.OrderID = iiOrderID AND
                    ttOrdProductParam.OrderProductID = iiOrderProductID
                    NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST ttOrdProductParam USE-INDEX OrderProductID NO-LOCK NO-ERROR.
   END.         
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN DO:
      IF iiOrderID > 0 AND iiOrderProductID > 0 THEN 
         FIND LAST ttOrdProductParam WHERE 
                    ttOrdProductParam.OrderID = iiOrderID AND
                    ttOrdProductParam.OrderProductID = iiOrderProductID
                    NO-LOCK NO-ERROR.
      ELSE
         FIND LAST ttOrdProductParam USE-INDEX OrderProductID NO-LOCK NO-ERROR.
   END.    
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN DO:
      IF iiOrderID > 0 AND iiOrderProductID > 0 THEN 
         FIND NEXT ttOrdProductParam WHERE 
                    ttOrdProductParam.OrderID = iiOrderID AND
                    ttOrdProductParam.OrderProductID = iiOrderProductID
                    NO-LOCK NO-ERROR.
      ELSE
         FIND NEXT ttOrdProductParam USE-INDEX OrderProductID NO-LOCK NO-ERROR.
   END.            
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN DO:
      IF iiOrderID > 0 AND iiOrderProductID > 0 THEN 
         FIND PREV ttOrdProductParam WHERE 
                    ttOrdProductParam.OrderID = iiOrderID AND
                    ttOrdProductParam.OrderProductID = iiOrderProductID
                    NO-LOCK NO-ERROR.
      ELSE
         FIND PREV ttOrdProductParam USE-INDEX OrderProductID NO-LOCK NO-ERROR.
   END.     
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY ttOrdProductParam.ParamName
               ttOrdProductParam.ParamValue
               WITH FRAME sel.  
END PROCEDURE.

PROCEDURE local-disp-lis:
   
   RUN local-find-others.
   CLEAR FRAME lis NO-PAUSE.
   DISPLAY
      ttOrdProductParam.OrderID        
      ttOrdProductParam.OrderProductID 
      ttOrdProductParam.ParamName             
      ttOrdProductParam.ValueType          
      ttOrdProductParam.CharValue      
      ttOrdProductParam.DateValue     
      ttOrdProductParam.DecValue       
      ttOrdProductParam.IntValue           
      with frame lis. 
    
END PROCEDURE.    

PROCEDURE local-find-others:
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
         ttOrdProductParam.ParamName
         ttOrdProductParam.ValueType
         ttOrdProductParam.CharValue
         ttOrdProductParam.DateValue  
         ttOrdProductParam.DecValue
         ttOrdProductParam.IntValue
         WITH FRAME lis.
      
      UPDATE
         ttOrdProductParam.ParamName
         ttOrdProductParam.ValueType
         ttOrdProductParam.CharValue
         ttOrdProductParam.DateValue  
         ttOrdProductParam.DecValue
         ttOrdProductParam.IntValue
         WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.