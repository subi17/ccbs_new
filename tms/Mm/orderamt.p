/* CREATED  13.03.06 kl
   MODIFIED 14.03.06 vk added the asking of password
            20.03.06 kl 2nd input parameter
            16.04.07/aam icSkipValue, icRunParam
            31.10.07 jp  new parameter for msrequest
                         
*/

DEF INPUT-OUTPUT PARAMETER llQueRights  AS LOGICAL   NO-UNDO.
DEF INPUT        PARAMETER lcTable      AS CHARACTER NO-UNDO.
DEF INPUT        PARAMETER icSkipValue  AS CHAR      NO-UNDO.
DEF INPUT        PARAMETER icRunParam   AS CHAR      NO-UNDO.

DEF VAR lcPassword   AS CHAR  NO-UNDO.
DEF VAR llHasRights  AS LOG   NO-UNDO.
DEF VAR lcAskPassWd  AS CHAR  NO-UNDO.  
DEF VAR lcValue      AS CHAR  NO-UNDO.
DEF VAR liParam      AS INT   NO-UNDO EXTENT 3.
DEF VAR liCount      AS INT   NO-UNDO.

{Syst/commali.i}   
{Func/cparam2.i}

    /* If the rights to use this module have been already cheked and */
    /* accepted, there is no meaning to ask it again.                */
    IF NOT llQueRights THEN DO:
        lcPassword = fCParamC("AdminUser").
        IF lcPassword = ? THEN lcPassword = "". 
          
        IF lcPassword > "" THEN DO:
           lcAskPassWd = "".
            PAUSE 0.
            UPDATE lcAskPassWd 
                BLANK
                FORMAT "X(20)" 
                LABEL "Password"
                HELP "Password for counting the queues"
            WITH OVERLAY ROW 10 CENTERED TITLE " COUNT QUEUES "
                SIDE-LABELS FRAME fPassword.
            llqUErIGHTS = (lcAskPassWd = lcPassword). 
        END.
        ELSE DO:
            ASSIGN llQueRights = TRUE.
        END.
    END.

    IF llQueRights THEN DO:
       
       HIDE FRAME fPassword.
       ehto = 5.
       RUN Syst/ufkey.
    
       IF lcTable = "Order" THEN DO:
   
          RUN Mm/orderamtbr(INPUT   "Order",
                         INPUT   "StatusCode",
                         INPUT   "",
                         INPUT   "Orders",
                         OUTPUT  lcValue).

          IF lcValue > "" THEN RUN Mc/order.p(1,8,lcValue,0).

       END.

       ELSE IF lcTable = "MsRequest" THEN DO:
       
          RUN Mm/orderamtbr(INPUT   "MsRequest",
                         INPUT   "ReqStatus",
                         INPUT   icSkipValue,
                         INPUT   icRunParam,
                         OUTPUT  lcValue).

          IF lcValue > "" THEN DO:
 
             DO liCount = 1 TO NUM-ENTRIES(icRunParam,";"):
                IF liCount <= 3 THEN 
                   liParam[liCount] = INTEGER(ENTRY(liCount,icRunParam,";"))
                   NO-ERROR.
             END.
      
             RUN Mm/msrequest(liParam[1],
                           INTEGER(lcValue),
                           liParam[2],
                           liParam[3],
                           0,
                           "").
          END.
       END.
       
    END.
    ELSE DO:
       MESSAGE "You don't have rights to do this operation." VIEW-AS ALERT-BOX.        HIDE FRAME fPassword.
    END.    
