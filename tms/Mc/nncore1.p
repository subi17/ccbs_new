/* -----------------------------------------------------------------------------
  MODULE .......: NNCORE1.P
  FUNCTION .....: Contract lines by product
  APPLICATION ..: NN
  AUTHOR .......: aam
  CREATED ......: 10.02.2002
  MODIFIED .....: 03.01.2003/aam also bitem-lines are combined by product
                  09.01.2003/aam row headers
                  23.01.2003/jp  ttfee.type 2
                  24.02.2003/jp  fat memo for invoice
                  30.04.2003/aam nncore1.i for common routines 
                  27.10.2003/aam vat headers into product level
                  08.12.2003/aam show unit price both with and without tax
                  16.06.2004/aam LineName
                  13.05.2005/aam 4 chr left margin 
  VERSION ......: M15
  --------------------------------------------------------------------------- */

{commali.i}
{refcode.i}
{transname.i}
{coinv.i}
{cparam2.i}

/* print-linemuuttujat */
{utumaa.i}
{edefine.i}

def input parameter invno  like Invoice.InvNum no-undo.
def input parameter epltul as log              no-undo. 

def var company     as char no-undo.
def var viiva1      as char format "x(117)" no-undo.
def var viiva2      like viiva1.
def var viiva3      like viiva1.
def var viiva4      like viiva1.
def var sl          as int no-undo.
def var rl          as int no-undo.
def var lev         as int no-undo.

def var cper        as int  no-undo.
def var cper2       as int  no-undo.

DEF VAR lcSpecDateHead AS CHAR NO-UNDO.
DEF VAR lcCustName     AS CHAR NO-UNDO. 

{nncore1.i}
{ereppage.i}

assign
lev = IF epltul THEN 88 ELSE 91
viiva1 = fill("=",lev)
viiva2 = fill("=",lev)
viiva3 = fill("-",lev)
viiva4 = fill("-",lev).

assign
   sl       = 0
   rl       = 0.

FIND Invoice WHERE Invoice.InvNum = invno NO-LOCK NO-ERROR.
IF NOT AVAIL Invoice THEN RETURN.

find first Customer of Invoice no-lock no-error.
if not available Customer then return.
lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                              BUFFER Customer).
 
fInitFeeValues().

fSpecGenHeader(Invoice.CustNum,
               Invoice.InvNum,
               Customer.Language).

form header
   viiva1 at 2 skip
   lcCustName at 2 format "x(30)"
      lcFeeHead[1] format "x(35)" at 35 
      lcFeeHead[2] format "x(4)" at 71                  /* Sivu */
      sl format "ZZZZ9" skip
   Invno AT 2
      Invoice.InvDate AT 71 FORMAT "99.99.9999" skip
   viiva2 at 2 skip
   with width 130 no-label no-box frame sivuotsi.

form header
   lcFeeHead[18] format "x(34)" AT 2                     /* product */
   lcFeeHead[14] format "x(8)"  at 52                    /* from */
   lcFeeHead[15] format "x(8)"  at 64                    /* to   */
   lcFeeHead[19] FORMAT "X(5)"  TO 81                    /* tax% */
   lcFeeHead[16] FORMAT "x(10)" TO 92                    /* amt */
   skip
   lcFeeHead[8] format "x(34)" AT 2                     /* product */
   lcFeeHead[4] format "x(8)"  at 52                    /* from */
   lcFeeHead[5] format "x(8)"  at 64                    /* to   */
   lcFeeHead[9] FORMAT "X(5)"  TO 81                    /* tax% */
   lcFeeHead[6] FORMAT "x(10)" TO 92                    /* amt */
   skip
   " " at 2 skip
with width 130 no-label no-box frame sarotsi.


FUNCTION fPaging RETURNS LOGICAL
    (iAddLine AS INT).

   if rl + iAddLine >= skayt1 then do:
      {uprfeed.i rl}
      ASSIGN sl = sl + 1.
      view stream tul frame sivuotsi. rl = 4.
      view stream tul frame sarotsi.  rl = rl + 3. 
   end.

END FUNCTION.

IF NOT epltul THEN DO:
    /* Haetaan raporttitehoste */
    FOR FIRST TMSRepCfg NO-LOCK WHERE
              TMSRepCfg.RepName   = "nncore1" AND 
              TMSRepCfg.UserCode  = "",
        FIRST PrintCodes NO-LOCK WHERE 
              PrintCodes.PrinterId = TMSRepCfg.PrinterId AND
              PrintCodes.Effect    = TMSRepCfg.Effect:

         ASSIGN spit1    = PrintCodes.PageLength
                skayt1   = PrintCodes.AvailLines.
         
         PUT STREAM tul control PrintCodes.EffOn[2].
    END.     
END.


/* company name */
find invgroup of Customer no-lock no-error.
if avail invgroup AND invgroup.CompName NE "" 
then company = invgroup.CompName.
else company = ynimi.
   
/* first collect contract and single fees into temp-table */
fCollectFees().   
                                  
FOR EACH ttFee                                   
BREAK BY ttFee.Type
      BY ttFee.VatPerc
      BY ttFee.Prod 
      BY ttFee.CLI
      BY ttFee.BegPer:

      /* product */
      if first-of(ttFee.Prod) then do:

         /* product name */
         fFeeProdName(Invoice.ToDate).
        
         if epltul then do:

            fNewPage(IF first(ttFee.Prod) THEN 999 ELSE 3).
         
            {nncore1e.i}

            if not first(ttFee.Prod) then do:
                put stream eKirje unformatted
                " I" 
                MY-NL.
                assign licalask = licalask + 1.
            end.
            
            put stream eKirje unformatted
                " I"
                SPACE(4)
                lcFeeProdName
                "   " 
                lcFeeVat 
                MY-NL.
            assign licalask = licalask + 1.
         end.
         else do:
         
            IF FIRST(ttFee.Prod) THEN ASSIGN rl = spit1.
            
            fPaging(3).
            
            if not first(ttFee.Prod) then do:
                put stream tul skip(1).
                assign rl = rl + 1.
            end.
            
            put stream tul unformatted 
                lcFeeProdName AT 2 
                "   "
                lcFeeVat SKIP.
            rl = rl + 1.
         end.
         
      end.
     
      fFeeSetValues().

      if epltul then do:

        fNewPage(1).
        {nncore1e.i}

        put stream eKirje unformatted
            " I" 
            SPACE(4)
            ttFee.LineName FORMAT "X(49)"
            SPACE(1).

        IF ttFee.Type = 0 OR ttfee.type = 2 THEN DO:
        
           IF ttFee.BegDate NE ? THEN put stream eKirje unformatted
              string(ttFee.BegDate,"99.99.99")
              " - ".
           ELSE put stream eKirje unformatted
              SPACE(11).
        
           IF ttFee.EndDate NE ? THEN put stream eKirje unformatted    
              string(ttFee.EndDate,"99.99.99").
           ELSE put stream eKirje unformatted
              SPACE(8).
        END.
        ELSE PUT STREAM eKirje UNFORMATTED 
           SPACE(19). 

        put stream eKirje unformatted    
            SPACE(2)
            STRING(ttFee.VatPerc,">>9 %")
            SPACE(2)
            STRING(ttFee.Amt,"->>>>>9.99")
            MY-NL.
        licalask = licalask + 1.
            
        /*  comment lines */
        DO liFeeCnt = 1 TO 5:
           IF ttFee.Memo[liFeeCnt] = "" OR 
              ttFee.Memo[liFeeCnt] = lcFeeOrgName
           THEN NEXT.

           fNewPage(0).
           {nncore1e.i}

           put stream eKirje unformatted
              " I" 
              space(7)
              STRING(TRIM(REPLACE(ttFee.Memo[liFeeCnt],
                                  lcFeeOrgName,"")),"X(70)")
              MY-NL.
           licalask = licalask + 1.   
        END.
                   
      END.

      else do:
      
        fPaging(2).

        PUT STREAM tul
            ttFee.LineName FORMAT "X(50)" AT 2.
           
        IF ttFee.Type = 0 OR ttfee.Type = 2 THEN DO:
           
           IF ttFee.BegDate NE ? THEN PUT STREAM tul
              string(ttFee.BegDate,"99.99.99") AT 52
              " - ".
            
           IF ttFee.EndDate NE ? THEN PUT STREAM tul
              string(ttFee.EndDate,"99.99.99") AT 64.
        END.

        PUT STREAM tul
            ttFee.VatPerc  FORMAT ">>9 %"         TO 81
            ttFee.Amt      FORMAT "->>>>>9.99"    TO 92
            SKIP.
        rl = rl + 1.    
      
      end.
      
      ACCUMULATE ttFee.Amt      (TOTAL BY ttFee.Prod)
                 ttFee.AmtNoVat (TOTAL BY ttFee.Prod).

      /* sub-total for product */
      if last-of(ttFee.Prod) then do:

         if epltul then do:
            fNewPage(1).
            {nncore1e.i}

            PUT STREAM eKirje UNFORMATTED
                " I"
                SPACE(4)
                viiva4
                my-nl 
                " I"
                SPACE(4)
                lcFeeProdName 
                SPACE(1)
                lcFeeHead[7]
                FILL(" ",75 - LENGTH(lcFeeProdName) - LENGTH(lcFeeHead[7]) - 1)
                STRING((ACCUM TOTAL BY ttFee.Prod ttFee.Amt),
                        "->>>>>>>>9.99")
                MY-NL.
            ASSIGN licalask = licalask + 2.
         end.

         ELSE DO: 
            if rl >= skayt1 - 2 then do:
                {uprfeed.i rl}
                ASSIGN sl = sl + 1.
                view stream tul frame sivuotsi. rl = 4.
                view stream tul frame sarotsi.  rl = rl + 3. 
            END.
            
            PUT STREAM TUL UNFORMATTED  
                viiva4 AT 2 SKIP
                lcFeeProdName + " " + lcFeeHead[7] AT 2 FORMAT "X(57)"
                (ACCUM TOTAL BY ttFee.Prod ttFee.Amt)
                    FORMAT "->>>>>>>>9.99" TO 92
                SKIP.
             ASSIGN rl = rl + 2.
         END.
      END.

      /* GRAND TOTAL */
      if LAST(ttFee.Prod) then do:
          
          if epltul then do:
             fNewPage(2).
             {nncore1e.i}

             PUT STREAM eKirje UNFORMATTED
                 " I"
                 my-nl
                 " I"
                 SPACE(4)
                 viiva1
                 my-nl 
                 " I"
                 SPACE(4)
                 lcFeeHead[7]
                 FILL(" ",75 - LENGTH(lcFeeHead[7]))
                 STRING((ACCUM TOTAL ttFee.Amt),"->>>>>>>>9.99")
                 my-nl. 
             ASSIGN licalask = licalask + 3.
          end.

          ELSE DO: 
             if rl >= skayt1 - 3 then do:
                 {uprfeed.i rl}
                 ASSIGN sl = sl + 1.
                 view stream tul frame sivuotsi. rl = 4.
                 view stream tul frame sarotsi.  rl = rl + 3. 
             END.

             PUT STREAM TUL UNFORMATTED  
                 viiva1 AT 2 SKIP
                 lcFeeHead[7] AT 2 FORMAT "X(57)"
                 (ACCUM TOTAL ttFee.Amt)     
                     FORMAT "->>>>>>>>9.99" TO 92
                 SKIP.
             ASSIGN rl = rl + 3.
             
             {uprfeed.i rl}
          END.
      END.
      
END. 

