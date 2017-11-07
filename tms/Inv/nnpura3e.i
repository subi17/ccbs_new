/* nnpura3e.i     07.12.2001/aam
                  17.01.2002/aam your price and discount removed,
                                 use font 1
                  27.10.2003/aam vat header to product level               
                  18.12.2003/aam row headers on 2 lines
                  11.04.2005/aam mpm and service added
*/


IF licalask <= 6 then do:
    {Inv/erepgen.i}
        
    put stream eKirje unformatted 
    " I" 
    space(6)
    lcASubHeader[13] format "x(20)"                    /* ccn         */
    space(8)
    lcASubHeader[14] format "x(8)"                     /* duration    */
    space(1)
    lcASubHeader[15] format "x(7)"                     /* data amount */
    space(4)      
    lcASubHeader[16] format "x(3)"                     /* qty         */
    lcASubHeader[19] format "x(11)"                    /* mpm         */ 
    space(1)
    lcASubHeader[20] format "x(11)"                    /* service     */ 
    space(1)
    lcASubHeader[17] format "x(11)"                    /* total       */ 
    MY-NL

    " I" 
    space(6)
    lcASubHeader[3] format "x(20)"                    /* ccn         */
    space(8)
    lcASubHeader[4] format "x(8)"                     /* duration    */
    space(1)
    lcASubHeader[5] format "x(7)"                     /* data amount */
    space(4)
    lcASubHeader[6] format "x(3)"                     /* qty         */
    lcASubHeader[9] format "x(11)"                    /* mpm         */ 
    space(1)
    lcASubHeader[10] format "x(11)"                   /* service     */ 
    space(1)
    lcASubHeader[7] format "x(11)"                    /* total       */ 
    MY-NL
    " I" 
    MY-NL.
    
    assign licalask = licalask + 3.
end.
