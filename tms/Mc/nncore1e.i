/* nncore1e.i     10.02.2002/aam
                  08.12.2003/aam price with and without vat
                  13.05.2005/aam 4 chr left margin 

*/


IF licalask <= 6 then do:
    {erepgen.i}
        
    put stream eKirje unformatted 
    " 8" 
    space(54)
    lcFeeHead[14] format "x(8)"                      /* from */
    space(3)
    lcFeeHead[15] format "x(8)"                      /* to   */
    space(2)
    lcFeeHead[19] format "x(5)"                      /* tax% */
    space(2)
    lcFeeHead[16] format "x(10)"                     /* price */
    MY-NL
    
    " 8" 
    space(54)
    lcFeeHead[4] format "x(8)"                      /* from */
    space(3)
    lcFeeHead[5] format "x(8)"                      /* to   */
    space(2)
    lcFeeHead[9] format "x(5)"                      /* tax% */
    space(2)
    lcFeeHead[6] format "x(10)"                     /* price */
    MY-NL.
    
    assign licalask = licalask + 2.
end.

