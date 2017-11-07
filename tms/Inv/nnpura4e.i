/* nnpura4e.i     07.12.2001/aam
                  10.12.2003/aam unit price etc.
                  22.06.2004/aam pulses
                  12.04.2005/aam mpm and service added,
                                 use font J
*/


IF licalask <= 6 THEN DO:
    {Inv/erepgen.i}

    PUT STREAM eKirje UNFORMATTED 

    " J" 
    SPACE(10)
    lcRep4UHead[1]  format "x(8)"                      /* date           */
    SPACE(1)
    lcRep4UHead[2]  format "x(8)"                      /* start time     */
    SPACE(1)
    lcRep4UHead[3]  format "x(13)"                     /* bdest          */
    SPACE(2)
    lcRep4UHead[4]  format "x(8)"                      /* Duration       */
    SPACE(1)        
    lcRep4UHead[9]  format "x(5)"                      /* pulses         */
    SPACE(1)
    lcRep4UHead[5]  format "x(7)"                      /* data amt       */
    SPACE(1) 
    lcRep4UHead[7]  format "x(8)"                      /* unit price     */
    SPACE(1)
    lcRep4UHead[10] format "x(11)"                     /* mpm            */    
    SPACE(1)
    lcRep4UHead[11] format "x(11)"                     /* service        */    
    SPACE(1)
    lcRep4UHead[8]  format "x(11)"                     /* to pay         */    
    MY-NL   

    " J" 
    SPACE(10)
    lcRep4Head[1]  format "x(8)"                      /* date           */
    SPACE(1)
    lcRep4Head[2]  format "x(8)"                      /* start time     */
    SPACE(1)
    lcRep4Head[3]  format "x(13)"                     /* bdest          */
    SPACE(2)
    lcRep4Head[4]  format "x(8)"                      /* Duration       */
    SPACE(1)
    lcRep4Head[9]  format "x(5)"                      /* pulses         */
    SPACE(1)
    lcRep4Head[5]  format "x(7)"                      /* data amt       */
    SPACE(1) 
    lcRep4Head[7]  format "x(8)"                      /* unit price     */
    SPACE(1)
    lcRep4Head[10] format "x(11)"                     /* mpm            */    
    SPACE(1)
    lcRep4Head[11] format "x(11)"                     /* service        */    
    SPACE(1)
    lcRep4Head[8]  format "x(11)"                     /* to pay         */      
    MY-NL   .

    ASSIGN licalask = licalask + 2.
END.

