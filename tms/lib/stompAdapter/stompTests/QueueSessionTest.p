/* Copyright (c) 2009 Flusso B.V.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE. 
*/
USING javax.jms.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{javax/jms/Session.i}

PROCEDURE testConstructorError:
  DEFINE VARIABLE queueSession AS QueueSession NO-UNDO.
  queueSession = NEW nl.flusso.stomp.QueueSession('mqhost', 0, 'user':U, 'password':U, {&CLIENT_ACKNOWLEDGE}).

  FINALLY:
    IF VALID-OBJECT(queueSession) THEN
      DELETE OBJECT queueSession.
  END FINALLY.
END PROCEDURE.

PROCEDURE testConstructorSuccess:
    DEFINE VARIABLE queueSession AS QueueSession NO-UNDO.
    queueSession = NEW nl.flusso.stomp.QueueSession('mqhost':U, 61613, ?, ?, {&CLIENT_ACKNOWLEDGE}).
    
    FINALLY:
      IF VALID-OBJECT(queueSession) THEN
        DELETE OBJECT queueSession.
    END FINALLY.
END PROCEDURE.

Logger:debugLogManager().
RUN testConstructorSuccess.

CATCH anyError AS Progress.Lang.SysError:
  DEFINE VARIABLE messageNr   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE messageDesc AS CHARACTER   NO-UNDO.
  DO messageNr = 1 TO anyError:NumMessages:
    messageDesc = messageDesc 
                + (IF messageDesc = '':U THEN '':U ELSE '~n':U)
                + anyError:GetMessage(messageNr).
  END.
  
  MESSAGE messageDesc
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END CATCH.
