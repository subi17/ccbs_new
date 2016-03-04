/*
Copyright (c) 2011-2012, Julian Lyndon-Smith (julian+maia@dotr.com)
http://www.dotr.com
All rights reserved.

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction except as noted below, including without limitation
the rights to use,copy, modify, merge, publish, distribute,
and/or sublicense, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

 The Software and/or source code cannot be copied in whole and
 sold without meaningful modification for a profit.

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.

 Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in
 the documentation and/or other materials provided with
 the distribution.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
*/

def input param p_Stomp as dotr.Stomp.StompConnection no-undo.

def var CurrentFrameBuffer as memptr no-undo.
def var CurrentFrameSize as int no-undo.
def var StompFrame as longchar no-undo.

def var CallObject as handle no-undo.

assign this-procedure:private-data = string(time,"hh:mm:ss").

/** using Call handle so that we can support other platforms by changing the call-name and library */
create call CallObject.

case opsys:
  when "win32" then assign CallObject:call-name = "memchr"
                           CallObject:library   = "msvcrt.dll".

  when "unix" then assign CallObject:call-name = "memchr"
                          CallObject:library   = "libc.so.6".

  otherwise assign CallObject:call-name = "unknown"
                   CallObject:library   = "unknown".
end case.

assign CallObject:call-type                  = dll-call-type
       CallObject:library-calling-convention = "CDECL"
       CallObject:return-value-dll-type      = "LONG"
       CallObject:num-parameters             = 3.

CallObject:set-parameter (2,"long","input",0). /** set the character to look for. In this case, we're looking for null - chr(0) */

procedure SocketReadHandler:

    def var lv_Data as memptr no-undo.

    def var lv_Start          as int no-undo.
    def var lv_Length         as int no-undo.
    def var lv_BytesAvailable as int  no-undo.

    def var lv_DataString as longchar no-undo.

    assign lv_BytesAvailable = self:get-bytes-available()
           self:sensitive    = no.

    if lv_BytesAvailable eq 0 then
    do:
            p_Stomp:DoSocketDisconnected(). /** socket has been disconnected, raise the event in case anyone cares */
            return.
    end.

    set-size(lv_Data) = lv_BytesAvailable.

    self:read(lv_Data, 1, lv_BytesAvailable, read-exact-num) .

    assign lv_BytesAvailable = self:bytes-read /** just in case we didn't get what was expected */
           lv_Start          = 1.

    do while lv_Start lt lv_BytesAvailable:
      assign lv_DataString = get-string(lv_Data, lv_Start)
             StompFrame = StompFrame + lv_DataString
             lv_Length = length(lv_DataString, "RAW")
             lv_Start   = lv_Start + lv_Length.

      if lv_Start le lv_BytesAvailable and get-byte(lv_Data, lv_Start) eq 0 then
      do:
        assign lv_Start   = lv_Start + 1.

        p_Stomp:ProcessData(StompFrame) no-error.

        assign StompFrame = "".
      end.

    end.

    finally:
      set-size(lv_Data) = 0.
      self:sensitive = yes.
    end finally.

end procedure. /* tcp_readhandler */

/** process big data received on the socket
  */

procedure SocketBigReadHandler:
  def var lv_Data  as memptr no-undo. /** data from the socket */

  def var lv_TempFrameBuffer as memptr no-undo.

  def var lv_Start          as int no-undo.
  def var lv_Length         as int no-undo.
  def var lv_BytesAvailable as int no-undo.
  def var lv_Chunk          as int64 no-undo. /** pointer to EOM character (null)  */
  
  def var lv_DataPointer    as int64 no-undo. /** pointer to current position in lv_Data  
  																							* changed to int64 to handle sporadic "value to large for int" errors 
  																							* reported when running on Linux64 
  																							*/

  assign lv_BytesAvailable = self:get-bytes-available() /** get number of bytes to read from socket */
         self:sensitive    = no. /** prevent any further data being recieved until we are done  */

  if lv_BytesAvailable eq 0 then
  do:
        p_Stomp:DoSocketDisconnected(). /** socket has been disconnected, raise the event in case anyone cares */
        return.
  end.

  set-size(lv_Data) = lv_BytesAvailable.

  self:read(lv_Data, 1, lv_BytesAvailable, read-exact-num) . /** load all available data into the memptr */

  assign lv_BytesAvailable = self:bytes-read /** just in case we didn't get what was expected */
         lv_Start          = 1.

  if lv_BytesAvailable eq 0 then return. /** be double-safe ;) */

  do while lv_Start lt lv_BytesAvailable: /** we may have 0..n complete messages and an incomplete message in this data set */
    assign lv_DataPointer = get-pointer-value(lv_Data) + lv_Start - 1. /** find next EOM character from this point forwards */

    CallObject:set-parameter (1,"long","input",lv_DataPointer).                         /** starting at position x */
    CallObject:set-parameter (3,"unsigned-long","input",1 + lv_BytesAvailable - lv_Start).  /** for y characters */

    CallObject:invoke(). /** search for EOM */

    assign lv_Chunk = CallObject:return-value.

    assign lv_length = if lv_Chunk = 0 then lv_BytesAvailable - lv_Start + 1
                                       else lv_Chunk - lv_DataPointer.

    if lv_length > 0 then
    do on error undo, throw:
        if CurrentFrameSize > 0 then
        do: /** Make a temp copy of the data already in the buffer */
            set-size(lv_TempFrameBuffer) = CurrentFrameSize.
            copy-lob from CurrentFrameBuffer to lv_TempFrameBuffer.
        end.

        /** Wipe, resize & reallocate CurrentFrameBuffer so that it can hold the new data */
        set-size(CurrentFrameBuffer) = 0.
        set-size(CurrentFrameBuffer) = CurrentFrameSize + lv_length.

        if CurrentFrameSize > 0 then
        do: /** Re-insert the old data at the start of the newly resized frame buffer */
            /** Can't use copy-lob as it would shrink the size of CurrentFrameBuffer to fit the data */
            put-bytes(CurrentFrameBuffer,1) = lv_TempFrameBuffer.
        end.

        /** Append the newly read data (up to EOM byte) */
        copy-lob from lv_Data
            starting at lv_Start for lv_length
            to CurrentFrameBuffer overlay at CurrentFrameSize + 1.

        assign CurrentFrameSize = get-size(currentFrameBuffer).

        finally:
            set-size(lv_TempFrameBuffer) = 0.
        end finally.
    end.
    assign lv_Start   = lv_Start + lv_Length + if lv_Chunk = 0 then 0
                                                               else 1.

    if lv_Chunk ne 0 then /** we must now have a complete message, so process it */
    do:
      copy-lob from CurrentFrameBuffer
          starting at 1 for CurrentFrameSize
          to StompFrame.

      p_Stomp:ProcessData(StompFrame).

      StompFrame = "".
      set-size(CurrentFrameBuffer) = 0.
      CurrentFrameSize = 0.
    end.
  end.

  finally:
    set-size(lv_Data) = 0. /** always ensure that we have cleared the memory used by lv_Data */
    self:sensitive = yes.  /** ready to get more data */
  end finally.

end procedure. /* tcp_readhandler */

procedure shutdown:
  if valid-handle(CallObject) then delete object CallObject no-error.
  set-size(CurrentFrameBuffer) = 0.
end procedure.

