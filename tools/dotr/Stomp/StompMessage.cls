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

using Progress.Lang.*.

routine-level on error undo, throw.

class dotr.Stomp.StompMessage implements dotr.Stomp.Interface.IStompMessage :
  def public property Destination  as char no-undo get . set .
  def public property FrameType    as char no-undo get . set .
  def public property Header       as char no-undo get . set .
  
  def public property Body as longchar no-undo get . set .
  
  constructor StompMessage():
  end constructor.

  constructor StompMessage(p_FrameType as char, p_Destination as char, p_Headers as char, p_Message as longchar):
    assign this-object:FrameType    = p_FrameType
           this-object:Destination  = p_Destination
           this-object:header       = p_Headers
           this-object:Body         = p_Message.
  end constructor.

  /** returns value of header within headers property
   * Header format: <HeaderName>:<HeaderValue>~n
   * @param header to find
   * @return value of header ("" if not present)
   */
  
	method public char GetHeader(p_Header as char):
		def var i         as int no-undo.
 		def var lv_header as char no-undo.
 		def var lv_HeaderPair as char no-undo.
         
 		do i = 1 to num-entries(this-object:Header,"~n"):
 			lv_headerPair = entry(i,this-object:Header,"~n"). 
 			lv_Header     = entry(1,lv_headerPair,":").
 
 			if lv_header eq p_header then return substring(lv_headerPair,index(lv_headerPair,":") + 1).
 		end. 
 		return "". 
 	end method.
end class.
