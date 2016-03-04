/*
Copyright (c) 2011-2013, Julian Lyndon-Smith (julian+maia@dotr.com)
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

using dotr.Stomp.*.
using dotr.Stomp.Interface.*.

routine-level on error undo, throw.

class dotr.Stomp.Helper.Stomp: 
	
	/** create a session-wide StompClient */
	def private static property StompClient as dotr.Stomp.StompClient no-undo
		get():
			
			if not valid-object(StompClient) then StompClient = new dotr.Stomp.StompClient(). 
			
			return StompClient.
		end get . private set.

  method static public void SendToTopic(p_dest as char,p_Message as longchar):
   StompClient:SendTopic(p_dest,p_message). 
  end method.

  method static public void SendToQueue(p_dest as char,p_Message as longchar):
   StompClient:SendQueue(p_dest,p_message). 
  end method.
  
  method static public void SendToTopic(p_dest as char,p_Message as longchar,p_Expires as datetime):
   stompClient:SendTopic(p_dest,p_message,p_Expires).
  end method.

  method static public void ToQueue(p_dest as char,p_Message as longchar,p_Expires as datetime):
   stompClient:SendQueue(p_dest,p_message,p_Expires).
  end method.


end class.