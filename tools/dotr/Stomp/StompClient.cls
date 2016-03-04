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

class dotr.Stomp.StompClient :
  def temp-table TTDestination no-undo
    field Destination as char
    field ID as char
    
    index Dest is primary unique
      Destination
    index ID ID  .

  def var Headers as char no-undo.
 
  def var StompConnection as dotr.Stomp.StompConnection no-undo.
  
  def var ProcHandler as handle no-undo.
  
  def var StompHandler as dotr.Stomp.Interface.IStompHandler no-undo.
 
  def property ConnectedBroker as char no-undo
  	get():
  		return StompConnection:ConnectedBroker.
  	end get . private set.
 
  def property ConnectionId as char no-undo
    get():
    	return StompConnection:ConnectionID.
    end get. private set.

  def property ClientId as char no-undo
    get():
     return StompConnection:ClientID.
    end get . private set.
         
  constructor StompClient(p_StompConfig as dotr.Stomp.StompConfig):
    StompConnection = new dotr.Stomp.StompConnection(p_StompConfig,"","","").
  end constructor.
  
  constructor StompClient():
    StompConnection = new dotr.Stomp.StompConnection().
  end constructor.

  constructor StompClient(p_ClientID as char,p_UserName as char,p_Password as char):
    StompConnection = new dotr.Stomp.StompConnection(p_ClientID,p_UserName,p_Password).
  end constructor.
  
  destructor StompClient():
    
    if valid-handle(ProcHandler) then StompConnection:NewMessage:Unsubscribe(ProcHandler,"NewStompMessage").
    
    for each TTDestination no-lock:
      this-object:unsubscribeID(TTDestination.Destination,TTDestination.ID).
      delete TTDestination.
    end.
    
    delete object StompConnection.
    
  end destructor.

  method public dotr.Stomp.StompClient WithReceipt(p_Value as char):
  	return WithHeader("Receipt",p_Value).
  end method.
  
  method public dotr.Stomp.StompClient WithHeader(p_Header as char,p_Value as char):
    
    /** according to the stomp spec ( http://stomp.github.com/stomp-specification-1.1.html#Repeated_Header_Entries ) 
      * repeat headers are ignored. We specifically add headers to the top of the header list, so that every new
      * header gets priority.
        
      * Headers are also now reset after every send, so there is no need to check for them 
      */
        
    assign Headers = substitute("&1:&2~n&3",p_header,trim(p_Value),Headers). 
    return this-object.
  end method.

  /** send message to queue 
   * @param queue name
   * @param message to send
   */

  method public void SendQueue(p_Dest as char,p_Message as longchar):
    SendQueue(p_Dest,p_Message,?).
  end method.

  /** send message to queue with an expiry time
   * @param queue name
   * @param message to send
   * @param expiry datetime
   */

  method public void SendQueue(p_Dest as char,p_Message as longchar,p_Expires as datetime):
    Send(substitute("/queue/&1",p_Dest),p_message,p_expires). 
  end method.

  /** send message to persistent queue 
   * @param queue name
   * @param message to send
   */

  method public void SendPersistentQueue(p_Dest as char,p_Message as longchar):
    SendPersistentQueue(p_Dest,p_Message,?).
  end method.
   
  /** send message to persistent queue  with an expiry time
   * @param queue name
   * @param message to send
   * @param expiry datetime
   */
  method public void SendPersistentQueue(p_Dest as char,p_Message as longchar,p_Expires as datetime):
    WithHeader("persistent","true").   
    Send(substitute("/queue/&1",p_Dest),p_message,p_expires). 
  end method.

  /** send message to topic
   * @param queue name
   * @param message to send
   */

  method public void SendTopic(p_Dest as char,p_Message as longchar):
    SendTopic(p_Dest,p_Message,?).
  end method.

  /** send message to persistent topic
   * @param queue name
   * @param message to send
   */

  method public void SendPersistentTopic(p_Dest as char,p_Message as longchar):
     SendPersistentTopic(p_Dest,p_Message,?).
  end method.

  /** send message to topic with an expiry time
   * @param queue name
   * @param message to send
   * @param expiry datetime
   */

  method public void SendTopic(p_Dest as char,p_Message as longchar,p_Expires as datetime):
    Send(substitute("/topic/&1",p_Dest),p_message,p_expires). 
  end method.

  /** send message to persistent topic with an expiry time
   * @param queue name
   * @param message to send
   * @param expiry datetime
   */

  method public void SendPersistentTopic(p_Dest as char,p_Message as longchar,p_Expires as datetime):
    WithHeader("persistent","true").   
    Send(substitute("/topic/&1",p_Dest),p_message,p_expires). 
  end method.

  method private void Send(p_Dest as char,p_Message as longchar,p_Expires as datetime):
    def var lv_Expires as int64 no-undo.
    
    def var lv_header as longchar no-undo.
    
    assign lv_expires = p_Expires - DATETIME(1, 1, 1970, 0, 0, 0, 0)
           lv_Header  = substitute("SEND~ndestination:&1~ncontent-type:text/plain~n&2&3~n~n",
                                   p_Dest,
                                   if p_expires eq ? then "" else substitute("expires:&1~n",lv_expires),
                                   Headers).

    StompConnection:sendFrame(lv_Header + p_message).
    
    assign Headers = "". /** reset headers after every send */
    
    return .
  end method.

  method public void Subscribe(p_handle as handle):
    assign ProcHandler = p_Handle.
    StompConnection:NewMessage:Subscribe(p_Handle,"NewStompMessage").
  end method.

  method public void Subscribe(p_handler as dotr.Stomp.Interface.IStompHandler):
    assign StompHandler = p_Handler.
    StompConnection:NewMessage:Subscribe(p_Handler:NewStompMessage).
  end method.

  method public void Unsubscribe(p_handle as handle):
    StompConnection:NewMessage:Unsubscribe(p_Handle,"NewStompMessage").
  end method.

  method public void UnSubscribe(p_handler as dotr.Stomp.Interface.IStompHandler):
    StompConnection:NewMessage:Unsubscribe(p_Handler:NewStompMessage).
  end method.

  method public void SubscribeToQueue(p_Destination as char):
    this-object:Subscribe("queue",p_Destination).
  end method.

  /** Subscribe to queue with ack mode set to client
   */
  method public void SubscribeToQueueACK(p_Destination as char):
    this-object:Subscribe("queue",p_Destination,"","client").
  end method.
 /*subscribe with mode defined, prefer client-individual*/
  method public void SubscribeToQueueACK(p_Destination as char, p_AckMode AS CHARACTER):
     this-object:Subscribe("queue",p_Destination,"",p_AckMode).
  end method.
   /** Subscribe to topic - non durable subscription
   * @param queue name
   */
  method public void SubscribeToTopic(p_Destination as char):
    this-object:Subscribe("topic",p_Destination).
  end method.

/*  /** Subscribe to topic - durable subscription                                  */
/*   * @param queue name                                                           */
/*   * @param Subscription Id                                                      */
/*   */                                                                            */
/*  method public void SubscribeToTopicDurable(p_Destination as char,p_Id as char):*/
/*    this-object:Subscribe("topic",p_Destination,p_Id,true).                      */
/*  end method.                                                                    */

  method public void UnSubscribeFromQueue(p_Destination as char):
    this-object:UnSubscribe("queue",p_Destination).
  end method.

  method public void UnSubscribeFromTopic(p_Destination as char):
    this-object:UnSubscribe("topic",p_Destination).
  end method.

  method private void Subscribe(p_type as char,p_Destination as char):
    this-object:Subscribe(p_type,p_Destination,""). 
  end method.

  method private void Subscribe(p_type as char,p_Destination as char,p_Id as char):
    this-object:Subscribe(p_type,p_Destination,p_id,"auto"). 
  end method.
  
  method private void Subscribe(p_type as char,p_Destination as char,p_Id as char,p_ACK_Mode as char):
    def var lv_i  as int no-undo.
    def var lv_id as char no-undo.
    
    lv_id = (if p_Id eq "" then guid(generate-uuid) else p_Id).
    
    do lv_I = 1 to num-entries(p_Destination):
      find TTDestination where TTDestination.Destination eq substitute("/&1/&2",p_type,entry(lv_i,p_Destination)) no-lock no-error.
      if avail TTDestination then return.
      
      create TTDestination.
      assign TTDestination.Destination = substitute("/&1/&2",p_type,entry(lv_i,p_Destination))
             TTDestination.ID          = lv_id.
      StompConnection:sendFrame(substitute("SUBSCRIBE~nid:&1~ndestination:&2~n&3ack:&4~n~n", TTDestination.ID,TTDestination.Destination,Headers,p_ACK_Mode)).
      Headers = "".
    end.                 
  end method.
  
  method private void UnSubscribe(p_type as char,p_Destination as char):
    find TTDestination where TTDestination.Destination eq substitute("/&1/&2",p_type,p_Destination) no-lock no-error.
    if not avail TTDestination then return.  
    this-object:unsubscribeID(TTDestination.Destination,TTDestination.ID).
    delete TTDestination.
  end method.

  method private void UnSubscribeID(p_Destination as char,p_ID as char):
    StompConnection:sendFrame(substitute("UNSUBSCRIBE~nid:&1~ndestination:&2~n~n", p_ID,p_Destination)) no-error. 
  end method.

  /** Send ACK for given message
   */
  method public void ACK(p_Msg as dotr.Stomp.StompMessage):
    def var lv_Destination as char no-undo.
    def var lv_Message_ID as char no-undo.

    lv_Destination = p_Msg:GetHeader('destination').
    lv_Message_ID = p_Msg:GetHeader('message-id').

    if lv_Destination = "" or lv_Message_ID = "" then return.

    find TTDestination where TTDestination.Destination eq lv_Destination no-lock no-error.
    if not avail TTDestination then return.

    StompConnection:sendFrame(substitute("ACK~nsubscription:&1~nmessage-id:&2~n~n", TTDestination.ID, lv_Message_ID)).
  end method.
end class.
