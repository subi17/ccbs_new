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
using dotr.Stomp.*.
using dotr.Stomp.Interface.*.

routine-level on error undo, throw.

class dotr.Stomp.StompConnection final:
	def var StompConfig  as StompConfig   no-undo.
	
  def public property UserName     as char no-undo get . private set . 
  def public property Password     as char no-undo get . private set . 
  def public property ConnectionID as char no-undo get . private set .
  def public property ClientID     as char no-undo get . private set .

  def public property stompVersion      as char no-undo get . private set .
  def public property stompServer       as char no-undo get . private set .

  def public property ConnectedBroker as char no-undo
    get():
			return substitute("&1:&2":U,StompConfig:StompActiveServer,StompConfig:StompActivePort).
		end get . private set.
    
  def var SocketHandle  as handle no-undo.
  def var SocketHandler as handle no-undo.
  
  def var ServerResponseFrames as char init "CONNECTED,RECEIPT,ERROR" no-undo.
  
  def public event NewMessage         signature void (p_Message as dotr.Stomp.StompMessage).
  def public event NewRawMessage      signature void (p_Message as longchar).
  
  def var DisconnectID as char no-undo.

  constructor StompConnection(p_StompConfig as dotr.Stomp.StompConfig,p_ClientID as char,p_UserName as char,p_Password as char):
    
    if p_UserName ne "" then assign p_StompConfig:login = p_UserName.
    if p_Password ne "" then assign p_StompConfig:passcode = p_Password.
    
    assign this-object:StompConfig = p_StompConfig
		       this-object:UserName    = p_StompConfig:login
		       this-object:Password    = p_StompConfig:passcode
		       this-object:ClientId    = (if p_ClientID eq "" then guid(generate-uuid) else p_ClientID).

    this-object:Connect().
  end constructor.
  
  constructor StompConnection():
    this-object(dotr.Stomp.StompConfig:Default,"","","").
  end constructor. 

  constructor StompConnection(p_ClientID as char,p_UserName as char,p_Password as char):
    this-object(dotr.Stomp.StompConfig:Default,p_ClientID,p_UserName,p_Password).
  end constructor. 
  
  destructor StompConnection():
    
    this-object:disconnect() no-error.
    
    if valid-handle(SocketHandle) then 
    do:
      SocketHandle:disconnect() no-error.
      delete object SocketHandle.
    end.

    if valid-handle(SocketHandler) then 
    do:
      run shutdown in SocketHandler.
      delete procedure SocketHandler.
    end.
    
  end destructor.
  
  method public void Disconnect():
		def var id as char no-undo.

		assign id = guid(generate-uuid).
		
		sendFrame(substitute("DISCONNECT~nreceipt:&1~n~n",id)).
		
		assign DisconnectId = id. /** no more frames to be sent */
		
		catch e as Progress.Lang.AppError : /** swallow all errors, as we're disconnecting anyway */
		end catch.
		
	end method.
  
	/** Raise the SocketDisconnected event
	 */
	
  method public void DoSocketDisconnected():
  	this-object:MessageParsed("ERROR","SocketError","","SocketDisconnected").
  end method.

  method public void ProcessData(p_Message as longchar):
    def var lv_Frame  as char no-undo.
    def var lv_Dest   as char no-undo.
    def var lv_Header as char no-undo.
    
    def var lv_Body   as longchar no-undo.
    
    def var lv_Index as int no-undo.
    
    assign lv_Frame  = entry(1,p_message,"~n").
    
    assign lv_Frame  = entry(2,p_message,"~n") when lv_Frame eq "" 
           lv_index  = index(p_Message,"~n~n")
           lv_Body   = if lv_index eq 0 then "" else trim(substring(p_message,lv_index + 1))
           lv_Header = trim(substring(p_message,1,lv_index))
           lv_Index  = index(lv_Header,"destination:").
    
    assign lv_Dest = entry(2,entry(1,substring(lv_Header,lv_index),"~n"),":") when lv_index gt 0
           lv_Dest   = substr(lv_Dest,index(lv_Dest,"/",2) + 1)               when lv_index gt 0 .
    
    if lv_header eq "" and index(ServerResponseFrames,lv_Frame) gt 0 then 
       lv_Header = string(trim(substring(p_message,length(lv_Frame) + 1))).
    
    case lv_frame:
    	when "CONNECTED" then setConnectionDetails(lv_Header).
    end case.
    
    NewRawMessage:Publish(p_Message) no-error.           
    
    this-object:MessageParsed(lv_Frame,lv_Dest,lv_Header,lv_Body) no-error.
  end method.
  
  method protected void MessageParsed(p_Frame as char,p_Dest as char,p_Header as char, p_Body as longchar):
    NewMessage:Publish(new dotr.Stomp.StompMessage(p_frame,p_Dest,p_Header,p_Body)) no-error.
  end method.

  method public void sendframe(input p_Data as longchar):
    def var lv_MemPtr as memptr no-undo.
    
    def var lv_ToWrite as int no-undo.
	  def var lv_offset  as int no-undo.
	  
	  def var err as char no-undo.
	  
	  if DisconnectId ne "" then return. /** Clients MUST NOT send any more frames after the DISCONNECT frame is sent. */
	  
    if not valid-handle(SocketHandle) or p_Data eq "" or p_Data eq ? then return.
    
    lv_ToWrite = length(p_Data) + 1.  /* add 1 bytes for 0-terminated string */
    
    set-size(lv_MemPtr) = lv_ToWrite.
    
    put-string(lv_MemPtr, 1) = p_Data. 
    
    do while lv_ToWrite gt 0:
    	SocketHandle:write(lv_MemPtr,lv_offset + 1, lv_ToWrite) no-error.
    	
			if SocketHandle:bytes-written eq 0 then undo, throw new AppError("IO Exception",0).
			
			assign lv_offset  = lv_offset  + SocketHandle:bytes-written
					   lv_ToWrite = lv_ToWrite - SocketHandle:bytes-written.
    end.
    
    finally:
	    set-size(lv_MemPtr) = 0.
	  end finally.

  end method.

  method protected void Connect():
    if not valid-handle(SocketHandle) then 
    do:
      create socket SocketHandle.
            
      run dotr/Stomp/stompSocketProc.p persistent set SocketHandler(this-object).
      
      SocketHandle:set-read-response-procedure(if StompConfig:LargeMessageSupport 
                                                  then "SocketBigReadHandler"
                                                  else "SocketReadHandler",SocketHandler).
    end.

    SocketHandle:connect(substitute("-H &1 -S &2", StompConfig:StompServer, StompConfig:StompPort)) no-error.
    
    if error-status:num-messages gt 0 or not SocketHandle:connected() then
    do:
       /* try connecting using the slave connection details, if set */
       if StompConfig:FailoverSupported then 
       do:
          SocketHandle:connect(substitute("-H &1 -S &2", StompConfig:StompSlaveServer, StompConfig:StompSlavePort)) no-error.
          StompConfig:SetActive(StompConfig:StompSlaveServer,StompConfig:StompSlavePort).
       end.
       
       if error-status:num-messages gt 0 or not SocketHandle:connected() then
          undo, throw new appError(substitute("Cannot connect to Stomp Server with Server:&1 Port:&2",
                                                 StompConfig:Stompserver + "/" + StompConfig:StompSlaveServer, 
                                                 StompConfig:StompPort + "/" + StompConfig:StompSlavePort),0).
    end.
    else StompConfig:SetActive(StompConfig:Stompserver,StompConfig:StompPort).
    
    SocketHandle:set-socket-option ("SO-RCVBUF",string(64 * 1024 * 1024)).
    SocketHandle:set-socket-option ("TCP-NODELAY","TRUE").
    SocketHandle:set-socket-option ("SO-KEEPALIVE","TRUE").
    
    sendFrame(substitute("CONNECT~naccept-version:1.0,1.1,2.0~nhost:&3~nclient-id:&4~nlogin:&1~npasscode:&2~n~n",  this-object:UserName, this-object:Password,StompConfig:StompHost,this-object:ClientID)).

  end method.

  method private void setConnectionDetails (p_Header as char):
     def var i         as int no-undo.
     def var lv_header as char no-undo.
     
     do i = 1 to num-entries(p_Header,"~n"):
         lv_header = entry(i,p_Header,"~n"). 
         case entry(1,lv_header,":"):
             when "session" then this-object:ConnectionID = substring(lv_header,index(lv_header,":") + 1).
             when "server"  then this-object:stompServer  = substring(lv_header,index(lv_header,":") + 1).
             when "version" then this-object:stompVersion = substring(lv_header,index(lv_header,":") + 1).
         end case.    
     end.  
     
     
  end method.

        
end class.