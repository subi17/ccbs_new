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

class dotr.Stomp.StompConfig: 
  def public static property Default as dotr.Stomp.StompConfig no-undo 
    get():
      if not valid-object(dotr.Stomp.StompConfig:Default) then dotr.Stomp.StompConfig:default = new dotr.Stomp.StompConfig().
      
      return dotr.Stomp.StompConfig:Default.
    end get . private set .
     
  def public property StompServer         as char init "localhost" no-undo get . set .  
  def public property StompVirtualServer  as char init "localhost" no-undo get . set .  
  def public property StompPort           as char init "61613"     no-undo get . set .  
  
  def public property StompSlaveServer         as char  no-undo get . set .  
  def public property StompSlaveVirtualServer  as char  no-undo get . set .  
  def public property StompSlavePort           as char  no-undo get . set .  
  def public property login                    as char  no-undo get . set .  
  def public property passcode                 as char  no-undo get . set .  
  
  
  /** Default  : StompHost is the same as StompServer
    * ActiveMQ : StompHost is the same as StompServer
    * RabbitMQ : if this property is set, it references a virtual host defined on the broker
    */
    
  def public property StompHost as char  no-undo 
    get():
      return if this-object:StompHost eq "" or this-object:StompHost eq ? 
                then this-object:StompActiveVirtualServer
                else this-object:StompHost.
    end get . set .
  
  def public property LargeMessageSupport as logical init yes no-undo get . set . 
  
  def public property FailoverSupported   as logical init no  no-undo get . set .  

  def public property StompActiveServer         as char  no-undo get . set .  
  def public property StompActiveVirtualServer  as char  no-undo get . set .  
  def public property StompActivePort           as char  no-undo get . set .  
  
  constructor StompConfig():
    def var lv_Data as longchar no-undo.
    /* this will read from RBS config. TaH 2.2.2015
    assign file-info:file-name = search("StompConfig.txt").
    
    if file-info:full-pathname eq ? then assign file-info:file-name = search("dotr/Stomp/StompConfig.txt").
    
    if file-info:full-pathname ne ? then
    do:
      copy-lob from file file-info:full-pathname to lv_data no-error.
    
      if error-status:num-messages eq 0 then 
      do:
         /* assign MASTER server/port */
         assign 
            StompServer               = entry(1,entry(1,lv_Data," "),",")
            StompVirtualServer        = entry(1,entry(2,lv_Data," "),",")  when num-entries(lv_Data," ") ge 2
            StompPort                 = entry(1,entry(3,lv_Data," "),",")  when num-entries(lv_Data," ") ge 3
            LargeMessageSupport       = logical(entry(4,lv_Data," "))      when num-entries(lv_Data," ") ge 4
            StompHost                 = entry(5,lv_Data," ")               when num-entries(lv_Data," ") ge 5
            login                     = entry(6,lv_Data," ")               when num-entries(lv_Data," ") ge 6
            passcode                  = entry(7,lv_Data," ")               when num-entries(lv_Data," ") ge 7
            no-error.
            
         /* assign SLAVE server/port if set */
         if num-entries(entry(1,lv_Data," "),",") eq 2 then 
            StompSlaveServer = entry(2,entry(1,lv_Data," "),",").

         if num-entries(entry(2,lv_Data," "),",") eq 2 then 
            StompSlaveVirtualServer = entry(2,entry(2,lv_Data," "),",").

         if num-entries(entry(3,lv_Data," "),",") eq 2 then 
            StompSlavePort = entry(2,entry(3,lv_Data," "),",").
         
         if StompSlavePort ne "" and StompSlaveServer ne "" then 
            FailoverSupported = true.
          
      end.      
    end.
    */                                                    
  end constructor.
   
  method public dotr.Stomp.StompConfig ToServer(p_Server as char):
    assign StompServer = p_Server.
    return this-object.
  end method.

  method public dotr.Stomp.StompConfig ToPort(p_Port as char):
    assign StompPort = p_Port.
    return this-object.
  end method.

  method public dotr.Stomp.StompConfig SetActive(p_Server as char,p_Port as char):
    assign
       StompActiveServer        = p_Server
       StompActivePort          = p_Port
       StompActiveVirtualServer = (if p_Server eq StompServer then StompVirtualServer else StompSlaveVirtualServer).
          
    return this-object.
  end method.
  
end class.