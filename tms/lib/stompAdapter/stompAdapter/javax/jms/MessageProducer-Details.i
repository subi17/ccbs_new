
/* DEFINE PUBLIC PROPERTY destination AS nl.flusso.stomp.Destination NO-UNDO
  GET.
*/

METHOD PUBLIC VOID sendMessage(jmsMessage AS nl.flusso.stomp.Message).

/* Not a part of real JMS API; workaround for OpenEdge limitation. */
METHOD PUBLIC VOID sendMessage(jmsMessage AS nl.flusso.stomp.TextMessage).
METHOD PUBLIC VOID sendMessage(jmsMessage AS nl.flusso.stomp.BytesMessage).

METHOD PUBLIC VOID sendMessage(messageDestination AS javax.jms.Destination, jmsMessage AS nl.flusso.stomp.Message):

/* Not a part of real JMS API; workaround for OpenEdge limitation. */
METHOD PUBLIC VOID sendMessage(messageDestination AS javax.jms.Destination, jmsMessage AS nl.flusso.stomp.TextMessage).
METHOD PUBLIC VOID sendMessage(messageDestination AS javax.jms.Destination, jmsMessage AS nl.flusso.stomp.BytesMessage).

/* Not a part of real JMS API; workaround for OpenEdge limitation. */
METHOD PUBLIC VOID sendMessage(messageDestination AS nl.flusso.stomp.Queue, jmsMessage AS nl.flusso.stomp.Message):
  
/* Not a part of real JMS API; workaround for OpenEdge limitation. */
METHOD PUBLIC VOID sendMessage(messageDestination AS nl.flusso.stomp.Queue, jmsMessage AS nl.flusso.stomp.TextMessage).
METHOD PUBLIC VOID sendMessage(messageDestination AS nl.flusso.stomp.Queue, jmsMessage AS nl.flusso.stomp.BytesMessage).

/* Not a part of real JMS API; workaround for OpenEdge limitation. */
METHOD PUBLIC VOID sendMessage(messageDestination AS nl.flusso.stomp.Topic, jmsMessage AS nl.flusso.stomp.Message):
  
/* Not a part of real JMS API; workaround for OpenEdge limitation. */
METHOD PUBLIC VOID sendMessage(messageDestination AS nl.flusso.stomp.Topic, jmsMessage AS nl.flusso.stomp.TextMessage).
METHOD PUBLIC VOID sendMessage(messageDestination AS nl.flusso.stomp.Topic, jmsMessage AS nl.flusso.stomp.BytesMessage).

