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

/*
 
DEFINE PUBLIC PROPERTY JMSCorrelationID AS CHARACTER NO-UNDO
  GET.
  SET.
  
DEFINE PUBLIC PROPERTY JMSMessageID AS CHARACTER NO-UNDO
  GET.
  
DEFINE PUBLIC PROPERTY JMSDeliveryMode AS CHARACTER NO-UNDO
  GET.
  SET.
  
*/

METHOD PUBLIC VOID acknowledge().

METHOD PUBLIC CHARACTER getStringProperty(PropertyName AS CHARACTER).
METHOD PUBLIC VOID      setStringProperty(PropertyName AS CHARACTER, PropertyValue AS CHARACTER).

METHOD PUBLIC INT64     getLongProperty(PropertyName AS CHARACTER).
METHOD PUBLIC VOID      setLongProperty(PropertyName AS CHARACTER, PropertyValue AS INT64).

METHOD PUBLIC DECIMAL   getDoubleProperty(PropertyName AS CHARACTER).
METHOD PUBLIC VOID      setDoubleProperty(PropertyName AS CHARACTER, PropertyValue AS DECIMAL).
