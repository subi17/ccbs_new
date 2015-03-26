0 "before start" yes "methodCall" 0 -1 1 ?
1 "methodcall opened" yes "methodName" 0 -1 2 ?
2 "awaiting methodname" no "methodName" 1 -1 3 "set methodname"
3 "got methodname" yes "params" 0 -1 4 ?
4 "opened params" no "params" 0 -1 98 ?
4 "opened params" yes "param" 0 -1 5 ?
5 "param opened" no "param" 0 -1 4 ?
5 "param opened" yes "value" 0 6 7 ?
6 "param: value given" no "param" 0 -1 4 ?
7 "value opened" no "value" 2 -1 -1 "save string"
7 "value opened" yes "array" 0 -1 13 "open array"
7 "value opened" yes "base64" 0 -1 36 "redir to file"
7 "value opened" yes "boolean" 0 -1 33 ?
7 "value opened" yes "dateTime.iso8601" 0 -1 35 ?
7 "value opened" yes "double" 0 -1 34 ?
7 "value opened" yes "i4" 0 -1 31 ?
7 "value opened" yes "int" 0 -1 32 ?
7 "value opened" yes "string" 0 -1 30 ?
7 "value opened" yes "struct" 0 -1 10 "open struct"
9 "data given" no "value" 0 -1 -1 ?
10 "struct started" no "struct" 0 -1 9 "close struct"
10 "struct started" yes "member" 0 -1 11 "start member"
11 "member started" no "member" 0 -1 10 "has name and value?"
11 "member started" yes "name" 0 -1 12 "has name?"
11 "member started" yes "value" 0 11 7 "has value?"
12 "membername opened" no "name" 1 -1 11 "set membername"
13 "array opened" yes "data" 0 -1 14 ?
14 "array data opened" no "data" 0 -1 15 ?
14 "array data opened" yes "value" 0 14 7 ?
15 "array data closed" no "array" 0 -1 9 "close array"
30 "datatype specified" no "string" 2 -1 9 "save string"
31 "datatype specified" no "i4" 1 -1 9 "save int"
32 "datatype specified" no "int" 1 -1 9 "save int"
33 "datatype specified" no "boolean" 1 -1 9 "save boolean"
34 "datatype specified" no "double" 1 -1 9 "save double"
35 "datatype specified" no "dateTime.iso8601" 1 -1 9 "save datetime"
36 "datatype specified" no "base64" 1 -1 9 "save base64"
98 "params closed" no "methodCall" 0 -1 99 ?
