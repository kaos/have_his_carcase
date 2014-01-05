Have his carcase
================

This cipher implements a version of the cipher described
in the book "Have his carcase" by Dorothy L. Sayers.


Example
=======

Create a cipher table, with a given key word:

    T = carcase:new("monarchy").

Encode a secret message:

    M = carcase:encode("this is a (secret) message.", T).
    %% M = "QBSZZPWZCÄ.VFRELW?XCYVUOLH."

The recipient of the secret message can now decode the message:

    carcase:decode(M, T).
    %% "THIS IS A (SECRET) MESSAGE."

To review the cipher table:

    carcase:print(T).
    %% ["MONARC","HYBDEF","GIJKLP","QSTUVW","XZÅÄÖ ",",.?!()"]
