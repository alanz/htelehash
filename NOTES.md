
Link Flow
---------

Calling `seek_auto` as part of the startup process in the app sets
`swHandler` to point to `_seek_auto`.

The initial mesh is booted by calling `link_hn` on each of the seeds
loaded from a file. The seed info in this file includes crypto and
path information, so nothing special is needed to establish the
connection.

Once a hashname is known, from whatever source, a message can be sent
to it. The first step is to create a channel of an appropriate type by
calling `chan_new`.

The channel manages communication of the specific type between this
node and the remote hashname.

Calling `chan_packet` creates a message ready to be sent to the
associated hashname. If it is an unreliable channel it will only have
a `c` parameter identifying the channel, except for the first which
will also populate the `type` parameter.

Once any additional fields have been set in the packet, it is sent by
calling `chan_send`.

Within this, reliable channels will receive appropriate values for
`seq`, `ack`, and `miss` as needed, and the amended packet is passed
on to `switch_send`.

This first tries to encrypt the packet. If it is the first time
sending to a new hashname, there will be no known crypto for it. In
this case the packet is stored in the `HashContainer` and
`switch_open` is called for the hashname.

When there is no crypto, and `seek_auto` has been set up, the
`_seek_auto` function is called.

This creates a new empty `Seek` structure and calls `seek_send` with
it.






crypto stuff - id hash
----------------------

id file

{"1a":"o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg==","1a_secret":"iollyIcHaGeD/JpUNn/7ef1QAzE="}

log from ping using this id

*** public key o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg== ***
*** secret key iollyIcHaGeD/JpUNn/7ef1QAzE= ***
loaded hashname 7ecf6a5884d483fde2f6a027e33e6e1756efdb70925557c3e3f776b35329aef5


From
<https://github.com/telehash/telehash.org/blob/master/hashnames.md>


```json
{
  "2a": "bf6e23c6db99ed2d24b160e89a37c9cd183fb61afeca40c4bc378cf6e488bebe",
  "1a": "a5c8b5c8a630c84dc01f92d2e5af6aa41801457a"
}
```

To calculate the hashname the parts are hashed in ascending order by
their CSID, and each part contributes two values, the CSID and the
fingerprint. The hash is rolled up, each resulting binary digest is
combined with the next string value as the input. For the above
example parts, the calculation would look like (in pseudo-code):

```js
hash = sha256("1a")
hash = sha256(hash + "a5c8b5c8a630c84dc01f92d2e5af6aa41801457a")
hash = sha256(hash + "2a")
hash = sha256(hash + "bf6e23c6db99ed2d24b160e89a37c9cd183fb61afeca40c4bc378cf6e488bebe")
print hex(hash)
"825df574f77ebe1380640a314b745ed761d4ec286f0208838bfc14e288b126c0"
```


Checking packet construction
----------------------------

output of ping with packet hex dump

```
$ ./bin/ping 
*** public key o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg== ***
*** secret key iollyIcHaGeD/JpUNn/7ef1QAzE= ***
loaded hashname 7ecf6a5884d483fde2f6a027e33e6e1756efdb70925557c3e3f776b35329aef5
channel new 2 seek
sending open packet 293 {"type":"ipv4","ip":"208.68.164.253","port":42424}
received open packet 368 {"type":"ipv4","ip":"208.68.164.253","port":42424}
line in 1 89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de
hex dump of packet out
 0x00, 0x01, 0x1a, 0x70, 0xf0, 0xd6, 0x5a, 0xc1, 0xae, 0xae, 0x58, 0xe4, 0xaf, 0x0e, 0x58, 0x27,
 0xa4, 0x4b, 0x4b, 0x0b, 0x0d, 0x39, 0x41, 0x15, 0x97, 0xb6, 0x35, 0x55, 0xf0, 0xf0, 0x99, 0x48,
 0xce, 0x81, 0xf5, 0xba, 0xd9, 0xdc, 0x3b, 0x05, 0xc5, 0x81, 0xce, 0x2e, 0x6d, 0xc9, 0x1a, 0xb9,
 0x87, 0xdc, 0xd9, 0x13, 0x44, 0x37, 0xb0, 0x68, 0x25, 0x62, 0xac, 0xc7, 0x07, 0x1e, 0x27, 0xff,
 0xb5, 0x15, 0x64, 0x2e, 0x1a, 0x38, 0xaa, 0x33, 0xe2, 0xaf, 0x1d, 0x74, 0x46, 0xef, 0x89, 0xdc,
 0xa8, 0x15, 0x66, 0x7a, 0x5f, 0xa6, 0x45, 0x9f, 0xbb, 0xdb, 0x7a, 0x27, 0xb5, 0xa9, 0x48, 0xff,
 0xc3, 0xf6, 0xc3, 0x1e, 0xf6, 0x83, 0xf5, 0x1e, 0x06, 0xb4, 0xb3, 0x13, 0xfc, 0x57, 0xa1, 0x2a,
 0xdf, 0x96, 0xdf, 0x90, 0x2d, 0x14, 0x24, 0x11, 0xa6, 0x01, 0x4b, 0xed, 0xf1, 0xd1, 0x32, 0x88,
 0x15, 0xb4, 0x25, 0x0f, 0xa8, 0xda, 0x19, 0xc4, 0xb1, 0xf3, 0xe3, 0x4c, 0x31, 0x4d, 0xfe, 0x36,
 0xcf, 0x76, 0xc8, 0x46, 0x04, 0x30, 0xd2, 0x96, 0x46, 0xec, 0x45, 0xd3, 0x06, 0xb7, 0x92, 0x61,
 0xe8, 0xcf, 0x57, 0xd7, 0x20, 0xc7, 0xf4, 0xcb, 0xab, 0x66, 0x73, 0x39, 0xc5, 0xe4, 0xb4, 0x11,
 0x34, 0xd3, 0x45, 0x4f, 0x06, 0x4e, 0x75, 0xa1, 0xa6, 0x33, 0x91, 0x71, 0x49, 0xeb, 0x6c, 0xd9,
 0x6b, 0xf3, 0x8b, 0x3f, 0x96, 0xe1, 0x2e, 0xad, 0xbc, 0xf0, 0x81, 0x60, 0xae, 0x3d, 0x7d, 0x59,
 0xad, 0x1a, 0x0f, 0xdb, 0x1f, 0xa7, 0x6b, 0x36, 0x24, 0xfc, 0x6a, 0x0c, 0x15, 0xe9, 0x32, 0x64,
 0xe4, 0x55, 0x3f, 0x19, 0xd9, 0x20, 0x4d, 0x80, 0x27, 0x50, 0x68, 0x77, 0x32, 0x27, 0x34, 0x66,
 0xc2, 0x76, 0x02, 0x8f, 0x14, 0xda, 0xe8, 0xfb, 0x89, 0x28, 0x27, 0xfd, 0xbd, 0x8f, 0x41, 0x3f,
 0x71, 0xaa, 0x50, 0xca, 0x21, 0x98, 0x0e, 0x44, 0x69, 0x49, 0xc7, 0x74, 0xf0, 0xa0, 0xc9, 0x0b,
 0x30, 0x8f, 0x99, 0x60, 0x87, 0xec, 0x35, 0x25, 0x0d, 0xeb, 0xa5, 0x0a, 0x29, 0xec, 0x22, 0x13,
 0xae, 0xae, 0xdb, 0x32, 0xf9,
hex dump of packet out done
Sending open packet 293 {"type":"ipv4","ip":"208.68.164.253","port":42424}
hex dump of packet out
 0x00, 0x00, 0x16, 0x60, 0xef, 0x04, 0x2e, 0x32, 0x1e, 0xfb, 0x11, 0x0d, 0xb8, 0x9f, 0xe7, 0x05,
 0x72, 0xf6, 0x06, 0x48, 0xe2, 0x9c, 0x00, 0x00, 0x00, 0x00, 0x08, 0xf2, 0x87, 0x9e, 0xb5, 0xb2,
 0x4c, 0x3f, 0xf3, 0xca, 0x4c, 0xa3, 0x18, 0xdc, 0x16, 0xac, 0x33, 0x94, 0x9a, 0xaa, 0xcc, 0x01,
 0xdf, 0xb8, 0x16, 0x7f, 0x48, 0xe1, 0x4c, 0xe4, 0x45, 0xa8, 0x4b, 0x61, 0xfa, 0x1e, 0xdb, 0x99,
 0xee, 0x83, 0xdb, 0xb0, 0xbf, 0x83, 0x33, 0x72, 0xbc, 0xf0, 0xbc, 0xfd, 0xda, 0x4a, 0x5c, 0x40,
 0x9d, 0xb6, 0xe1, 0x33, 0x38, 0xc3, 0x9a, 0x54, 0x3e, 0x9e, 0xf6, 0xbe, 0x11, 0x39, 0x2c, 0x0f,
 0x57, 0xb0, 0xc9, 0x27, 0x97, 0x20, 0x8e, 0xf5, 0xf2, 0x38, 0x0a, 0xc1, 0xb9, 0x95, 0xf1, 0xe4,
 0x68, 0x34, 0xd0, 0xc8, 0x55, 0x9b, 0x8a, 0x87, 0xa5, 0xc5, 0xe3,
hex dump of packet out done
Sending line packet 123 {"type":"ipv4","ip":"208.68.164.253","port":42424}
Received line packet 235 {"type":"ipv4","ip":"208.68.164.253","port":42424}
channel new 55 path

```








------------------------

https://stackoverflow.com/questions/4702325/best-way-to-convert-between-char-and-word8




-------------------------------------------

Normal line setup from remote seed, as reported by the node version

>>>> Thu Apr 17 2014 12:02:06 GMT+0200 (SAST) 71 0 ipv4,208.68.164.253,42424,
unknown line received 4429b958adce73f77093f39460b390ab
>>>> Thu Apr 17 2014 12:03:01 GMT+0200 (SAST) 71 0 ipv4,208.68.164.253,42424,
unknown line received 4429b958adce73f77093f39460b390ab
>>>> Thu Apr 17 2014 12:03:01 GMT+0200 (SAST) 1094 1 ipv4,208.68.164.253,42424,
AZ our hashname: 3036d9b6f9525660b71d58bacd80d0ef0f6e191f1622daefd461823c366eb4fc
inOpen verified 89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de
adding new path 0 {"type":"ipv4","ip":"208.68.164.253","port":42424}
89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de ipv4 {"type":"ipv4","ip":"208.68.164.253","port":42424}
PATH INNEW {"type":"ipv4","ip":"208.68.164.253","port":42424} [ '{"type":"ipv4","ip":"208.68.164.253","port":42424}' ]
new line
<<<< Thu Apr 17 2014 12:03:01 GMT+0200 (SAST) 1166 ipv4,208.68.164.253,42424, 89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de
line open 89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de 36722f8135083a9d3a2d443f4e5d85ec 4485b0484e2d29166b65cf86a2dcf877
SYNCING 89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de [ '{"type":"ipv4","ip":"208.68.164.253","port":42424}' ]
PATHLOOP 1 {"type":"ipv4","ip":"208.68.164.253","port":42424}
new unreliable channel 89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de path 2
SEND path {"path":{"type":"ipv4","ip":"208.68.164.253","port":42424},"priority":1,"paths":[{"type":"http","http":"http://172.17.42.1:42424"}],"type":"path","c":2}
line sending 89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de 4485b0484e2d29166b65cf86a2dcf877
<<<< Thu Apr 17 2014 12:03:01 GMT+0200 (SAST) 204 ipv4,208.68.164.253,42424, 89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de
>>>> Thu Apr 17 2014 12:03:01 GMT+0200 (SAST) 1094 1 ipv4,208.68.164.253,42424,
AZ our hashname: 3036d9b6f9525660b71d58bacd80d0ef0f6e191f1622daefd461823c366eb4fc
inOpen verified 89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de
<<<< Thu Apr 17 2014 12:03:01 GMT+0200 (SAST) 1166 ipv4,208.68.164.253,42424, 89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de
line open 89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de 36722f8135083a9d3a2d443f4e5d85ec 4485b0484e2d29166b65cf86a2dcf877
>>>> Thu Apr 17 2014 12:03:01 GMT+0200 (SAST) 71 0 ipv4,208.68.164.253,42424,
LINEIN {"seed":true,"c":2}
>>>> Thu Apr 17 2014 12:03:01 GMT+0200 (SAST) 141 0 ipv4,208.68.164.253,42424,
LINEIN {"end":true,"priority":2,"path":{"type":"ipv4","ip":"105.236.67.202","port":42424},"c":2}




-- -----------------------------------------------------------------------------------------

tftp stuff
----------

Initial packet from telehash-c

```
chan_in:p=RxTelex
{rtId = 0
, rtSender = PIPv4 (PathIPv4 {v4Ip = 10.0.0.33, v4Port = 57158})
, rtAt = Wed May  7 15:01:31 SAST 2014
, rtJs = fromList
  [("end", Bool True),
   ("seq", Number 0.0),
   ("c",   Number 1.0),
   ("type",String "thtp")]
, rtPacket = Packet
  {paHead = HeadJson "{\"seq\":0,\"type\":\"thtp\",\"c\":1,\"end\":true}"
 , paBody = Body "\NUL/{\"path\":\"/chat/56419861/roster\",\"method\":\"get\"}"}
, rtChanId = Nothing}
```


ext_thtp:got req:HeadJson "{\"path\":\"/chat/56419861/roster\",\"method\":\"get\"}"
ext_thtp:req json={"method":"get","path":"/chat/56419861/roster"}


chat stuff
----------

create chat

created chat:("tft@0ecedc9f49472737b9285c0e10066fd860983bb5aa3a04e1f0acc3d3b3c5e348",Just "186919c2,1000","a23a9c7c")

## on tft side

joining chat tft@0ecedc9f49472737b9285c0e10066fd860983bb5aa3a04e1f0acc3d3b3c5e348 3f3d248b,1000 0b99e271


## hence request received

receive roster req from telehash-c tft for "tft@ecedc9f49472737b9285c0e10066fd860983bb5aa3a04e1f0acc3d3b3c5e348"

```
chan_in:p=RxTelex
{rtId = 0
, rtSender = PIPv4 (PathIPv4 {v4Ip = 172.17.42.1, v4Port = 46940})
, rtAt = Mon May 12 10:24:30 SAST 2014
, rtJs = fromList
  [("end",Bool True),
   ("seq",Number 0.0),
   ("c",Number 1.0),
   ("type",String "thtp")]
, rtPacket = Packet
  { paHead = HeadJson "{\"seq\":0,\"type\":\"thtp\",\"c\":1,\"end\":true}"
  , paBody = Body "\NUL/{\"path\":\"/chat/56419861/roster\",\"method\":\"get\"}"}
, rtChanId = Nothing}
```

tftp

thtp_req:setting CArgTx to TxTelex
{tId = 0
, tTo = HN "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a"
, tOut = PNone
, tJs = fromList []
, tPacket = Packet {paHead = HeadEmpty, paBody = Body ""}
, tChain = Just (TxTelex
 {tId = 0
 , tTo = HN "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a"
 , tOut = PNone
 , tJs = fromList
    [("to",String "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a")
    ,("for",String "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a")
    ,(".from",Number 5.0)
    ,("path",String "/chat/0df5f84f/roster")]
 , tPacket = Packet {paHead = HeadEmpty, paBody = Body ""}
 , tChain = Nothing, tLp = Nothing}), tLp = Nothing}

-------------------------------

ext_thtp:CArgTx: (r,r2)=
(TxTelex
{tId = 0
, tTo = HN "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a"
, tOut = PNone
, tJs = fromList []
, tPacket = Packet {paHead = HeadEmpty, paBody = Body ""}
, tChain = Just (TxTelex
 {tId = 0
 , tTo = HN "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a"
 , tOut = PNone
 , tJs = fromList
    [("to",String "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a")
    ,("for",String "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a")
    ,(".from",Number 5.0)
    ,("path",String "/chat/0df5f84f/roster")]
 , tPacket = Packet {paHead = HeadEmpty, paBody = Body ""}
 , tChain = Nothing, tLp = Nothing}), tLp = Nothing}
,

TxTelex
{tId = 0
, tTo = HN "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a"
, tOut = PNone
, tJs = fromList []
, tPacket = Packet
  { paHead = HeadEmpty
  , paBody = Body "\NUL\SO{\"status\":200}{\"*\":\"invited\",\"49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a\":\"d21d7472,1000\"}"
  }
 , tChain = Just (TxTelex {tId = 0
 , tTo = HN "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a"
 , tOut = PNone
 , tJs = fromList
    [("to",String "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a")
    ,("for",String "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a")
    ,(".from",Number 5.0)
    ,("path",String "/chat/0df5f84f/roster")]
  , tPacket = Packet {paHead = HeadEmpty, paBody = Body ""}
, tChain = Nothing, tLp = Nothing}), tLp = Nothing})

decoded line packet from above
ext_thtp:PingPongPacket received:Packet {paHead = HeadJson "{\"status\":200}", paBody = Body "{\"*\":\"invited\",\"49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a\":\"d21d7472,1000\"}"}


thtp get response for roster
---------------------------

```
chat_hub:got note TxTelex
{tId = 0
, tTo = HN "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a"
, tOut = PNone
, tJs = fromList
   [("to",String "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a")
   ,("for",String "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a")
   ,(".from",Number 6.0)
   ,("thtp",String "resp")
   ,(".to",Number 5.0)
   ,("path",String "/chat/0df5f84f/roster")]
, tPacket = Packet
    { paHead = HeadJson "{\"status\":200}"
    , paBody = Body "{\"*\":\"invited\",\"49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a\":\"a8232b15,1000\"}"}
, tChain = Nothing
, tLp = Nothing}
```

thtp get request for roster
---------------------------

```
chat_hub:got note TxTelex
{tId = 0
, tTo = HN "0ecedc9f49472737b9285c0e10066fd860983bb5aa3a04e1f0acc3d3b3c5e348"
, tOut = PNone
, tJs = fromList
  [("glob",String "/chat/56419861/")
  ,(".from",Number 6.0)
  ,("thtp",String "req")
  ,(".to",Number 1.0)]
, tPacket = Packet { paHead = HeadEmpty, paBody = Body ""}
, tChain = Just (TxTelex
   {tId = 0
   , tTo = HN "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a"
   , tOut = PNone
   , tJs = fromList []
   , tPacket = Packet
     { paHead = HeadJson "{\"path\":\"/chat/56419861/roster\",\"method\":\"get\"}"
     , paBody = Body ""}
   , tChain = Nothing
   , tLp = Nothing})
, tLp = Nothing}

```

chat flow
---------

hosted here, joined from there
--------------------------------

```
ext_chat:chan start:mp=Just (RxTelex
{rtId = 0
, rtSender = PIPv4 (PathIPv4 {v4Ip = 172.17.42.1, v4Port = 51130})
, rtAt = Thu May 15 08:24:49 SAST 2014
, rtJs = fromList
  [("seq",Number 0.0)
  ,("c",Number 5.0)
  ,("to",String "tft@0ecedc9f49472737b9285c0e10066fd860983bb5aa3a04e1f0acc3d3b3c5e348")
  ,("from",String "c2b0425e,1000")
  ,("type",String "chat")
  ,("roster",String "5dfc44e8")]
, rtPacket = Packet
    {paHead = HeadJson "{\"seq\":0,\"type\":\"chat\",\"c\":5,\"to\":\"tft@0ecedc9f49472737b9285c0e10066fd860983bb5aa3a04e1f0acc3d3b3c5e348\",\"from\":\"c2b0425e,1000\",\"roster\":\"5dfc44e8\"}"
    , paBody = Body ""}
, rtChanId = Nothing})
```

This results in a local chat structure

```
ext_chat: got chat
Chat
{ ecEp = "tft"
, ecId = ChatId {ciEndpoint = "tft"
                , ciOriginator = Just (HN "0ecedc9f49472737b9285c0e10066fd860983bb5aa3a04e1f0acc3d3b3c5e348")}
, ecIdHash = CH "56419861"
, ecOrigin = HN "0ecedc9f49472737b9285c0e10066fd860983bb5aa3a04e1f0acc3d3b3c5e348"
, ecHub = 1
, ecRHash = CH "74299b2c"
, ecLocal = True
, ecSeed = CH "726b08b1"
, ecSeq = 999
, ecRoster = fromList
  [("*","invited")
  ,("0ecedc9f49472737b9285c0e10066fd860983bb5aa3a04e1f0acc3d3b3c5e348","ee1f7099,1000")]
, ecConn = fromList []
, ecLog = fromList
   [("ee1f7099,1000"
     ,TxTelex
      {tId = 0
      , tTo = HN "0ecedc9f49472737b9285c0e10066fd860983bb5aa3a04e1f0acc3d3b3c5e348"
      , tOut = PNone
      , tJs = fromList
        [("id",String "ee1f7099,1000")
        ,("type",String "join")
        ,("at",Number 1.400135083e9)
        ,("text",String "ThreadId 168")]
      , tPacket = Packet {paHead = HeadEmpty, paBody = Body ""}
      , tChain = Nothing
      , tLp = Nothing})]
, ecMsgs = []
, ecJoin = Just "ee1f7099,1000"
, ecSent = Nothing
, ecAfter = Nothing}
```


---------------------------------------------------------

LINKTRY:HN "ce9d2cfccf34345b1c1a1c5b6c72cb0cf625ec88cdc64b54921303b26a655949"
16:40:11:<<<<:(HN "ce9d2cfccf34345b1c1a1c5b6c72cb0cf625ec88cdc64b54921303b26a655949","PNone","Packet HeadJson {\"seed\":true,\"c\":102} 0 bytes")
16:40:11:<<<<: { type: 'ipv4', ip: '208.68.164.253', port: 42424},len 49
16:40:11:<<<<: { type: 'ipv4', ip: '10.0.0.30', port: 42424},len 301
16:40:11:<<<<: { type: 'ipv4', ip: '172.17.42.1', port: 42424},len 301
16:40:11:<<<<: { type: 'ipv4', ip: '208.68.164.253', port: 42424},len 49
16:40:13:<<<<:(HN "ce9d2cfccf34345b1c1a1c5b6c72cb0cf625ec88cdc64b54921303b26a655949","PNone","Packet HeadJson {\"c\":360,\"paths\":[{\"ip\":\"105.237.110.153\",\"port\":54232,\"type\":\"ipv4\"}],\"type\":\"path\"} 0 bytes")
16:40:13:<<<<: { type: 'ipv4', ip: '208.68.164.253', port: 42424},len 113
16:40:13:<<<<: { type: 'ipv4', ip: '10.0.0.30', port: 42424},len 301
16:40:13:<<<<: { type: 'ipv4', ip: '172.17.42.1', port: 42424},len 301
16:40:13:>>>>:(Just "(chan:(284,CID 360,0,ChanStarting,\"path\"))","{ type: 'ipv4', ip: '208.68.164.253', port: 42424}","Packet HeadJson {\"path\":{\"type\":\"ipv4\",\"ip\":\"105.237.110.153\",\"port\":54232},\"c\":360} 0 bytes")
path_handler:got our remote ip:105.237.110.153
16:40:15:>>>>:(Just "(chan:(287,CID 365,0,ChanStarting,\"connect\"))","{ type: 'ipv4', ip: '208.68.164.253', port: 42424}","Packet HeadJson {\"from\":{\"1a\":\"453f34b17f16d9928a74e2aaca588e9e76e3a9e206b6d28378bb8cdf8bb93b65\"},\"paths\":[{\"type\":\"webrtc\"}],\"type\":\"connect\",\"c\":365} 40 bytes")
*** Exception: Data.Binary.Get.runGet at position 401: demandInput: not enough bytes

Hex logging for above
-----------------------

07:57:33:>>>>:recvTelex:208.68.164.253:42424 (48) 0000a23c11a9edecdeb73e8e6b24e546b7d39de155d6e194869cf398da76ba138f046ad6b903d166e41c472f73cfbc7e
07:57:33:>>>>:crypt_delineize_1a:inner a23c11a9edecdeb73e8e6b24e546b7d3 (22) 00147b2273656564223a747275652c2263223a31387d
07:57:33:>>>>:(Just "(chan:(14,CID 18,0,ChanOpen,\"link\"))","{ type: 'ipv4', ip: '208.68.164.253', port: 42424}","Packet HeadJson {\"seed\":true,\"c\":18} 0 bytes")
07:57:33:<<<<:(HN "ce9d2cfccf34345b1c1a1c5b6c72cb0cf625ec88cdc64b54921303b26a655949","PNone","Packet HeadJson {\"seed\":true,\"c\":18} 0 bytes")
07:57:33:<<<<: { type: 'ipv4', ip: '208.68.164.253', port: 42424},len 48
07:57:33:<<<<: { type: 'ipv4', ip: '208.68.164.253', port: 42424} (48) 00001a8838bffb9ef46e8b1d9cbcc7b1747eac38bbae0d3742f7dd5036b537ab249bef5cdbfe2bbddc4e329a3adea251
07:57:38:>>>>:recvTelex:208.68.164.253:42424 (2) 0000
07:57:38:>>>>:recvTelex:208.68.164.253:42424 (64) 0000a23c11a9edecdeb73e8e6b24e546b7d3e369db0be294869c89e837bde28869000c4ca05262c00d8e0ed8f634cc3999e6d97ff644406aec12c4d2202abd3c
07:57:38:>>>>:crypt_delineize_1a:inner a23c11a9edecdeb73e8e6b24e546b7d3 (38) 00247b227365656b223a22373736222c2274797065223a227365656b222c2263223a3133377d
07:57:38:>>>>:(Just "(chan:(33,CID 137,0,ChanStarting,\"seek\"))","{ type: 'ipv4', ip: '208.68.164.253', port: 42424}","Packet HeadJson {\"seek\":\"776\",\"type\":\"seek\",\"c\":137} 0 bytes")
07:57:38:>>>>:recvTelex:208.68.164.253:42424 (460) 00013a6fb5bc1b0312e8d8715f3959720b68efd941ec5f9d8e1090f775ebd4c4d9804d4f648ac46ecbb8605a020a250ebd38275fe9c31f6498d1cdd349ebf9c932af6b993ab852b453b8174eb552fea3e2505b2f4f619a37db796695ca22d802a3e978b0e02d811a1c9fd5483f343b7cb0b8e0ff06d72f84039384b66c4b680aba6992dca6834a6db688c882f0c830c6ee1c1838b4407eadb5631cb239ff93d282e3ba099396ea657f519b2ef103dd7d647fcc72b548f9af27f4dd029b0553e140d6a44db8a63e3e422a6c0e72b37886b896326611a050a7a8fbed607fae16595705be849e28966f6672130308a7452b312ed01e08de7cca710cfb23dc6bf04babab15170941383f412bc432b463204be8eaeff2d0dfa85c4149888692425504515629c7ed95901d07d8edba0cbe1d6789f6a060731502ff8f557c878fe9e8d0086fc813ed5188321fc16051fde50bc6645d091332f33efb828960d476f02987dca6ff1410ae821f3fba939b70521bc324abd89945efb9003cd7283bda1535472a92ebc7f458ad5e4a0e839768e98ac1212ca0e4db6453279001e4ff713964f4b64f69938c5236f11cd76bd3475eda215c2fba1077a1ca85c71a803e9f322766159274e06b9d7c6aba545971
07:57:38:>>>>:crypt_deopenize_1a:inner 31e463916f3012a0a5016a2bc6f2c391 (413) 6c3258f8cc77b7f6e7eaf963b02b2121fb9a7eec724160b3a20450c90b4bd195742bc892b6793876583b0ee51a254f7955eebd14180f44290700ae5cdec67626931c8c947d252e627ce84a130adf753cabcb33f05a72e43b26930a960a47fcc9adea862a5af26bd2f15c81e4a3c33b910dd15b2435d6c14bfdfe42c72e93078b78a2386cd61414744a8a3264b4aacea222af54d698b5e5f7e74c2c0476882f30173103fcbd2817467ddbdc7f20ae1f40b91c949a19f036d6135f4f1b4ccaf73c54de5c329510df2034a1a8b02b904f0ba8672fd8ef2024b2e6083e37821075d16371fe02622405e382352e9167c81ffcbaba2586b83006dba9a8f8aec8c660ca194c1976613c6565248bac3b3c41df2e3ed2e2715faa4f901514bf92bf2d314bedd1abfb41ed46b5f4891dd0a7ee22892b55337ac6d5d6eee4883a4cbb258924399683cbb0e7e6a7bdf18bd70f50748994b6db0f52ba1b25328e2faa5c7db7c080124dd9dc4e0831c6574ede3bc9971557ff0b4bc1daa421b7d5d0c64579e2951c541934363c60e32ab0462e30de4d396ea6f01a62686e1c5307e7feb2
*** Exception: Data.Binary.Get.runGet at position 413: demandInput: not enough bytes
