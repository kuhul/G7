#include "SDL/SDL_net.bi"
#include "SDL/SDL.bi"
#include "SDL/SDL.bi"


DIM shared sock AS tcpsocket 'Socket einstellen
dim shared sockset as SDLNet_SocketSet
dim shared gensoc as SDLNet_GenericSocket
DIM shared ip AS ipaddress 'Variable IP vom Typ IP Addresse
DIM shared sockclient(10) AS TCPsocket
DIM shared names(10) AS string
dim shared as integer anzclients
dim shared as Uint16 port=5678

enum servercommands
    senderror  =-1
    sendstring =1
    sendinteger=2
    askfordata =3
    fullack    =42
    wrongtype  =43
end enum


' ****************************************************** WaitForClient
function WaitForClient(num as integer) as integer
  sockclient(num) = NULL
  PRINT "Warte auf Client ";num;"...";
  WHILE sockclient(num) = NULL
    sleep 100
    sockclient(num) = SDLNet_TCP_Accept( sock )
  WEND
  gensoc=cast(SDLNet_GenericSocket,sockclient(num))
  return SDLNet_AddSocket(sockset,gensoc)
end function

' ****************************************************** SendString
function SendString(client as integer,txt as string) as integer
dim as integer count,succ,comm,ack
  comm=servercommands.sendstring
  count=len(txt)
  SDLNet_TCP_Send(sockclient(client), @comm, 2 )
  SDLNet_TCP_Send(sockclient(client), @count, 2 )
  SDLNet_TCP_Send(sockclient(client), StrPtr(txt), count )
  succ=SDLNet_TCP_Recv(sockclient(client), @ack, 2 )
  if succ<=0 then ? "Err TCP RECV":SDLNet_TCP_Close( sock ):return -1
  return ack
end function

' ****************************************************** SendInteger
function SendInteger(client as integer,wert as integer) as integer
dim as integer succ,comm,ack
  comm=servercommands.sendinteger
  SDLNet_TCP_Send(sockclient(client), @comm, 2 )
  SDLNet_TCP_Send(sockclient(client), @wert, 2 )
  succ=SDLNet_TCP_Recv(sockclient(client), @ack, 2 )
  if succ<=0 then ? "Err TCP RECV":SDLNet_TCP_Close( sock ):return -1
  return ack
end function

' ****************************************************** GetCommand
function GetCommand(client as integer, BYREF comm  as integer) as integer
dim as integer succ,ack
  succ=SDLNet_TCP_Recv(sockclient(client), @comm, 2 )
  if succ<=0 then ? "Err TCP RECV":SDLNet_TCP_Close( sock ):return servercommands.senderror
  return comm
end function

' ****************************************************** GetString
function GetString(client as integer, BYREF txt  as string) as integer
dim as integer count,succ,ack,comm
  succ=SDLNet_TCP_Recv(sockclient(client), @count, 2 )
  if succ<=0 then ? "Err TCP RECV":SDLNet_TCP_Close( sock ):return -1
  txt=space(count)
  SDLNet_TCP_Recv(sockclient(client), StrPtr(txt), count)
  ack=servercommands.fullack
  SDLNet_TCP_Send(sockclient(client), @ack, 2 )
  return ack
end function

' ****************************************************** GetInteger
function GetInteger(client as integer, BYREF wert  as integer) as integer
dim as integer count,succ,ack,comm
  'succ=SDLNet_TCP_Recv(sockclient(client), @comm, 2 )
  'if comm=servercommands.sendinteger then
    succ=SDLNet_TCP_Recv(sockclient(client), @wert, 2 )
    if succ<=0 then ? "Err TCP RECV":SDLNet_TCP_Close( sock ):return -1
    ack=servercommands.fullack
  'else
  '  ack=servercommands.wrongtype
  'end if
  SDLNet_TCP_Send(sockclient(client), @ack, 2 )
  return ack
end function


' ****************************************************** 
' main
' ****************************************************** 

dim as string host,txt,clientname
dim as integer nummer,count,succ,i,bef,wert

DO: Input "1-Server 2-Client :",nummer :LOOP UNTIL nummer=1 OR nummer=2

select case(nummer)

case (1) ' Server
   input "# Clients: ",anzclients
   sockset=SDLNet_AllocSocketSet(anzclients)
   IF sockset=0 THEN ? "SDLNet_AllocSocketSet":sleep:end
   
   IF SDLNet_Init() <> 0 THEN ? "Err SDL_INIT":sleep:end
   
   SDLNet_Write32( INADDR_ANY, @ip.host )
   SDLNet_Write16( port, @ip.port )
   l1:
   sock = SDLNet_TCP_Open( @ip )'TCP Verbindung öffnen, in den Socket schreiben
   if sock=NULL then ? "Err TCP OPEN":sleep 1000:goto l1
   
   for i=1 to anzclients
     succ=WaitForClient(i)
     if succ<>i then ? "Err SDLNet_TCP_AddSocket":sleep 1000:goto l1
     succ=GetCommand(i,bef)
     if (bef<>servercommands.sendstring) then ? "Err playername":sleep 1000:end
     succ=GetString(i,names(i))
     ? " Hi ";names(i);"!"
   next i
   
   do
     succ=SDLNet_CheckSockets(sockset, 1000)
     if succ>0 then
       for i=1 to anzclients
         if (sockclient(i) <> 0 and cast(SDLNet_GenericSocket,sockclient(i))-> ready) then
           succ=GetCommand(i,bef)
           select case (bef)
           case(servercommands.sendstring)
               succ=GetString(i,txt)
               ? names(i);":";txt
           case(servercommands.sendinteger)
               succ=GetInteger(i,wert)
               ? names(i);":";wert
           end select
         end if
       next i
     end if
   loop

case (2) 'Client
   IF SDLNet_Init() <> 0 THEN ? "Err SDL_INIT":sleep:end
   l2:
   host="192.168.2.151"
   clientname="Bock2Zock"
   'INPUT "IP Addresse des Hostes: ", host
   ? "IP Addresse des Hostes: ", host
   SDLNet_ResolveHost( @ip, host, port )
   sockclient(1) = SDLNet_TCP_Open( @ip )
   succ=SendString(1,clientname)
   do
     input "?: ",txt
     if txt="quit" then SDLNet_TCP_Close( sockclient(1) ):end
     succ=SendString(1,txt)
     if succ<=0 then ? "Err TCP RECV":SDLNet_TCP_Close( sock ):goto l2
   loop
end select
