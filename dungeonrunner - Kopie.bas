' ************************
' DUNGEONRUNNA
' (c) 2016-2019
' ************************


#include "SDL/SDL_net.bi"
#include "SDL/SDL.bi"
#include "SDL/SDL.bi"


DIM shared sock AS tcpsocket 'Socket einstellen
dim shared sockset as SDLNet_SocketSet
dim shared gensoc as SDLNet_GenericSocket
DIM shared ip AS ipaddress 'Variable IP vom Typ IP Addresse
DIM shared sockclient(1) AS TCPsocket
dim shared port as Uint16

enum servercommands
    senderror  =-1
    sendstring =1
    sendinteger=2
    askfordata =3
    fullack    =42
    wrongtype  =43
end enum


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



'------------------------------------------------------------------------------- Includes
#include once "GL/gl.bi"
#include once "GL/glu.bi"
#include once "crt.bi"         '' scanf is used to parse data file
#include once "fbgfx.bi"        '' for Scan code constants
#include once "createtex.bi"
#include once "objFormat.bi"
#include once "interpolation.bi"

'------------------------------------------------------------------------------- Strukturen
type Monster
    hp as integer
    hpm as integer
    dmg as integer
    xpos as single
    ypos as single
    angle as single
    isAlive as integer
    attack as integer
    dreh as single
    typ as integer
    texture as integer
    agility as single
    stein as integer
end type


'------------------------------------------------------------------------------- Konstanten
const piover180 = 0.0174532925f ' für trigo-Fkt
const sichtweite = 10 '25 '9
const stepphi = 4
const steptheta = 2
const stepforward = 0.1
const coll = 0.2  ' Kollisionstoleranz Spieler->Raum
const coll2 = 0.3 ' Kollisionstoleranz Monster->Raum
const coll3 = 0.5 ' Kollisionstoleranz Monster->Spieler
const bounce = 1./80. ' auf/abwaertsbewegung spieler
const maxobjects = 1000 ' Maximale Anzahl Objekte
const maxtriags = 10000 ' Maximale Anzahl Dreiecke pro Objekt
const maxmonsters = 50 ' Maximale Anzahl an Monstern gleichzeitig
const screenx = 1900., screeny=1000. ' Auflösung Gamefenster
const flaechentol = 0.01
const path = "c:/G7/"
const modelPath = path + "model/"
const texturePath = path + "texturen/"
const levelPath = path + "level/"
const mouseSensitivity = 0.2
const playerSpeed = 20

'------------------------------------------------------------------------------- Deklarationen
declare sub readstr(byval f as integer, byref Buffer as string)
declare sub SetupPlayfield()
declare sub drawobject(byval goffx as single,byval goffy as single,byval l1 as single,byval l2 as single,_
					   byval txt1 as integer,byval j as integer,byval ang as integer,byval ang2 as integer)
declare sub BuildFont()
declare sub BuildTextures()
declare sub glPrint cdecl (byval x as integer, byval y as integer, byval gset as integer,_
						   byref fmt as string, ...)
declare sub frontpos(byval yrot as integer, byref xpos as integer, byref ypos as integer)
declare sub BehindActive(byref xp as integer, byref yp as integer)
declare sub inputstls()
declare sub readstl(byval filename as string,byval number as integer,byval scale as single,_
					byval orientation as integer)
declare sub readAnimation(byval filename as string,byval number as integer,byval scale as single,_
						  byval orientation as integer, byval numberOfFrames as integer)
declare sub readvertex(byref vx as single,byref vy as single,byref vz as single,byref oline as string)
declare sub readnormal(byref vx as single,byref vy as single,byref vz as single,byref oline as string)
declare sub definetextcoords(byval number as integer,byval counter as integer,byval triag as integer,_
							 byval vx as single,byval vy as single,byval vz as single,byval orientation as integer)
declare sub switchall(byval startx as integer,byval starty as integer,byval c1 as string,byval c1 as string,_
					  byval layer as integer)
declare sub toggleall(byval startx as integer,byval starty as integer,byval c1 as string,byval c1 as string,_
					  byval layer as integer)
declare function loadtexture(byval filename as string, byval number as integer) as integer
declare function loadtexturePNG(byval filename as string, byval number as integer) as integer
declare sub DrawField(byval locx as integer, byval locy as integer)
declare sub ApplySwitches(byval startx as integer,byval starty as  integer)
declare sub TestCollision(byval startx as single,byval starty as single, byref deltax as single,_
						  byref deltay as single,byref deltaz as single)
declare sub TestTreppe(byval startx as integer,byval starty as integer, byref deltay as single)
declare sub TestLoch(byval startx as integer,byval starty as integer, byref deltax as single,byref deltay as single,_
					 byref deltaz as single)
declare sub TestTeleport(byref  startx as integer,byref starty as integer, byref deltax as single,_
						 byref deltay as single,byref deltaz as single)
declare sub TestMonster(byval startx as integer,byval starty as integer)
declare sub PlayerMove(byref startx as single,byref starty as single, byval deltax as single,byval deltay as single)
declare sub PrintStatus()
declare sub PrintMonsterHealth(byval index as integer)
declare function winkeladd(byval w1 as single,byval w2 as single) as single
declare function limit(byval w as single,byval l1 as single,byval l2 as single) as single
declare sub PlayerKeyUp(byval heading as single,byref deltax as single,byref deltay as single,byref deltaz as single)
declare sub PlayerKeyDown(byval heading as single,byref deltax as single,byref deltay as single,byref deltaz as single)
declare sub PlayerKeyLeft(byval heading as single,byref deltax as single,byref deltay as single,byref deltaz as single)
declare sub PlayerKeyRight(byval heading as single,byref deltax as single,byref deltay as single,_
						   byref deltaz as single)
declare sub FindSteinId(byval xp as integer, byval yp as integer, byref fx as integer, byref fy as integer)
declare sub SwitchAction(byval startx as integer,byval starty as integer, byval c1 as integer,byval c2 as integer)
declare sub DrawPlayField(byval xpos as integer,byval ypos as integer )
declare function getMonsterIndexAt(byval xpos as integer, byval ypos as integer) as integer
declare sub updateMonstersOnMap()
declare sub readUserDataFromFile(byval filename as string, byref serverIP as string, byref serverPort as string,_
								 byref clientname as string)
declare sub initClient()
declare sub drawlocalobject(byval txt1 as integer,byval j as integer)
declare sub drawplayerobject(byval goffx as single,byval goffy as single,byval goffz as single,byval goffw as single,_
							 byval goffw2 as single,byval hitx as single,byval hity as single,byval hitz as single,_
							 byval txt1 as integer,byval j as integer)
declare sub TestCollision2(byval startx as single,byval starty as single, byref deltax as single,_
						   byref deltay as single)
declare sub readobj(byval filename as string, byval number as integer, byval scale as single)
declare sub drawUIRect(w as integer, h as integer, x as integer, y as integer, txtx as single, txty as single, txtnr as integer)

'------------------------------------------------------------------------------- Globale Variablen
dim shared as integer playfield(64,64,8,9)        ' playfield: x,y, funktionsebene,level
dim shared filter as uinteger                  '' Which Filter To Use
dim shared texture(0 to 30) as uinteger         '' Storage For Textures
dim shared gbase as uinteger                       '' Base Display List For The Font
dim shared triags(maxobjects,maxtriags,3,3) as single
dim shared triagstexture(maxobjects,maxtriags,3,2) as single
dim shared triagsn(maxobjects,maxtriags,3) as single
dim shared triagsvertexn(1 to maxobjects, 1 to maxtriags, 1 to 3, 1 to 3) as single 'je Objekt, je Dreieck, je Vertex die Vertexnormale
dim shared triagsanz(maxobjects) as integer
dim shared as single ex(3), ey(3), ez(3)
dim shared as integer hp,sp,st ',startx,starty
dim shared as integer hpm,spm,stm,monsteranz(9)
dim shared as integer lv,coin,anztriag
dim shared as single ypos,yrot,walkbiasangle,lookupdown,ztol,ichx,ichy,monsterx, monstery
dim shared as integer level,treppenflag,treppenvorflag,lochflag,monsterflag
dim shared savenames(9) as string
dim shared as single deckentol,bodentol
dim shared as integer animationTimer = 0 'Zähler um zum nächsten Animationsframe zu switchen
dim shared as double attackCooldown
dim shared monsters(maxmonsters, 9) as Monster
dim shared as string serverIP, serverPort, clientname

'------------------------------------------------------------------------------- lokale Variablen Main
dim as single matdata(4),lightpos1(4),lightpos2(4),diffuse(4)
dim as integer face,tflag,lx,ly,l1,l2,monID,stein,i,j,k
dim as integer dummyx,dummyy,mousex,mousey,mouseWheel,mouseButton,mouseofx,mouseofy,htemp,ltemp,waffani,waffenmode,weaponflag,timerflag
dim as single deltax,deltay,deltaz, waffendreh,mdx,mdy,md
dim as single lifebar(1 to 7) 'w, h, x, y, txtx, txty, txtnr
dim as double deltaTime, startTime, endTime, mouseTimer
dim as integer playerDamage

'------------------------------------------------------------------------------- init Globale Variablen
'Serverzeug
readUserDataFromFile("c:\G7\userdata\userdata.txt", serverIP, serverPort, clientname)
'''''''''''''''''''''''''''''initClient()
'Einheitsvektoren
ex(1)=1:  ey(2)=1:  ez(3)=1
'Kameradaten
lookupdown=0: yrot=0
walkbiasangle=0
'Waffen
'  basicWeapon.title = "Holzkeule"
'  basicWeapon.bonusDmg = 25
'Spielerdata
hp=2000: hpm=2000
sp=0:  spm=0
st=5:  stm=5
coin=0: lv=1
'  attackCooldown = 0
'  player1.weapon = basicWeapon
'levels
savenames(1)=levelPath+"level1.txt":  savenames(2)=levelPath+"level2.txt":  savenames(3)=levelPath+"level3.txt"
savenames(4)=levelPath+"level4.txt":  savenames(5)=levelPath+"level5.txt":  savenames(6)=levelPath+"level6.txt"
savenames(7)=levelPath+"level7.txt":  savenames(8)=levelPath+"level8.txt":  savenames(9)=levelPath+"level9.txt"
deckentol=bodentol=0.

'Animationen
'  affenAnimation.numOfFrames = 4
'  affenAnimation.duration = 30


'monsters(0).path = "c:\G7\animation\monster\affe\"
'------------------------------------------------------------------------------- init main Variablen
deltaTime = 0
startTime = 0
endTime = 0
mouseTimer = 1.0

lifebar(1) = 137
lifebar(2) = 31
lifebar(3) = 500
lifebar(4) = 100
lifebar(5) = 0
lifebar(6) = 0
lifebar(7) = 25

yrot=3*90:level=1 'Startpunkt Player
ypos=0
ichx=2.5: ichy=2.5
tflag=0 'Maustotzeit-Flag
face=4
waffendreh=15
waffani=0
waffenmode=0
weaponflag=-1
playerDamage = 50
timerflag=0
'------------------------------------------------------------------------------- init gamedata
SetupPlayfield()
inputstls()
'stein2=playfield(fix(monsters(0).xpos), fix(monsters(0).ypos),1,level)
'playfield(fix(monsters(0).xpos), fix(monsters(0).ypos),1,level) = asc("X")

'------------------------------------------------------------------------------- init Screen & GL
ScreenRes(screenx, screeny, 16, , 2)
setmouse(screenx/2, screeny/2, 1, 0)
glViewport(0, 0, screenx,screeny)
glMatrixMode(GL_PROJECTION)
glLoadIdentity()
gluPerspective(75.0, screenx/screeny, 0.1, 100.0)   ' Sichtinkel, Persp Verzerrung, Sichtweite min, max
glMatrixMode(GL_MODELVIEW)
glLoadIdentity()

BuildTextures()' Texturen laden
BuildFont()

glEnable(GL_TEXTURE_2D)
glBlendFunc(GL_SRC_ALPHA, GL_ONE)
glClearColor(0.0, 0.0, 0.0, 0.5)
glClearDepth(1.0)
glDepthFunc(GL_LESS)
glEnable(GL_DEPTH_TEST)
glShadeModel(GL_SMOOTH)
glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST)

' Material
matdata(0)=1
matdata(1)=1
matdata(2)=1
matdata(3)=2
glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, @matdata(0))
' Licht
lightpos1(0)=-1
lightpos1(1)=1
lightpos1(2)=1
lightpos1(3)=0
' Color
diffuse(0)=1
diffuse(1)=1
diffuse(2)=1
diffuse(3)=1

glEnable(GL_LIGHTING)
glLightfv(GL_LIGHT1, GL_DIFFUSE, @diffuse(0))
glLightf(GL_LIGHT1, GL_CONSTANT_ATTENUATION, 0.5)
glLightf(GL_LIGHT1, GL_LINEAR_ATTENUATION, 0.3)
glLightf(GL_LIGHT1, GL_QUADRATIC_ATTENUATION, 0.1)
glEnable(GL_LIGHT1)

'------------------------------------------------------------------------------- monsterinit
stein = playfield(fix(ichx), fix(ichy),1,level)
playfield(fix(ichx), fix(ichy),1,level)=asc("Y") ' Player

for k=1 to 9
	monsteranz(k) = 0
Next

for k=1 to 9 'Level
	for i=1 to 64
		for j=1 to 64
			if (playfield(i,j,1,k) = asc("X")) then
			monsters(monsteranz(k),k).hpm = 500
			monsters(monsteranz(k),k).hp = 500
			monsters(monsteranz(k),k).xpos = i+0.5
			monsters(monsteranz(k),k).ypos = j+0.5
			monsters(monsteranz(k),k).isAlive = 1
			monsters(monsteranz(k),k).attack = 0
			monsters(monsteranz(k),k).dreh = 0
			monsters(monsteranz(k),k).texture = 24
			monsters(monsteranz(k),k).typ = 35
			monsters(monsteranz(k),k).agility = 0.02
			monsters(monsteranz(k),k).stein = 32
			monsteranz(k) = monsteranz(k)+1
			end if
		next j
	next i
next k

 '------------------------------------------------------------------------------- MAINLOOP
  do
  	startTime = timer()
    treppenflag=0:treppenvorflag=0:lochflag=0:monsterflag=0
    if (hp<0) then
      lookupdown=80
    else
      ' Action
      GetMouse(mousex, mousey, mouseWheel, mouseButton)
      if (mouseButton=1 and waffani<10 and weaponflag=1) then ' Schlag zu
      	waffendreh = sin(waffani)*15
      	waffani += 1
      	waffenmode = 1
      end if
      if (mouseButton<>1) then ' Totzeit
      	tflag = 0
      	waffenmode=0
      	waffendreh=15
      	waffani=0
      EndIf
      if (mouseButton=1 and tflag=0 and waffenmode=0) then ApplySwitches(fix(ichx),fix(ichy)):tflag=1
      if MULTIKEY(FB.SC_W) then PlayerKeyUp(yrot, deltax,deltay,deltaz)
      if MULTIKEY(FB.SC_S) then PlayerKeyDown(yrot, deltax,deltay,deltaz)
      if MULTIKEY(FB.SC_A) then PlayerKeyLeft(yrot, deltax,deltay,deltaz)
	  if MULTIKEY(FB.SC_D) then PlayerKeyRight(yrot, deltax,deltay,deltaz)
	  if MULTIKEY(FB.SC_X and timerflag=0) then
	  	weaponflag = -weaponflag
	  	timerflag = 5
	  end if
      mouseofx = screenx/2
      mouseofy = screeny/2
      htemp = yrot
      ltemp = lookupdown
      if (mousex<>mouseofx) then yrot = winkeladd(htemp,-(mousex-mouseofx)/3)
      if (mousey<>mouseofy) then lookupdown = limit(ltemp+(mousey-mouseofy)/3, -50, 50)
    end if
    setmouse(lerp(mousex, screenx/2, 0.7), lerp(mousey, screeny/2, 0.7)) ' Move mouse back to center

    ' DEBUG MODE
    dummyx=fix(ichx): dummyy=fix(ichy)
    frontpos(yrot,dummyx,dummyy)
    if MULTIKEY(FB.SC_0) then playfield(dummyx,dummyy,1,level)=asc(" ")   'kill block

    ' Kollisionen ?
    playfield(fix(ichx), fix(ichy),1,level)=stein
    TestLoch(fix(ichx),fix(ichy), deltax,deltay,deltaz) ' Lochabfrage
    TestTreppe(fix(ichx),fix(ichy), deltay) ' Treppenabfrage
    'TestCollision(ichx,ichy, deltaz,deltax,deltay) ' Kollisionsabfrage4
    TestTeleport(fix(ichx),fix(ichy), deltax,deltay,deltaz)
    'TestMonster(startx,starty) 'Monsterabfrage
    PlayerMove(ichx,ichy, deltaz,deltax) ' Spieler-Bewegung
    stein=playfield(fix(ichx), fix(ichy),1,level)
    playfield(fix(ichx), fix(ichy),1,level)=asc("Y")
    deltax=0.
    deltaz=0.

    ' GL init
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)  'Screen&Buffer loeschen
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    gluPerspective(75.0, screenx/screeny, 0.1, 100.0)
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    glPushMatrix()

    ' GL-Rotation / Translation
    glRotatef(lookupdown, 1.0, 0,0)    ' hoch/runter
    glRotatef(-yrot, 0, 1.0, 0)   ' drehen
    glTranslatef(-ichx, -(level+deltay-0.5), -ichy)

    'Beleuchtung
    lightpos2(0)=ichx :   lightpos2(1)=level+deltay-0.5: lightpos2(2)=ichy:  lightpos2(3)=1
    glLightfv GL_LIGHT1,GL_POSITION,@lightpos2(0)

    ' Auf Treppe oberes Playfield zeichnen
    if treppenflag=1 or treppenvorflag=1 then
      level=level+1 'Darstellung verschoben
      DrawPlayField(fix(ichx),fix(ichy))
      level=level-1
    end if

    ' Vor Loch unteres Playfield zeichnen
    if lochflag=1 then
      level=level-1 'Darstellung verschoben
      DrawPlayField(fix(ichx),fix(ichy))
      level=level+1
    end if

    ' Playfield zeichnen
    deckentol=-flaechentol:bodentol=flaechentol
    DrawPlayField(fix(ichx),fix(ichy))
    deckentol=0.:bodentol=0.

    ' Spielerwaffe zeichnen falls gezückt
    if (weaponflag=1) then DrawObject(-0.2,0.0,ichx,ichy,1,34,(180-yrot),waffendreh)

    for i=0 to monsteranz(level)+1
      if (monsters(i,level).isAlive=1 and hp>=0) then
        ' Monsterdrehung
        monsters(i,level).dreh=atan2((ichy-monsters(i,level).ypos),(ichx-monsters(i,level).xpos))/3.14*180-90

        ' Monsterbewegung
        mdx=(ichx-monsters(i,level).xpos)
        mdy=(ichy-monsters(i,level).ypos)
        md=monsters(i,level).agility/sqr(mdx*mdx+mdy*mdy)
        mdx=mdx*md
        mdy=mdy*md
        playfield(fix(monsters(i,level).xpos), fix(monsters(i,level).ypos),1,level) = monsters(i,level).stein
        TestCollision2((monsters(i,level).xpos),(monsters(i,level).ypos), mdx,mdy) ' Kollisionsabfrage4
        monsters(i,level).xpos=monsters(i,level).xpos+mdx
        monsters(i,level).ypos=monsters(i,level).ypos+mdy
        monsters(i,level).stein=playfield(fix(monsters(i,level).xpos), fix(monsters(i,level).ypos),1,level)
        playfield(fix(monsters(i,level).xpos), fix(monsters(i,level).ypos),1,level) = asc("X")

       ' Monster in der Naehe Spieler ?
        if (abs(monsters(i,level).xpos-ichx)<=1 and abs(monsters(i,level).ypos-ichy)<=1) then
          monsters(i,level).attack=monsters(i,level).attack+1
          if (monsters(i,level).attack>10) then
          	monsters(i,level).attack=1
          	hp -= 10
          EndIf
          ' ist das Monster in Sichtrichtung ?
          if (abs(atan2((ichy-monsters(i,level).ypos),(ichx-monsters(i,level).xpos))/3.14*180-90+yrot)<15) then
              if (waffenmode=1 and waffani=5) then
                monsters(i,level).hp -= playerDamage
                if (monsters(i,level).hp <= 0) then
                    monsters(i,level).isAlive = 0
                    playfield(fix(monsters(i,level).xpos), fix(monsters(i,level).ypos),1,level) = monsters(i,level).stein
                    monsters(i,level).xpos = 0
                    monsters(i,level).ypos = 0
                end if
              end if
              PrintMonsterHealth(i)
          end if
        else
          monsters(i,level).attack=0
        end if
      end if
    next i
    
    glPopMatrix()

    lv=level
    PrintStatus()
    
	glFlush() ' GL-Ausführen
    screensync
    if (timerflag>0) then timerflag=timerflag-1
    flip ' Doublebuffering
    ' Timeing
    mouseTimer += deltaTime
    endTime = timer()
    deltaTime = endTime - startTime
  loop while MULTIKEY(FB.SC_ESCAPE)=0  ' ESC = Exit aus dem Spiel
  while inkey <> "": wend ' Tastenbuffer leeren
  end

'------------------------------------------------------------------------------- initClient
' intitialisiert den client
sub initClient()
    dim succ as integer
    dim txt as string
    IF SDLNet_Init() <> 0 THEN ? "Err SDL_INIT":sleep:end
    l2:
    SDLNet_ResolveHost( @ip, serverIP, port )
    sockclient(1) = SDLNet_TCP_Open( @ip )
    succ=SendString(1,clientname)
    do
        input "?: ",txt
        if txt="quit" then SDLNet_TCP_Close( sockclient(1) ):end
        succ=SendString(1,txt)
        if succ<=0 then ? "Err TCP RECV":SDLNet_TCP_Close( sock ):goto l2
    loop
end sub

'------------------------------------------------------------------------------- readUserDataFromFile
' liest spielerspezifische Daten von einer Textdatei und speichert diese in die Variablen serverIP, serverPort...
sub readUserDataFromFile(byval filename as string, byref serverIP as string, byref serverPort as string, byref clientname as string)
    dim as integer fp
    dim as string temp = "#"
    fp = freefile
    if (open (filename for input as #fp) <> 0) then end 1
    readstr(fp,serverIP)
    readstr(fp,serverPort)
    readstr(fp,clientname)
    port = cast(Uint16,val(serverPort))
    'print (port)
    close #fp
end sub

'------------------------------------------------------------------------------- updateMonstersOnMap
' synchronisiert die Position des X-Markers auf der Map mit der Monsterposition
'sub updateMonstersOnMap()
'    dim as integer x,y,i
'    'löscht alle X-Marker auf der Map und setzt dann neue
'    for y = 1 to 64
'        for x = 1 to 64
'            if (playfield(x,y,1,level) = asc("X")) then
'                playfield(x,y,1,level) = asc(" ")
'            end if
'        next x
''    next y
'   for i = 0 to maxmonsters-1
'        if (monsters(i).isAlive <> 0) then
'            playfield(floor(monsters(i).xpos), floor(monsters(i).ypos),1,level) = asc("X")
'        end if
'    next i
'end sub

'------------------------------------------------------------------------------- getMonsterIndexAt
' Gibt den index des Monsters an der position xpos,ypos zurück oder -1
' falls kein Monster an der Position
'function getMonsterIndexAt(byval xpos as integer, byval ypos as integer) as integer
'    dim as integer i = 0
'    for i = 0 to maxmonsters-1
'        if ((monsters(i).xpos = xpos) and (monsters(i).ypos = ypos) and (monsters(i).isAlive = 1)) then
'            return i
'        end if
'    next i
'
'    return -1
'end function

'------------------------------------------------------------------------------- PrintStatus
' Zeichnet Statusanzeige
sub PrintStatus()
	glEnable(GL_BLEND)
	glDisable(GL_LIGHTING)
	glMatrixMode (GL_PROJECTION)
	glPushMatrix()
	glLoadIdentity()
	glOrtho(0, screenx, 0, screeny, - 1, 1)
	glMatrixMode(GL_MODELVIEW)
	glColor3f(1.0, 1.0, 1.0)
	glPrint(30, 150, 0,"HP: "+str(hp)+"/"+str(hpm)) 'hp hmp
	'glPrint (30, 150-30, 0,"SP: "+str(ichx)+"/"+str(ichy)) ' sp spm
	'glPrint (30, 150-2*30, 0,"ST: "+str(monsters(0).xpos)+"/"+str(monsters(0).ypos)) 'st stm
	'glPrint (30, 150-3*30, 0,"Weapon: "+player1.weapon.title) 'st stm
	glPrint(30, screeny, 0,"Level: "+str(lv))
	glPrint(30, screeny-30, 0,"Coinage: "+str(coin))
	glViewport(0, 0, screenx, screeny)
	glMatrixMode(GL_PROJECTION)
	glPopMatrix()
	glMatrixMode(GL_MODELVIEW)
	glEnable(GL_LIGHTING)
	glDisable(GL_BLEND)
end sub

'------------------------------------------------------------------------------- PrintMonsterHealth
' Zeichnet Healthbar des Monsters
' index: index des Monsters in monsters liste
sub PrintMonsterHealth(byval index as integer)
    glDisable(GL_LIGHTING)
    glMatrixMode(GL_PROJECTION)
    glPushMatrix()
    glLoadIdentity()
    glOrtho(0, screenx, 0, screeny, - 1, 1)
    glMatrixMode(GL_MODELVIEW)
    glColor3f(1.0, 1.0, 1.0)
    glPrint(screenx/2, screeny-20, 0, "HP: "+str(monsters(index,level).hp)+"/"+str(monsters(index,level).hpm)) 'Healthbar
    glViewport(0, 0, screenx, screeny)
    glMatrixMode(GL_PROJECTION)
    glPopMatrix()
    glMatrixMode(GL_MODELVIEW)
    glEnable(GL_LIGHTING)
end sub

'------------------------------------------------------------------------------- PlayerKeyUp
' Key-Up Action
sub PlayerKeyUp(byval heading as single, byref deltax as single, byref deltay as single, byref deltaz as single)
	deltaz = -sin(heading*piover180) * stepforward
	deltax = -cos(heading*piover180) * stepforward
	walkbiasangle = winkeladd(walkbiasangle,10.)  'Laufbewegung
	deltay = sin(walkbiasangle * piover180)*bounce
end sub

'------------------------------------------------------------------------------- PlayerKeyDown
sub PlayerKeyDown(byval heading as single, byref deltax as single,byref deltay as single,byref deltaz as single)
	deltaz = sin(heading*piover180) * stepforward
	deltax = cos(heading*piover180) * stepforward
	walkbiasangle = winkeladd(walkbiasangle,-10.)  'Laufbewegung
	deltay = sin(walkbiasangle * piover180)*bounce
end sub

'------------------------------------------------------------------------------- PlayerKeyLeft
sub PlayerKeyLeft(byval heading as single, byref deltax as single,byref deltay as single,byref deltaz as single)
	deltaz = -sin((heading+90)*piover180) * stepforward
	deltax = -cos((heading+90)*piover180) * stepforward
end sub

'------------------------------------------------------------------------------- PlayerKeyRight
sub PlayerKeyRight(byval heading as single, byref deltax as single,byref deltay as single,byref deltaz as single)
	deltaz = -sin((heading-90)*piover180) * stepforward
	deltax = -cos((heading-90)*piover180) * stepforward
end sub

'------------------------------------------------------------------------------- frontpos
' gibt die feldpos vor dem Spieler
sub frontpos(byval winkel as integer, byref xp as integer, byref yp as integer)
	if (winkel >=365-45 or winkel<45)  then
        if (yp>1) then yp=yp-1
    elseif (winkel >=45 and winkel<45+90)  then
        if (xp>1) then xp=xp-1
    elseif (winkel>=45+90 and winkel<45+180)  then
        if (yp<64) then yp=yp+1
    elseif (winkel>=45+180 and winkel<45+180+90)  then
        if (xp<64) then xp=xp+1
    end if
end sub

'------------------------------------------------------------------------------- BehindActive
' gibt die feldpos nach dem aktiven Stein (beruecksichtigt Drehung)
sub BehindActive(byref xp as integer, byref yp as integer)
dim as integer ang
  ang=playfield(xp,yp,3,level)-asc("0")
  select case (ang)
  case 0:
    xp=xp+1
  case 1:
    yp=yp-1
  case 2:
    xp=xp-1
  case 3:
    yp=yp+1
  end select
end sub

'------------------------------------------------------------------------------- FindSteinId
' gibt die zum Stein xp,yp gehörigen Stein zurück
sub FindSteinId( byval xp as integer, byval yp as integer, byref fx as integer, byref fy as integer)
dim as integer l1,l2
  fx=0
  fy=0
  for l1=1 to 64
    for l2=1 to 64
      ' muss Ziel (actionid="0"), darf keine Quelle(actionid<>"0") sein
      if (((l1<>xp) or (l2<>yp)) and (playfield(l1,l2,5,level)=asc("0"))and (playfield(l1,l2,4,level)=playfield(xp,yp,4,level))) then
          fx=l1
          fy=l2
          return
      end if
    next
  next
end sub

'------------------------------------------------------------------------------- gettriagnormal
'holt Normalenvektor Dreieck: objektnummer, dreiechnummer, vektor
sub gettriagnormal(byval number as integer, byval counter as integer, v() as single)
    v(1)=triagsn(number,counter,1)
    v(2)=triagsn(number,counter,2)
    v(3)=triagsn(number,counter,3)
end sub

'------------------------------------------------------------------------------- sprod
'Skalarprodukt: vektor 1,2
function sprod(v1() as single,v2() as single) as single
    return v1(1)*v2(1)+v1(2)*v2(2)+v1(3)*v2(3)
end function

'------------------------------------------------------------------------------- definetextcoords
'projektion Texturkoordinaten:
'objektnummer,Dreiecknummer, Nummer Ecke, Eckkoord x/y/z, Projektionsstrategie (1,2,3)
sub definetextcoords(byval number as integer,byval counter as integer,byval triag as integer,byval vx as single,byval vy as single,byval vz as single,byval orientation as integer)
dim as single nv(3)
  triags(number,counter,triag,1)=vx
  triags(number,counter,triag,2)=vy
  triags(number,counter,triag,3)=vz
  select case (orientation)
  case 1: 'vorne/hinten
    triagstexture(number,counter,triag,1)=vx
    triagstexture(number,counter,triag,2)=vy
  case 2: 'oben/unten
    triagstexture(number,counter,triag,1)=vx
    triagstexture(number,counter,triag,2)=vz
  case 3: 'vorne/hinten/links/rechts
    gettriagnormal(number,counter, nv())
    if (abs(sprod(nv(),ex()))>cos(45*piover180)) then ' vorne/hinten
      triagstexture(number,counter,triag,1)=vz
      triagstexture(number,counter,triag,2)=vy
    elseif (abs(sprod(nv(),ez()))>cos(45*piover180)) then 'links/rechts
      triagstexture(number,counter,triag,1)=vx
      triagstexture(number,counter,triag,2)=vy
    end if
  case 4: 'vorne/hinten/links/rechts/oben/unten
    gettriagnormal(number,counter, nv())
    if (abs(sprod(nv(),ex()))>cos(45*piover180)) then ' vorne/hinten
      triagstexture(number,counter,triag,1)=vz
      triagstexture(number,counter,triag,2)=vy
    elseif (abs(sprod(nv(),ez()))>cos(45*piover180)) then 'links/rechts
      triagstexture(number,counter,triag,1)=vx
      triagstexture(number,counter,triag,2)=vy
    elseif (abs(sprod(nv(),ey()))>cos(45*piover180)) then 'oben/unten
      triagstexture(number,counter,triag,1)=vx
      triagstexture(number,counter,triag,2)=vz
    end if
  end select
end sub

'------------------------------------------------------------------------------- readobj
' liest Geometrie im obj-Format: filename, objektnummer, Skalierung
' Gemetrie muss im Einheitswuerfel liegen
sub readobj(byval filename as string, byval number as integer, byval scale as single)

    dim as integer vCount, vtCount, vnCount, fCount, isSmooth
    countElements(filename, vCount, vtCount, vnCount, fCount, isSmooth)

    dim as single vertices(1 to vCount, 1 to 3)
    dim as single textCoords(1 to vtCount, 1 to 2)
    dim as single normals(1 to vnCount, 1 to 3)
    dim fp as integer
    dim oneline as string
    dim elementType as string
    dim as single vx, vy, vz, vtx, vty, vnx, vny, vnz
    dim as integer vCounter, vtCounter, vnCounter, fCounter

    vCounter = 0
    vtCounter = 0
    vnCounter = 0
    fCounter = 0
    fp = freefile
    if (open(filename, for input, as #fp) <> 0) then ? "ERROR loading obj":end 1

    ' read vertices, texture coords, normals and faces
    while not eof(fp)
        readString(fp, oneline)
        elementType = firstWord(oneline)
        if (elementType = "v") then
            vCounter = vCounter + 1
            parseVertex(oneline, vx, vy, vz)
            vertices(vCounter, 1) = vx
            vertices(vCounter, 2) = vy
            vertices(vCounter, 3) = vz
        elseif (elementType = "vt") then
            vtCounter = vtCounter + 1
            parseTextureCoord(oneline, vtx, vty)
            textCoords(vtCounter, 1) = vtx
            textCoords(vtCounter, 2) = vty
        elseif (elementType = "vn") then
            vnCounter = vnCounter + 1
            parseNormal(oneline, vnx, vny, vnz)
            normals(vnCounter, 1) = vnx
            normals(vnCounter, 2) = vny
            normals(vnCounter, 3) = vnz
        elseif (elementType = "f") then
            dim as integer vindex, vtindex, vnindex
            fCounter = fCounter + 1
            for i as integer = 1 to 3 ' vertex 1, 2, 3
                parseFace(oneline, i, vindex, vtindex, vnindex)
                ' save triag
                triags(number, fCounter, i, 1) = vertices(vindex, 1) * scale
                triags(number, fCounter, i, 2) = vertices(vindex, 2) * scale
                triags(number, fCounter, i, 3) = vertices(vindex, 3) * scale
                ' save vertex normals
                triagsvertexn(number, fCounter, i, 1) = normals(vnindex, 1) * scale
                triagsvertexn(number, fCounter, i, 2) = normals(vnindex, 2) * scale
                triagsvertexn(number, fCounter, i, 3) = normals(vnindex, 3) * scale
                ' save texture coords
                triagstexture(number, fCounter, i, 1) = textCoords(vtindex, 1) * scale
                triagstexture(number, fCounter, i, 2) = textCoords(vtindex, 2) * scale
            next i
            ' save face normals
            triagsn(number, fCounter, 1) = normals(vnindex, 1) * scale
            triagsn(number, fCounter, 2) = normals(vnindex, 2) * scale
            triagsn(number, fCounter, 3) = normals(vnindex, 3) * scale
        end if
    wend
    close #fp
    triagsanz(number) = fcounter
end sub

'------------------------------------------------------------------------------- readstl
' liest Geometrie im stl-Format: filename, objektnummer, Skalierung, Orientierung
' Orientierung: Texturprojektionsmethode
' Gemetrie muss im Einheitswuerfel liegen
sub readstl(byval filename as string,byval number as integer,byval scale as single,byval orientation as integer)
dim oneline as string
dim fp as integer
dim as integer stlpos,stlpos2,stlpos3,counter
dim as single vx,vy,vz
  counter=0
  fp = freefile
  oneline=" "
  if (open (filename, for input, as #fp) <> 0) then ? "ERROR loading stl":end 1
  while instr(oneline,"endsolid")=0
    readstr(fp, oneline)
    if instr(oneline,"facet normal")<>0 then
        counter=counter+1
        '
        readnormal(vx,vy,vz,oneline)
        triagsn(number,counter,1)=vx*scale
        triagsn(number,counter,2)=vy*scale
        triagsn(number,counter,3)=vz*scale
        
        triagsvertexn(number, counter, 1, 1) = vx*scale
        triagsvertexn(number, counter, 1, 2) = vy*scale
        triagsvertexn(number, counter, 1, 3) = vz*scale
        triagsvertexn(number, counter, 2, 1) = vx*scale
        triagsvertexn(number, counter, 2, 2) = vy*scale
        triagsvertexn(number, counter, 2, 3) = vz*scale
        triagsvertexn(number, counter, 3, 1) = vx*scale
        triagsvertexn(number, counter, 3, 2) = vy*scale
        triagsvertexn(number, counter, 3, 3) = vz*scale
        
        '
        readstr(fp, oneline)  'dummy "outer loop" lesen
        ' Vertex 1
        readstr(fp, oneline)
        readvertex(vx,vy,vz,oneline)
        definetextcoords( number ,counter, 1, vx*scale ,vy*scale ,vz*scale , orientation)
        ' Vertex 2
        readstr(fp, oneline)
        readvertex(vx,vy,vz,oneline)
        definetextcoords( number ,counter, 2, vx*scale ,vy*scale ,vz*scale , orientation)
        ' Vertex 3
        readstr(fp, oneline)
        readvertex(vx,vy,vz,oneline)
        definetextcoords( number ,counter, 3, vx*scale ,vy*scale ,vz*scale , orientation)
    end if
  wend
  close #fp
  triagsanz(number)=counter
end sub

'------------------------------------------------------------------------------- readvertex
'liest Punktkoordinate im stl-Format
sub readvertex(byref vx as single,byref vy as single,byref vz as single,byref oline as string)
dim as integer stlpos,stlpos2,stlpos3
  stlpos=instr(oline,"vertex")
  vx=val(mid(oline,stlpos+6))
  stlpos2=instr(stlpos+7,oline," ")
  vy=val(mid(oline,stlpos2))
  stlpos3=instr(stlpos2+1,oline," ")
  vz=val(mid(oline,stlpos3))
end sub

'------------------------------------------------------------------------------- readnormal
'liest Normalenvektor im stl-Format
sub readnormal(byref vx as single,byref vy as single,byref vz as single,byref oline as string)
dim as integer stlpos,stlpos2,stlpos3
dim as single nrm
  stlpos=instr(oline,"facet normal")
  vx=val(mid(oline,stlpos+12))
  stlpos2=instr(stlpos+13,oline," ")
  vy=val(mid(oline,stlpos2))
  stlpos3=instr(stlpos2+1,oline," ")
  vz=val(mid(oline,stlpos3))
  nrm=sqrt(vx*vx+vy*vy+vz*vz)
  if (nrm<>1) then vx=vx/nrm:vy=vy/nrm:vz=vz/nrm
end sub

'------------------------------------------------------------------------------- TestCollision
' Prueft ob Bewegung deltax,deltay kollisionsfrei moeglich ist -> sonst deltax,deltay=0
sub TestCollision( byval sx as single,byval sy as single, byref deltax as single,byref deltay as single,byref deltaz as single)
dim as single dx,dy
  if (deltax>0) then dx=deltax+coll else dx=deltax-coll
  if (deltax=0) then dx=0

  if (deltay>0) then dy=deltay+coll else dy=deltay-coll
  if (deltay=0) then dy=0

  ' Treppe ? Dann evtl ein Level hoch
  if (treppenflag=1 and playfield(sx,fix(sy+deltay),1,level)>63 and playfield(sx,fix(sy+deltay),1,level+1)<63) then level=level+1:deltaz=level-1:return
  'if (treppenflag=1 and t2+deltax<=1 and playfield(startx,starty-1,1,level)>63 and playfield(startx,starty-1,1,level+1)<63) then level=level+1:ypos=level-1:return
  'if (treppenflag=1 and t3+deltaz>2  and playfield(startx+1,starty,1,level)>63 and playfield(startx+1,starty,1,level+1)<63) then level=level+1:ypos=level-1:return
  'if (treppenflag=1 and t3+deltaz>2  and playfield(startx+1,starty,1,level)>63 and playfield(startx+1,starty,1,level+1)<63) then level=level+1:ypos=level-1:return
  'if (treppenflag=1 and t4+deltaz<=1 and playfield(startx-1,starty,1,level)>63 and playfield(startx-1,starty,1,level+1)<63) then level=level+1:ypos=level-1:return

  ' 1. zu nah am Front/Back/Left/Right Stein
  'if (playfield(fix(sx+dx),sy,1,level)>63) then deltax=0
  'if (playfield(sx,fix(sy+dy),1,level)>63) then deltay=0

  'if (playfield(fix(sx-coll-deltax),sy,1,level)>63) then deltax=0
  'if (t2+deltax<=1 and playfield(startx,starty-1,1,level)>63) then deltax=0
  'if (t3+deltaz>2  and playfield(startx+1,starty,1,level)>63 ) then deltaz=0
  'if (t4+deltaz<=1 and playfield(startx-1,starty,1,level)>63) then deltaz=0

  ' 2. zu nah an den Diagonalsteinen
  'if (t1+deltax>2 and t3>2 and playfield(startx+1,starty+1,1,level)>63 ) then deltax=0
  'if (t1+deltax>2 and t4<=1 and playfield(startx-1,starty+1,1,level)>63 ) then deltax=0
  'if (t2+deltax<=1 and t3>2 and playfield(startx+1,starty-1,1,level)>63) then deltax=0
  'if (t2+deltax<=1 and t4<=1 and playfield(startx-1,starty-1,1,level)>63) then deltax=0

  'if (t3+deltaz>2  and t1>2 and playfield(startx+1,starty+1,1,level)>63 ) then deltaz=0
  'if (t3+deltaz>2  and t2<=1 and playfield(startx+1,starty-1,1,level)>63 ) then deltaz=0
  'if (t4+deltaz<=1 and t1>2 and playfield(startx-1,starty+1,1,level)>63) then deltaz=0
  'if (t4+deltaz<=1 and t2<=1 and playfield(startx-1,starty-1,1,level)>63) then deltaz=0
end sub

sub TestCollision2( byval sx as single,byval sy as single, byref deltax as single, byref deltay as single)
dim as single tolx,toly

  if (deltax>0) then tolx=coll2 else tolx=-coll2
  if (deltay>0) then toly=coll2 else toly=-coll2

  if (playfield(fix(sx+deltax+tolx),fix(sy),1,level)>63) then deltax=0
  if (playfield(fix(sx),fix(sy+deltay+toly),1,level)>63) then deltay=0
  if (playfield(fix(sx+deltax+tolx),fix(sy+deltay+toly),1,level)>63) then deltay=0:deltax=0
  if (playfield(fix(sx+deltax+tolx),fix(sy+toly),1,level)>63) then deltax=0
  if (playfield(fix(sx+tolx),fix(sy+deltay+toly),1,level)>63) then deltay=0

end sub

'------------------------------------------------------------------------------- TestTreppe
' Prueft ob Bewegung auf Treppe stattfindet
sub TestTreppe(byval xp as integer,byval yp as integer, byref deltay as single)
dim as integer frontx,fronty
  frontx=xp:fronty=yp
  frontpos(yrot,frontx,fronty):if (playfield(frontx,fronty,2,level)=asc("(")) then treppenvorflag=1

  if (playfield(xp,yp,2,level)=asc("(")) then
    select case ((playfield(xp,yp,3,level)-asc("0"))*90) ' Winkel
    case 0:
      if (frac(ichx)<0.7) then
        deltay=frac(ichx)+level-1
      else
        deltay=0:level=level+1
      end if
    case 90:
      if (frac(ichy)>0.3) then
        deltay=-frac(ichy)+level-1
      else
        deltay=0:level=level+1
      end if
    case 180:
      if (frac(ichy)>0.3) then
        deltay=-frac(ichx)+level-1
      else
        deltay=0:level=level+1
      end if
    case 270:
      if (frac(ichy)<0.7) then
        deltay=frac(ichy)+level-1
      else
        deltay=0:level=level+1
      end if
    end select
    treppenflag=1
  end if
end sub

'------------------------------------------------------------------------------- TestLoch
' Prueft ob in Loch reingefallen
sub TestLoch(byval startx as integer,byval starty as integer, byref deltax as single,byref deltay as single,byref deltaz as single)
dim as integer frontx,fronty
  frontx=startx:fronty=starty
  frontpos(yrot,frontx,fronty): if (playfield(frontx,fronty,2,level)=asc(")")) then lochflag=1
  frontpos(yrot,frontx,fronty): if (playfield(frontx,fronty,2,level)=asc(")")) then lochflag=1

  if (level>1 and  playfield(startx,starty,2,level)=asc(")") and playfield(startx,starty,2,level-1)<>asc("(")) then ' ins Loch reinfallen
    level=level-1
    lochflag=0
    ypos=level-1
    ichx=floor(ichx)+0.5: ichy=floor(ichy)+0.5 ' beim runterfallen zentrieren
  end if

  if (playfield(startx,starty,2,level)=asc(")")) then ' in treppe reinfallen
    level=level-1
    lochflag=1
  end if

end sub

'------------------------------------------------------------------------------- TestTeleport
' Prueft ob in Teleporter
sub TestTeleport(byref startx as integer,byref starty as integer, byref deltax as single,byref deltay as single,byref deltaz as single)
dim as integer destx,desty
  if (playfield(startx,starty,2,level)=asc("*")) then ' im Teleporter
    FindSteinId(startx,starty, destx,desty)
    if (destx=0 and desty=0) then return
    BehindActive(destx,desty)
    'GetLocalPlayfield(startx,starty)
    ichx=floor(destx)+0.5: ichy=floor(desty)+0.5 ' beim beamen zentrieren
  end if
end sub


'------------------------------------------------------------------------------- PlayerMove
'Bewegt Spieler um deltax,deltay
sub PlayerMove(byref ichx as single, byref ichy as single, byval deltax as single, byval deltay as single)
  if playfield(fix(ichx+deltax+sgn(deltax)*coll),fix(ichy),1,level)<64 then
    ichx=ichx+deltax
  end if
  if playfield(fix(ichx),fix(ichy+deltay+sgn(deltay)*coll),1,level)<64 then
    ichy=ichy+deltay
  end if
end sub

'------------------------------------------------------------------------------- readstr
' holt eine Zeile aus File
sub readstr(byval f as integer, byref Buffer as string)
  do
    line input #f, Buffer
  loop while (left(Buffer,1) = "/") or (Buffer = "")
end sub

'------------------------------------------------------------------------------- ReadLayer
' laed Spielfeld layer
sub ReadLayer(byval f as integer, byval layer as integer,byval level as integer)
dim oneline as string
dim as integer l1, l2
  for l1=1 to 64
    readstr(f, oneline)
    if oneline = "" then end 1
    for l2=1 to 64
      playfield(l2,l1,layer,level)=asc(mid(oneline,l2,l2+1))
    next
  next
end sub

'------------------------------------------------------------------------------- SetupPlayfield
' laed Spielfelddata aus File: filename
sub SetupPlayfield()
dim as integer fp,i,j
  fp = freefile
  for j=1 to 9
    if (open (savenames(j) for input as #fp) <> 0) then end 1
    For i=1 to 9:ReadLayer(fp,i,j):next i ' Wand/Objekte laden,Boden,Drehungen,SteinID,ActionID,Steinreserve,Drehmatrix,SteinID,ActionID
    close #fp
  next
end sub

'------------------------------------------------------------------------------- DrawPlayField
sub DrawPlayField(byval xpos as integer,byval ypos as integer )
dim as integer l1, l2,lx,ly
    for l1=1 to 2*sichtweite+1
      for l2=1 to 2*sichtweite+1
        lx=xpos+l2-sichtweite-1
        ly=ypos+l1-sichtweite-1
        DrawField(lx,ly)
      next
    next
end sub

'------------------------------------------------------------------------------- DrawField
' zeichnet Spielfeld + 3D Objekte
' drawobject (verschiebung x,y, position x,y, Texturnummer, Objektnummer, Drehwinkel
sub DrawField(byval l1 as integer, byval l2 as integer)
dim as integer ang,monid, stein,i

  ztol=0.

  if (l1>64 or l2>64 or l1<1 or l2 <1) then
        stein=32:ang=0
  else
     stein=playfield(l1,l2,1,level)
     ang=(playfield(l1,l2,3,level)-asc("0"))*90 ' Playfield 3 gibt Drehwinkel
  end if

  ' Objekte
  select case stein 'locplay1(l2,l1)
  case 65: 'holz
    drawobject(0.,0.,l1,l2, 2,23,ang,0) ' 4fachWand
  case 97: 'holzsaeule
    drawobject(0.,0.,l1,l2, 2,19,ang,0)
    drawobject(0.,0.,l1,l2, 2,20,ang,0)  'extra
  case 66: ' sandstein
    drawobject(0.,0.,l1,l2, 6,23,ang,0) ' 4fachWand
  case asc("W"): ' Marmorwand
    drawobject(0.,0.,l1,l2, 11,23,ang,0) ' 4fachWand
  case 98: ' marmorsaeule
    drawobject(0.,0.,l1,l2, 18,17,ang,0)
  case 67: 'Baum
    drawobject(0.,0.,l1,l2, 1,6,ang,0)
    drawobject(0.,0.,l1,l2, 3,7,ang,0)
    drawobject(0.,0.,l1,l2, 5,8,ang,0)
    drawobject(0.,0.,l1,l2, 3,9,ang,0)
  case 68: ' granit
    drawobject(0.,0.,l1,l2, 8,23,ang,0) ' 4fachWand
  case 100: ' granitsaeule
    drawobject(0,0,l1,l2, 17, 11,ang,0)
  case asc("O"): ' Jaegerzaun
    drawobject(0,0.5,l1,l2, 2,10,winkeladd(ang,0),0 )
  case asc("T")' tuer zu
    drawobject(0,0.3,l1,l2, 23, 2,winkeladd(ang,90),0)
  case asc("=")' tuer auf
    drawobject(0.3,0.9,l1,l2, 23, 2,winkeladd(ang,0),0)
  case asc("t")' Marmorvertafelung+Rand
    drawobject(0.,-0.025,l1,l2, 11,24,winkeladd(ang,0),0) ' Marmorvertafelung
    drawobject(0.,0.,l1,l2, 11, 25,winkeladd(ang,0),0) 'Rand
  case asc("S") 'Schatz mit Inhalt
    drawobject(0.,0.,l1,l2, 15,3,winkeladd(ang,-90),0) ' kiste
    drawobject(0.,0.,l1,l2, 16,4,winkeladd(ang,-90),0)  'riemen
    drawobject(0.,0.,l1,l2, 14,5,winkeladd(ang,-90),0) ' gold
  case asc("s") ' Schatz gepluendert
    drawobject(0.,0.,l1,l2, 15,3,winkeladd(ang,-90),0)
    drawobject(0.,0.,l1,l2, 16,4,winkeladd(ang,-90),0) 'riemen
  case asc("H") 'Hebel oben
    drawobject(0.,-0.1,l1,l2, 21,12,winkeladd(ang,-90),0) ' wand
    drawobject(0.,-0.1,l1,l2, 2,13,winkeladd(ang,-90),0)  'hebel oben
  case asc("h") ' Hebel unten
    drawobject(0.,-0.1,l1,l2, 21,12,winkeladd(ang,-90),0) ' wand
    drawobject(0.,-0.1,l1,l2, 2,14,winkeladd(ang,-90),0)  'hebel unten
  case asc("U") 'Urne
    drawobject(0.,0.,l1,l2, 22,1,ang,0)
    drawobject(0.,0.,l1,l2, 22,18,ang,0)
  case asc("m") 'Marmorvertafelung
    drawobject(0.,-0.025,l1,l2, 11,24,winkeladd(ang,90),0) ' wand
  case asc("X") 'Monster
      'welches ?
      for i=0 to monsteranz(level)
        if (fix(monsters(i,level).xpos)=l1 and fix(monsters(i,level).ypos)=l2) then monID=i:goto lab1
      next
      goto lab2
      'Monsterwaffe falls gezueckt
 lab1:
      if (monsters(monID,level).attack<>0) then drawobject(frac(monsters(monID,level).xpos)-0.4,frac(monsters(monID,level).ypos)-0.4,l1,l2, 1,34, 0,monsters(monID,level).dreh+monsters(monID,level).attack)
      drawobject(frac(monsters(monID,level).xpos)-0.5,frac(monsters(monID,level).ypos)-0.5,l1,l2, monsters(monID,level).texture,monsters(monID,level).typ, 0,monsters(monID,level).dreh)
 lab2:

  end select

  'Boden+Decke
  if (l1>64 or l2>64 or l1<1 or l2 <1) then
        stein=asc(".")
  else
    stein=playfield(l1,l2,2,level)
  end if

  select case stein 'locplay2(l2,l1) ' Bodentextur
  case asc("."):' pflastersteine
    ztol=bodentol:drawobject(0.,0.,l1,l2, 1,21,ang,0) 'Boden
    ztol=deckentol:drawobject(0.,0.,l1,l2, 0,22,ang,0) 'Decke
  case 32:' pflastersteine
    ztol=bodentol:drawobject(0.,0.,l1,l2, 1,21,ang,0)'Boden
    ztol=deckentol:drawobject(0.,0.,l1,l2, 0,22,ang,0) 'Decke
  case 33:' gras
    ztol=bodentol:drawobject(0.,0.,l1,l2, 3,26,ang,0)'Boden
    ztol=deckentol:drawobject(0.,0.,l1,l2, 0,22,ang,0) 'Decke
  case 34:' Marmor
    ztol=bodentol:drawobject(0.,0.,l1,l2, 4,21,ang,0) 'Boden
    ztol=deckentol:drawobject(0.,0.,l1,l2, 0,22,ang,0) 'Decke
  case 35:' Gartensteine
    ztol=bodentol:drawobject(0.,0.,l1,l2, 5,21,ang,0)'Boden
    ztol=deckentol:drawobject(0.,0.,l1,l2, 0,22,ang,0) 'Decke
  case asc("$"):' Druckplatte oben
    ztol=bodentol:drawobject(0.,0.,l1,l2, 5,15,ang,0)
    ztol=deckentol:drawobject(0.,0.,l1,l2, 0,22,ang,0) 'Decke
  case asc("%"):' Druckplatte unten
    ztol=bodentol:drawobject(0.,0.,l1,l2, 5,16,ang,0)
    ztol=deckentol:drawobject(0.,0.,l1,l2, 0,22,ang,0) 'Decke
  case asc("("):' Treppe
    drawobject(0.,0.,l1,l2, 1,27,winkeladd(ang,-90),0) 'Treppe
    drawobject(0.,0.,l1,l2, 4,28,winkeladd(ang,-90),0) 'gelaender
    drawobject(0.,0.,l1,l2, 4,29,winkeladd(ang,-90),0) 'vase
  case asc(")"):' Treppenloch
    ztol=deckentol:drawobject(0.,0.,l1,l2, 0,22,ang,0) 'Decke
  case asc("*") 'Portal
    drawobject(0.,0.,l1,l2, 11,30,winkeladd(ang,90),0)
    drawobject(0.,0.,l1,l2, 11,31,winkeladd(ang,90),0)
    drawobject(0.,0.,l1,l2, 11,32,winkeladd(ang,90),0)
    drawobject(0.,0.,l1,l2, 11,33,winkeladd(ang,90),0)
    ztol=bodentol:drawobject(0.,0.,l1,l2, 11,21,ang,0) 'Boden
    ztol=deckentol:drawobject(0.,0.,l1,l2, 0,22,ang,0) 'Decke
end select
end sub

'------------------------------------------------------------------------------- drawobject
' Zeichnet Objekt: offset x,y, position x,y, textur, objektnr
sub drawobject(byval goffx as single, byval goffy as single, byval l1 as single, byval l2 as single,_
			   byval txt1 as integer, byval j as integer, byval ang as integer, byval ang2 as integer)
  glPushMatrix  ' globale Drehmatrix retten
  if (ang2<>0 and ang=0) then
    glTranslatef l1, level-1+ztol, l2    ' Spielfeldkoordinaten
    glTranslatef goffx, 0.0, goffy 'Mitte Element mit offsetkorrektur
    glTranslatef 0.5, 0.,0.5  ' Mitte Element
    glRotatef -ang2, 0.0,1.0, 0. ' Drehung vor Translation
    glTranslatef -0.5, 0.,-0.5  ' Mitte Element
  elseif (ang<>0 and ang2=0) then
    glTranslatef l1, level-1+ztol, l2    ' Spielfeldkoordinaten
    glTranslatef 0.5, 0., 0.5  ' Mitte Element für Drehung rueckgängig
    glRotatef -ang, 0.0,1.0, 0. ' Drehung
    glTranslatef -0.5, 0.,-0.5  ' Mitte Element
    glTranslatef goffx, 0.0, goffy 'Mitte Element mit offsetkorrektur
  elseif (ang<>0 and ang<>0) then
    glTranslatef l1, level-1+ztol, l2    ' Spielfeldkoordinaten
    glRotatef -ang, 0.0,1.0, 0. ' Drehung
    glTranslatef goffx, 0.0, goffy 'Mitte Element mit offsetkorrektur
    glRotatef -ang2, 0.0,1.0, 0. ' Drehung vor Translation
    glTranslatef -0.5, 0.,-0.5  ' Mitte Element
  else
    glTranslatef l1, level-1+ztol, l2    ' Spielfeldkoordinaten
    glTranslatef goffx, 0.0, goffy 'Mitte Element mit offsetkorrektur
  end if

  drawlocalobject(txt1,j)
  glPopMatrix ' globale Drehmatrix zurueckholen
end sub

sub drawplayerobject(byval goffx as single, byval goffy as single, byval goffz as single, byval goffw as single,_
                     byval goffw2 as single, byval hitx as single, byval hity as single, byval hitz as single,_
                     byval txt1 as integer, byval j as integer)
  glPushMatrix  ' globale Drehmatrix retten
  glTranslatef goffx, goffy,goffz  ' Spielerpos
  glRotatef -goffw, 0, 1.0, 0   ' drehen
  glTranslatef hitx, hity,hitz  ' Spielerpos
  glRotatef goffw2, 1.0,0.0,0.0 ' Drehung
  drawlocalobject(txt1,j)
  glPopMatrix ' globale Drehmatrix zurueckholen
end sub

sub drawlocalobject(byval txt1 as integer, byval j as integer)
'param txt1: texture id
'param j: object id
    ' dim as integer i
    ' glBindTexture GL_TEXTURE_2D, texture(txt1)
    ' for i=1 to triagsanz(j)
        ' glBegin GL_TRIANGLES                          '' Start Drawing Triangles
        ' glNormal3f triagsn(j,i,1), triagsn(j,i,2), triagsn(j,i,3)

        ' glTexCoord2f triagstexture(j,i,1,1), triagstexture(j,i,1,2)
        ' glVertex3f triags(j,i,1,1), triags(j,i,1,2), triags(j,i,1,3)

        ' glTexCoord2f triagstexture(j,i,2,1), triagstexture(j,i,2,2)
        ' glVertex3f triags(j,i,2,1), triags(j,i,2,2), triags(j,i,2,3)

        ' glTexCoord2f triagstexture(j,i,3,1), triagstexture(j,i,3,2)
        ' glVertex3f triags(j,i,3,1), triags(j,i,3,2), triags(j,i,3,3)
        ' glEnd
    ' next i
	glBindTexture(GL_TEXTURE_2D, texture(txt1))
	glBegin(GL_TRIANGLES)
	for i as integer = 1 to triagsanz(j)
		for k as integer = 1 to 3 ' vertex 1, 2, 3
            glNormal3f(triagsvertexn(j, i, k, 1), triagsvertexn(j, i, k, 2), triagsvertexn(j, i, k, 3))
			glTexCoord2d(triagstexture(j, i, k, 1), triagstexture(j, i, k, 2))
			glVertex3d(triags(j, i, k, 1), triags(j, i, k, 2), triags(j, i, k, 3))
		Next
	Next
	glEnd
end sub

' ------------------------------------------------------------------------------ loadtexture
' laed textur: filename, Texturnummer
function loadtexture(byval filename as string, byval number as integer) as integer
  redim buffer(512*512*4+4) as ubyte          ' x*y*4Bytes+4 Headerbytes
  bload filename, @buffer(0)                  ' bitmap daten
  texture(number) = CreateTexture(@buffer(0))
  if (texture(number)=0) then return -1 ' Fehler: -1
  return 0
end function

function loadtexturePNG(byval filename as string, byval number as integer) as integer
    texture(number) = CreateTexturePNG(filename, 1)
    if (texture(number)=0) then return -1 ' Fehler: -1
    return 0
end function

'------------------------------------------------------------------------------- BuildFont
'Font aus Textur aufbauen
sub BuildFont()
dim loop1 as integer
dim as single cx,cy
  gbase = glGenLists(256)                      ' 256 Display List
  glBindTexture GL_TEXTURE_2D, texture(12)     ' Font
  for loop1=0 to 255
    cx = (loop1 mod 16)/16.0                 'xpos/ypos des Zeichens
    cy = (loop1\16)/16.0
    glNewList gbase+loop1, GL_COMPILE
    glBegin GL_QUADS                         ' Zeichen als GLQuad
      glTexCoord2f cx, 1-cy-0.0625         ' unten links
      glVertex2i 0, 16
      glTexCoord2f cx+0.0625, 1-cy-0.0625  ' unten rechts
      glVertex2i 16, 16
      glTexCoord2f cx+0.0625, 1-cy         ' o rechts
      glVertex2i 16, 0
      glTexCoord2f cx, 1-cy                ' o links
      glVertex2i 0, 0
    glEnd
    glTranslated 14, 0, 0         ' Startpunkt Zeichen
    glEndList
  next
end sub

'------------------------------------------------------------------------------- glPrint
' Schreibt Strings an Position x,y,  Zeichensatznummer, Strings
sub glPrint cdecl (byval x as integer, byval y as integer, byval gset as integer, byref fmt as string, ...)
dim text as string * 256
dim ap as any ptr     ' Pointer zur Argumentliste
  if len(fmt) = 0 then exit sub ' kein String da
  ap = va_first()               'erstes Argument
  vsprintf(text, fmt, ap)
  if gset>1 then gset=1         'zu viel
  glBindTexture GL_TEXTURE_2D, texture(12)
  glEnable(GL_TEXTURE_2D)
  glLoadIdentity()
  glTranslated(x,y,0)              'Textpos
  glListBase(gbase-32+(128*gset))  'char offset
  glScalef 1.0, -2.0, 1.0          'Textskalierung
  glCallLists(strlen(text),GL_UNSIGNED_BYTE, strptr(text))
end sub

'------------------------------------------------------------------------------- toggleall
' schaltet Element c1 zu c2 (und umgekehrt) in der Umgebung des Spielers
sub toggleall(byval startx as integer,byval starty as integer,byval c1 as string,byval c2 as string,byval layer as integer)
  if playfield(startx+1,starty,layer,level)=asc(c1) then
    playfield(startx+1,starty,layer,level)=asc(c2)
    SwitchAction(startx+1,starty,asc(c1),asc(c2))
  elseif playfield(startx+1,starty,layer,level)=asc(c2) then
    playfield(startx+1,starty,layer,level)=asc(c1)
    SwitchAction(startx+1,starty,asc(c1),asc(c1))
  end if
  if playfield(startx,starty+1,layer,level)=asc(c1) then
    playfield(startx,starty+1,layer,level)=asc(c2)
    SwitchAction(startx,starty+1,asc(c1),asc(c2))
  elseif playfield(startx,starty+1,layer,level)=asc(c2) then
    playfield(startx,starty+1,layer,level)=asc(c1)
    SwitchAction(startx,starty+1,asc(c1),asc(c1))
  end if
  if playfield(startx-1,starty,layer,level)=asc(c1) then
    playfield(startx-1,starty,layer,level)=asc(c2)
    SwitchAction(startx-1,starty,asc(c1),asc(c2))
  elseif playfield(startx-1,starty,layer,level)=asc(c2) then
    playfield(startx-1,starty,layer,level)=asc(c1)
    SwitchAction(startx-1,starty,asc(c1),asc(c1))
  end if
  if playfield(startx,starty-1,layer,level)=asc(c1) then
    playfield(startx,starty-1,layer,level)=asc(c2)
    SwitchAction(startx,starty-1,asc(c1),asc(c2))
  elseif playfield(startx,starty-1,layer,level)=asc(c2) then
    playfield(startx,starty-1,layer,level)=asc(c1)
    SwitchAction(startx,starty-1,asc(c1),asc(c1))
  end if
end sub

'------------------------------------------------------------------------------- DoIdAction
'
sub DoIdAction(byval xp as integer,byval yp as integer)
dim as integer actionid, destx,desty, cpy
  actionid=playfield(xp,yp,5,level)
  select case (actionid)
  case asc("1"): ' Stein killen
    FindSteinId(xp,yp, destx,desty)
    if (xp=0 and yp=0) then return
    playfield(destx,desty,1,level)=asc(" ")
  case asc("2"): ' Stein aus reserve switchen
    FindSteinId(xp,yp, destx,desty)
    if (xp=0 and yp=0) then return
    swap playfield(destx,desty,1,level),playfield(destx,desty,6,level)
    swap playfield(destx,desty,3,level),playfield(destx,desty,7,level)
    swap playfield(destx,desty,4,level),playfield(destx,desty,8,level)
    swap playfield(destx,desty,5,level),playfield(destx,desty,9,level)
  case asc("3"): ' Stein aus reserve holen
    FindSteinId(xp,yp, destx,desty)
    if (xp=0 and yp=0) then return
    playfield(destx,desty,1,level)=playfield(destx,desty,6,level)
    playfield(destx,desty,3,level)=playfield(destx,desty,7,level)
    playfield(destx,desty,4,level)=playfield(destx,desty,8,level)
    playfield(destx,desty,5,level)=playfield(destx,desty,9,level)
  end select
end sub

'------------------------------------------------------------------------------- SwitchAction
'
sub SwitchAction(byval xp as integer,byval yp as integer, byval c1 as integer,byval c2 as integer)
dim as integer actionid, destx,desty, cpy
  if (c2=asc("h")) then DoIdAction(xp,yp) ' Hebel unten
  if (c1=asc("S")) then coin=coin+100:DoIdAction(xp,yp) 'Schatz
  if (c1=asc("U")) then hp=hpm 'Urne

end sub


'------------------------------------------------------------------------------- switchall
' schaltet Element c1 zu c2 in der Umgebung des Spielers an pos x,y,layer
sub switchall(byval startx as integer,byval starty as integer,byval c1 as string,byval c2 as string,byval layer as integer)
  if playfield(startx+1,starty,layer,level)=asc(c1) then
    playfield(startx+1,starty,layer,level)=asc(c2)
    SwitchAction(startx+1,starty,asc(c1),asc(c2))
  end if
  if playfield(startx,starty+1,layer,level)=asc(c1) then
    playfield(startx,starty+1,layer,level)=asc(c2)
    SwitchAction(startx,starty+1,asc(c1),asc(c2))
  end if
  if playfield(startx-1,starty,layer,level)=asc(c1) then
    playfield(startx-1,starty,layer,level)=asc(c2)
    SwitchAction(startx-1,starty,asc(c1),asc(c2))
  end if
  if playfield(startx,starty-1,layer,level)=asc(c1) then
    playfield(startx,starty-1,layer,level)=asc(c2)
    SwitchAction(startx,starty-1,asc(c1),asc(c2))
  end if
end sub

'------------------------------------------------------------------------------- winkeladd
function winkeladd(byval w1 as single,byval w2 as single) as single
dim w3 as single
 w3=w1+w2
 if w3<0 then w3=w3+360
 if w3>360 then w3=w3-360
 if w3=360 then w3=0
 return w3
end function

'------------------------------------------------------------------------------- limit
function limit(byval w as single,byval l1 as single,byval l2 as single) as single
 if (w>l2) then return l2
 if (w<l1) then return l1
 return w
end function

' ***********************************************************************************************************
' ***********************************************************************************************************

'------------------------------------------------------------------------------- ApplySwitches
' Definition der Schaltelemente an Position x,y
sub ApplySwitches(byval startx as integer,byval starty as  integer)
  toggleall(startx,starty,"T","=",1) ' Tuer
  switchall(startx,starty,"S","s",1) ' Schatz
  switchall(startx,starty,"U"," ",1) ' Urne
  toggleall(startx,starty,"H","h",1) ' Hebel
end sub


'------------------------------------------------------------------------------- BuildTextures
'  Texturen laden: loadtexture file,Texturnummer
sub BuildTextures()
    if loadtexture("c:/G7/texturen/Decke1_512.bmp",0)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Boden1_512.bmp",1)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Wand1_512.bmp",2)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Grass1_512.bmp",3)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Marmorplatten.bmp",4)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Boden3_512.bmp",5)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Wand2_512.bmp",6)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Wand3_512.bmp",7)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Wand4_512.bmp",8)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Saeule1_512.bmp",9)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Tuer1_512.bmp",10)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Marmorwand.bmp",11)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Font.bmp",12)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Icons.bmp",13)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/SchatzAusGold.bmp",14)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/KisteVorderseite.bmp",15)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/EisenRiemen.bmp",16)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Saeule0.bmp",17)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/MarmorSaeule.bmp",18)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Steinwand.bmp",19)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Affe.bmp",20)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Hebelwand.bmp",21)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Urne.bmp",22)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Gittertor.bmp",23)<>0 then ? "ERROR loading bmp":end 1
    if loadtexture("c:/G7/texturen/Daemon.bmp",24)<>0 then ? "ERROR loading bmp":end 1
    if loadtexturePNG("c:/G7/texturen/UI.png",25)<>0 then ? "ERROR loading png":end 1
end sub


'------------------------------------------------------------------------------- readAnimation
'  mehrere stls laden
' numberOfFrames muss unter 100 sein
sub readAnimation(byval filename as string,byval number as integer,byval scale as single,byval orientation as integer, byval numberOfFrames as integer)
    dim i as integer
    for i=0 to numberOfFrames-1
        readstl(filename+"_"+str(i)+".stl",number+i,scale,orientation)
    next i
end sub

'------------------------------------------------------------------------------- inputstls
'  Objekte laden
sub inputstls()
  ' readstl: file, objektnummer, Skalierung, Orientierung
  ' readAnimation: file, objektnummer, Skalierung, Orientierung, Anzahl der Frames
  ' Orientierung: 1-Vorne,2 Oben, 3-NV gesteuert
  readobj("c:\G7\model\Urne.obj",1,1)     ' Urne
  readstl("c:\G7\model\Gitter.stl",2,1 ,1)  ' Gittertuer
  readstl("c:\G7\model\Deckel.stl",3,1,  1)     ' Schatztruhe
  readstl("c:\G7\model\Riemen.stl",4,1, 1)
  readstl("c:\G7\model\Schatz.stl",5,1, 2)
  readstl("c:\G7\model\Topf.stl",6,1,  1)    ' Topf-pflanze
  readstl("c:\G7\model\Erde.stl",7,1,  1)
  readstl("c:\G7\model\Baum.stl",8,1,  1)
  readstl("c:\G7\model\Blaetter.stl",9,1,  1)
  readstl("c:\G7\model\Zaun.stl",10,1,  1)
  readobj("c:\G7\model\Saeule.obj",11,1)
  readobj("c:\G7\model\Wand.obj",12,1) ' Wand mit
  readstl("c:\G7\model\Hebeloben.stl",13,1,  1) ' Hebel oben
  readstl("c:\G7\model\Hebelunten.stl",14,1,  1) ' Hebel unten
  readstl("c:\G7\model\Druckplatte.stl",15,1,  2) ' druckplatte
  readstl("c:\G7\model\DruckplatteAktiviert.stl",16,1,  2) ' druckplatte aktiviert
  readstl("c:\G7\model\MarmorSaeule.stl",17,1,  1) ' MarmorSaeule
  '-------- Platz 18 frei ---------------------------------
  readstl("c:\G7\model\Holzsaeule.stl",19,1,  1) ' Holzsaeule
  readstl("c:\G7\model\HolzsaeulenExtra.stl",20,1,  1) ' HolzsaeuleExtra
  readstl("c:\G7\model\Bodeneinfach.stl",21,1,  2) '
  readstl("c:\G7\model\DeckeEinfach.stl",22,1,  2) '
  readstl("c:\G7\model\WandEinfach.stl",23,1,  3)
  readstl("c:\G7\model\Marmorwand.stl",24,1,  3)
  readstl("c:\G7\model\Rueckwaende.stl",25,1,  3)
  readstl("c:\G7\model\Grasboden.stl",26,1,  2)
  readstl("c:\G7\model\Treppe.stl",27,1,  4)
  readstl("c:\G7\model\Vasen.stl",28,1,  1)
  readstl("c:\G7\model\Gelaender.stl",29,1,  1)
  readstl("c:\G7\model\Portal.stl",30,1,  1) 'Portal
  readstl("c:\G7\model\Portalschienen.stl",31,1,  1)
  readstl("c:\G7\model\Portalrahmen.stl",32,1,  1)
  readstl("c:\G7\model\Portalbaum.stl",33,1,  1)
  readstl("c:\G7\model\Keule.stl",34,1,1) 'Holzkeule
  readobj("c:\G7\model\daemon.obj",35,1) 'Daemon

  readAnimation("c:\G7\animation\test\test",500,1,1,  4) 'animierter affe
end sub


sub drawUIRect(w as integer, h as integer, x as integer, y as integer, txtx as single, txty as single, txtnr as integer)
    dim as single txtw, txth
    txtw = w/512
    txth = h/512
    
    ' Set ortho view
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(0, screenx, 0, screeny, -1, 1)
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    ' Setup rendering
    glEnable(GL_BLEND)
    glDisable(GL_LIGHTING)
    glEnable(GL_TEXTURE_2D)
    ' Draw object
    glBindTexture(GL_TEXTURE_2D, texture(txtnr))
    'glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_Modulate)
    glBegin(GL_QUADS)
    glColor3f(1, 1, 1)
    glTexCoord2d(txtx, txty)
    glVertex2f(x, y)
    
    glTexCoord2d(txtx+txtw, txty)
    glVertex2f(x+w, y)
    
    glTexCoord2d(txtx+txtw, txty+txth)
    glVertex2f(x+w, y+h)
    
    glTexCoord2d(txtx, txty+txth)
    glVertex2f(x, y+h)
    glEnd
    ' Desetup rendering
    glPopMatrix()
    glEnable(GL_LIGHTING)
    glDisable(GL_BLEND)
    
end sub

' ##############################################################################