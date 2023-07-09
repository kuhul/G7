' ************************
' DUNGEONRUNNA LevelEdit
' (c) 2016
' ************************

#include once "GL/gl.bi"
#include once "GL/glu.bi"
#include once "crt.bi"          '' scanf is used to parse data file
#include once "fbgfx.bi"        '' for Scan code constants
#include once "createtex.bi"

'' Setup our booleans
const screenx=1900
const screeny=1080

const piover180 = 0.0174532925f
const sichtweite=25 '25 '9
const stepphi=4
const steptheta=2
const stepforward=0.1
const coll=0.2
const soff=0.25 'saeulenoffset
const coff=0.4 'cubeoffset
const bounce=1./80. ' auf abwaertsbewegung spieler

const edix=870
const ediy=10
const menux=1600
const menuy=1040
const lagerx=870
const lagery=1040

const path="c:\G7\"
const levelPath=path+"level\"
''------------------------------------------------------------------------------
'' Types used by the model
type VERTEX                      '' Build Our Vertex Structure called VERTEX
	x as single                  '' 3D Coordinates (x, y, z)
	y as single
	z as single
	u as single                  '' Texture Coordinates (u, v)
	v as single
end type

type TRIANGLE                    '' Build Our Triangle Structure called TRIANGLE
	vertex(0 to 2) as VERTEX     '' Array Of Three Vertices
end type

type SECTOR                      '' Build Our Sector Structure called SECTOR
	numtriangles as integer      '' Number Of Triangles In Sector
	triangle as TRIANGLE ptr     '' Pointer To Array Of Triangles
end type

''------------------------------------------------------------------------------
declare sub readstr(byval f as integer, byref Buffer as string)
declare sub SetupPlayfield()
declare sub PrintPlayfield()
declare sub GetLocalPlayfield(byval xpos as integer,byval ypos as integer)
declare sub drawmauer(byval tscale as single,byval l1 as single,byval l2 as single, byval txt1 as integer)
' declare sub drawsaeule(byval tscale as single,byval l1 as single,byval l2 as single,byval txt1 as integer,byval txt2 as integer)
declare sub drawvoid(byval tscale1 as single,byval tscale2 as single,byval l1 as single,byval l2 as single, byval txt1 as integer,byval txt2 as integer)
declare sub drawsaeule(byval tscale1 as single,byval tscale2 as single,byval l1 as single,byval l2 as single,byval txt1 as integer,byval txt2 as integer)
declare sub drawcube(byval tscale1 as single,byval tscale2 as single,byval l1 as single,byval l2 as single,byval txt1 as integer,byval txt2 as integer)
declare sub BuildFont()
declare sub BuildIcons()
declare sub glPrintIcon cdecl (byval x as integer, byval y as integer, byval gset as integer, byref icon as byte,byval ang as integer)
declare sub SavePlayfield(byval levelnum as integer)
declare sub LoadPlayfield()
declare sub SaveLayer(byref f as integer, byval layer as integer,byval level as integer)
declare sub PrintTyp(byval l1 as integer,byval l2 as integer,byval typ as byte, byval offx as integer,byval offy as integer, byval ang as integer)
declare sub glPrint cdecl (byval x as integer, byval y as integer, byval gset as integer, byref fmt as string, ...)
declare sub frontpos(byval yrot as integer, byref xpos as integer, byref ypos as integer)
declare sub ConvertPlayfield()
declare sub DeletePlayfield()
declare sub Editorloop(byval locx as integer, byval locy as integer)
declare sub EditorDisplay()
declare sub EditorInit()
declare sub writestr(byval f as integer, byref Buffer as string)
declare function testmouse(byval mx as integer,byval my as integer,byval ox as integer,byval oy as integer, byval dx as integer,byval dy as integer,byref locx as integer,byref locy as integer) as integer
dim shared sector1 as SECTOR                   '' Our Model Goes Here
	
dim shared filter as uinteger                  '' Which Filter To Use
dim shared texture(0 to 20) as uinteger         '' Storage For 3 Textures
dim shared icons(0 to 20) as uinteger         '' Storage For 3 Textures

dim shared gbase as uinteger                       '' Base Display List For The Font
dim shared startx as integer
dim shared starty as integer
dim shared testx as integer
dim shared testy as integer
dim shared editmode as integer
dim shared editmodeDescription(10) as string
dim shared feldrot as integer
dim shared rotlock as integer
dim shared savenames(9) as string

dim shared level as integer

	dim blend as integer                    '' Blending OFF/ON?
	dim fp as integer                       '' F Pressed?
	dim bp as integer                       '' B Pressed?

	dim heading as single              '' direction of movement
	dim  xpos as single                 '' X position
	dim  zpos as single                 '' Y position
	
	dim yrot as single                 '' Y Rotation = heading
	dim walkbias as single             '' used with walkbiasangle for bouncing effect
	dim walkbiasangle as single        '' used with walkbias for bouncing effect
	dim lookupdown as single           '' View direction
	
	dim x_m as single            '' Floating Point For Temp X, Y, Z, U And V Vertices
	dim y_m as single
	dim z_m as single
	dim u_m as single
	dim v_m as single
	
	dim xtrans as single         '' Used For Player Translation
	dim ztrans as single         '' Used For Player Translation
	dim ytrans as single         '' Used For Bouncing Motion Up And Down
	dim sceneroty as single      '' 360 Degree Angle For Player Direction

	dim as integer numtriangles  '' Integer To Hold The Number Of Triangles
	dim as integer loop_m        '' Loop counter
    
    ' playfield start
    dim shared as integer playfield(64,64,9,9)
    
    dim as integer face,lflag,rflag,nn,tflag,locx,locy
    dim as integer lx,ly,l1,l2
    dim as integer dummyx,dummyy,mousex,mousey,mouseb1,mouseb2,mouseofx,mouseofy,drehmode,htemp,ltemp
    startx=2
    starty=2
    lflag=0
    rflag=0
    tflag=0
    
    dim as single deltax,deltaz
    dim as single tscale 
    dim shared as integer locplay(2*sichtweite+1,2*sichtweite+1)

    dim shared as integer hp,sp,st
    dim shared as integer hpm,spm,stm
    dim shared as integer lv,coin,steinwahl
    
    EditorInit()
    


    drehmode=0
    hp=10
    hpm=10
    sp=0
    spm=0
    st=5
    stm=5
    lv=1
    coin=100

    tscale=1
    
    lookupdown=0
    ScreenRes  screenx,screeny,16,,2

    SetupPlayfield()

    'Screen 19,16,,2
	'' ReSizeGLScene
	glViewport 0, 0, screenx/2.,screeny                      '' Reset The Current Viewport
	glMatrixMode GL_PROJECTION                     '' Select The Projection Matrix
	glLoadIdentity                                 '' Reset The Projection Matrix
	gluPerspective 75.0, screenx/2./screeny, 0.1, 100.0   '' Calculate The Aspect Ratio Of The Window
	glMatrixMode GL_MODELVIEW                      '' Select The Modelview Matrix
	glLoadIdentity                                 '' Reset The Modelview Matrix

	'' This Lesson is the first to demonstrate the use of BLOAD to load the bitmaps.
	redim buffer(512*512*4+4) as ubyte                    '' Size = Width x Height x 4 bytes per pixel + 4 bytes for header
	bload "c:/G7/texturen/Decke1_512.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(0) = CreateTexture(@buffer(0))                '' Linear Texture (default)
    
    bload "c:/G7/texturen/Boden1_512.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(1) = CreateTexture(@buffer(0))                '' Linear Texture (default)
    
    bload "c:/G7/texturen/Wand1_512.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(2) = CreateTexture(@buffer(0))                '' Linear Texture (default)
    
    bload "c:/G7/texturen/Grass1_512.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(3) = CreateTexture(@buffer(0))                '' Linear Texture (default)
    
    bload "c:/G7/texturen/Boden2_512.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(4) = CreateTexture(@buffer(0))                '' Linear Texture (default)
    
    bload "c:/G7/texturen/Boden3_512.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(5) = CreateTexture(@buffer(0))                '' Linear Texture (default)
    
    bload "c:/G7/texturen/Wand2_512.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(6) = CreateTexture(@buffer(0))                '' Linear Texture (default)
    
    bload "c:/G7/texturen/Wand3_512.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(7) = CreateTexture(@buffer(0))                '' Linear Texture (default)
    
    bload "c:/G7/texturen/Wand4_512.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(8) = CreateTexture(@buffer(0))                '' Linear Texture (default)
    
    bload "c:/G7/texturen/Saeule1_512.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(9) = CreateTexture(@buffer(0))                '' Linear Texture (default)

    bload "c:/G7/texturen/Tuer1_512.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(10) = CreateTexture(@buffer(0))                '' Linear Texture (default)

    bload "c:/G7/texturen/kaeschtli_512.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(11) = CreateTexture(@buffer(0))                '' Linear Texture (default)

    bload "c:/G7/texturen/Font.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(12) = CreateTexture(@buffer(0))                '' Linear Texture (default)
    
     bload "c:/G7/texturen/Icons.bmp", @buffer(0)                      '' BLOAD data from bitmap
	texture(13) = CreateTexture(@buffer(0))                '' Linear Texture (default)
    
    'glBindTexture(GL_TEXTURE_2D, 12)         '' Bind Our Texture
	'glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)  '' Linear Filtered
	'glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)  '' Linear Filtered
    'glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 256, 256, 0, GL_RGB, GL_UNSIGNED_BYTE, texture(12))

	'' Exit if error loading textures
	if texture(0) = 0 or texture(1) = 0 or texture(2) = 0 then end 1

    BuildFont()                                           '' Build The Font
    BuildIcons()


' ***************** OpenGl

	'' All Setup For OpenGL Goes Here
	glEnable GL_TEXTURE_2D                                '' Enable Texture Mapping
	glBlendFunc GL_SRC_ALPHA, GL_ONE                      '' Set The Blending Function For Translucency
	glClearColor 0.0, 0.0, 0.0, 0.5                       '' Black Background
	glClearDepth 1.0                                      '' Depth Buffer Setup
	glDepthFunc GL_LESS                                   '' The Type Of Depth Test To Do
	glEnable GL_DEPTH_TEST                                '' Enables Depth Testing
	glShadeModel GL_SMOOTH                                '' Enable Smooth Shading
	glHint GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST      '' Really Nice Perspective Calculations

	'setupWorld()
    xpos=sichtweite+1.5
    zpos=sichtweite+1.5
    face=4
    heading=3*90
    yrot=heading
	do
        
    glEnable GL_DEPTH_TEST  
    'glMatrixMode (GL_PROJECTION)                          '' Select The Projection Matrix
	' glPushMatrix ()     

        ' Kollisionsabfrage
        
        if (xpos+deltax>sichtweite+2.-coll and playfield(startx,starty+1,1,level)>63) then
            deltax=0
        elseif (xpos+deltax<=sichtweite+1+coll and playfield(startx,starty-1,1,level)>63) then 
            deltax=0
        end if
        
        if (zpos+deltaz>sichtweite+2.-coll and playfield(startx+1,starty,1,level)>63 ) then
            deltaz=0
        elseif (zpos+deltaz<=sichtweite+1.+coll and playfield(startx-1,starty,1,level)>63) then 
            deltaz=0
        end if
        
        ' Spieler Bewegung
        
        if (xpos+deltax>sichtweite+2.) then
            if playfield(startx,starty+1,1,level)<64 then
             xpos=xpos+deltax-1.
             starty=starty+1
            else
                xpos=xpos-2*deltax
            end if
        elseif (xpos+deltax<=sichtweite+1.) then 
            if playfield(startx,starty-1,1,level)<64 then
              xpos=xpos+deltax+1.
              starty=starty-1
            end if
        else
             xpos=xpos+deltax
        end if
        
        if (zpos+deltaz>sichtweite+2.) then
            if playfield(startx+1,starty,1,level)<64 then
             zpos=zpos+deltaz-1.
             startx=startx+1.
            end if
        elseif (zpos+deltaz<=sichtweite+1.) then 
            if playfield(startx-1,starty,1,level)<64 then
             zpos=zpos+deltaz+1.
             startx=startx-1.
            end if
        else
            zpos=zpos+deltaz
        end if
        
        deltax=0.
        deltaz=0.
        
		glClear GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT      '' Clear Screen And Depth Buffer
		glLoadIdentity()                                        '' Reset The View
		
		xtrans = - xpos
		ztrans = - zpos
		ytrans = - walkbias - 0.5                      '' Used For Bouncing Motion Up And Down
		sceneroty = 360.0 - yrot                        '' 360 Degree Angle For Player Direction
		
		glRotatef lookupdown, 1.0, 0,0                  '' Rotate Up And Down To Look Up And Down
		glRotatef sceneroty, 0, 1.0, 0                  '' Rotate Depending On Direction Player Is Facing
		
		glTranslatef xtrans, ytrans, ztrans             '' Translate The Scene Based On Player Position
		glBindTexture GL_TEXTURE_2D, texture(filter)    '' Select A Texture Based On filter
		
		numtriangles = sector1.numtriangles             '' Get The Number Of Triangles In Sector 1
		
        
        
        
        GetLocalPlayfield(startx,starty)
        
        
        for l1=1 to 2*sichtweite+1
         for l2=1 to 2*sichtweite+1
          if locplay(l2,l1)>63 then
            select case locplay(l2,l1)
            case 65: 'holz
              drawmauer(1,l1,l2, 2)
           case 97: 'holzsaeule
              drawsaeule(1,5,l1,l2, 2,5)
            case 66: ' sandstein
              drawmauer(2,l1,l2, 6)
            case 98: ' sandsteinsaeule
              drawsaeule(1,3,l1,l2, 6,4)
            case 67: 'GLASSSAEULE
              drawsaeule(1,1,l1,l2, 7,3)
            case 68: ' granit
              drawmauer(1,l1,l2, 8)
            case 100: ' granitsaeule
              drawsaeule(1,1,l1,l2, 8,1)
            case 79: ' saeule baum
              drawsaeule(1,1,l1,l2, 9,3)
            case 84' tuer zu
              drawmauer(1,l1,l2, 10)
            case 83' Schatz
              drawcube(1,1,l1,l2, 11,1)
            end select
        
            
            
            
          else
                    
           
            select case locplay(l2,l1) ' Bodentextur
            case 32:
              ' pflastersteine
               drawvoid(1,1,l1,l2,0,1)
            case 33:
               ' grass
                drawvoid(1,2,l1,l2,0,3)
            case 34:
               ' fliesen
                drawvoid(1,3,l1,l2,0,4)
             case 35:
              ' x steine
                drawvoid(1,5,l1,l2,0,5)
              case 61:
              ' tuer offen
                drawvoid(1,1,l1,l2,0,10)
            end select
            
           
          end if
         next
        next
        

		
    ' ******************************** BLOCK EDIT
        dummyx=startx
        dummyy=starty
        frontpos(heading,dummyx,dummyy)
        if MULTIKEY(FB.SC_0) then           'kill block
			'
		end if
        
        
        
        
		if MULTIKEY(FB.SC_F) and not fp then           '' F Key down
			fp = true
			filter += 1                             '' Cycle filter 0 -> 1 -> 2
			if (filter > 2) then filter = 0         '' 2 -> 0
		end if
		if not MULTIKEY(FB.SC_F) then fp = false       '' F Key Up
	
		if MULTIKEY(FB.SC_B) and not bp then           '' B Key down
			bp = true
			blend = not blend                       '' toggle blending On/Off
			if blend then
				glEnable(GL_BLEND)                  '' Turn Blending On
				glDisable(GL_DEPTH_TEST)            '' Turn Depth Testing Off
			else
				glDisable(GL_BLEND)                 '' Turn Blending Off
				glEnable(GL_DEPTH_TEST)             '' Turn Depth Testing On
			end if
		end if
		if not MULTIKEY(FB.SC_B) then bp = false       '' B Key up
	
     

' ############ ACTION
	    if mouseb2=1 and tflag=0 then
           if playfield(startx+1,starty,1,level)=asc("T") then
            playfield(startx+1,starty,1,level)=asc("=")
           elseif playfield(startx+1,starty,1,level)=asc("=") then
            playfield(startx+1,starty,1,level)=asc("T")
           end if

           if playfield(startx,starty+1,1,level)=asc("T") then
            playfield(startx,starty+1,1,level)=asc("=")
           elseif playfield(startx,starty+1,1,level)=asc("=") then
            playfield(startx,starty+1,1,level)=asc("T")
           end if

           if playfield(startx-1,starty,1,level)=asc("T") then
            playfield(startx-1,starty,1,level)=asc("=")
           elseif playfield(startx-1,starty,1,level)=asc("=") then
            playfield(startx-1,starty,1,level)=asc("T")
           end if

           if playfield(startx,starty-1,1,level)=asc("T") then
            playfield(startx,starty-1,1,level)=asc("=")
           elseif playfield(startx,starty-1,1,level)=asc("=") then
            playfield(startx,starty-1,1,level)=asc("T")
           end if
         

           if playfield(startx+1,starty,1,level)=asc("S") then
            playfield(startx+1,starty,1,level)=asc(" ")
           end if
           if playfield(startx,starty+1,1,level)=asc("S") then
            playfield(startx,starty+1,1,level)=asc(" ")
           end if
           if playfield(startx-1,starty,1,level)=asc("S") then
            playfield(startx-1,starty,1,level)=asc(" ")
           end if
           if playfield(startx,starty-1,1,level)=asc("S") then
            playfield(startx,starty-1,1,level)=asc(" ")
           end if


        tflag=1
        end if
 if mouseb2<>1 then tflag = 0


' ********************************* Bewegung

		if MULTIKEY(FB.SC_W) then
			deltax = -sin(heading*piover180) * stepforward    '' Move On The X-Plane Based On Player Direction
			deltaz = -cos(heading*piover180) * stepforward   '' Move On The Z-Plane Based On Player Direction
			
            if walkbiasangle >= 359.0 then                 '' Is walkbiasangle>=359?
				walkbiasangle = 0.0                        '' Make walkbiasangle Equal 0
			else
				walkbiasangle = walkbiasangle + 10         '' If walkbiasangle < 359 Increase It By 10
			end if
			walkbias = sin(walkbiasangle * piover180)*bounce '' Causes The Player To Bounce
		end if
	
		if MULTIKEY(FB.SC_S) then
            deltax = sin(heading*piover180) * stepforward    '' Move On The X-Plane Based On Player Direction
			deltaz = cos(heading*piover180) * stepforward   '' Move On The Z-Plane Based On Player Direction
			
            if walkbiasangle <= 1.0 then                   '' Is walkbiasangle<=1?
				walkbiasangle = 359.0                      '' Make walkbiasangle Equal 359
			else
				walkbiasangle = walkbiasangle - 10         '' If walkbiasangle > 1 Decrease It By 10
			end if
			walkbias = sin(walkbiasangle * piover180)*bounce '' Causes The Player To Bounce
		end if
	
     if MULTIKEY(FB.SC_A) then 'and lflag=0 then
            deltax = -sin((heading+90)*piover180) * stepforward    '' Move On The X-Plane Based On Player Direction
			deltaz = -cos((heading+90)*piover180) * stepforward   '' Move On The Z-Plane Based On Player Direction
        end if
        'if not MULTIKEY(FB.SC_LEFT) then lflag = 0

        if MULTIKEY(FB.SC_D) then 'and rflag=0 then
             deltax = -sin((heading-90)*piover180) * stepforward    '' Move On The X-Plane Based On Player Direction
			deltaz = -cos((heading-90)*piover180) * stepforward   '' Move On The Z-Plane Based On Player Direction
			
        end if
        'if not MULTIKEY(FB.SC_RIGHT) then rflag = 0
      
  ' ******************************** Maus
        GetMouse( mousex, mousey , mouseb1,mouseb2 )
        if (mouseb2)<>2 then drehmode=0
        if (mouseb2=2) and drehmode=0 then
            drehmode=1
            mouseofx=mousex
            mouseofy=mousey
            htemp=heading
            ltemp=lookupdown
        end if
        
        if drehmode=1 and mousex<>mouseofx then
            heading = htemp-(mousex-mouseofx)/3
            if heading>360 then heading=heading-360
            if heading<0 then heading=heading+360
            yrot = heading
            lflag=1
        end if
        if drehmode=1 and mousey<>mouseofy then
            lookupdown = ltemp+(mousey-mouseofy)/3
            if lookupdown>50 then lookupdown=50
            if lookupdown<-50 then lookupdown=-50
        end if
' ****************************************************************************************************** EDITOR
  Editorloop(locx,locy)

' ******************************** Status
        
        glMatrixMode (GL_PROJECTION)  
        glPushMatrix () 
        glLoadIdentity ()                                 '' Reset The Projection Matrix
		glOrtho (0, screenx, 0, screeny, - 1, 1)                '' Set Up An Ortho Screen
		glMatrixMode (GL_MODELVIEW)                       '' Select The Modelview Matrix

        
        
         glPrint (30, 150, 0,"HP: "+str(hp)+"/"+str(hpm)) 'hp hmp
         glPrint (30, 150-30, 0,"SP: "+str(mousex)+"/"+str(mousey)) ' sp spm
         glPrint (30, 150-2*30, 0,"X/Y: "+str(startx)+"/"+str(starty)) 'st stm
         
         glPrint (30, 1050, 0,"Level: "+str(level))
         glPrint (30, 1050-30, 0,"Editmode: "+str(editmode))
         glPrint (30, 1050-30*2, 0,editmodeDescription(editmode))

        glViewport 0, 0, screenx,screeny
        'glLoadIdentity ()
        ' glOrtho (0, 1920, 0, 1080, - 1, 1)
        glMatrixMode (GL_MODELVIEW)
        glDisable GL_TEXTURE_2D
        
        
        ' #######################################################################
        EditorDisplay()


    glMatrixMode (GL_PROJECTION)                      '' Select The Projection Matrix
	glPopMatrix ()                                        '' Restore The Old Projection Matrix
	glMatrixMode (GL_MODELVIEW)                           '' Select The Modelview Matrix
	glFlush ()                                            '' Flush The GL Rendering Pipeline


        screensync
		flip '' flip or crash
		' if inkey = chr(255)+"k" then exit do
        'ScreenSet 1,1

	loop while MULTIKEY(FB.SC_ESCAPE) = 0

	'' Empty keyboard buffer
	while inkey <> "": wend

	end

' ****************************************************************************** Editor

sub EditorInit()
dim as integer l3,l1,l2
  editmode=1 ' 1-Steineditor 2-Bodeneditor 3-Dreh Editor, 4-ActionID, 5-ActionNr, 6-Steinreservoir, 7-Drehung Reservoir, 8 SteinID Reservoir, 9-ActionID reservoir
  editmodeDescription(0) = ""
  editmodeDescription(1) = "Steineditor"
  editmodeDescription(2) = "Bodeneditor"
  editmodeDescription(3) = "Dreh Editor"
  editmodeDescription(4) = "ActionID"
  editmodeDescription(5) = "ActionNr"
  editmodeDescription(6) = "Steinreservoir"
  editmodeDescription(7) = "Drehung Reservoir"
  editmodeDescription(8) = "SteinID Reservoir"
  editmodeDescription(9) = "ActionID reservoir"

  for l3=1 to 9
    for l1=1 to 64
      for l2=1 to 64
       playfield(l1,l2,1,l3)=asc("."):playfield(l1,l2,2,l3)=asc("."):playfield(l1,l2,3,l3)=asc("."):playfield(l1,l2,4,l3)=asc(".")
       playfield(l1,l2,5,l3)=asc("."):playfield(l1,l2,6,l3)=asc("."):playfield(l1,l2,7,l3)=asc("."):playfield(l1,l2,8,l3)=asc(".")
       playfield(l1,l2,9,l3)=asc(".")
      next
    next 
  next  
  
  savenames(1)=levelPath+"level1.txt"
  savenames(2)=levelPath+"level2.txt"
  savenames(3)=levelPath+"level3.txt"
  savenames(4)=levelPath+"level4.txt"
  savenames(5)=levelPath+"level5.txt"
  savenames(6)=levelPath+"level6.txt"
  savenames(7)=levelPath+"level7.txt"
  savenames(8)=levelPath+"level8.txt"
  savenames(9)=levelPath+"level9.txt"
  
  level=1
  feldrot=0
  rotlock=0
  steinwahl=32
end sub

sub EditorDisplay()        
        PrintPlayfield() ' ***** Editor zeichnen
        
        PrintTyp(1,1,asc("u"), menux,menuy,0)  'save
        PrintTyp(2,1,asc("v"), menux,menuy,0)  'load
        PrintTyp(3,1,asc("1"), menux,menuy,0)  'editmode1
        PrintTyp(4,1,asc("2"), menux,menuy,0)  'editmode2
        PrintTyp(5,1,asc("3"), menux,menuy,0)  'editmode3
        PrintTyp(6,1,asc("9"), menux,menuy,0)  'convert
        PrintTyp(7,1,asc("8"), menux,menuy,0)  'delete
        
        'glPrint (screenx/2.+60, screeny, 0,"Save") 'hp hmp
        'glPrint (screenx/2.+160, screeny, 0,"Load") 'hp hmp
        
        'glViewport 0, 0, screenx/2.,screeny 
        glColor3f 1.,1.,1.
        glEnable GL_TEXTURE_2D
end sub

' ------------------------------------------------------------------------------ Editorloop
sub Editorloop(byval locx as integer, byval locy as integer)
dim as integer mousex, mousey , mouseb1,mouseb2
 
  if MULTIKEY(FB.SC_1) then editmode=1
  if MULTIKEY(FB.SC_2) then editmode=2 
  if MULTIKEY(FB.SC_3) then editmode=3
  if MULTIKEY(FB.SC_4) then editmode=4
  if MULTIKEY(FB.SC_5) then editmode=5
  if MULTIKEY(FB.SC_6) then editmode=6
  if MULTIKEY(FB.SC_7) then editmode=7
  if MULTIKEY(FB.SC_8) then editmode=8
  if MULTIKEY(FB.SC_9) then editmode=9
  
  if MULTIKEY(FB.SC_F1) then level=1
  if MULTIKEY(FB.SC_F2) then level=2
  if MULTIKEY(FB.SC_F3) then level=3
  if MULTIKEY(FB.SC_F4) then level=4
  if MULTIKEY(FB.SC_F5) then level=5
  if MULTIKEY(FB.SC_F6) then level=6
  if MULTIKEY(FB.SC_F7) then level=7
  if MULTIKEY(FB.SC_F8) then level=8
  if MULTIKEY(FB.SC_F9) then level=9
    
        
 GetMouse( mousex, mousey , mouseb1,mouseb2 )
' Editor
if (mouseb2<>2) then rotlock=0

if (mouseb2=1 and testmouse(mousex,mousey, edix,ediy,64,64,locx,locy)=1 and (editmode<3 or editmode=6)) then
     playfield(locx,locy,editmode,level)=steinwahl

elseif (mouseb2=2 and testmouse(mousex,mousey, edix,ediy,64,64,locx,locy)=1 and rotlock=0 and (editmode<4 or (editmode>5 and editmode<8))) then
  if editmode<4 then
     if playfield(locx,locy,3,level)=asc("0") then 
        playfield(locx,locy,3,level)=asc("1")
     elseif playfield(locx,locy,3,level)=asc("1") then 
        playfield(locx,locy,3,level)=asc("2")
     elseif playfield(locx,locy,3,level)=asc("2") then 
         playfield(locx,locy,3,level)=asc("3")
     elseif playfield(locx,locy,3,level)=asc("3") then 
         playfield(locx,locy,3,level)=asc("0")
     end if
     rotlock=1
  elseif editmode>5 then
      if playfield(locx,locy,7,level)=asc("0") then 
        playfield(locx,locy,7,level)=asc("1")
     elseif playfield(locx,locy,7,level)=asc("1") then 
        playfield(locx,locy,7,level)=asc("2")
     elseif playfield(locx,locy,7,level)=asc("2") then 
         playfield(locx,locy,7,level)=asc("3")
     elseif playfield(locx,locy,7,level)=asc("3") then 
         playfield(locx,locy,7,level)=asc("0")
     end if
     rotlock=1
  end if
     
' SteinID
elseif (mouseb2=2 and testmouse(mousex,mousey, edix,ediy,64,64,locx,locy)=1 and rotlock=0 and (editmode=4 or editmode=8)) then
     playfield(locx,locy,editmode,level)=playfield(locx,locy,editmode,level)+1
     if playfield(locx,locy,editmode,level)>asc("9") then  playfield(locx,locy,editmode,level)=asc("0")
     rotlock=1

' ActionID
elseif (mouseb2=2 and testmouse(mousex,mousey, edix,ediy,64,64,locx,locy)=1 and rotlock=0 and (editmode=5 or editmode=9)) then
     playfield(locx,locy,editmode,level)=playfield(locx,locy,editmode,level)+1
     if playfield(locx,locy,editmode,level)>asc("9") then  playfield(locx,locy,editmode,level)=asc("0")
     rotlock=1


elseif (mouseb2=1 and testmouse(mousex,mousey, lagerx,lagery,26,1,locx,locy)=1) then
        select case (locx)
        case 1: 
          steinwahl=32
        case 2:
          steinwahl=66
        case 3:
          steinwahl=65
        case 4: 
          steinwahl=84 ' tuer vert
        case 5: 
          steinwahl=100
        case 6: 
          steinwahl=asc("t") ' Tuer Quer
        case 7: 
          steinwahl=asc("S") ' Schatz
        case 8: 
          steinwahl=asc("U") ' Urne
        case 9: 
          steinwahl=asc("D") ' steinwand
        case 10: 
          steinwahl=asc("O") ' Jaegerzaun
        case 11: 
          steinwahl=asc("a") ' holzsaeule
        case 12: 
            steinwahl=asc("#")   'Gartensteine
        case 13: 
            steinwahl=asc("b")  ' sandsteinsäule
        case 14: 
            steinwahl=33  ' gras
        case 15: 
            steinwahl=asc("H")  ' Hebel
        case 16: 
            steinwahl=asc("$")  ' Druckplatte oben
        case 17: 
            steinwahl=asc("%")  ' Druckplatte oben
        case 18: 
            steinwahl=asc("h")  ' Hebel
        case 19: 
            steinwahl=67  ' Baum
        case 20: 
            steinwahl=asc("W")  ' Marmorwand
        case 21: 
            steinwahl=34  ' Marmorboden
        case 22: 
            steinwahl=asc("m")  ' Marmorwand
        case 23: 
            steinwahl=asc("(")  ' Treppe
        case 24: 
            steinwahl=asc(")")  ' Treppenloch
        case 25: 
            steinwahl=asc("*")  ' Portal
        case 26: 
            steinwahl=asc("X")  ' Monster
            
        end select 
elseif (mouseb2=1 and testmouse(mousex,mousey, menux,menuy,7,1,locx,locy)=1) then
      select case (locx)
      case 1: 
        SavePlayfield(level)
      case 2:
        LoadPlayfield()
      case 3:
        editmode=1
      case 4:
        editmode=2
      case 5:
        editmode=3
      case 6:
        ConvertPlayfield() 
      case 7:
        DeletePlayfield() 
      end select 
end if

end sub


' **********************************************************************
' MAP
' **********************************************************************

' ------------------------------------------------------------------------------testmouse
function testmouse(byval mx as integer,byval my as integer,byval ox as integer,byval oy as integer, byval dx as integer,byval dy as integer,byref locx as integer,byref locy as integer) as integer
  if ((mx>=ox) and (my>=oy) and (mx<=ox+(dx)*16) and (my<=oy+(dy)*16)) then
      locx=floor((mx-ox)/16.)+1
      locy=floor((my-oy)/16.)+1
      hp=locx
      hpm=locy
      return 1
  end if
  return 0
end function

' ------------------------------------------------------------------------------PrintTyp
sub PrintTyp(byval l1 as integer,byval l2 as integer,byval typ as byte,byval offx as integer,byval offy as integer, byval ang as integer)
    glPrintIcon ((l1-1)*16+offx, 1080-(offy+(l2-1)*16),0,typ, ang)
end sub

'------------------------------------------------------------------------------- SaveLayer
' speichert Spielfeld layer
sub SaveLayer(byref fp as integer, byval layer as integer,byval level as integer)
dim oneline as string
dim as integer l1, l2
  for l1=1 to 64
    oneline=""
    for l2=1 to 64
      oneline=oneline+chr(playfield(l2,l1,layer,level))  
    next l2
    writestr(fp, oneline)
  next l1
end sub

' ------------------------------------------------------------------------------SavePlayfield
sub SavePlayfield(byval levelnum as integer)
dim oneline as string               '' String To Store Data In
dim as integer fp,i
  fp = freefile
  if (open (savenames(levelnum) for output as #fp) <> 0) then end 1
  For i=1 to 9:SaveLayer(fp,i,levelnum):next i ' Wand/Objekte laden,Boden,Drehungen,SteinID,ActionID,Steinreserve,Drehmatrix,SteinID,ActionID
  close #fp
end sub

' ------------------------------------------------------------------------------ConvertPlayfield
sub ConvertPlayfield()
	
	dim as integer l1, l2,stein1,stein2
	
	for l1=1 to 64
        for l2=1 to 64
          stein1=playfield(l2,l1,1,level)
          stein2=playfield(l2,l1,2,level)
          if (stein1<64) and (stein2=asc(".")) then
              playfield(l2,l1,2,level)=playfield(l2,l1,1,level)
          elseif (stein1=66 or stein1=65 or stein1=asc("D") or stein1=asc("W") ) and (stein2=asc(".")) then
              playfield(l2,l1,2,level)=32
          elseif (stein1=32) and (stein2<>asc(".")) then
              playfield(l2,l1,1,level)=playfield(l2,l1,2,level)
          end if
          
       next
	next
end sub

' ------------------------------------------------------------------------------DeletePlayfield
sub DeletePlayfield()
	
	dim as integer l1, l2	
	for l1=1 to 64
        for l2=1 to 64
          if (editmode=3) then playfield(l2,l1,editmode,level)=asc("0") else playfield(l2,l1,editmode,level)=asc(".")
       next
	next
end sub
' ------------------------------------------------------------------------------PrintPlayfield
sub PrintPlayfield()
	
	dim oneline as string               '' String To Store Data In
	dim as integer l1, l2,ang
	dim fp as integer
    dim as integer fak1,fak2,ofx,ofy
    
    fak2=7
    fak1=2*fak2
    ofx=800
    ofy=0

    ' Spielfeld
	for l1=1 to 64
      for l2=1 to 64
          if (editmode=1) then 
              ang=(playfield(l1,l2,3,level)-asc("0"))*90 
          elseif (editmode=6) then 
              ang=(playfield(l1,l2,7,level)-asc("0"))*90 
          else 
              ang=0
          end if
          PrintTyp(l1,l2,playfield(l1,l2,editmode,level),edix,ediy,ang)
      next
	next

   ' Spielerpos
   'glBegin GL_LINE_LOOP
   '  glColor3f 1.,1.,0.
	' glVertex3f  (startx+0.2)*fak1+ofx,(starty+0.2)*fak2+ofy,0.
    ' glVertex3f  (startx+0.8)*fak1+ofx,(starty+0.2)*fak2+ofy,0.
     'glVertex3f  (startx+0.8)*fak1+ofx,(starty+0.8)*fak2+ofy,0.
     'glVertex3f  (startx+0.2)*fak1+ofx,(starty+0.8)*fak2+ofy,0.
  'glEnd
  
  ' Spiel-Steine
  PrintTyp(1,1,32       ,lagerx,lagery,0)
  PrintTyp(2,1,66       ,lagerx,lagery,0)
  PrintTyp(3,1,65       ,lagerx,lagery,0)
  PrintTyp(4,1,asc("T") ,lagerx,lagery,0)
  PrintTyp(5,1,100       ,lagerx,lagery,0)
  PrintTyp(6,1,asc("t")  ,lagerx,lagery,0)
  PrintTyp(7,1,asc("S")  ,lagerx,lagery,0)
  PrintTyp(8,1,asc("U")  ,lagerx,lagery,0) 'Urne
  PrintTyp(9,1,asc("D")  ,lagerx,lagery,0) 'Steinwand
  PrintTyp(10,1,asc("O")  ,lagerx,lagery,0) 'Steinwand
  PrintTyp(11,1,asc("a")  ,lagerx,lagery,0) 'holzsaeule
  PrintTyp(12,1,asc("#")  ,lagerx,lagery,0) 'Gartensteine
  PrintTyp(13,1,asc("b")  ,lagerx,lagery,0) 'marmorsaeule
  PrintTyp(14,1,33         ,lagerx,lagery,0) 'gras
  PrintTyp(15,1,asc("H")  ,lagerx,lagery,0) 'Hebel
  PrintTyp(16,1,asc("$")  ,lagerx,lagery,0) 'Druckplatte oben
  PrintTyp(17,1,asc("%")  ,lagerx,lagery,0) 'Druckplatte unten
  PrintTyp(18,1,asc("h")  ,lagerx,lagery,0) 'hebel
  PrintTyp(19,1,67  ,lagerx,lagery,0) 'Baum
  PrintTyp(20,1,asc("W")  ,lagerx,lagery,0) 'marmorwand
  PrintTyp(21,1,34  ,lagerx,lagery,0) 'marmorboden
  PrintTyp(22,1,asc("m")  ,lagerx,lagery,0) 'marmorboden
  PrintTyp(23,1,asc("(")  ,lagerx,lagery,0) 'Treppe
  PrintTyp(24,1,asc(")")  ,lagerx,lagery,0) 'Treppenloch
  PrintTyp(25,1,asc("*")  ,lagerx,lagery,0) 'Portal
  PrintTyp(26,1,asc("X")  ,lagerx,lagery,0) 'Monster


end sub

' ******************************************************************************
' ******************************************************************************

''------------------------------------------------------------------------------
sub readstr(byval f as integer, byref Buffer as string)
	do
		line input #f, Buffer        '' Get one line
	loop while (left(Buffer,1) = "/") or (Buffer = "")   '' See If It Is Worthy Of Processing
end sub

sub writestr(byval f as integer, byref Buffer as string)
	print #f, Buffer        ' Get one line
end sub
' ---------------------------- gibt die feldpos vor dem Spieler
sub frontpos(byval winkel as integer, byref xp as integer, byref yp as integer)
	if (winkel >=365-45 or winkel<45)  then
        xp=xp-1
    elseif (winkel >=45 and winkel<45+90)  then
        yp=yp-1
    elseif (winkel>=45+90 and winkel<45+180)  then
        xp=xp+1
    elseif (winkel>=45+180 and winkel<45+180+90)  then
        yp=yp+1
    end if
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

'-------------------------------------------------------------------------------
sub LoadPlayfield()
  dim as integer fp,i
  fp = freefile
  if (open (savenames(level), for input, as #fp) <> 0) then end 1
  For i=1 to 9:ReadLayer(fp,i,level):next i ' Wand/Objekte laden,Boden,Drehungen,SteinID,ActionID,Steinreserve,Drehmatrix,SteinID,ActionID
  close #fp
end sub

sub SetupPlayfield()
	
	dim oneline as string               '' String To Store Data In
	dim as integer l1, l2,l3
	dim fp as integer

   for l3=1 to 9
	for l1=1 to 64
      for l2=1 to 64
          playfield(l2,l1,1,l3)=32
          playfield(l2,l1,3,l3)=asc("0")
          playfield(l2,l1,4,l3)=asc("0")
          playfield(l2,l1,5,l3)=asc("0")
          playfield(l2,l1,7,l3)=asc("0")
          playfield(l2,l1,8,l3)=asc("0")
          playfield(l2,l1,9,l3)=asc("0")
          if (l1=1) or (l2=1) or (l1=64) or (l2=64) then
           playfield(l2,l1,1,l3)=66 
          end if
      next
      next 
	next
end sub

sub GetLocalPlayfield(byval xpos as integer,byval ypos as integer)
	
	dim as integer l1, l2,lx,ly

	 for l1=1 to 2*sichtweite+1
        for l2=1 to 2*sichtweite+1
          lx=xpos+l2-sichtweite-1
          ly=ypos+l1-sichtweite-1
          if lx>64 or ly>64 or lx<1 or ly <1 then
              locplay(l2,l1)=32
          else
              locplay(l2,l1)=playfield(lx,ly,1,level)
          end if
        next
     next

end sub





' #######################

sub drawvoid(byval tscale1 as single,byval tscale2 as single,byval l1 as single,byval l2 as single, byval txt1 as integer,byval txt2 as integer)

 glBindTexture GL_TEXTURE_2D, texture(txt1) 
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale1 : glVertex3f l1, 1., l2+1   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1, 1., l2   '' Set The TexCoord And Vertice
				glTexCoord2f tscale1, 0. : glVertex3f l1+1, 1., l2   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0.,tscale1: glVertex3f l1, 1., l2+1   '' Set The TexCoord And Vertice
				glTexCoord2f tscale1, tscale1 : glVertex3f l1+1., 1., l2+1.   '' Set The TexCoord And Vertice
				glTexCoord2f tscale1, 0. : glVertex3f l1+1, 1., l2   '' Set The TexCoord And Vertice
			glEnd

glBindTexture GL_TEXTURE_2D, texture(txt2) 
 'glBindTexture GL_TEXTURE_2D, texture(1) 
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale2 : glVertex3f l1, 0., l2+1   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1, 0., l2   '' Set The TexCoord And Vertice
				glTexCoord2f tscale2, 0. : glVertex3f l1+1, 0., l2   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale2 : glVertex3f l1, 0., l2+1   '' Set The TexCoord And Vertice
				glTexCoord2f tscale2, tscale2 : glVertex3f l1+1., 0., l2+1.   '' Set The TexCoord And Vertice
				glTexCoord2f tscale2, 0. : glVertex3f l1+1, 0., l2   '' Set The TexCoord And Vertice
			glEnd
end sub


sub drawmauer(byval tscale as single,byval l1 as single,byval l2 as single, byval txt1 as integer)
 glBindTexture GL_TEXTURE_2D, texture(txt1)
 glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1, 1., l2   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1, 0., l2   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1, 0., l2   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1, 1., l2   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1+1, 1., l2   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1, 0., l2   '' Set The TexCoord And Vertice
			glEnd
            
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1, 1., l2   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1, 0., l2   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1, 0., l2+1   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1, 1., l2   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1, 1., l2+1   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1, 0., l2+1   '' Set The TexCoord And Vertice
			glEnd
            
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+1, 1., l2   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1+1, 0., l2   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1, 0., l2+1   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+1, 1., l2   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1+1, 1., l2+1   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1, 0., l2+1   '' Set The TexCoord And Vertice
			glEnd
            
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1, 1., l2+1   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1, 0., l2+1   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1, 0., l2+1   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1, 1., l2+1   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1+1, 1., l2+1   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1, 0., l2+1   '' Set The TexCoord And Vertice
			glEnd
end sub

sub drawsaeule(byval tscale as single,byval tscale2 as single,byval l1 as single,byval l2 as single,byval txt1 as integer,byval txt2 as integer)
   
   drawvoid(1,tscale2,l1,l2,0,txt2)
   
   glBindTexture GL_TEXTURE_2D, texture(txt1)

   glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+soff, 1., l2+soff   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1+soff, 0., l2+soff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-soff, 0., l2+soff   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+soff, 1., l2+soff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1+1-soff, 1., l2+soff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-soff, 0., l2+soff   '' Set The TexCoord And Vertice
			glEnd
            
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+soff, 1., l2+soff   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1+soff, 0., l2+soff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+soff, 0., l2+1-soff   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+soff, 1., l2+soff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1+soff, 1., l2+1-soff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+soff, 0., l2+1-soff   '' Set The TexCoord And Vertice
			glEnd
            
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+1-soff, 1., l2+soff   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1+1-soff, 0., l2+soff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-soff, 0., l2+1-soff   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+1-soff, 1., l2+soff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1+1-soff, 1., l2+1-soff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-soff, 0., l2+1-soff   '' Set The TexCoord And Vertice
			glEnd
            
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+soff, 1., l2+1-soff   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1+soff, 0., l2+1-soff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-soff, 0., l2+1-soff   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+soff, 1., l2+1-soff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1+1-soff, 1., l2+1-soff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-soff, 0., l2+1-soff   '' Set The TexCoord And Vertice
			glEnd

end sub

sub drawcube(byval tscale as single,byval tscale2 as single,byval l1 as single,byval l2 as single,byval txt1 as integer,byval txt2 as integer)
   
   drawvoid(1,tscale2,l1,l2,0,txt2)
   
   glBindTexture GL_TEXTURE_2D, texture(txt1)

   glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+coff, coff, l2+coff   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1+coff, 0., l2+coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-coff, 0., l2+coff   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+coff, coff, l2+coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1+1-coff, coff, l2+coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-coff, 0., l2+coff   '' Set The TexCoord And Vertice
			glEnd
            
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+coff, coff, l2+coff   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1+coff, 0., l2+coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+coff, 0., l2+1-coff   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+coff, coff, l2+coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1+coff, coff, l2+1-coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+coff, 0., l2+1-coff   '' Set The TexCoord And Vertice
			glEnd
            
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+1-coff, coff, l2+coff   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1+1-coff, 0., l2+coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-coff, 0., l2+1-coff   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+1-coff, coff, l2+coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1+1-coff, coff, l2+1-coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-coff, 0., l2+1-coff   '' Set The TexCoord And Vertice
			glEnd
            
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+coff, coff, l2+1-coff   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1+coff, 0., l2+1-coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-coff, 0., l2+1-coff   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale : glVertex3f l1+coff, coff, l2+1-coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1+1-coff, coff, l2+1-coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-coff, 0., l2+1-coff   '' Set The TexCoord And Vertice
			glEnd

            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0., tscale: glVertex3f l1+coff, coff, l2+1-coff   '' Set The TexCoord And Vertice
				glTexCoord2f 0., 0. : glVertex3f l1+coff, coff, l2+coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1-coff, coff, l2+coff   '' Set The TexCoord And Vertice
			glEnd
            glBegin GL_TRIANGLES                          '' Start Drawing Triangles
				glNormal3f 0.0, 0.0, 1.0                  '' Normal Pointing Forward
				glTexCoord2f 0.,tscale: glVertex3f l1+coff, coff, l2+1.-coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, tscale : glVertex3f l1+1.-coff, coff, l2+1.-coff   '' Set The TexCoord And Vertice
				glTexCoord2f tscale, 0. : glVertex3f l1+1.-coff, coff, l2+coff   '' Set The TexCoord And Vertice
			glEnd

            


end sub


'------------------------------------------------------------------------------- ICONS
sub BuildIcons()                                  '' Build Our Font Display List
	dim loop1 as integer
	dim cx as single                             '' Holds Our X Character Coord
	dim cy as single                             '' Holds Our Y Character Coord

	gbase = glGenLists(256)                      '' Creating 256 Display Lists
	glBindTexture GL_TEXTURE_2D, texture(13)      '' Select Our Font Texture
	for loop1 = 0 to 255                         '' Loop Through All 256 Lists

		cx = (loop1 mod 16)/16.0                 '' X Position Of Current Character
		cy = (loop1\16)/16.0                     '' Y Position Of Current Character

		glNewList gbase+loop1, GL_COMPILE        '' Start Building A List
		glBegin GL_QUADS                         '' Use A Quad For Each Character
			glTexCoord2f cx, 1-cy-0.0625         '' Texture Coord (Bottom Left)
			glVertex2i 0, 16                     '' Vertex Coord (Bottom Left)
			glTexCoord2f cx+0.0625, 1-cy-0.0625  '' Texture Coord (Bottom Right)
			glVertex2i 16, 16                    '' Vertex Coord (Bottom Right)
			glTexCoord2f cx+0.0625, 1-cy         '' Texture Coord (Top Right)
			glVertex2i 16, 0                     '' Vertex Coord (Top Right)
			glTexCoord2f cx, 1-cy                '' Texture Coord (Top Left)
			glVertex2i 0, 0                      '' Vertex Coord (Top Left)
		glEnd                                    '' Done Building Our Quad (Character)
		glTranslated 14, 0, 0                    '' Move To The Right Of The Character
		glEndList                                '' Done Building The Display List
	next                    
end sub

'' Where The Printing Happens
sub glPrintIcon cdecl (byval x as integer, byval y as integer, byval gset as integer, byref icon as byte, byval ang as integer)
	dim text as string * 256                '' Holds Our String
	dim ap as any ptr                       '' Pointer To List Of Arguments
    
    'if icon=0 then icon=asc(".")
    text=chr(icon)
	if gset>1 then                          '' Did User Choose An Invalid Character Set?
		gset=1                              '' If So, Select Set 1 (Italic)
	end if
    glBindTexture GL_TEXTURE_2D, texture(13) 
	glEnable(GL_TEXTURE_2D)                 '' Enable Texture Mapping
	glLoadIdentity()                        '' Reset The Modelview Matrix
	                  '' Position The Text (0,0 - Bottom Left)
    glTranslated(x,y,0)
    if (ang<>0) then
     glRotated(ang,0,0,1)
	 if ang=90 then glTranslated(-16,0,0)
     if ang=180 then glTranslated(-16,16,0)
     if ang=270 then glTranslated(0,16,0)
    end if
    glListBase(gbase-32+(128*gset))         '' Choose The Font Set (0 or 1)

	glScalef 1., -1., 1.0                  '' Make The Text 2X Taller

	glCallLists(strlen(text),GL_UNSIGNED_BYTE, strptr(text)) '' Write The Text To The Screen
	' glDisable(GL_TEXTURE_2D)                '' Disable Texture Mapping
end sub


'------------------------------------------------------------------------
sub BuildFont()                                  '' Build Our Font Display List
	dim loop1 as integer
	dim cx as single                             '' Holds Our X Character Coord
	dim cy as single                             '' Holds Our Y Character Coord

	gbase = glGenLists(256)                      '' Creating 256 Display Lists
	glBindTexture GL_TEXTURE_2D, texture(12)      '' Select Our Font Texture
	for loop1 = 0 to 255                         '' Loop Through All 256 Lists

		cx = (loop1 mod 16)/16.0                 '' X Position Of Current Character
		cy = (loop1\16)/16.0                     '' Y Position Of Current Character

		glNewList gbase+loop1, GL_COMPILE        '' Start Building A List
		glBegin GL_QUADS                         '' Use A Quad For Each Character
			glTexCoord2f cx, 1-cy-0.0625         '' Texture Coord (Bottom Left)
			glVertex2i 0, 16                     '' Vertex Coord (Bottom Left)
			glTexCoord2f cx+0.0625, 1-cy-0.0625  '' Texture Coord (Bottom Right)
			glVertex2i 16, 16                    '' Vertex Coord (Bottom Right)
			glTexCoord2f cx+0.0625, 1-cy         '' Texture Coord (Top Right)
			glVertex2i 16, 0                     '' Vertex Coord (Top Right)
			glTexCoord2f cx, 1-cy                '' Texture Coord (Top Left)
			glVertex2i 0, 0                      '' Vertex Coord (Top Left)
		glEnd                                    '' Done Building Our Quad (Character)
		glTranslated 14, 0, 0                    '' Move To The Right Of The Character
		glEndList                                '' Done Building The Display List
	next                    
end sub
'------------------------------------------------------------------------
'' Where The Printing Happens
sub glPrint cdecl (byval x as integer, byval y as integer, byval gset as integer, byref fmt as string, ...)
	dim text as string * 256                '' Holds Our String
	dim ap as any ptr                       '' Pointer To List Of Arguments

	if len(fmt) = 0 then                    '' If There's No Text
		exit sub                            '' Do Nothing
	end if

	ap = va_first()                         '' get pointer to first arg
	vsprintf(text, fmt, ap)                 '' And Converts Symbols To Actual Numbers

	if gset>1 then                          '' Did User Choose An Invalid Character Set?
		gset=1                              '' If So, Select Set 1 (Italic)
	end if
    glBindTexture GL_TEXTURE_2D, texture(12) 
	glEnable(GL_TEXTURE_2D)                 '' Enable Texture Mapping
	glLoadIdentity()                        '' Reset The Modelview Matrix
	glTranslated(x,y,0)                     '' Position The Text (0,0 - Bottom Left)
	glListBase(gbase-32+(128*gset))         '' Choose The Font Set (0 or 1)

	glScalef 1.0, -1.0, 1.0                  '' Make The Text 2X Taller

	glCallLists(strlen(text),GL_UNSIGNED_BYTE, strptr(text)) '' Write The Text To The Screen
	' glDisable(GL_TEXTURE_2D)                '' Disable Texture Mapping
end sub

