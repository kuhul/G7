declare sub countElements(byval filename as string, byref v as integer,_
                          byref vt as integer, byref vn as integer,_
                          byref f as integer, byref isSmooth as integer)
declare function firstWord(byval s as string) as string
declare sub parseVertex(byval s as string, byref vx as single, byref vy as single,_
                      byref vz as single)
declare sub parseTextureCoord(byval s as string, byref vtx as single,_
                           byref vty as single)
declare sub parseNormal(byval s as string, byref vnx as single, byref vny as single,_
                      byref vnz as single)
declare sub parseFace(byval s as string, byval vnumber as integer,_
                    byref vindex as integer, byref vtindex as integer,_
                    byref vnindex as integer)
declare sub readString(byval f as integer, byref Buffer as string)



sub readString(byval f as integer, byref Buffer as string)
  do
    line input #f, Buffer
  loop while (left(Buffer,1) = "/") or (Buffer = "")
end sub

sub countElements(byval filename as string, byref v as integer, byref vt as integer,_
                  byref vn as integer, byref f as integer, byref isSmooth as integer)
    ' Counts the elements of an object stored in an obj file
    ' v -> number of vertices
    ' vt -> number of texture coords
    ' vn -> number of normals
    ' f -> number of faces
    dim fp as integer
    dim oneline as string
    dim elementType as string
    
    v = 0
    vt = 0
    vn = 0
    f = 0
    fp = freefile
    
    if (open(filename, for input, as #fp) <> 0) then end 1
    do
        readString(fp, oneline)
        elementType = firstWord(oneline)
        if (elementType = "v") then
            v = v + 1
        elseif (elementType = "vt") then
            vt = vt + 1
        elseif (elementType = "vn") then
            vn = vn + 1
        elseif (elementType = "f") then
            f = f + 1
        elseif (elementType = "s") then
            if (mid(oneline, 3) <> "off") then
                isSmooth = 1
            else
                isSmooth = 0
            end if
        end if
    loop until (eof(fp))
end sub

sub parseVertex(byval s as string, byref vx as single, byref vy as single,_
                byref vz as single)
    dim as integer parser
    parser = instr(s, " ") + 1
    vx = val(mid(s, parser))
    parser = instr(parser, s, " ") + 1
    vy = val(mid(s, parser))
    parser = instr(parser, s, " ") + 1
    vz = val(mid(s, parser)) 
end sub

sub parseTextureCoord(byval s as string, byref vtx as single, byref vty as single)
    dim as integer parser
    parser = instr(s, " ") + 1
    vtx = val(mid(s, parser))
    parser = instr(parser, s, " ") + 1
    vty = val(mid(s, parser))
end sub

sub parseNormal(byval s as string, byref vnx as single, byref vny as single,_
              byref vnz as single)
    dim as integer parser
    dim as single nrm
    parser = instr(s, " ") + 1
    vnx = val(mid(s, parser))
    parser = instr(parser, s, " ") + 1
    vny = val(mid(s, parser))
    parser = instr(parser, s, " ") + 1
    vnz = val(mid(s, parser))
    
    nrm = sqr(vnx*vnx + vny*vny + vnz*vnz)
    if (nrm<>1) then vnx=vnx/nrm : vny=vny/nrm : vnz=vnz/nrm
end sub

sub parseFace(byval s as string, byval vnumber as integer, byref vindex as integer,_
            byref vtindex as integer, byref vnindex as integer)
    dim as integer parser
    parser = 1
    for i as integer = 1 to vnumber
        parser = instr(parser, s, " ") + 1
    next i
    vindex = val(mid(s, parser))
    parser = instr(parser, s, "/") + 1
    vtindex = val(mid(s, parser))
    parser = instr(parser, s, "/") + 1
    vnindex = val(mid(s, parser))
end sub

function firstWord(byval s as string) as string
    ' Gibt alles vor dem ersten Leerzeichen zurück
    dim splitAt as integer
    dim word as string
    splitAt = instr(s, " ")
    word = left(s, splitAt-1)
    return word
end function