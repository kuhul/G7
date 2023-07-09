declare function lerp(byval value as single, byval endValue as single, byval weight as single) as single







function lerp(byval v0 as single, byval v1 as single, byval t as single) as single
    if (abs(v1 - v0) < 0.00001) then
        return v1
    end if
    return (1-t)*v0 + t*v1
end function




