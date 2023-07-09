#ifndef _PNG_LOAD_BI_
#define _PNG_LOAD_BI_

#ifndef PNG_STATICZ
#inclib "z"
#endif

#ifdef PNG_DEBUG
#include once "fbmld.bi"
#endif
#inclib "fbpng"

enum png_target_e
	PNG_TARGET_BAD
	PNG_TARGET_FBOLD
	PNG_TARGET_FBNEW
	PNG_TARGET_OPENGL
end enum

Declare Function png_load Cdecl Alias "png_load" _
        ( _
                Byref filename As String, _
                Byval target   As png_target_e = PNG_TARGET_FBNEW _
        ) As Any Ptr

declare function png_load_mem cdecl alias "png_load_mem" _
	( _
		byval buffer     as any ptr, _
		byval buffer_len as integer, _
		byval target     as png_target_e = PNG_TARGET_FBNEW _
	) as any ptr

declare function png_save cdecl alias "png_save" _
	( _
		byref filename as string, _
		byval img      as any ptr _
	) as integer

declare sub png_dimensions cdecl alias "png_dimensions" _
	( _
		byref filename as string, _
		byref w        as uinteger, _
		byref h        as uinteger _
	)

declare sub png_dimensions_mem cdecl alias "png_dimensions_mem" _
	( _
		byval buffer as any ptr, _
		byref w      as uinteger, _
		byref h      as uinteger _
	)

#endif '_PNG_LOAD_BI_
