Red/System [
	Title:   "Red/System glfw3 binding - A library for OpenGL, window and input"
	Author:  "Oldes"
	File: 	 %glfw3.reds
	Rights:  "Copyright (C) 2017 David 'Oldes' Oliva. All rights reserved."
	License: "BSD-3 - https://github.com/red/red/blob/master/BSD-3-License.txt"
	Note: {
/*************************************************************************
 * GLFW 3.1 - www.glfw.org
 * A library for OpenGL, window and input
 *------------------------------------------------------------------------
 * Copyright (c) 2002-2006 Marcus Geelnard
 * Copyright (c) 2006-2010 Camilla Berglund <elmindreda@elmindreda.org>
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would
 *    be appreciated but is not required.
 *
 * 2. Altered source versions must be plainly marked as such, and must not
 *    be misrepresented as being the original software.
 *
 * 3. This notice may not be removed or altered from any source
 *    distribution.
 *
 *************************************************************************/
	}
]

#switch OS [
	Windows   [ #define GLFW3_LIBRARY "glfw3.dll" #define GLFW3_CALLING stdcall]
	macOS     [ #define GLFW3_LIBRARY "glfw3.dylib" #define GLFW3_CALLING cdecl] ;@@ not tested!
	#default  [ #define GLFW3_LIBRARY "glfw3.so" #define GLFW3_CALLING cdecl] ;@@ not tested!
]

string-ref!:  alias struct! [value [c-string!]]

** Access Error: Cannot open /X/GIT/Red-c-header-parsing/OpenGL/glfw3.h
** Near: parse/all read %glfw3.h [
    thru "/*! @name GLFW version macros" 
    any [
        "/*! @defgroup " thru #" " copy ti...
