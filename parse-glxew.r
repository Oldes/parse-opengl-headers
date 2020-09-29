Rebol []

header-file: %include/glxew.h
output-file: %glx.reds

do %common-init.r

echo output-file


print {Red/System [
	Title:   "Red/System OpenGL binding - GLX 1.3 and above API and GLX extension interfaces"
	Author:  "Oldes"
	File: 	 %glx.reds
	Rights:  "Copyright (C) 2017 David 'Oldes' Oliva. All rights reserved."
	License: "BSD-3 - https://github.com/red/red/blob/master/BSD-3-License.txt"
	Note: {
This source was made from glxew.h file, which contains these copyrights:

/*
** The OpenGL Extension Wrangler Library
** Copyright (C) 2008-2017, Nigel Stewart <nigels[]users sourceforge net>
** Copyright (C) 2002-2008, Milan Ikits <milan ikits[]ieee org>
** Copyright (C) 2002-2008, Marcelo E. Magallon <mmagallo[]debian org>
** Copyright (C) 2002, Lev Povalahev
** All rights reserved.
** 
** Redistribution and use in source and binary forms, with or without 
** modification, are permitted provided that the following conditions are met:
** 
** * Redistributions of source code must retain the above copyright notice, 
**   this list of conditions and the following disclaimer.
** * Redistributions in binary form must reproduce the above copyright notice, 
**   this list of conditions and the following disclaimer in the documentation 
**   and/or other materials provided with the distribution.
** * The name of the author may be used to endorse or promote products 
**   derived from this software without specific prior written permission.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
** AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
** ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
** LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
** CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
** SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
** INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
** CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
** ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
** THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
 * Mesa 3-D graphics library
 * Version:  7.0
 *
 * Copyright (C) 1999-2007  Brian Paul   All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * BRIAN PAUL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
** Copyright (c) 2007 The Khronos Group Inc.
** 
** Permission is hereby granted, free of charge, to any person obtaining a
** copy of this software and/or associated documentation files (the
** "Materials"), to deal in the Materials without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Materials, and to
** permit persons to whom the Materials are furnished to do so, subject to
** the following conditions:
** 
** The above copyright notice and this permission notice shall be included
** in all copies or substantial portions of the Materials.
** 
** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
*/
	}
]

;--> content of this file makes sense only on Linux <--

#if OS = 'Linux  [

;@@ some of these should be in shared definitions for Linux

#define GLXContext!                  handle!
#define GLXContextID!                handle!
#define GLXDrawable!                 handle!
#define GLXFBConfig!                 handle!
#define GLXFBConfigSGIX!             handle!
#define GLXPbuffer!                  handle!
#define GLXPixmap!                   handle!
#define GLXVideoCaptureDeviceNV!     handle!
#define GLXVideoDeviceNV!            handle!
#define GLXWindow!                   handle!
#define XColormap!                   handle!
#define XPixmap!                     handle!
#define XStatus!                     handle!
#define XWindow!                     handle!
#define XFont!                       handle!
#define XDisplay!                    handle!

#define GLXFBConfig-ptr!             handle-ptr!
#define GLXFBConfigSGIX-ptr!         handle-ptr!
#define GLXHyperpipeConfigSGIX-ptr!  handle-ptr!
#define GLXHyperpipeNetworkSGIX-ptr! handle-ptr!
#define GLXVideoCaptureDeviceNV-ptr! handle-ptr!
#define GLXVideoDeviceNV-ptr!        handle-ptr!
#define XVisualInfo-ptr!             handle-ptr!

}


parse-callback-args: func[raw-args [string!] /local n][
	clear args
	if "void" = raw-args [return args]
	n: 1
	foreach type parse/all raw-args "," [
		repend args [
			join "arg" n
			get-red-type type
		]
		n: n + 1
	]
	args
]
parse-args: func[raw-args [string!] /local n p p2 type name][
	clear args
	if "void" = raw-args [return args]
	foreach val parse/all raw-args "," [
		p:  find/last val #" "
		p2: find/last val #"*"
		either all [not none? p2 (length? p2) < (length? p)][
			name: copy next p2
			type: copy/part val next p2
		][
			name: copy next p
			type: copy/part val p
		]
		repend args [name get-red-type type]
	]
	args
]

pad: func[value len][
	value: either block? value [rejoin value][copy value]
	len: len - length? value
	if len > 0 [
		insert/dup tail value #" " len
	]
	value
]

pad-en: func[en /local s][pad en 43]

defines: make hash! 1024

title: none
in-import?: false
brief: brief-desc: none

in-extensions?: false
title-ext: none

add-title: does [
	print "^/^/;-------------------------------------------"
	print [";--" title]
	print ";-------------------------------------------^/"
	title: none
]

log: copy ""
ext-files: []

parse/all read header-file [
	thru {extern "C" ^{}
	any [
		"/* ---" some #"-" any #" " copy title some ch_name  (
			if all [
				parse title ["GLX_" copy new-title-ext to #"_" to end]
			][
				add-title
			]
		)
		|
		"^/#define" some ch_space copy def to #"^/" (
			if in-import? [ print "]]^/" in-import?: false ]
			parse def [ copy name to #" " def: opt [
				to "0x" s: (e: remove/part s 2) e: some ch_hexdigit s: (
					tmp: uppercase copy/part e s
					if 0 <> ((length? tmp) // 2) [
						insert tmp #"0"
					]
					append tmp #"h"
					insert remove/part e s tmp
				)
			]]
			if not any [
				parse name ["glew" to end]
				;name = title
			][
			;append log rejoin ["^/??? " mold def]
				case [
					find def "GLXEW_GET_FUN(" [
					
						if in-import? [ print "]]^/" in-import?: false ]
						either tmp: find procedures rejoin ["PFN" name "PROC"][
							ret: tmp/2
							raw-args: tmp/3

							if title [add-title]
							;print [";@@" ret #"(" raw-args ");"]
							print rejoin [name "!: alias function! ["]
							args: parse-args raw-args
							n: 7
							foreach [name type] args [
								if n < length? name [n: length? name]
							]

							foreach [name type] args [
								either all [
									parse name [copy name to #"[" copy arg-a to end]
									none? find type "pointer!"
									none? find type " value"
								][
									print rejoin [ "^-" pad name n " [ pointer! [" type "] ] ;@@" arg-a ]
								][	print rejoin [ "^-" pad name n " [ " type " ]"	]]
							]
							if ret <> "void" [
								ret: get-red-type ret
								print rejoin ["^-return: [ " ret " ]"]
							]
							print "]"
							num-prc: num-prc + 1
						][
							append log rejoin ["^/!!! Cannot find proc for:" mold name]
						]
					]
					find def "GLXEW_GET_" [] ;nothing?
					true [
						replace def "/*" ";"
						replace/all def " */" ""

						if title [add-title]

						either tmp: find/skip defines name 2 [
							if tmp/2 <> def [
								print ["!!! Redefinition of:" mold name "from:" def "to:" tmp/2]
								halt
							]
							prin #";"
						][
							repend defines [name def]
						]

						print ["#define " pad-en name def]
						num-def: num-def + 1
					]
				]
			]
		)
		|
		"^/typedef" some #" "
			copy ret [any ["const" some #" " | "unsigned" some #" "] [some ch_name any #" " some #"*" | some ch_name]]
			some #" " "( *" any #" "
			copy name some ch_name any #" "#")"
			thru #"(" copy raw-args to ");" (
				repend procedures [name ret raw-args]
			) 
		|
		"^/extern" some #" "
			copy ret [opt ["const" some #" "] [some ch_name any #" " some #"*" | some ch_name]]
			any #" " copy name some ch_name
			thru #"(" copy raw-args to ");" (

				unless in-import? [
					print "^/#import [ GL_LIBRARY GL_CALLING ["
					in-import?: true
				]
				parse-args raw-args
				;print rejoin ["^-;@@ " ret #" " name #"(" raw-args #")"]
				either 2 < length? args [
					print rejoin [#"^-" name ": " mold name "["]
					foreach [arg type] args [
						either all [
							parse arg [copy arg to #"[" copy arg-a to end]
							none? find type "pointer!" 
							none? find type " value"
						][
							print rejoin [ "^-^-" pad arg 11 " [ pointer! [" type "] ] ;@@" arg-a ]
						][
							print rejoin [ "^-^-" pad arg 11 " [ " type " ]"  ]
						]
					]
					if ret <> "void" [
						ret: get-red-type ret
						print rejoin ["^-^-return:     [ " ret " ]"]
					]
					print "^-]"
				][
					prin rejoin [#"^-" name ": " mold name " [ "]
					foreach [arg type] args [
						either all [
							parse arg [copy arg to #"[" copy arg-a to end]
							none? find type "pointer!" 
							none? find type " value"
						][
							prin rejoin [ arg " [pointer! [" type "]] " ]
						][
							prin rejoin [ arg " [" type "] "  ]
						]
					]
					if ret <> "void" [
						ret: get-red-type ret
						prin rejoin ["return: [" ret "] "]
					]
					print "]"
				]
				num-imp: num-imp + 1
			)
		| 1 skip
	]
]

echo none

;removing unnwanted result code:
if f: find raw: read output-file "#define  GLXEW_FUN_EXPORT" [
	clear f
]

append raw "^/] ;end of #if OS = 'Linux"
write output-file raw



err: sort unique err
new-line/all err true

foreach type err [
	print rejoin [
		pad-en mold type
		either #"*" = last type [{"pointer! [integer!]"}][{""}]
	]
]

print rejoin [
	"^/; defines: " num-def
	"^/; procedures: " num-prc
	"^/; imports: " num-imp
]
print log
probe err
