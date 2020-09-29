Rebol []

header-file: %include/glfw3.h
output-file: %glfw3.reds

do %common-init.r

ch_hexdigit: charset "0123456789ABCDEF"

echo output-file

print {Red/System [
	Title:   "Red/System glfw3 binding - A library for OpenGL/Vulkan, window and input"
	Author:  "Oldes"
	File: 	 %glfw3.reds
	Rights:  "Copyright (C) 2017 David 'Oldes' Oliva. All rights reserved."
	License: "BSD-3 - https://github.com/red/red/blob/master/BSD-3-License.txt"
	Note: {
/*************************************************************************
 * GLFW 3.2 - www.glfw.org
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

#include %../os/definitions.reds ;common aliases and defines

#switch OS [
	Windows   [
		#define GLFW3_LIBRARY "glfw3.dll"
		#define GLFW3_CALLING cdecl
	]
	macOS     [;@@ not tested!
		#define GLFW3_LIBRARY "glfw3.dylib"
		#define GLFW3_CALLING cdecl
	] 
	#default  [;@@ not tested!
		#define GLFW3_LIBRARY "glfw3.so"
		#define GLFW3_CALLING cdecl
	]
]

#define GLFWmonitor-ptr! int-ptr!

}

callback-types: [
    "GLFWerrorfun!"
    "GLFWmonitorfun!"
    "GLFWwindowposfun!"
    "GLFWwindowsizefun!"
    "GLFWwindowclosefun!"
    "GLFWwindowrefreshfun!"
    "GLFWwindowfocusfun!"
    "GLFWwindowiconifyfun!"
    "GLFWframebuffersizefun!"
    "GLFWkeyfun!"
    "GLFWcharfun!"
    "GLFWcharmodsfun!"
    "GLFWmousebuttonfun!"
    "GLFWcursorposfun!"
    "GLFWcursorenterfun!"
    "GLFWscrollfun!"
    "GLFWdropfun!"
    "GLFWglproc!"
    "GLFWvkproc!"
    "GLFWjoystickfun!"
]


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
parse-args: func[raw-args [string!] /local n][
	clear args
	if "void" = raw-args [return args]
	foreach val parse/all raw-args "," [
		p: find/last val " "
		repend args [
			copy next p
			get-red-type copy/part val p
		]
	]
	args
]

in-import?: false

parse/all read header-file [
	thru {/*! @name GLFW version macros}
	any [
		"/*! @defgroup " thru #" " copy title to lf 1 skip (
			print "^/^/;-------------------------------------------"
			print [";--" title]
			print ";-------------------------------------------^/^/"
		)
		|
		"/*! @brief " copy tmp thru {*/} (
			brief: none
			brief-desc: none
			parse/all tmp [
				[  copy brief to {^/ *^/} thru {*^/}
				 | copy brief to {^/ */}
				]
				copy brief-desc to end
			]
			replace/all brief "^/ *  " " "
			insert brief-desc #"^/"
			replace brief-desc "^/ */" ""
			replace/all brief-desc "^/ *" "^/; "
			trim/head/tail brief
			trim/head/tail brief-desc
			;parse brief-desc [";   " brief copy brief-desc to end]
			if brief-desc [trim/head/tail brief-desc]
			if any [
				all [string? brief-desc empty? brief-desc]
				brief = brief-desc
			] [brief-desc: none]
			;print [tab brief]
			;probe brief-desc
		)
		|
		"^/#define " copy def to #"^/" (
			if in-import? [ print "]]^/" in-import?: false ]
			if brief [ print ["^/;- " brief] ]
			if brief-desc [ print brief-desc ]
			parse def [ to "0x" s: (e: insert remove/part s 2 " ") e: some ch_hexdigit s: (insert s #"h")]
			replace def "/*" ";"
			replace/all def " */" ""
			print ["#define " def]
			
			brief: brief-desc: none
		)
		|
		"/****************************" thru "^/ * " copy header to "^/ **********" (
			if in-import? [ print "]]^/" in-import?: false ]
			print "^/^/;-=================================================="
			print [";-" header ]
			print ";-==================================================^/^/"
		)
		|
		"typedef struct " copy struct some ch_name opt ch_space [
			#"{" copy def to "^/}" (
				if in-import? [ print "]]^/" in-import?: false ]
				trim/head/tail def
				print rejoin [lf struct "!: alias struct! ["]
				if brief [ prin [";- " brief] ]
				if brief-desc [ print brief-desc ]
				;probe def

				parse/all def [
					some [
						any ch_space [
							"/*! " copy comm to lf (print ["^-;" comm])
							| "*/" opt #" "
							| s:
								[
									"char" any #" " "const" any #" "
									|
									"unsigned" any #" "
									opt ["int" | "char" | "short"] any #" "
									|
									opt "const" any #" "
									opt ["unsigned long" | "unsigned"] any #" "
									some ch_name any #" "
								] 
								any #"*"
								e: (type: trim/tail copy/part s e)
								any ch_space
								copy name some ch_name #";" (
									print [#"^-" name "[" get-red-type type "]"]
								)
						]
					]
				]
				print "]"
			)
			|
			(
				prin rejoin ["#define " struct "!^-[pointer! [integer!]]"]
				either brief [
					print ["^-;" brief]
					brief: none
				][ prin lf]
			)
		]
		|
		"typedef void (*" any #" " copy callback some ch_name any #" " ")" any #" " #"(" copy raw-args to ");"
		(
			if in-import? [
				print "]]^/"
				in-import?: false
			]
			parse-callback-args raw-args
			prin rejoin ["#define " callback "! [ function! ["]
			either brief [ print ["^/;- " brief] ] [ prin lf ]
			if brief-desc [ print brief-desc ]
			n: 7 foreach [name type] args [ if n < length? name [n: length? name] ]
			foreach [arg type] args [
				print rejoin [ #"^-" pad arg n " [ " type " ]"  ]
			]
			print "]]"
			brief: brief-desc: none
		)
		|
		"GLFWAPI" any #" " opt ["const" some #" "] copy ret to #" " any #" " copy name some ch_name #"(" copy raw-args to ");" (
			unless in-import? [
				print "^/#import [ GLFW3_LIBRARY GLFW3_CALLING ["
				in-import?: true
			]
			;print ["!!!!!!!!!!!!!!!!!! " mold ret mold name]
			parse-args raw-args
			print rejoin ["^-;@@ " ret #" " name #"(" raw-args #")"]
			prin rejoin [#"^-" name ": " mold name "["]
			either brief [ print ["^/^-  ;- " brief] ] [ prin lf ]
			if brief-desc [
				replace/all brief-desc ";   " "^-  ; "
				replace/all brief-desc "^/; " "^/^-  ; "
				print brief-desc
			]
			n: 7 foreach [name type] args [ if n < length? name [n: length? name] ]
			foreach [arg type] args [
				print rejoin [ "^-^-" pad arg n " [ " type " ]"  ]
			]
			if ret <> "void" [
				ret: get-red-type ret
				if find callback-types ret [ret: "pointer! [integer!]"]
				print rejoin ["^-^-return: [ " ret " ]"]
			]
			print "^-]"
		)
		| 1 skip
	]
]

echo none


err: unique err
new-line/all err true

probe err
