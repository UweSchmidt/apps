#!/bin/bash
# the next line restarts using wish \
exec wish "$0" "$@"

package require tdom
package require BLT
package require Img

namespace import blt::tabnotebook

set theArchivePath [lindex $argv 0]
set testDir "/home/uwe/haskell/photo2/trunk/test"

if {$tcl_interactive} {
    cd $testDir
    set theArchivePath "archive2.xml"
}

if {"$theArchivePath" == ""} {
    set theArchivePath  "archive2.xml"
}

if {! [file readable $theArchivePath]} {
    puts stderr "archive \"$theArchivePath\" not found"
    exit 1
}

set theArchive ""
array set theAlbums {}

array set state {
    message ""
    after ""
    columns 8
    iconwidth 160
    iwpad 10
    ihpad 10
    iconheight 160
    iconbase "100x100"
    outline 0
}

if {"[pwd]" == "$testDir"} {
    array set state {
	iconwidth 110
	iconheight 110
	iconbase "100x100"
    }
} else {
    array set state {
	iconwidth 170
	iconheight 170
	iconbase "160x160"
    }
}

proc getXML f {
    set c [open $f r]
    set r [read $c]
    close $c
    return [dom parse -simple $r]
}

proc putXML {d ref} {
    regsub -all -- {[^/]} $ref {} p
    regsub -all -- {/} $p {../} p1
    set path "\"${p1}config/archive.dtd\""

    dom setResultEncoding iso8859-1
    set r [$d asXML -indent 2 -escapeNonASCII]

    regsub "\n" $r "\n<!DOCTYPE album SYSTEM $path>\n" r
    return $r
}

proc initArchive {} {
    global theArchive theArchivePath
    set theArchive [getXML $theArchivePath]
    statusline "archive $theArchivePath read"
}

proc finArchive {} {
    global theArchive theArchivePath theAlbums
    if {"$theArchive" != ""} {
	$theArchive delete
	set theArchive ""
    }
    foreach a [array names theAlbums] {
	finAlbum $a
    }
    statusline "archive $theArchivePath cleared"
    set theArchivePath ""
}

proc init {} {
    global theArchivePath

    initGUI
    initArchive
    statusline "archive $theArchivePath initialized"
    initAlbum [getRootAlbum]
}

proc fin    {} {
    finGUI
    finArchive
}

proc reinit {} { fin ; init }
proc quit   {} {
    global tcl_interactive
    fin
    if {! $tcl_interactive} {
	destroy .
	exit 0
    } else {
	statusline "quit simulated"
    }
}

proc isChangedAlbum ref {
    global theAlbums
    lindex $theAlbums($ref) 1
}

proc changedAlbum {ref {changed 1}} {
    global theAlbums
    set theAlbums($ref) [lreplace $theAlbums($ref) 1 1 $changed]
    return $changed
}

proc initAlbum ref {
    global theAlbums
    if {[info exists theAlbums($ref)]} {
	statusline "album $ref already loaded"
    } else {
	set d [getXML "$ref.xml"]
	set changed 0
	set theAlbums($ref) [list $d $changed]
	statusline "album $ref loaded"
	createTab $ref
    }
}

proc changeAlbum {album ids} {
    statusline "changeAlbum $album $ids"
    array set nmap {}
    set pns [getPics $album]
    set r [getRoot $album]

    # remove all pics from doc
    foreach id [getPicNames $album] n $pns {
	set nmap($id) $n
	$r removeChild $n
    }
    #insert all pics in right sequence into doc
    foreach id $ids {
	$r appendChild $nmap($id)
    }

    puts stderr [$r asXML]

    changedAlbum $album 1
}

proc saveAlbum ref {
    global theAlbums
    if {! [info exists theAlbums($ref)]} {
	statusline "album $ref not loaded"
	return
    }
    foreach {d changed} $theAlbums($ref) {break}

    if {! $changed} {
	statusline "album $ref not changed"
	return
    }
    set changed [saveAlbumDialog $ref]
    if {! $changed} {
	statusline "album changes in $ref not saved"
	return
    }
	
    file rename -force -- "$ref.xml" "$ref.xml~"
    set f [open "$ref.xml" w]
    puts $f [putXML $d $ref]
    close $f
    statusline "album $ref saved"
    changedAlbum $ref 0
}

proc finAlbum ref {
    global theAlbums
    if {! [info exists theAlbums($ref)]} {
	statusline "album $ref not loaded"
    } else {
	set d [lindex $theAlbums($ref) 0]
	saveAlbum $ref
	$d delete
	unset theAlbums($ref)
	statusline "album $ref cleared"
    }
}

proc reloadAlbum ref {
    changedAlbum $ref 0
    closeAlbum $ref
    initAlbum $ref
}

proc closeAlbum ref {
    deleteTab $ref
    finAlbum $ref
}

proc reloadCurrAlbum {} {
    global currAlbum
    if {"$currAlbum" == ""} return
    reloadAlbum $currAlbum
}

proc closeCurrAlbum {} {
    global currAlbum
    if {"$currAlbum" == ""} return
    storeChangesAlbum $currAlbum
    closeAlbum $currAlbum
    set currAlbum ""
}

proc saveCurrAlbum {} {
    global currAlbum
    if {"$currAlbum" == ""} return
    storeChangesAlbum $currAlbum
    saveAlbum $currAlbum
}

proc openCurrSubAlbum {} {
    global currAlbum
    if {"$currAlbum" == ""} return

    global currPic
    if {"$currPic" == ""} return

    if {! [isSubAlbum $currAlbum $currPic]} return

    initAlbum [file rootname [getSubAlbumRef $currAlbum $currPic]]
    
}

proc openCurrPic {w x y} {
    setCurrPic $w $x $y
    openCurrSubAlbum
}

proc deleteCurrPic {} {
    global currAlbum
    if {"$currAlbum" == ""} return

    global currPic
    if {"$currPic" == ""} return

    deletePic $currAlbum $currPic
}

#------------------------------------------------------------

proc getRootAlbum {} {
    global theArchive
    file rootname [[[$theArchive documentElement] selectNodes "/*/*"] getAttribute href]
}

proc getRoot     album {
    global theAlbums
    set d [lindex $theAlbums($album) 0]
    $d documentElement
}

proc getPics          album { filter isAlbumOrPic [[getRoot $album] childNodes] }
proc getSubAlbums     album { filter isAlbum [[getRoot $album] childNodes] }
proc getPicNames      album { map {getAttr "id"} [getPics      $album] }
proc getSubAlbumNames album { map {getAttr "id"} [getSubAlbums $album] }

proc getIcons         album {
    global state
    map [list iconPath $album $state(iconbase)] \
	[map {getAttr0 "id" ""} \
	     [getPics $album]]
}

proc iconPath {album base pic} {
    set l [file split $album]
    set l [lrange $l 1 end]
    set l [linsert $l 0 $base]
    set l [linsert $l end $pic]
    set res [eval file join $l]
    return "${res}.jpg"
}

proc getAlbumId       album { getAttr "id" [getRoot $album] }

proc getSubAlbumRef   {album id} {
    foreach n [filter [list hasId $id] [getSubAlbums $album]] {
	return [getAttr "href" $n]
    }
}

#------------------------------------------------------------

proc isSubAlbum {album id} {
    set al [getSubAlbumNames $album]
    expr {[lsearch $al $id] != -1}
}

proc isNull s    { expr {"$s" == ""} }
proc isNotNull s { expr {"$s" != ""} }

proc isAlbumOrPic n { set nn [$n nodeName]; expr {"$nn" == "album" || "$nn" == "picture"} }
proc isPic        n { set nn [$n nodeName]; expr {"$nn" == "picture"} }
proc isAlbum      n { set nn [$n nodeName]; expr {"$nn" == "album"} }
proc isCopy       n { set nn [$n nodeName]; expr {"$nn" == "copy"} }

proc hasId   {id n} { expr {"[$n getAttribute id]" == "$id"} }

proc getAttr {a n} { $n getAttribute $a }

proc getAttr0 {a d n} {
    if {"$n" == ""} {
	set r $d
    } elseif {[$n hasAttribute $a]} {
	set r [getAttr $a $n]
    } else {
	set r $d
    }
    return $r
}

proc getChildren n { $n childNodes }

proc getCopyChildren {res n} {
    set cl [filter isCopy [getChildren $n]]
    proc hasRes {res n} {
	set r [$n getAttribute "base"]
	expr {"$r" == "$res"}
    }
    conc [filter [list hasRes $res] $cl]
}

#------------------------------------------------------------

proc conc args {
    set r {}
    foreach a $args {
	eval lappend r $a
    }
    return $r
}

proc map {cmd l} {
    set r {}
    foreach n $l {
	lappend r [eval $cmd [list $n]]
    }
    return $r
}

proc filter {pred l} {
    set r {}
    foreach n $l {
	if {[eval $pred $n]} {
	    lappend r $n
	}
    }
    return $r
}

#------------------------------------------------------------

proc getxmlns d {
    [$d documentElement] namespaceURI
}

#------------------------------------------------------------

proc initGUI {} {
    finGUI

    createWidgets

    wm protocol . WM_DELETE_WINDOW "quit"
    # bind all <Control-KeyPress-c>  "fin"

    picsPerRow
    statusline "GUI initialized"
}

proc finGUI {} {
    global state
    foreach n [array names state "tabs,*"] {
	catch {
	    deleteTab [lindex [split $n ","] 1]
	}
    }
    catch {destroy .contents}
    catch {destroy .statusline}
    catch {destroy .menu}
}

#------------------------------------------------------------

proc createWidgets {} {
    wm title . "Photo2 Album Editor"
    wm iconname . "edit2"

    frame .menu -relief raised
    pack .menu -side top -fill x

    label .menu.task

    menubutton .menu.program -text "Album" -menu .menu.program.m -underline 0
    menu .menu.program.m

    .menu.program.m add command \
        -label "reinitialize" \
        -command "reinit" \
        -underline 2

    .menu.program.m add command \
        -label "reload album" \
        -command "reloadCurrAlbum" \
        -underline 0 \
	-accelerator Alt+r

    .menu.program.m add command \
        -label "open subalbum" \
        -command "openCurrSubAlbum" \
        -underline 0 \
	-accelerator Alt+o

    .menu.program.m add command \
        -label "sort album" \
        -command "sortCurrAlbum" \
        -underline 0 \
	-accelerator Alt+s

    .menu.program.m add command \
        -label "delete picture" \
        -command "deleteCurrPic" \
        -underline 0 \
	-accelerator Delete

    .menu.program.m add command \
        -label "write album" \
        -command "saveCurrAlbum" \
        -underline 0 \
	-accelerator Alt+w

    .menu.program.m add command \
        -label "close album" \
        -command "closeCurrAlbum" \
        -underline 0 \
	-accelerator Alt+c

    .menu.program.m add command \
        -label "Quit" \
        -command "quit"

    pack .menu.program   -side left
    # pack .menu.options   -side left
    # pack .menu.problems  -side left
    # pack .menu.time      -side left
    # pack .menu.task      -side right -fill x

    label .statusline -relief raised -textvariable "state(message)" -anchor w
    pack .statusline -side bottom -fill x

    tabnotebook .contents  -relief sunken -bd 2

    bind .contents <Alt-Key-r>  reloadCurrAlbum
    bind .contents <Alt-Key-o>  openCurrSubAlbum
    bind .contents <Alt-Key-s>  sortCurrAlbum
    bind .contents <Key-Delete> deleteCurrPic
    bind .contents <Alt-Key-d>  deleteCurrPic
    bind .contents <Alt-Key-w>  saveCurrAlbum
    bind .contents <Alt-Key-c>  closeCurrAlbum

    pack .contents -side top -fill both -expand yes

    bind . <Configure> picsPerRow
}

proc id2win id {
    regsub -all {[.]} $id {-} res
    return $res
}

proc createTab album {
    global state
    set label [getAlbumId $album]
    set f [frame ".contents.[id2win $album]"]
    set t [.contents insert end \
	       -window $f -fill both -text $label \
	       -command [list setCurrAlbum $album]]

    set w [canvas $f.c \
	       -scrollregion {0c 0c 1600 5000} -width 1600 -height 5000 \
	       -xscrollcommand "$f.hscroll set" \
	       -yscrollcommand "$f.vscroll set" \
	       -bg \#555555]

    scrollbar $f.vscroll               -command "$w yview"
    scrollbar $f.hscroll -orient horiz -command "$w xview"

    grid $w -in $f  -row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
    grid $f.vscroll -row 0 -column 1 -rowspan 1 -columnspan 1 -sticky news
    grid $f.hscroll -row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
    grid rowconfig    $f 0 -weight 1 -minsize 0
    grid columnconfig $f 0 -weight 1 -minsize 0

    setCurrAlbum $album
    .contents select $t
    focus .contents

    set icons [getIcons    $album]
    set pics  [getPicNames $album]
    set i 0
    foreach ic $icons pn $pics {
	set x0 [expr ( $i % $state(columns) ) * $state(iconwidth)]
	set y0 [expr ( $i / $state(columns) ) * $state(iconheight)]
	set x1 [expr $x0 + $state(iconwidth)  / 2]
	set y1 [expr $y0 + $state(iconheight) / 2]
	set y2 [expr $y1 + $state(iconheight) / 2 - 20]
	set xr0 [expr $x0 + $state(outline)]
	set yr0 [expr $y0 + $state(outline)]
	set xr1 [expr $x0 + $state(iconwidth)  - 2 * $state(outline)]
	set yr1 [expr $y0 + $state(iconheight) - 2 * $state(outline)]

	set it [$w create rectangle $xr0 $yr0 $xr1 $yr1 \
		    -fill \#333333 -outline white \
		    -width $state(outline) \
		    -tags [list [n2t $pn] rect]]

	set im [image create photo \
		    -file $ic \
		   ]
	$w create image $x1 $y1 -anchor center -image $im -tags [list [n2t $pn] image]

	set it [$w create text  $x1 $y2 \
		    -anchor center \
		    -text $pn \
		    -fill white \
		    -tags [list [n2t $pn] text]]
	incr i
    }
    set x [expr {$state(columns) * $state(iconwidth)}]
    set y [expr {( $i + $state(columns) - 1) / $state(columns) * $state(iconheight)}]
	   
    $w configure -scrollregion [list 0 0 $x $y]

    bind $w <1>         [list setCurrPic   $w %x %y]
    bind $w <Double-1>  [list openCurrPic  $w %x %y]
    bind $w <B1-Motion> [list dragCurrPic  $w %x %y]
    bind $w <Enter>	[list setCurrAlbum $album]

    set state(tabs,$album) [list $t $f $label $icons $pics]
}

proc n2t s { return "_$s" }
proc t2n s { string range $s 1 end }

proc storeChangesAlbum album {
    if {! [isChangedAlbum $album]} return

    global state
    foreach {t w label icons pics} $state(tabs,$album) {break}
    changeAlbum $album $pics
}

proc checkChangedAlbum album {
    if {[isChangedAlbum $album]} return

    global state
    foreach {t w label icons pics} $state(tabs,$album) {break}

    # set initPics [map "file rootname" [map "file tail" $icons]]

    set initPics [getPicNames $album]
    if {"$pics" != "$initPics"} {
	changedAlbum $album
    }
}

proc deleteTab album {
    global state
    foreach {t w label icons pics} $state(tabs,$album) {break}

    checkChangedAlbum $album

    set c $w.c
    # destroy photos
    foreach i [$c find withtag image] {
	image delete [$c itemcget $i -image]
    }

    # delete tab
    .contents delete $t
    destroy $w

    unset state(tabs,$album)
}

proc highlightPic   {w p} { configPic yellow  \#880000 $w $p }
proc unhighlightPic {w p} { configPic white   \#333333 $w $p }

proc configPic {fill bg w p} {
    statusline "configPic $fill $bg $w $p"
    foreach i [$w find withtag $p] {
	foreach {p1 t1} [$w itemcget $i -tags] {
	    if {"$t1" == "text"} {
		$w itemconfigure $i -fill $fill
	    }
	    if {"$t1" == "rect"} {
		$w itemconfigure $i -fill $bg
	    }
	    break
	}
    }
    return
}

set currAlbum ""

proc setCurrAlbum a {
    global currAlbum
    set currAlbum $a
    # statusline "current album is $a"
}

set currPic ""
set lastX ""
set lastY ""
set clearCurrPicCmd ""

proc clearCurrPic {} {
    global clearCurrPicCmd currPic
    catch {eval $clearCurrPicCmd}
    set clearCurrPicCmd ""
    set currPic ""
    set lastX ""
    set lastY ""
}

proc setCurrPic {w x y} {
    clearCurrPic

    global clearCurrPicCmd currPic
    global lastX lastY
    set lastX [$w canvasx $x]
    set lastY [$w canvasy $y]
    foreach ci [$w find withtag current] {
	foreach t [$w itemcget $ci -tags] {
	    set currPic [t2n $t]
	    highlightPic $w $t
	    $w raise $t
	    set clearCurrPicCmd [list unhighlightPic $w $t]
	    break
	}
	break
    }
}

proc dragCurrPic {c x y} {
    global currPic
    global lastX lastY
    set x [$c canvasx $x]
    set y [$c canvasy $y]
    $c move [n2t $currPic] [expr {$x-$lastX}] [expr {$y-$lastY}]
    set lastX $x
    set lastY $y
}

proc sortCurrAlbum {} {
    global currAlbum state

    if {"$currAlbum" == ""} return

    set w ".contents.[id2win $currAlbum].c"

    set r {}
    set s $state(tabs,$currAlbum)
    foreach p [lindex $s 4] {
	lappend r [eval list $p [lrange [$w coords [n2t $p]] 0 1]]
    }
    set r [lsort -command comparePicPos $r]

    set r2 {}
    foreach e $r {
	lappend r2 [lindex $e 0]
    }

    set i 0
    foreach e $r2 {
	eval movePic $w $e [ixToPos $i]
	incr i
    }
    set state(tabs,$currAlbum) [lreplace $s 4 4 $r2]

    checkChangedAlbum $currAlbum
}

proc movePic {w p x y} {
    foreach {x1 y1} [$w coords [n2t $p]] {break}
    $w move [n2t $p] [expr $x - $x1] [expr $y - $y1]
    # statusline "movePic $w $p $x $y"
}

proc ixToPos i {
    global state
    set x [expr {$i % $state(columns) * $state(iconwidth)}]
    set y [expr {$i / $state(columns) * $state(iconheight)}]
    list $x $y
}

proc comparePicPos {e1 e2} {
    global state
    # statusline "compare $e1 $e2"
    foreach {p1 x1 y1} $e1 {break}
    foreach {p2 x2 y2} $e2 {break}
    set y1 [expr {( round($y1) + $state(iconheight) / 2) / $state(iconheight)}]
    set y2 [expr {( round($y2) + $state(iconheight) / 2) / $state(iconheight)}]
    if {$y1 < $y2} {return -1}
    if {$y1 == $y2} {
	if {$x1 < $x2} {return -1}
	if {$x1 > $x2} {return 1}
	return 0
    }
    return 1
}

proc deletePic {album pic} {
    global state

    changedAlbum $album 1
    foreach {t w label icons pics} $state(tabs,$album) break

    set c $w.c
    foreach i [$c find withtag [n2t $pic]] {
	catch {image delete [$c itemcget $i -image]}
    }
    $c delete [n2t $pic]

    set i [lsearch $pics $pic]
    set state(tabs,$album) [list $t $w $label $icons [lreplace $pics $i $i]]
}

proc picsPerRow {} {
    global state
    set w [winfo width .]
    set state(columns) [max [expr {$w / $state(iconwidth)}] 3]
}

proc min {x args} {
    foreach x2 $args {
	if {$x2 < $x} {set x $x2}
    }
    return $x
}

proc max {x args} {
    foreach x2 $args {
	if {$x2 > $x} {set x $x2}
    }
    return $x
}

#------------------------------------------------------------

proc statusline text {
    global state
    puts stderr $text
    catch {after cancel $state(after)}
    set state(message) $text
    set state(after) [after 5000 clearstatusline]
    update idletasks
}

proc clearstatusline {} {
    global state
    set state(message) ""
    catch {unset state(after)}
}

#------------------------------------------------------------

proc saveAlbumDialog album {
    tk_dialog .saveDialog "Save Album" "Save album \"$album\"?" info 0 {ignore changes} {save}
}

#------------------------------------------------------------

# let's start

if {! $tcl_interactive} {
    init
}

#------------------------------------------------------------


