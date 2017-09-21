
package require PWI_Glyph 2

# Start timer
set startTime [clock seconds]

# Reset environment
pw::Application reset
pw::Application clearModified

pw::DomainUnstructured setDefault Algorithm Delaunay
pw::BlockUnstructured setDefault TRexGrowthRate 1.2

# Project Key parameters
global refinementFactor nBufferCell 
global refinementFactor; set refinementFactor 2.0
global nBufferCell; set nBufferCell 1

# Project key files
set cwd [file dirname [info script]]
puts "Current working dictionary: $cwd"
# Load wind Turbine sites
# set siteInfo "sitingPositions_1pos.dat"
set siteInfo "sitingPositions_2pos.dat"
# set siteInfo "sitingPositions_10posScatter.dat"
# set siteInfo "sitingPositions_14posScatter.dat"

set fName   "FinalMesh.pw"

# Load custom procedures
pw::Script source [file join $cwd "ProcLibrary.glf"]
pw::Script source [file join $cwd "Proc_PlaceWindTurbine.glf"]

#-- Load ABL Cartesian mesh
# set ablMesh "ABL_cartBlockV17.pw"
set ablMesh "FlatTerrain_5000x5000x500_10mRes.pw"; #"FlatTerrain_5km500m_V2.pw"
pw::Application load [file join $cwd $ablMesh]

global terrainDB; set terrainDB [pw::DatabaseEntity getByName "Terrain-quilt"]
set blkABL [pw::GridEntity getByName "ABL"]

#-- Load wind turbine mesh

# set wtMesh "WindTower_3.pw"; #"SWiFT_Tower.pw"
set wtMesh "WindTower_5_noTRex.pw"; #"SWiFT_Tower.pw"
# set wtMesh "SWiFT_tower_QuadFull_topOnly_V6.pw"

pw::Application load [file join $cwd $wtMesh]
set wtLayers [list 50 51 52 53 54 55]
#-----------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------------# 
#--Step 0: get or define parameters
#--Step0.1: Get Wind Turbine mesh extents 
global extWT; 
set extWT {{-10 -10 0} {10 10 35}} ;# Wind Turbine Mesh extents
# set extWT [GetMeshExtents $wtLayers]
puts "Wind Turbine Mesh Extents: $extWT"
# pw::Display hideLayer $wtLayers
pw::Display isolateLayer 0

# Step 0.2: Get turbine siting positions and corresposnding refinemet zone parameters (direction and length)
global sitingPos; set sitingPos [list ]
set wakeRefInfo [list  ]

# Read siting positions from file
set infile [open [file join $cwd $siteInfo]  r]

while { [gets $infile line] >= 0 } {
    lappend posData $line
}
close $infile


puts "Siting positions and wake refinement info:"
puts $posData

set nPos [llength $posData]

for {set n 1} {$n<$nPos} {incr n} {
    lappend sitingPos [lrange [lindex $posData $n] 0 2]
    lappend wakeRefInfo [lrange [lindex $posData $n] 3 5]
}


global nSite; set nSite [llength $sitingPos]              
puts "Number of locations: $nSite"

if {$nSite == 0} {
    puts "Siting locations are not defined!"
    exit
}

puts "wakeRefInfo: $wakeRefInfo"

# Get Wake refinement zone extents
set refVec  [GetWakeRefExts $wakeRefInfo]
puts "refVec $refVec"

#--Step 0.3: get or define refinement level and refineemnt factor buffers 
# Determine refinement level required and corresposnding refinement factor
set sOutter 10; # enclosing structured block cell size  
set sInner 1  ; # wind turbine mesh farfield cell size 

set n [expr {log($sOutter/$sInner)/log(2)}]
puts "Current power of default refinement factor(2): $n"

if {$n >= [expr {int($n)+0.2}] } {
    set nNew [expr {int($n)+1}]
} else {
    set nNew [expr {int($n)}]
}
unset n

set refFactor [FloatPrecision [expr {exp(log($sOutter/$sInner)/$nNew)}] 2]

puts "suggested refinement level: $nNew"
puts "suggested refinement factor: $refFactor"

# Adopt these refinement level & factor suggestions
set refinementFactor $refFactor
global nLevel;  
set nLevel $nNew
#set nLevel 2
set refZone [list {20 20 10.1}]

for {set l 1} {$l<$nLevel} {incr l} {
    puts [lindex $refZone [expr {$l-1}]] 
    lappend refZone [pwu::Vector3 scale [lindex $refZone [expr {$l-1}]] 2]
}

puts $refZone


#---- Manual Entry
# global refZone; set refZone [list {15 15 10} {30 30 20} {60 60 40}]; # {50 50 40}
# global nLevel;  set nLevel [llength $refZone]
puts "Total refinement levels: $nLevel"


#----****----****----****----****----****----****----****----****----****----#
#Step 1:
#Step1.1: GEt Wind-farm mesh extents
set wfExt [GetBlklExtDict [$blkABL getExtents]]
puts $wfExt

# Step1.2: Refinement level extent for each turbine position at each refinement level
#global blkSplExt
set blkSplExt [GetAllSplExts $extWT $nLevel $refZone $nSite $sitingPos $refVec] ; # writes blkSplExt
# puts $blkSplExt


#-- Create rectangle to visualize the blocks extents
pw::Display setCurrentLayer 10
# pw::Display isolateLayer 10 

# Create extents box for wind farm mesh
CreateRectangle $wfExt

# for {set l 0} {$l<=$nLevel} {incr l} {; #$nLevel
#     for {set n 0} {$n<$nSite} {incr n} {
#         set key [join [list $l $n] ","]
#         set nDict [dict get $blkSplExt $key]
#         CreateRectangle $nDict
#     }
# }




# Step1.3: Check whether siiting position of their buffer_unstructured boundary overlaps
puts "Siting position overlap check" 
CheckSitingOverlap $blkSplExt $nSite; # Scipt will be aborted if overlap found
puts "No overlap found"

# Step1.4: Check whether all siting position and their refinement zone are within windfarm boundary
puts "Refinement area containment check"
for {set l 0} {$l <= $nLevel} {incr l} {
    for {set n 0} {$n <$nSite} {incr n} {
        set nKey [join [list $l $n] ","]
        set nExt [dict get $blkSplExt $nKey]
        
        if {[WithinTest $wfExt $nExt]} {
            puts "Ref-level $l, Pos: $n is outside of wind farm mesh boundary"
            puts "Aborting Script"
            exit
        }
    }
}

puts "All refinement are in the background ABL mesh extent"

#----****----****----****----****----****----****----****----****----****----#
#Step2:  Block overlap check and merging blocks extents
global nMergedBlks; # global Merged block extents count
dict set nMergedBlks [expr {$nLevel+1}] 1; # only one block at the coarsest level

set merSplExt [list ]
set tempMerSplExt [list ]


for {set l $nLevel} {$l>=1} {incr l -1} {
    puts "Refinement level: $l"
    
    # First tag overlapped blocks
    set blkSplExt [GetOvlTags $l $nSite $blkSplExt]
    
    # Then apply siblingTags (combing the overlapBlkTags)
    set blkSplExt [MergeOvlTags $l $nSite $blkSplExt]
    
    # Get merged extents for overlapped blocks (temporay)
    set tempMerSplExt [concat $tempMerSplExt [MergOvlExts $l $nSite $blkSplExt]]
    
    # Check whether merged extents overlapped themselves and tag
    set mCount [dict get $nMergedBlks $l]
    set tempMerSplExt [GetOvlTags $l $mCount $tempMerSplExt]
    
    # Then apply mergeTags (combing the overlapBlkTags) for the temp_merged_exts
    set tempMerSplExt [ConbineMergeOvlTags $l $mCount $tempMerSplExt ]
    
    # Finaly, recombine the temp_merged_exts to merge_ext (ready for block splitting)
    set merSplExt [concat $merSplExt [MergOvlExts $l $mCount $tempMerSplExt "mergeTag"]]
}
puts "blkSplExt"
PrintDict $blkSplExt
puts ""

puts "tempMerSplExt"
PrintDict $tempMerSplExt
puts ""

puts "merSplExt"
PrintDict $merSplExt
puts ""

puts "nMergedBlks"
PrintDict $nMergedBlks

UnsetEntities [list l mCount tempMerSplExt]


# Create rectangle to visualize the MERGED blocks extents
pw::Display setCurrentLayer 11
# pw::Display isolateLayer 11
for {set l 1} {$l<=$nLevel} {incr l} {; #
    for {set n 0} {$n<[dict get $nMergedBlks $l]} {incr n} {
        set key [join [list $l $n] ","]
        set nDict [dict get $merSplExt $key]
        CreateRectangle $nDict
    }
}

#----****----****----****----****----****----****----****----****----****----#
set startLayer 100; # layer to start placing split and refined blocks
#(start from coarsest level and gradually finner)
for {set l $nLevel} {$l>=1} {incr l -1} {
    puts "Refinement level: $l"
    #Step3: Split the current block to next refinement level 
    set lm1 [expr {$l-1}]; # l minus 1; one level finner values
    set lp1 [expr {$l+1}]; # l plus 1; one level coarser values
    
    set coordinates [GetCoords $l $merSplExt]; # Write $coordinates with provided refinement level
    puts ""
    PrintDict $coordinates    
      
    #-- Layer manangement
    set a [expr {round($nSite/10)+1}]; # number of turbine sites round to 10
    global levelPrimaryLayer; set levelPrimaryLayer [expr {$a*20*($nLevel-$l)+$startLayer}]
    pw::Display setCurrentLayer $levelPrimaryLayer
    pw::Layer setDescription $levelPrimaryLayer [concat "RefLevel-" [expr {$l}]] 
    
    #-- Assign parent blocks. Split the current level parent block(s) 
    if {$l==$nLevel} {
        set nKey [join [list $l 0] ","]        
        dict set blkParent $nKey $blkABL
    } else {
        set mCount [dict get $nMergedBlks $lp1] 
        for {set n 0} {$n< $mCount} {incr n} {
            set nKey [join [list $l $n] ","]
            set nDict [dict get $blkRefined $nKey]
            dict set blkParent $nKey [dict get $nDict bottom]
        }
    }
     
    #Step4.1:-- Split the parent block(s) and get blkChildren
    puts "Building Children blocks"
    set blkChildren [SplBlkIJK $l $coordinates $blkParent $merSplExt $levelPrimaryLayer]; # Writes blkChildren dictionaries
    #puts $blkChildren    
    
    #Step4.2: Spliting Complete :) Now Refine and create unstructured buffer-block operation
    if {$l>1} {
        puts "Refining Blocks"
        set blkCount [dict get $nMergedBlks $l]
        for {set n 0} {$n<$blkCount} {incr n} {
            set nKey [join [list $l $n] ","] 
            set childBlk [dict get $blkChildren $nKey]
            set childExt [dict get $merSplExt   $nKey]
            
            set lm1Key [join [list $lm1 $n] ","]
            dict set blkRefined $lm1Key [RefBlkFillBufferNew $childBlk $childExt]
        }
    }

}

#-------------------------------------------------------------------------#
puts "Level 0 blocks"
PrintDict $blkChildren


proc SetTRexBC { } {
    #--Create T-Rex boundary conditions
    TRexBC "WindTurbine" "Wall" 0.002
    TRexBC "Stage1" "Wall" 0.10; #0.2
    TRexBC "Stage2" "Wall" 0.20; #0.4
    TRexBC "Stage3" "Wall" 0.40; #0.8

    TRexBC "MatchBC" "Match"
}

SetTRexBC
global entsWT; set entsWT [GetWTents]
puts "Entities in Wind Turbine Mesh: $entsWT\n\n"

# for {set n 0} {$n<4} {incr n} { ;# $nSite
#     set blkFiller1 [PlaceWindTurbine $n [lindex $blks $n]]
# }


# Place Wind Turbine meshes at desired locations and stich to Wind-farm mesh
for {set n 0} {$n<$nSite} {incr n} {
    set key [join [list 1 $n] ","]
    set blkTemp [dict get $blkChildren $key]
    puts [$blkTemp getName]
#     catch {
        set blkFiller($n) [PlaceWindTurbine $n $blkTemp]
        puts [$blkFiller($n) getName]
#     }
    
}
#--------------------------------------------------------------------------------------------------#


# End timer
set endTime [clock seconds]

puts ""
puts "Pointwise script executed in [expr $endTime-$startTime] seconds"
puts ""

pw::Display resetView +Z
pw::Display setShowDatabase 0
pw::Display showAllLayers
pw::Display hideLayer $wtLayers 


#END_SCRIPT
