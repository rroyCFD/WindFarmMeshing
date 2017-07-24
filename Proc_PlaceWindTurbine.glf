
# Load Pointwise Glyph package and Tk
package require PWI_Glyph 2


#-- Get Wind Turbine entities (blocks and database models)
#   here performend by groups > entity-list
#   alternate way is to do by layers > entity-list 
#   (didn't applied because I don't know which db models do intersect with terrain quilt
proc GetWTents {} {
    #-- Copy the Wind Turbine ( grid blocks and database models)
    set grpsWT [concat [pw::Group getAll -type pw::Block] \
               [pw::Group getAll -type pw::DatabaseEntity]]
    #puts $grpsWT

    set entitiesOnWT [list ]; # entity list within the wind turbine mesh groups
    for {set i 0} {$i < [llength $grpsWT]} {incr i} {
        set entitiesOnWT [concat $entitiesOnWT [[lindex $grpsWT $i] getEntityList]]
    }
    unset grpsWT 
    
    return $entitiesOnWT
}


proc PlaceWindTurbine {n blkLev1 {initialize ""}} {

# Step1: define global variables that inherited from the main project file
global sitingPos entsWT terrainDB

# Step2: Set a message output file 
set cwd [file dirname [info script]]
set wtMesFile [open [file join $cwd "WT-PlacementMessages.out"] a]
puts "------------------------------------------------------------"
puts "Starts to place wind turbine at site $n"


#--Step3: Define site position and layers 
set currPos [lindex $sitingPos $n] 
set currLayer [$blkLev1 getLayer]

pw::Display setCurrentLayer $currLayer
pw::Display isolateLayer $currLayer
pw::Layer setDescription $currLayer [concat "WT_" [expr {$n+1}]]
puts "Current working Layer: $currLayer"

#--Step4: Identify necessary domains
$blkLev1 setLayer -parents $currLayer ; # Move all parents domain to block layer

set domsFF  [GetDomains $blkLev1] ; # farfield domains of the block
# if the block is deleted, the bottom domains and intermidiate connectors will be free
pw::Entity checkDelete -freed tmpEnts $blkLev1;

set dom(Bot) [list ]
set freeCons [list ]
foreach ent $tmpEnts {
    if {[$ent isOfType pw::Domain]} {
        lappend dom(Bot) $ent
    } elseif {[$ent isOfType pw::Connector]} {
        lappend freeCons $ent
    }
}

#PrintEntities $dom(Bot)
#PrintEntities $freeCons

set domsFF [ListOperation $domsFF $dom(Bot) "subtract"]
set botCons [ListOperation [GetConnectors $dom(Bot)] $freeCons "subtract"] 
puts "Bottom Connectors: $botCons"

# Delete the Cartesian block; to be replace with Wind Turbine Hybrid Block
# Delete the bottom domain too (wind turbine will be placed)
pw::Entity delete [concat $blkLev1 $dom(Bot) $freeCons]


puts "Current Position $currPos"
#-- Paste the wind turbine 
puts "wind-turbine entites: $entsWT"
set entsList [PasteEntities $entsWT $currPos]

puts "Pasted Entities: $entsList"
puts "\n Tower $n pasted in desired location"

#-- Sort the pasted entities by entity-type (step 1 nad 2 could be merged, kept for future usability)
# 1-- Create groups by type for pasted entities 
set grpsWT1_db  [pw::Group create ]; $grpsWT1_db  setEntityType pw::DatabaseEntity
set grpsWT1_blk [pw::Group create ]; $grpsWT1_blk setEntityType pw::Block
set grpsWT1_dom [pw::Group create ]; $grpsWT1_dom setEntityType pw::Domain
set grpsWT1_con [pw::Group create ]; $grpsWT1_con setEntityType pw::Connector
  
for {set i 0} {$i < [llength $entsList]} {incr i} {
    set iEnt [lindex $entsList $i]
    
    if  {[$iEnt isOfType pw::DatabaseEntity]} {
        $grpsWT1_db addEntity $iEnt
    } elseif {[$iEnt isOfType pw::Block]} {
        $grpsWT1_blk addEntity $iEnt
    } elseif {[$iEnt isOfType pw::Domain]} {
        $grpsWT1_dom addEntity $iEnt
    } elseif {[$iEnt isOfType pw::Connector]} {
        $grpsWT1_con addEntity $iEnt
    } else { 
        puts "Entity type match failed" 
    }
    
    unset iEnt
}
unset i


# 2 -- Isolate database models from parent database group (may have quilts and curves)
set dbList [$grpsWT1_db getEntityList]
set modList [list ]

for {set i 0} {$i < [llength $dbList]} {incr i} {
    set iDb [lindex $dbList $i]
    if {[$iDb isOfType pw::Model]} {
        lappend modList $iDb
    }
}
unset iDb
puts "Model list: $modList \n\n"
  
   

# Check whether the database models intersect with the terrain model:

# foreach db $modList {
# 
#     puts [pw::Database intersect -actual 0 $db $terrainDB]
# 
# }

set wtMod [lindex $modList 0]
set wtQuilts [GetQuilts $wtMod]

# puts "WT database model: [$wtMod getName]"
puts "WT database model: [$wtMod getName]"
puts "WT database qulits:$wtQuilts"



# # Trim tower quilts and terrain quilt
# #*** if the quilts doesn't intersect, trimBySurfaces doesn't work well
# # pw::Display showLayer 0; #Turn on the terrain geometry layer
# set modifyMode [pw::Application begin Modify [concat $wtQuilts $terrainDB]]
# # the terrain surface normal shall orinet in +Z direction
# # and the wind turbine tower(model) surface normal shall orient inward direction
# pw::Quilt trimBySurfaces -mode Both -keep Outside $wtQuilts [list $terrainDB]; # -tolerance 0.01
# #pw::Quilt trimBySurfaces -mode Both -keep Inside $wtQuilts [list $terrainDB]; # -tolerance 0.01
# $modifyMode end
# unset modifyMode


if {[catch {
        set modifyMode [pw::Application begin Modify [concat $wtQuilts $terrainDB]]
        pw::Quilt trimBySurfaces -mode Both -keep Outside $wtQuilts [list $terrainDB]; # -tolerance 0.01
        $modifyMode end
        unset modifyMode

    } errmsg]} {
    puts "ErrorMsg: $errmsg"
    puts "ErrorCode: $errorCode"
    puts "ErrorInfo:\n$errorInfo\n"
} else {
    puts "Terrain to Wind Turbine tower trimming is performed" 
}

## A checkpoint to observe whether the triming performed as intended
pw::Display showLayer 1
pw::Display setShowDomains 0

# Create Domain on Database Entties--------------------- #
pw::DomainUnstructured setDefault Algorithm AdvancingFrontOrtho
pw::DomainUnstructured setDefault IsoCellType TriangleQuad

# -- Wind Turbine Tower Database -----#
pw::Display setCurrentLayer [expr {$currLayer+0}]
pw::Connector setCalculateDimensionSpacing 0.1
pw::DomainUnstructured setDefault EdgeMinimumLength 0.1
pw::DomainUnstructured setDefault EdgeMaximumLength 0.1

set domsOnWT [pw::DomainUnstructured createOnDatabase -parametricConnectors Aligned -merge 0.05 -joinConnectors 30 -reject _TMP(unused) $wtMod]
unset _TMP(unused)

#------------------------------------------------------------------------------------------#

set mergeConns [pw::Application begin Merge]
  $mergeConns findPairs -visibleOnly -tolerance 0.005 -exclude None Connector
  if {[$mergeConns getPairCount]>0} {
    $mergeConns setPairStatus 1 TRUE
    $mergeConns mergePairs
    }
$mergeConns end
unset mergeConns

#------------------------------------------------------------------------------------------#
set consWT [GetConnectors $domsOnWT]
puts "Connectors on wind tower lower portion: $consWT"

# Sort the connectors based on domain usage
set consFree [list ]
set consManifold [list ]
set consNonManifold [list ]

set matchDomains [list ]
set nonManifoldDoms [list ] 

foreach con $consWT {
    set domains [pw::Domain getDomainsFromConnectors $con]
    
    if {[llength $domains] ==1} {
        lappend consFree $con
    } elseif {[llength $domains] ==2} {
        lappend consManifold $con 
    } elseif {[llength $domains] > 2} {
        lappend consNonManifold $con
        ladd nonManifoldDoms $domains            
    } else { 
        puts "[$con getName] is not associated with any domain"
    }
}
unset con


puts "Free Connectors"
PrintEntities $consFree 
puts ""

# Trim additional connectors
set consFree [lrange $consFree 0 1]
PrintEntities $consFree 
puts ""

#---------------------------------------------------------------------------------------------------------------#
# Scale Free connectors at the tower and terrain intersection with provide scale value
set cons(stage1) [ScaleProjectCns $consFree 1.5 $currPos] ; #
puts "con> stage1"
PrintEntities $cons(stage1)
puts ""

puts "con> stage2"
set cons(stage2) [ScaleProjectCns $cons(stage1) 2.0 $currPos] 
PrintEntities $cons(stage2)
puts ""

puts "con> stage3"
set cons(stage3) [ScaleProjectCns $cons(stage2) 2.0 $currPos] 
PrintEntities $cons(stage3)
puts ""

pw::DomainUnstructured setDefault Algorithm AdvancingFrontOrtho
pw::DomainUnstructured setDefault IsoCellType TriangleQuad
pw::DomainUnstructured setDefault EdgeMinimumLength Boundary 
pw::DomainUnstructured setDefault EdgeMaximumLength Boundary 
pw::DomainUnstructured setDefault BoundaryDecay 0.5
 
set domsBot(stage0) [CreateUnsDom $cons(stage1) $consFree "reverse"]
set domsBot(stage1) [CreateUnsDom $cons(stage2) $cons(stage1) "reverse"]
set domsBot(stage2) [CreateUnsDom $cons(stage3) $cons(stage2) "reverse"]

pw::DomainUnstructured setDefault Algorithm AdvancingFront
pw::DomainUnstructured setDefault IsoCellType Triangle

set domsBot(stage3) [CreateUnsDom $botCons $cons(stage3)]

if {[$domsBot(stage3) isValid] == 0} {
        puts "$domsBot(stage3) invlaid, reversing edge and recreating"
	pw::Entity delete $domsBot(stage3)
	set domsBot(stage3) [CreateUnsDom $botCons $cons(stage3) "reverse"]
	}

puts [pw::DomainUnstructured getInitializeInterior]

PrintEntities $domsBot(stage3)


exit

#---------------------------------------------------------------------------------------#
#Apply 2D T-Rex on stage_0 domain
set mode2D_TRex [pw::Application begin UnstructuredSolver [list $domsBot(stage0)]]
  set wallBC_TRex [pw::TRexCondition getByName {WindTurbine}]
    foreach _con $consFree {
        $wallBC_TRex apply [list  [list $domsBot(stage0) $_con]]  
    }
    $domsBot(stage0) setUnstructuredSolverAttribute TRexMaximumLayers 51
    $domsBot(stage0) setUnstructuredSolverAttribute TRexPushAttributes True
  $mode2D_TRex run Initialize
  $mode2D_TRex end
unset mode2D_TRex
unset wallBC_TRex

puts "stage 0 domain: [$domsBot(stage0) getName]"



exit


#-------------------------------------------------------------------------------------- #
# Find Match-type Domains      
set nonManifoldDoms [FlattenList $nonManifoldDoms]
puts "Non-manifold domains: $nonManifoldDoms"

foreach _dom $nonManifoldDoms {
    set nBlk [llength [pw::Block getBlocksFromDomains $_dom]]    
     if {$nBlk == 1} {
        set _blk [pw::Block getBlocksFromDomains $_dom]
        #puts [$_blk getName]; puts [$_dom getName]
        
        if {[ string match "Match*" [[pw::TRexCondition getByEntities [list $_blk $_dom]] getType]]} {
            lappend matchDomains $_dom
        }
        unset _blk
     }
}
unset _dom

# Print out the match domains
puts  "Match domains from WT blocks "
PrintEntities $matchDomains

# Build the domains list except which are T-Rex wall condition type
# Used to create a buffer-unstructured block
set domsBuffer [list ]
set domsTemp [pw::Layer getLayerEntities -type pw::Domain $currLayer]

puts "Buffer block domains"

foreach _dom $domsTemp {
    set _blk [pw::Block getBlocksFromDomains $_dom]
    set nBlk  [llength $_blk]
    #puts "[$_dom getName] $nBlk "
    
    if {$nBlk == 0} {
        lappend domsBuffer $_dom
    } elseif {$nBlk == 1} {
        # discard wall type domains
        if {![ string match "Wall*" [[pw::TRexCondition getByEntities [list $_blk $_dom]] getType]]} {
            lappend domsBuffer $_dom
        } 
    }
}
unset _blk _dom domsTemp nBlk
puts "length of buffer domains [llength $domsBuffer] "


#-- Create buffer block
# set blksInBuffer [pw::BlockUnstructured createFromDomains -reject _TMP(unusedDoms) -voids _TMP(voidBlocks) -baffles _TMP(baffleFaces) $domsBuffer]
# unset _TMP(unusedDoms)
set blksInBuffer [pw::BlockUnstructured createFromDomains $domsBuffer]

set blkBuffer [list ]

foreach _blk $blksInBuffer {
    if {[string match "Empty" [$_blk getInteriorState]]} {
        ladd blkBuffer $_blk
        puts "Empty block: [$_blk getName]"
    } else {
        puts "Non Empty block: [$_blk getName] [$_blk getInteriorState]"
    }
}


# Initialize blocks with T-Rex--------------
set mode3D_TRex [pw::Application begin UnstructuredSolver [list $blkBuffer]]
  set WT_BC [pw::TRexCondition getByName {WindTurbine}]
  $WT_BC  apply [list [list $blkBuffer [lindex $domsOnWT 0]] [list $blkBuffer [lindex $domsOnWT 1]]]
    
  set ST1_BC [pw::TRexCondition getByName {Stage1}]
  $ST1_BC apply [list [list $blkBuffer $domsBot(stage1)]]
  
  set ST2_BC [pw::TRexCondition getByName {Stage2}]
  $ST2_BC apply [list [list $blkBuffer $domsBot(stage2)]]
    
  set ST3_BC [pw::TRexCondition getByName {Stage3}]
  $ST3_BC apply [list [list $blkBuffer $domsBot(stage3)]]
  
  set Mt_BC [pw::TRexCondition getByName {MatchBC}]
  $Mt_BC apply [list [list $blkBuffer $domsBot(stage0)]]
  foreach _dom [concat $matchDomains $domsFF] {
    $Mt_BC apply [list [list $blkBuffer $_dom]]
  }; 
  unset _dom
  
  #$Mt_BC apply [list [list $blkBuffer $dom(West)] [list $blkBuffer $dom(East)] [list $blkBuffer $dom(South)] [list $blkBuffer $dom(North)]]  
  
  $blkBuffer setUnstructuredSolverAttribute TRexSpacingSmoothing 10
  $blkBuffer setUnstructuredSolverAttribute EdgeMaximumLength Boundary
  $blkBuffer setUnstructuredSolverAttribute TRexMaximumLayers 51
$mode3D_TRex end
unset mode3D_TRex

#-- Initialize block
if {[string match "ini*" $initialize]} {
    set initializeBlk [pw::Application begin UnstructuredSolver [list $blkBufffer]]
    $initializeBlk run Initialize
    $initializeBlk end
    unset initializeBlk
}

puts "Wind turbine placed at site $n"
close $wtMesFile

UnsetEntities [list currPos  currLayer blkLev1 domsFF botCons entsList wtMod domsOnWT consconsFree \
               consManifold consNonManifold matchDomains nonManifoldDoms domsBot domsBuffer]

return $blkBuffer

}

