package require PWI_Glyph 2


##################################################################################################################
#BEGIN_OF_CUSTOM_PROCEDURES
# --------------- Custom Procedures Begin -----------------------------------------------------------------------#

#--Incrementing doubles (as well as integers):
#  Default increment is 1
proc Add {varName {amount 1}} {
    upvar 1 $varName var
    set var [expr {$var+$amount}]
}


#-- Format a floating point list upto desired precision
proc FloatPrecision {floatList {precision 1}}  {
    set myList {}
    foreach float $floatList {
        lappend myList [expr {round( 10 ** $precision * $float) / (10.0 ** $precision)} ]
    }
    
    return $myList
}


#-- Tweak of lappend command. Aappend an element to a list if not already included within the list
#   Avoid repeated addition
#   Args:  a list and an element
#   Return: modified list
proc ladd {_list el} {
   upvar 1 $_list list
   if {$el ni $list} {lappend list $el}
}

#-- A procedure to delete a given element from a list:
proc lremove {listVariable value} {
    upvar 1 $listVariable var
    set idx [lsearch -exact $var $value]
    set var [lreplace $var $idx $idx]
}


#-- Prepend to a list
proc lprepend {varName args} {
   upvar 1 $varName var
   # Ensure that the variable exists and contains a list
   lappend var
   # Now we insert all the arguments in one go
   set var [eval [list linsert $var 0] $args]
}


#-- Add a dictionary to an existing one
#   Args: "sourceDict" a dictionary which will be added to the "targetDict" dictionary
#   Return: joined "targetDict" dictionary
proc AddDict {targetDict sourceDict } {
    foreach key [dict keys $sourceDict] {
        dict append targetDict $key [dict get $sourceDict $key]
    }
    return $targetDict 
}


#-- Print the keys and values of a dictionary (similar to parray command for array
proc PrintDict {myDict} {
    foreach key [dict keys $myDict] {
        set value [dict get $myDict $key]
        if {[catch {puts "$key = [$value getName]"}]} {
            puts "$key = $value"
        }
    }
}



#-- Print an entities list
# ** need a catch rule out non-pw objects
proc PrintEntities {_ents} {
    foreach _ent $_ents {
        catch { puts [$_ent getName] }
    }
}

# proc PrintArray {_arr} {
#     foreach key [array names $_arr] {
#         catch { puts "$_arr($key)" }    
#     }
# 
# }

#-- ListOperation: List SET operation. Args: two lists. 
#   Operation modes: common, union, difference, subtract
#   Returns a list
proc ListOperation { list1 list2 {operation ""} } {
   set common    [list ] 
   set subtract2from1 [list ]
   set subtract1from2 [list ]
   
   foreach item $list1 {
      if {$item  in $list2} {
         ladd common $item
      } else {
         ladd subtract2from1 $item
      }
   }    
   
   foreach item $list2 {
      if {$item  ni $list1} {
         ladd subtract1from2 $item
      }
   }
   
   if {[string match "com*" $operation]} {
      return $common
      
   } elseif {[string match "dif*" $operation]} {
      return [concat $subtract2from1 $subtract1from2]
      
   } elseif {[string match "uni*" $operation]} {
      return [concat $subtract2from1 $common $subtract1from2]
      
   } elseif {[string match "sub*" $operation]} {
      return $subtract2from1
   }
}



#-- Flattens a nested list. Removes all inner nestation except the outermost one
#   Args: a list (probably nestated!) 
#   Return: a flattend list 
proc FlattenList { aList } {
    set lenOld [llength $aList]
    set lenNew 0
    set count 1
    
    while {$count < 20} {
        set lenOld $lenNew
        set aList  [join $aList]
        set lenNew [llength $aList]
        if {$lenNew == $lenOld} {
            break
        }
        incr count
    }
    return $aList
}



#-- Get domains associated with a block(s) list
#   Args: a block or blocks list (one level nestation reqiired)
#   Return: a list of associated domains 
#   *** consider adding a catch statement
proc GetDomains { blocks } {
  set doms [list]
  foreach _blk $blocks {
    set faceCount [$_blk getFaceCount]
    for {set i 1} {$i <= $faceCount} {incr i} {
        set face [$_blk getFace $i]
        set domainCount [$face getDomainCount]
        for {set j 1} {$j <= $domainCount} {incr j} {
            ladd doms [$face getDomain $j]
        }
    }
  }
    
  return $doms
}


#-- Get connectors associated with a domain(s) list
#   Args: a domain or domains list (one level nestation reqiired)
#   Return: a list of associated connectors 
#   *** consider adding a catch statement
proc GetConnectors { domains } {
  set conns [list]
  foreach _dom $domains {
    set edgeCount [$_dom getEdgeCount]
    for {set i 1} {$i <= $edgeCount} {incr i} {
        set edge [$_dom getEdge $i]
        set connectorCount [$edge getConnectorCount]
        for {set j 1} {$j <= $connectorCount} {incr j} {
        ladd conns [$edge getConnector $j]
        }
    }
  
  }
  return $conns
}

#-- Get quilts associated with a model(s) list
#   Args: a model(s) list (one level nestation required)
#   Return: a list of associated quilts
#   *** consider adding a catch statement
proc GetQuilts { models } {
  set models [FlattenList $models]
  
  set quilts [list]
  foreach _mod $models {
    set quiltCount [$_mod getQuiltCount]
    for {set i 1} {$i <= $quiltCount} {incr i} {
        lappend quilts [$_mod getQuilt $i]
    }
  
  }
  return $quilts
}


#-- Set a list of entities to a prescribed layer
#   Args: an entities list (myList)
#         a layer number
#   Return: none
proc SetEnts2Layer {myList nLayer} {
    set myList [FlattenList $myList]
    
    set entsList [list ]
    foreach ent $myList {
        catch { \
        if {[$ent isOfType pw::Object]} {
            lappend entsList $ent
        } else {
            puts $ent
        } \
        }
    }

    set entsCollection [pw::Collection create]
    $entsCollection set $entsList
    $entsCollection do setLayer -parents $nLayer
    $entsCollection delete
}



#-- Unset a list of entities
#   Args: entsList
#   Return: none
proc UnsetEntities {entsList} {
        set entsList [FlattenList $entsList]
        foreach var $entsList {
        catch {
            unset var
        }
    }
}


#-- Create a linear connector from 2 points
#   Args: pt1 and pt 2 (vectors)
#   Return: alinear connector
proc Con4m2pts {pt1 pt2} {
    set crtConn [pw::Application begin Create]
    set connSeg [pw::SegmentSpline create]
    $connSeg addPoint $pt1
    $connSeg addPoint $pt2
    set conn [pw::Connector create]
    $conn addSegment $connSeg
    unset connSeg
    $conn setDimension 2
    $crtConn end
    unset crtConn
    
    return $conn
}



#-- Creates a rectangle based on two diagonal vertices corrdinate
#   Args: Rectangle extent
#   Return: a list of 4 connectors
proc CreateRectangle {exts} {
    set xMin [dict get $exts xMin]
    set yMin [dict get $exts yMin]
    set xMax [dict get $exts xMax]
    set yMax [dict get $exts yMax]
    
    lappend conns [Con4m2pts [list $xMin $yMin 0] [list $xMax $yMin 0] ]
    lappend conns [Con4m2pts [list $xMax $yMin 0] [list $xMax $yMax 0] ]
    lappend conns [Con4m2pts [list $xMax $yMax 0] [list $xMin $yMax 0] ]
    lappend conns [Con4m2pts [list $xMin $yMax 0] [list $xMin $yMin 0] ]
    
    #set connGrp [pw::Group create]
    #$connGrp setEntityType pw::Connector
    #$connGrp addEntity $conns
    #return $connGrp
    
    return $conns
}


#-- Joins structured blocks 
#   Args: blocks list. The flag (optional) checks for a parent key and if present, attempts to join domains and connectors
#   Return: the joined block only
proc JoinStucBlks {blksList {flag ""}} {
    if {[llength $blksList] > 1} {
        set joinedBlks [pw::BlockStructured join -reject _TMP(ignored) $blksList]
        unset _TMP(ignored)
    
        if {[string match "*parents*" $flag ]} {
            set domsList [GetDomains $joinedBlks]
            set joinedDoms [pw::DomainStructured join $domsList]
            
            set connsList [GetConnectors $joinedDoms]
            set joinedConns [pw::Connector join $connsList]
        }
        
        return $joinedBlks
    } else {
        return $blksList
    }
}



#-- Get offest values (from Wind Turbine Mesh) to trim-off the Cartesian block
#   Args:   Wind turbine mesh horizontal dimention (dim) and cartesian block grid resolution (res)
#   Return: offset value to trim
proc GetOffset {dim res} {
    set mul 100; # i.e., consider values upto 2 decimal precision
    set dim [expr {int(abs($dim*$mul))}]
    set res [expr {int(abs($res*$mul))}]
    
    puts $res
    
    if {[expr {$dim%$res}] >= [expr {$res/2}]} {
    set offsetVal [expr {(round($dim/$res)+3)*$res/$mul}]
    } else {
    set offsetVal [expr {(round($dim/$res)+2)*$res/$mul}]
    }
    
    return $offsetVal
}


#-- Scale connectors in horizontal plane (XY) and then vertically project onto database surface 
#   Args:  connector list and scale Value; the database (terrain surface is kept fixed)
#   Return: new connector list
#   *** Caution: clear the Clipboard entities !!!
proc ScaleProjectCns {cons sVal currPos} {
    
    global terrainDB   
    
    pw::Application clearClipboard
    pw::Application setClipboard $cons;

    set z0 [pwu::Vector3 z [[lindex $cons 0] getXYZ -arc 0.0]]; # height of a connector 
    
    set pasteMode [pw::Application begin Paste]
        set consNew [$pasteMode getEntities]
        set modifyMode [pw::Application begin Modify $consNew]            
            pw::Entity transform [pwu::Transform scaling -anchor \
            [list [lindex $currPos 0] [lindex $currPos 1] $z0] [list $sVal $sVal 1]]\
            [$modifyMode getEntities ]
            
            pw::Entity project -type Linear -direction {0 0 1} $consNew $terrainDB
        $modifyMode end
        unset modifyMode
    $pasteMode end
    unset pasteMode
    
    pw::Application clearClipboard
    
    return $consNew
}


#-- Create unstructured doimain with an interior edge (closed)
#   Args: two connector lists to build two domain edges. "reverse" is an optional argument to reverse one of the edges
#   Return: unstructured domain
proc CreateUnsDom {consOuter consInner {rev ""}} {
    
    global terrainDB

    set crtDom [pw::Application begin Create]
        set edgeOutter [pw::Edge createFromConnectors -single $consOuter]
        set edgeInner [pw::Edge createFromConnectors -single $consInner]
		
        #puts "Qualifying Edges: boolean"
	#puts [pw::DomainUnstructured qualifyEdges $edgeOutter]
        #puts [pw::DomainUnstructured qualifyEdges $edgeInner]
		
        
        if {[string equal -nocase "reverse" $rev]} {$edgeInner reverse }
        
        set _domain [pw::DomainUnstructured create]
        $_domain setUnstructuredSolverAttribute EdgeMaximumLength Boundary
            if {[$edgeOutter isClosed] && [$edgeInner isClosed]} {
                    $_domain addEdge $edgeOutter
                    $_domain addEdge $edgeInner
            } else { 
                puts "Edge is open... Cannot create domain"
                return
            }

    $crtDom end
    unset crtDom edgeOutter edgeInner
    
    # Project on DB
    pw::Entity project -type Linear -direction {0 0 1} $_domain $terrainDB
    
    return $_domain
}


#-- Create a T-Rex boundary condition, either Wall or Match type
#   Args: name of BC, type of BC (Wall or Match) and spacing (optional) in case of WAll type BC 
#   Returns: None
proc TRexBC {nameBC typeBC {spacing 0.0} } {
    set crtBC [pw::TRexCondition create]
        $crtBC setName $nameBC; 
        if {$spacing <= 0.0} {
            set $spacing [pw::TRexCondition getAutomaticWallSpacing]
        }
        
        if {[string match "Wall*" $typeBC]} {
                $crtBC setType {Wall}
                $crtBC setSpacing $spacing
        } elseif {[string match "Match*" $typeBC]} {
                $crtBC setType {Match}
        }

    unset crtBC
}



#-- SplitBlock (Splits a stuctured block 6 child blocks (north, south, east, west, top and bottom)
#   Args: 
#       "block2Trim" The structured block to trim
#       "blkSplExts"   Trim parameters (block corrdinate extents) [a dictionary]
#       "nLayer"     Layer number to place the splited blocks [an integer]
#   Returns: A blkChild Dictionary with keys mentioned above
proc SplitBlock {blk2Trim blkSplExts} {; # nLayer
    
#     if {$nLayer != 0} {
#         pw::Display setCurrentLayer $nLayer
#     }
#     puts "nLayer $nLayer"
#     puts [pw::Display getCurrentLayer]
    
    #pw::Display setCurrentLayer $nLayer
    #$blk2Trim setLayer -parents $nLayer
    
    dict set blkParent blockID $blk2Trim
    
    #-- Split in X (East-West) direction
    set split_params [list]
        lappend split_params [lindex [[dict get $blkParent blockID] closestCoordinate -boundary [list [dict get $blkSplExts xMin] 0 0]] 0];
        lappend split_params [lindex [[dict get $blkParent blockID] closestCoordinate -boundary [list [dict get $blkSplExts xMax] 0 0]] 0];
        dict set blkParent splEW [[dict get $blkParent blockID] split -I $split_params]; # East-West splited blocks list
    unset split_params
        
    dict set blkChild west [lindex [dict get $blkParent splEW] 0] 
    dict set blkChild east [lindex [dict get $blkParent splEW] 2] 
    dict set blkParent center1 [lindex [dict get $blkParent splEW] 1]
     
    [dict get $blkChild east] setName "blkEast"; 
    [dict get $blkChild west] setName "blkWest"; 
    
    
    
    #-- Split in Y (North-South) direction
    set split_params [list]
        lappend split_params [lindex [[dict get $blkParent center1] closestCoordinate -boundary [list [dict get $blkSplExts xMin] [dict get $blkSplExts yMin] 0]] 1]
        lappend split_params [lindex [[dict get $blkParent center1] closestCoordinate -boundary [list [dict get $blkSplExts xMin] [dict get $blkSplExts yMax] 0]] 1]
        dict set blkParent splNS [[dict get $blkParent center1] split -J $split_params]; # Split the center block in North-South direction
    unset split_params

    dict set blkChild south [lindex [dict get $blkParent splNS] 0]; 
    dict set blkChild north [lindex [dict get $blkParent splNS] 2];
    dict set blkParent center2 [lindex [dict get $blkParent splNS] 1]
    
    [dict get $blkChild south] setName "blkSouth";    
    [dict get $blkChild north] setName "blkNorth"; 
    
    
    #-- Split in Z (Vertical:Top-Bottom) direction
    set split_params [list]
        lappend split_params [lindex [[dict get $blkParent center2] closestCoordinate -boundary [list [dict get $blkSplExts xMin] [dict get $blkSplExts yMin] [dict get $blkSplExts zMax]]] 2]
        dict set blkParent splTB [[dict get $blkParent center2] split -K $split_params]; # Split in vertical direction 
    unset split_params

    dict set blkChild bottom [lindex [dict get $blkParent splTB] 0];
    dict set blkChild top    [lindex [dict get $blkParent splTB] 1];
    
    [dict get $blkChild bottom] setName "blkBottom"; 
    [dict get $blkChild top] setName "blkTop";
    
    #[dict get $blkChild bottom] setLayer -parents [expr {$nLayer+1}]

    return $blkChild
}



#-- Redimension connectors (refine by using a refinement factor)
#   Args: a list of connectors
#   Return: connectors refined by the refinementFactor (global variable)
#   * grid point increases and node spacing decreases
#   ** doesn't balance a structured block
proc RedimensionConnectors { conList } {
  global refinementFactor

  set conMode [pw::Application begin Modify $conList]
  foreach con $conList {
  
    # Get connector distribution type
    set conDist [$con getDistribution 1]

    # Check if distribution is of type growth
    if { [$conDist isOfType "pw::DistributionGrowth"] } {
      # Decrease grid point spacing
      $conDist setBeginSpacing [expr {(1.0 / $refinementFactor) * \
        [[$conDist getBeginSpacing] getValue]}]
      $conDist setEndSpacing [expr {(1.0 / $refinementFactor) * \
        [[$conDist getEndSpacing] getValue]}]

      # Set optimal connector dimension
      $con setDimensionFromDistribution
    } else {
      # Increase connector dimension in 3 steps ...
      # 1) Store refined subconnector dimensions
      set totalDim 0
      set subConnCount [$con getSubConnectorCount]
      for {set i 1} {$i <= $subConnCount} {incr i} {
        set dim [expr {round($refinementFactor * \
          [$con getSubConnectorDimension $i] - 1)}]
        lappend conSubDim $dim
        incr totalDim $dim
      }

      # 2) Redimension connector
      $con setDimension [expr {$totalDim - ($subConnCount - 1)}]

      # 3) Adjust subconnector dimension
      if { $subConnCount > 1 } {
        $con setSubConnectorDimension $conSubDim
      }
      catch {unset conSubDim}

      # Decrease grid point spacing
      for {set i 1} {$i <= $subConnCount} {incr i} {
        set conDist [$con getDistribution $i]
        $conDist setBeginSpacing [expr (1.0 / $refinementFactor)* \
          [[$conDist getBeginSpacing] getValue]]
        $conDist setEndSpacing [expr (1.0 / $refinementFactor)* \
          [[$conDist getEndSpacing] getValue]]
      }
    }
  }
  $conMode end
  catch {unset conMode}
}


#-- Get grid spacing from a grid point coordinates list
#   Args: corrdinate list
#   Return: a list of spacing values between consequtive coordinates
proc GetGridSpacing {myList} {
    set spaceList [list ]
    for {set i 1} {$i < [llength $myList]} {incr i} {
        lappend spaceList [pwu::Vector3 length [pwu::Vector3 subtract\
        [lindex $myList $i]  [lindex $myList [expr {$i-1}] ]]]
    }
    
    return $spaceList
}


#-- Get the structured block grid resolution
#   Args: A structured block
#   Return: resVal dictionary (with KEYS: iMin, iMax, jMin, jMax, kMin, kMax)
proc GetBlockResolution {block} {
    
    # Check if the block is structured    
    if {![$block isOfType pw::BlockStructured]} {
        puts "[$block getName] is not a structured block!"
        exit
    }
    
    # Get block dimension
    set dim [$block getDimensions];  
    
    # Set domain sequence: 
    # -for I direction (min or max domain)> follow K-I-J sequence, i.e. get domains with K min and max. 
    # -for each domain, find grid coordinates at J min and max (I varies only),
    #  which are the first and last row of the domain corrdinate list.
    # -Similar loop for J and K coordinates
    
    set dimSeq  [list "-K" "-I" "-J"]; 
    
    # List with min and max values of K, I, J direction respectively (following the sequence).
    set dimList [list [list 1 [lindex $dim 2]] \
                      [list 1 [lindex $dim 0]] \
                      [list 1 [lindex $dim 1]] ]
    #puts $dimList
    
    # The iterative loop. i for direction, j for min and max extreme
    for {set i 0} {$i<3} {incr i} {
        set rest($i) [list ]; # resolution list
        
        for {set j 0} {$j<2} {incr j} {
        
        # Get domain cordinates
        set indexVal [lindex [lindex $dimList $i] $j]
        #puts "$i $j $indexVal [lindex $dimSeq $i]"
        set Coords($i,$j) [$block getXYZsAtIndex [lindex $dimSeq $i] $indexVal ]
        
        # Get grid spacing for each of these direction and extreme combination
        set rowLength [expr {[lindex $dim $i] -1}]
        lappend res($i) [GetGridSpacing [lrange $Coords($i,$j) 0 $rowLength ] ]
        lappend res($i) [GetGridSpacing [lrange $Coords($i,$j) end-$rowLength end] ]
        }
        
        set res($i) [lsort -real [join [join $res($i)]] ]
    }
    
    set resVal [dict create iMin [lindex $res(0) 0] iMax [lindex $res(0) end] \
                            jMin [lindex $res(1) 0] jMax [lindex $res(1) end] \
                            kMin [lindex $res(2) 0] kMax [lindex $res(2) end] ]
    
    unset i j dim dimList dimSeq rowLength Coords res
    return $resVal
}


#-- Get extent of a mesh project (Based on extent of blocks and models within the specified layers)
#   Args: a list layers
#   Return: 2 vectors: minimum and maximum extents
proc GetMeshExtents {layersList} {   
    set ents [list ]
    set minVec [pwu::Vector3 zero]
    set maxVec [pwu::Vector3 zero]
    
    # Build block and model entity list across all spcified layers
    foreach layer $layersList {
#         if {[pw::Layer getLayerEntityCounts $layer] >0} {
            lappend ents [pw::Layer getLayerEntities -type pw::Block $layer]
            lappend ents [pw::Layer getLayerEntities -type pw::Model $layer]
#         }
    }
    set ents [join $ents]
    
    # Get the minimum and maximum extents
    foreach ent $ents {
        set vec [$ent getExtents]
        set minVec [pwu::Vector3 minimum $minVec [lindex $vec 0] ]
        set maxVec [pwu::Vector3 maximum $maxVec [lindex $vec 1] ]
    }

    return [list $minVec $maxVec]
}


#-- Build structured block splitting extent
#   Args: extents vector list
#   Return: block spliting dictionary
#   *** consider to chack if there only one block in args
proc GetBlklExtDict {exts} {     
    set splDict [dict create \
        xMin [lindex [lindex $exts 0] 0] \
        yMin [lindex [lindex $exts 0] 1] \
        zMin [lindex [lindex $exts 0] 2] \
                                         \
        xMax [lindex [lindex $exts 1] 0] \
        yMax [lindex [lindex $exts 1] 1] \
        zMax [lindex [lindex $exts 1] 2] ]
        
    dict set splDict zMin 0.0; # Hardcoded for this project

    return $splDict
}



#-- Check whether two block horizontal plane extents overlaps or not
#   Args: two block extents
#   Return: a boolean, "bool": 0 for No-Overlap, 1 for Overlap
proc OverlapTest {blkExt1 blkExt2} {
    set x(1) [dict get $blkExt1 xMin]
    set y(1) [dict get $blkExt1 yMin]
    set x(2) [dict get $blkExt1 xMax]
    set y(2) [dict get $blkExt1 yMax] 

    set x(3) [dict get $blkExt2 xMin]
    set y(3) [dict get $blkExt2 yMin]
    set x(4) [dict get $blkExt2 xMax]
    set y(4) [dict get $blkExt2 yMax] 


    if {$x(1)>$x(4) || $x(3)>$x(2) || $y(1)>$y(4) || $y(3)>$y(2)} {
        set bool 0
        #puts "blocks do not overlap"
    } else {
        set bool 1
        #puts "blocks overlap"  
    }
    
    unset x y
    return $bool
}



#-- Check whether an inner-block resides within an outter-block or not
#   Args: two block extents, outterExt and innerExt 
#   Return: a boolean, "bool": 0 for Within, 1 for Overlap
proc WithinTest {outterExt innerExt} {
    set x(1) [dict get $outterExt xMin]
    set y(1) [dict get $outterExt yMin]
    set x(2) [dict get $outterExt xMax]
    set y(2) [dict get $outterExt yMax] 

    set x(3) [dict get $innerExt xMin]
    set y(3) [dict get $innerExt yMin]
    set x(4) [dict get $innerExt xMax]
    set y(4) [dict get $innerExt yMax] 


    if {$x(3)>$x(1) && $x(2)>$x(4) && $y(3)>$y(1) && $y(2)>$y(4)} {
        set bool 0
        #puts "block is Within the boundary ext"
    } else {
        set bool 1
        #puts "block is outside of boundary ext"  
    }
    
    unset x y
    return $bool
}



#-- Check if any siting position (upto Level 1) overlaps!
#   Args: blkSplExt > block spliting extents
#   Return: None
proc CheckSitingOverlap {blkSplExt nSite} {
    for {set l 0} {$l<=1} {incr l} {
        for {set n 0} {$n<$nSite} {incr n} {
            for {set m [expr {$n+1}]} {$m<$nSite} {incr m} {
                set nKey [join [list $l $n] ","]
                set mKey [join [list $l $m] ","]
                
                set nExt [dict get $blkSplExt $nKey]
                set mExt [dict get $blkSplExt $mKey]
                
                if {[OverlapTest $nExt $mExt]} {
                    puts "Ref-level: $l, position $n and $m overlaps!"
                    puts $nExt
                    puts $mExt
                    puts "Aborting Script"
                    exit
                }
            }
        }
    }
}



#-- Get overlaptags among provided block extents (splExt)
#   Args: l > level of refinement. Related to the dictionary parent key 
#         blkCount > number of blks to be compred for overlapping
#         splExt > a dictionary that contains "l" level block extents,
#                > overlap tag will be added and retured the same
#   Return: splExt (updated dictionary provided in args)
#   Dependent procs: OverlapTest

proc GetOvlTags {l blkCount splExt } {
   #-- Check if any block extents overlaps and mark them with a tag
   if {$blkCount ==1} {
    return $splExt
   } else {
        for {set m 0} {$m < $blkCount} {incr m} {
            for {set n [expr {$m+1}]} {$n<$blkCount} {incr n} {
                #puts "m:$m, n:$n"
                
                set mKey [join [list $l $m] ","]
                set nKey [join [list $l $n] ","]
                
                set mDict [dict get $splExt $mKey]
                set nDict [dict get $splExt $nKey]
                
                if {[OverlapTest $mDict $nDict]} {
                    #puts "block $m and $n Overlaps"
                    #Tag both blocks
                    dict lappend nDict overlapBlkTag $m
                    dict lappend mDict overlapBlkTag $n
                    
                    set splExt [dict replace $splExt $mKey $mDict $nKey $nDict]
                }
            }
        }
        
        return $splExt
    }
}


#-- Merge Overlap tags 
#   Args: l > level of refinement. Related to the dictionary parent key
#         blkCount > number of blks to be compred for overlapping
#         splExt > a dictionary that contains "l" level block extents,
#                > gets sibling tag combing overlap tags
#         tagBl > tag to combine overlapped block extents (default "siblingBlks")
#   Return: splExt (updated dictionary provided in args)
#   Dependent procs: OverlapTest
proc MergeOvlTags {l blkCount splExt {tagBl "siblingBlks"} } {
#-- Get the siblings (overlaped) blocks together, combine the overlap tags
    set depBlks [list ]; # dependent/connected blocks
    set indpBlks [list ]; # independent/free blocks

    for {set m 0} {$m< $blkCount} {incr m} {
        
        set mKey [join [list $l $m] ","]
        set mDict [dict get $splExt $mKey]
            
        if {($m ni $depBlks) && [dict exists $mDict overlapBlkTag]} {
            set ovlBlks [dict get $mDict overlapBlkTag]; # overlaped blocks list with block m
            set sibBlks [concat $m $ovlBlks];            # Re-initialize siblings list
        
            set bool 1; # boolean trigger to toggle on/off search for overlaped blocks
            set k 1;    # while loop iteration safety upper-limit
            while {$bool && $k <=$blkCount } {
                set bool 0; # declare bool to 0, unless altered overlapped blocks found (in the for loop)
                
                # Nested overlaped block search
                set nestedOvlBlks [list ]; # reinitialize the nested overlapped blocks list
                foreach val $ovlBlks {
                    set valKey [join [list $l $val] ","]
                    set valDict [dict get $splExt $valKey]
                
                    if {[dict exists $valDict overlapBlkTag]} {
                        set bool 1; # keep the control loop alive
                        set nestedOvlBlks [lsort -unique [concat $nestedOvlBlks [dict get $valDict overlapBlkTag]]]
                    }
                }
                set sibBlks [lsort -unique [concat $sibBlks $nestedOvlBlks]]; # add nested blocks to sibling family
                set ovlBlks $nestedOvlBlks; # Reassign overlaped blocks for next level iteration
                
                #puts "m: $m, k: $k, ovlBlks $ovlBlks sibBlks $sibBlks"
                incr k
            }
            
            set sibBlks [lsort -unique $sibBlks]
            dict set mDict $tagBl $sibBlks; # add the siblings to the fist block dictionary
            #puts "siblingBlks: [dict get $mDict $tagBl]"
            
            set depBlks [lsort -unique [concat $depBlks $sibBlks]]
        } elseif {($m ni $depBlks) } {
            ladd indpBlks $m
           }
        
        set splExt [dict replace $splExt $mKey $mDict]
    }
    UnsetEntities [list k m bool val ovlBlks nestedOvlBlks sibBlks]

    puts "Dependent/connected blocks: $depBlks"
    puts "Independent/free blocks: $indpBlks \n\n"
    
    return $splExt
}



#-- Combine merge tags 
#   Args: l > level of refinement. Related to the dictionary parent key
#         blkCount > number of blks to be compred for overlapping
#         splExt > a dictionary that contains "l" level block extents,
#                > gets sibling tag combing overlap tags
#         tagM > mergeTag: tags that combine overlapep tags sfrom merge-exts (default "mergeTag")
#         tagBl > tag to combine overlapped block extents (default "siblingBlks")
#   Return: splExt (updated dictionary provided in args)
#   Dependent procs: OverlapTest
proc ConbineMergeOvlTags {l blkCount merExt {tagM "mergeTag"} {tagBl "siblingBlks"} } {
#-- Get the siblings (overlaped) blocks together, combine the overlap tags
    set depExts [list ]; # dependent/connected merged extents
    set indpExts [list ]; # independent/free merged extents

    for {set m 0} {$m< $blkCount} {incr m} {
        
        set mKey [join [list $l $m] ","]
        set mDict [dict get $merExt $mKey]
            
        if {($m ni $depExts) && [dict exists $mDict overlapBlkTag]} {
            
            set sibBlks [dict get $mDict siblingBlks]
            set ovlExts [dict get $mDict overlapBlkTag]; # overlaped blocks list with block m
            set mTags   [concat $m $ovlExts]
            foreach o $ovlExts {
                set oKey [join [list $l $o] ","]
                set oDict [dict get $merExt $oKey]
                set sibBlks [concat $sibBlks [dict get $oDict siblingBlks]];   # Concat siblings list
            }
            
            set bool 1; # boolean trigger to toggle on/off search for overlaped blocks
            set k 1;    # while loop iteration safety upper-limit
            while {$bool && $k <=$blkCount } {
                set bool 0; # declare bool to 0, unless altered overlapped blocks found (in the for loop)
                
                # Nested overlaped block search
                set nestedOvlExts [list ]; # reinitialize the nested overlapped blocks list
                foreach val $ovlExts {
                    set valKey [join [list $l $val] ","]
                    set valDict [dict get $merExt $valKey]
                
                    if {[dict exists $valDict overlapBlkTag]} {
                        set bool 1; # keep the control loop alive
                        set nestedOvlExts [lsort -unique [concat $nestedOvlExts [dict get $valDict overlapBlkTag]]]
                    }
                    
                    # add nested blocks to sibling family    
                    foreach o $nestedOvlExts {
                        set oKey [join [list $l $o] ","]
                        set oDict [dict get $merExt $oKey]
                        set sibBlks [concat $sibBlks [dict get $oDict siblingBlks]];   # Concat siblings list
                    }  
                }
                set sibBlks [lsort -unique $sibBlks]; 
                set mTags   [concat $mTags $nestedOvlExts]
                set ovlExts $nestedOvlExts; # Reassign overlaped blocks for next level iteration
                
                #puts "m: $m, k: $k, ovlExts $ovlExts sibBlks $sibBlks"
                incr k
            }
            
            set sibBlks [lsort -unique $sibBlks]
            set mTags [lsort -unique $mTags]
            
            dict set mDict $tagBl $sibBlks; # add the siblings to the fist block dictionary
            dict set mDict $tagM $mTags;
            #puts "siblingBlks: [dict get $mDict $tagBl]"
            
            set depExts [lsort -unique [concat $depExts $mTags]]
        } elseif {($m ni $depExts) } {
            ladd indpExts $m
           }
        
        set merExt [dict replace $merExt $mKey $mDict]
    }
    UnsetEntities [list k m bool val ovlExts nestedOvlExts sibBlks]

    puts "Dependent/connected Merged Extents: $depExts"
    puts "Independent/free Merged Extents: $indpExts \n\n"
    
    return $merExt
}



#-- Merge overlaped block extents to one and also merge overlaptag into a siblings tag
#   Args:l > level of refinement. Related to the dictionary parent key
#         blkCount > number of blks to be compred for overlapping
#         splExt > a dictionary that contains "l" level block extents,
#                   overlap tags and sibling tags
#         tagBl > tag to combine overlapped block extents (default "siblingBlks")
#   Return: mergeSplExt merged extents with sibling tag
#   Dependent procs: GetMergedBlkExts

proc MergOvlExts {l blkCount splExt {tagBl "siblingBlks"}} {
    global nMergedBlks; # global Merged block extents count
    
    set n 0
    for {set m 0} {$m<$blkCount} {incr m} {
        set mKey [join [list $l $m] ","]
        set nKey [join [list $l $n] ","]
        
        set mDict [dict get $splExt $mKey]
    
        if {[dict exists $mDict $tagBl]} {
            dict set mergeSplExt $nKey [GetMergedBlkExts $l $mDict]
            incr n
        } elseif {![dict exists $mDict overlapBlkTag]} {
            # add siblingBlks tag only
            if {$tagBl == "siblingBlks"} {
                dict lappend mDict $tagBl $m
            }
            dict set mergeSplExt $nKey $mDict
            incr n
        }
    }
    dict set nMergedBlks $l $n
    
    UnsetEntities [list l m n blkCount mKey nKey mDict splExt]
    return $mergeSplExt
}



#-- Check whether a vector is is first quadrant
#   checks in XY plane only
#   example: puts [PositiveVec {-0.1 2 1} 0.5]
proc PositiveVec {vec {tol 0}} {
    # turn the tolerance to negative to allow a value within tolerance radius as point match
    if {$tol > 0} {
        set tol [expr {-1.0*$tol}]
    }
    
    if {([pwu::Vector3 x $vec] >= $tol) &&([pwu::Vector3 y $vec] >= $tol)} {
        # to incluse z coordinate, add:  && ([pwu::Vector3 z $vec] >= $tol)
        return 1
    } else {
        return 0
    }
}



#-- Find interior blocks residing in a prescribed extents
#   Args: Desired Extents (min and max vector) and a list of blocks from which interior blocks are sought
#   Return: a list of interior blocks 

proc FindInteriorBlks {exts blksList} {
    set minExt [list [dict get $exts xMin] [dict get $exts yMin] [dict get $exts zMin]]
    set maxExt [list [dict get $exts xMax] [dict get $exts yMax] [dict get $exts zMax]]
    
    set blkInt {} ; #interior blocks
    foreach blk $blksList { 
        set blkExt [$blk getExtents]
        set minPos [lindex $blkExt 0]
        set maxPos [lindex $blkExt 1]
        
        # Define tolerance for extents search. Use grid resolution as the tolerance
        set resBlk [GetBlockResolution $blk]
        set tol [expr {max ([dict get $resBlk iMax],[dict get $resBlk jMax])}]
        #puts "Block [$blk getName] tolerance: $tol"
        
        # minimum and maximum extent check       
        if {([PositiveVec [pwu::Vector3 subtract $minPos $minExt] $tol]) && ([PositiveVec [pwu::Vector3 subtract $maxExt $maxPos] $tol])} {
            lappend blkInt $blk
            incr b
        }
    }
    unset minExt maxExt blk blkExt minPos maxPos
    return $blkInt
}



#-- Get extents of mutiple sibling blocks 
#   Args: Refinement level and the block with siblings key 
#   Return: Merged extents at the prescibed level 
proc GetMergedBlkExts {Level blkDict} {
    global blkSplExt

    if {[dict exists $blkDict siblingBlks]} {
        set siblings [join [dict get $blkDict siblingBlks]]
        
        foreach mem $siblings {
            set mKey [join [list $Level $mem] ","]        
            set mDict [dict get $blkSplExt $mKey]
        
            lappend min(x) [dict get $mDict xMin]
            lappend min(y) [dict get $mDict yMin]
            lappend min(z) [dict get $mDict zMin]
            lappend max(x) [dict get $mDict xMax]
            lappend max(y) [dict get $mDict yMax]
            lappend max(z) [dict get $mDict zMax]
        }
        
        set mergedExts [dict create\
            xMin  [lindex [lsort -real $min(x)] 0]\
            yMin  [lindex [lsort -real $min(y)] 0]\
            zMin  [lindex [lsort -real $min(z)] 0]\
            xMax  [lindex [lsort -real $max(x)] end] \
            yMax  [lindex [lsort -real $max(y)] end] \
            zMax  [lindex [lsort -real $max(z)] end] \
            siblingBlks $siblings]
    return $mergedExts
    }
}


#-- SplitBlk (Splits a stuctured block by direction and coordinates
proc SplitBlk {block2Trim dir locations} {
    set split_params [list ]
    
    # Catch error if block extents not found and exit the proc
    if {[catch {set exts [$block2Trim getExtents]}]} {
        puts "!!! Extents not found for a block"
        return
    }
    
    set dimBlk [$block2Trim getDimensions]
    #puts [$block2Trim getName]
    
    set blk(xMin) [lindex [lindex $exts 0] 0] 
    set blk(yMin) [lindex [lindex $exts 0] 1] 
    set blk(zMin) [lindex [lindex $exts 0] 2] 
    
    #parray blk
    
    if {$dir == "I"} {
        set i 0
        foreach _corrd $locations {
            set loc [lindex $locations $i]
            lappend split_params [lindex [$block2Trim closestCoordinate -boundary [list $loc $blk(yMin) $blk(zMin)]] 0];
            incr i
        }
        set blkSplited [$block2Trim split -I $split_params];
    
    } elseif {$dir == "J"} {
        set i 0
        foreach _corrd $locations {
            set loc [lindex $locations $i]
            lappend split_params [lindex [$block2Trim closestCoordinate -boundary [list $blk(xMin) $loc $blk(zMin)]] 1];
            incr i
        }
        set blkSplited [$block2Trim split -J $split_params];
        
    } elseif {$dir == "K"} {
        set i 0
        foreach _corrd $locations {
            set loc [lindex $locations $i]; incr i
            set splVal [lindex [$block2Trim closestCoordinate -boundary [list $blk(xMin) $blk(yMin) $loc]] 2]
            #puts $splVal
            if {($splVal != [lindex $dimBlk 2])} {
                lappend split_params $splVal;
            }
        }
        set blkSplited [$block2Trim split -K $split_params];
    }
    
    unset i loc split_params blk block2Trim dir locations
    return $blkSplited
}


#-- Get trim extents of a structured block
#   Trim the block by number of buffer cells required to create the buffer block
#   Args: blk2Trim > the block to trim down
#         nBufferCell (global), number of buffer cell btween trimmed and surrounding blocks
#   Return: trimExt > triming extents
proc GetTrimExt {blk2Trim} {    
    global nBufferCell
    
    set splExt [GetBlklExtDict [$blk2Trim getExtents]]
    set resVal [GetBlockResolution $blk2Trim];  # Get Structured child block grid resolution

    # Define Refined Block Extents
    dict set trimExt xMin [expr {[dict get $splExt xMin] + [dict get $resVal iMax]*$nBufferCell}]
    dict set trimExt yMin [expr {[dict get $splExt yMin] + [dict get $resVal jMax]*$nBufferCell}]
    dict set trimExt zMin [dict get $splExt zMin]

    dict set trimExt xMax [expr {[dict get $splExt xMax] - [dict get $resVal iMax]*$nBufferCell}]; 
    dict set trimExt yMax [expr {[dict get $splExt yMax] - [dict get $resVal jMax]*$nBufferCell}]; 
    dict set trimExt zMax [expr {[dict get $splExt zMax] - [dict get $resVal kMax]*$nBufferCell}];
    
    return $trimExt
}


#-- Get refinement zone extenst out of wakeinfo
#   Args: wakeRefInfo, a list of ref-zone direction and length
#   Return: refVec, refinement extents for each site 
proc GetWakeRefExts {wakeRefInfo} { 
    set nRef [llength $wakeRefInfo]
    set refVec [list ]
    
    for {set n 0} {$n<$nRef} {incr n} {
        set dir   [lindex [lindex $wakeRefInfo $n] 0]
        set len   [lindex [lindex $wakeRefInfo $n] 1]
        
        if {[string match "x*" $dir] && ($len>=0)} {
            lappend refVec [list {0 0 0} [list $len 0 0]]
        } elseif {[string match "x*" $dir] && ($len<0)} {
            set len [expr {abs($len)}]
            lappend refVec [list [list $len 0 0] {0 0 0}]
        } elseif {[string match "y*" $dir] && ($len>=0)} {
            lappend refVec [list {0 0 0} [list 0 $len 0]]
        } elseif {[string match "y*" $dir] && ($len<0)} {
            set len [expr {abs($len)}]
            lappend refVec [list [list 0 $len 0] {0 0 0}]
        } else {
            lappend refVec [list  {0 0 0} {0 0 0}]
            puts "Site $n: wake refinemt info is not defined!"
        }
    }
    return $refVec
}



#-- Get spliting extents for each turbine at each refinement levels
#   Args: 
#         extWT: Wind Turbine Mesh extents
#         nLevel: number of refinement levels
#         refZone: buffer dimention between two refineMent zones (shall be same length as nLevel)
#         nSite: number of sitting positions
#         sitingPos: list of siting positions
  
#   DependentProc: GetBlklExtDict, UnsetEntities 
#   Return: splExt(local) extents of wind turbine mesh refinement levels
proc GetAllSplExts {extWT nLevel refZone nSite sitingPos refVec} { 
    for {set l 0} {$l<=$nLevel} {incr l} {
        if {$l==0} {
            # Get level zero  Extents --------------------------------------------------------------------------------- #
            for {set n 0} {$n<$nSite} {incr n} {
                set key [join [list $l $n] ","]
                set tempPos [lindex $sitingPos $n]
                set wtMeshPos($n) [pwu::Vector3 set [lindex $tempPos 0] [lindex $tempPos 1] [lindex $tempPos 2]]

                set ext(0,$n) [list [pwu::Vector3 add  $wtMeshPos($n) [lindex $extWT 0]] \
                                    [pwu::Vector3 add  $wtMeshPos($n) [lindex $extWT 1]] ]
                
                dict set splExt $key [GetBlklExtDict $ext(0,$n)]
            }
        } elseif {$l >=1} {; #-- Level 1+ 
            # write the refinement zone extents 
            set lm1 [expr {$l-1}]; # l minus 1; the previous level values   
            set buffer($l) [list [lindex $refZone $lm1] [lindex $refZone $lm1]]
            
            for {set n 0} {$n< [llength $sitingPos]} {incr n} {
                # Add refinement zone starting from level 2
                if {$l >=2} {
                    set ref [lindex $refVec $n]
                    set buffer($l) [list [pwu::Vector3 add  [lindex $ref 0] [lindex $refZone $lm1]] \
                                         [pwu::Vector3 add  [lindex $ref 1] [lindex $refZone $lm1]] ]
                }
                
                set key [join [list $l $n] ","]
                set ext($l,$n) [list [pwu::Vector3 subtract  [lindex $ext($lm1,$n) 0] [lindex $buffer($l) 0]] \
                                     [pwu::Vector3 add       [lindex $ext($lm1,$n) 1] [lindex $buffer($l) 1]] ]
                
                dict set splExt $key [GetBlklExtDict $ext($l,$n)]
            }
        } 
    }   
    UnsetEntities [list buffer l lm1 n tempPos wtMeshPos ext ref nLevel refZone nSite sitingPos refVec] 
    
    return $splExt
}


#-- Get min and max coordinates from the block spliting extents
#   Args: l > level of refinement
#         mergedBlkSplExt> extents of the merged blocks
#         nMergedBlks(global) > number of merged blocks in each refinement level
#   Dependent Procs: UnsetEntities
#   Return: coord dict with x, y and z keys
proc GetCoords { l mergedBlkSplExt} {
    global nMergedBlks
    set mCount [dict get $nMergedBlks $l]
    
    for {set n 0} {$n<$mCount} {incr n} {
        set nKey [join [list $l $n] ","]        
        set nDict [dict get $mergedBlkSplExt $nKey]
        
        lappend coordinates(x) [dict get $nDict xMin] 
        lappend coordinates(x) [dict get $nDict xMax] 
        lappend coordinates(y) [dict get $nDict yMin] 
        lappend coordinates(y) [dict get $nDict yMax] 
        lappend coordinates(z) [dict get $nDict zMin] 
        lappend coordinates(z) [dict get $nDict zMax] 
    } 
    
    # HardCoded, Case Specific: remove z,=0.0 corrdinate location s
    foreach el $coordinates(z) {
        if {$el <= 0.0} { lremove coordinates(z) $el }
    }
    
    dict set coord x [lsort -real -unique $coordinates(x)]
    dict set coord y [lsort -real -unique $coordinates(y)]
    dict set coord z [lsort -real -unique $coordinates(z)]
    
    UnsetEntities [list l n nKey nDict el coordinates]
    
    return $coord
}    



#-- Split block(s) (blkParent > global) at a refinement level ($l) by its spliting coordinates (I&J)
#   Find interior blocks, joind and split in K direction
#   Args: l > refinement level
#         coordinates > spliting coordinate dictionary with x, y and z keys
#         blkParent > the dictinary of blocks on which trim and reifine operation will be applied
#         nMergedBlks(global) > number of merged blocks in each refinement level
#   DependentProc: SplitBlk, FindInteriorBlks, ListOperation, UnsetEntities
#   Write: blkChildren (global) > children blocks
#   Return: None
proc SplBlkIJK { l coordinates blkParent mergedBlkSplExt levelPrimaryLayer} {
    
    global nMergedBlks
    
    set lm1 [expr {$l-1}]; # l minus 1; one level finner values
    set lp1 [expr {$l+1}]; # l plus 1; one level coarser values
    
    set iLocs [dict get $coordinates x]
    set jLocs [dict get $coordinates y]
    set kLocs [dict get $coordinates z]
    
    # Split in I direction ------------------#
    set blocksI [list ]
    set blkCount [dict get $nMergedBlks $lp1]    
    
    for {set n 0} {$n<$blkCount} {incr n} {
        set nKey [join [list $l $n] ","]
        set blk2Split [dict get $blkParent $nKey]
        
        set blocksI [concat $blocksI [SplitBlk $blk2Split "I" $iLocs]]
    }
    
    # Split in J direction ------------------#
    set blkFamily [list ]
    foreach blk $blocksI {
        set blkFamily [concat $blkFamily [SplitBlk $blk "J" $jLocs]]
    }

    # Find the inerior blocks, isolate and join, trim in vertical direction, transfer the bottom block to a new layer 
    set mCount [dict get $nMergedBlks $l]
    for {set n 0} {$n <$mCount} {incr n} {
        set nKey [join [list $l $n] ","]
        set merExt [dict get $mergedBlkSplExt $nKey]
    
        set tempBlks [FindInteriorBlks $merExt $blkFamily]; # Find Interior blocks
        set blkFamily [ListOperation $blkFamily $tempBlks "subtract"]; # Discard the current interior blocks from blocks-family list
        
        set tempBlks [JoinStucBlks $tempBlks ]; # flag "parents" off. # join the structured blocks inside the extents
        set tempBlks [SplitBlk $tempBlks "K" $kLocs]; # split the block in vertical direction
        
        #*** sequence of assigning layers with parents tag is important.
        # the latest layer assignment have the commonly shared domains in that layer.
        catch {
            [lindex $tempBlks 1] setLayer -parents $levelPrimaryLayer; #;  assign top block to level primary layer
        }
        
        # the bottom block
        set botBlock [lindex $tempBlks 0]
        $botBlock setLayer -parents [expr {int($levelPrimaryLayer+$n*2+1)}]; # Assign to a new layer with domains  
        SetEnts2Layer [GetConnectors [GetDomains $botBlock]] [expr {int($levelPrimaryLayer+$n*2+1)}]; # Assign conns too
        $botBlock setRenderAttribute FillMode Shaded ; # Optional
        dict set blkChildren $nKey $botBlock; # assign the bottom block to dictionary with key
        
        unset tempBlks 
    }

    # Join rest of the exterior blocks
    set blkFamily [JoinStucBlks $blkFamily];# flag "parents" off; Doesn't want to join domains and concectors

    # Find which domains and connectors are exclusively associated with outter family blocks
    # Using the check delete function
    pw::Entity checkDelete -freed freedEnts $blkFamily
    SetEnts2Layer [concat $freedEnts $blkFamily] $levelPrimaryLayer 
    
    UnsetEntities [list lm1 lp1 iLocs jLocs kLocs coordinates nKey merExt mergedBlkSplExt blkParent blk2Split blkFamily\
                    blocksI botBlock ]
    
    return $blkChildren
}



# --------------------------------------------------------------------------#
#-- Refines a structured block and fill the gap between coarse and fine blocks with
#   an unstructured buffer block. 
#   Args: parent structured block and split extents (min and max values in 3 directions)
#   Return: a dictorant of splited, refined and buffer blocks
proc RefBlkFillBufferNew {blk2Refine splExt {initialize ""}} {
    
    global refinementFactor nBufferCell
    
    # Set current working layer
    set currLayer1 [$blk2Refine getLayer]
    set currLayer2 [expr {$currLayer1 + 1}]
    
    pw::Display setCurrentLayer $currLayer1
    puts "Current Layer: $currLayer1 "
    
    #-- Stage 1: Trim the strucutred block; clean up byproduct blocks and domains
    # Move domain (& associated connectors) to same layer as the block
    set domsRefBlk [GetDomains $blk2Refine] 
    SetEnts2Layer $domsRefBlk $currLayer1
    SetEnts2Layer [GetConnectors $domsRefBlk] $currLayer1
    
    # Trim the child block
    set refExt [GetTrimExt $blk2Refine]
    set blkTrim [SplitBlock $blk2Refine $refExt] 
    
    # The bottom trimmed domain
    set blkBottom [dict get $blkTrim bottom]

    #-- Delete blocks surrounding blocks around trimmed block and interfacial domains & connectors
    #   get the free enties upon delete
    set tmpBlks [list [dict get $blkTrim west] [dict get $blkTrim east] \
                      [dict get $blkTrim north] [dict get $blkTrim south] [dict get $blkTrim top]]
    pw::Entity checkDelete -freed tmpEnts $tmpBlks
       
    # Find the domains on database (which are belong to the trim blocks)
    set domsOnDB [list ]
    foreach ent $tmpEnts {
        if {[$ent isOfType pw::DomainStructured] && ([$ent getEllipticSolverAttribute ShapeConstraint] ne "Free")} {
            ladd domsOnDB $ent
        }
    }
    
    # connectors on Database
    set connsOnDB [GetConnectors $domsOnDB]
    set connsOnDB [ListOperation $connsOnDB $tmpEnts "subtract"]
    
    # Delete by-products
    set tmpEnts [ListOperation $tmpEnts $connsOnDB "subtract"]
    pw::Entity delete [concat $tmpBlks $tmpEnts]
    #------------------------------------------------------------------------- # 
    
    
    #-- Stage 2: Refine the block and built an unstructured domain on the database-----------------#
    #Get all connectors in the trimmed block and REFINE Block  
    set connsInTrimBlk [GetConnectors [GetDomains $blkBottom]] 
    RedimensionConnectors $connsInTrimBlk
       
    # Get all domains from the currLayer1
    set domsList [pw::Layer getLayerEntities -type pw::Domain $currLayer1]
    #puts "Length of Domains List in layer $currLayer1: [llength $domsList]"
    
    # the bottom domains and rest of (Other) domains and 
    set domsBottom [list ]
    set domsOther [list ]
    
    foreach dom $domsList {
        if {[$dom isOfType pw::DomainStructured]} {
            if {([$dom getEllipticSolverAttribute ShapeConstraint] eq "Free")} {
                lappend domsOther $dom
            } else {
                lappend domsBottom $dom
            }
        }
    }
    #SetEnts2Layer $domsOther 21
    
    # Get all connectors in the trimmed block and REFINE Block  
    set connsInBottom [GetConnectors $domsBottom] 

    # Create buffer domain on databases-----------------------#
    # First identify connectors for the outer edge and the innner edge
    set connsIn [ListOperation $connsOnDB $connsInBottom "common" ]
    set connsOut [ListOperation $connsOnDB $connsInBottom "subtract"]
        
    puts "Number of Connectors in the Refined Block floor: [llength $connsIn]"
    puts "Number of Connectors out of Refined Block: [llength $connsOut]"
    
    # Create Bottom Domain 
    pw::Application setGridPreference Unstructured
    
    set nCon 0; set avgSpacing 0
    foreach _con [concat $connsIn] {
        incr nCon
        Add avgSpacing [$_con getAverageSpacing]
        #puts "[$_con getName] $avgSpacing"
    }

    set avgSpacing [expr {$avgSpacing/$nCon}]
    puts "Average Spacing of inner connectors $avgSpacing"
    pw::DomainUnstructured setDefault EdgeMinimumLength [expr {$avgSpacing*(1+ abs($refinementFactor-1)/2)}]
    
    #  *** in times non-empty domains get created with overlapped cells.
    set domBuffer  [CreateUnsDom $connsOut $connsIn  "reverse"]
    if {[$domBuffer getInteriorState] eq "Empty"} {
        pw::Entity delete $domBuffer
        puts "Altering inner edge orientation"
        set domBuffer  [CreateUnsDom $connsOut $connsIn]  
    }

    puts "[$domBuffer getName] interior state: [$domBuffer getInteriorState] "
    #------------------------------------------------------------------------------------# 
    
    # Stage 3: Build the unstructured buffer block ----------------------------------#
    pw::BlockUnstructured setDefault PyramidAspectRatio 1
    pw::DomainUnstructured setDefault IsoCellType Triangle
    
    # build the domains list--------------------#    
    pw::BlockUnstructured setDefault TRexCellType TetPyramidPrismHex
    set blkBufffer [pw::BlockUnstructured createFromDomains [concat $domsOther $domBuffer]] 
#     $blkBufffer setName "blkBuffer"
    
    #-- Initialize block
    if {[string match "ini*" $initialize]} {
        set initializeBlk [pw::Application begin UnstructuredSolver [list $blkBufffer]]
        $initializeBlk run Initialize
        $initializeBlk end
        unset initializeBlk
    }

    #-- Asign the bufferblock to split dictionary
    set blkSpl [dict create buffer $blkBufffer bottom $blkBottom children [dict get $splExt siblingBlks]]
    
    # Assign bottom block to the next layer
    #$blkBottom setLayer -parents $currLayer2;
    
    # unset variables
    UnsetEntities [list nCon nBlk avgSpacing currLayer1 currLayer2 connsIn connsOnDB connsInBottom \
                     connsInTrimBlk ent tmpEnts dom domsList domsBottom domsOther domsOnDB domBuffer \
                     blkBufffer blkBottom blkTrim blk2Refine refExt resVal splExt _TMP(freed)]
    
return $blkSpl
}



#-- Paste a list of entities at prescribed location.
#   *** the proc clear clipboard (use with caution)
#   Args: 
#        ents2Copy > entitityList to be copied
#        pos > (a vector) position to place the entities
#   Return: entsPasted > the enties generated though this copy-paste operation
proc PasteEntities {ents2Copy pos} {
    pw::Application clearClipboard
    pw::Application setClipboard $ents2Copy

    set pasteMode [pw::Application begin Paste]
        set entsPasted [$pasteMode getEntities]
        set modifyMode [pw::Application begin Modify $entsPasted]
            pw::Entity transform [pwu::Transform translation $pos] [$modifyMode getEntities]; 
        $modifyMode end
        unset modifyMode
    $pasteMode end
    unset pasteMode
    
    pw::Application clearClipboard
    
    return $entsPasted
}


#END_OF_CUSTOM_PROCEDURES
# ---------------------- Custom Procedures End -------------------------------------------------------#

