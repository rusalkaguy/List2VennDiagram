#
# Take lists, compute intesections and build VennDiagrams
#
require(VennDiagram)  # install.packages("VennDiagram")

#
#' Venn Diagram from a list of sets
#'
#' Given a list of 2 to 5 sets, draw the Venn Diagram depecting their overlap.
#' All parameters for VennDiagram will be passed trhough.
#' @export
#' @param sets List of sets (2-5)
#' @param sets_col vector mapping set names to colors
#' @param ... passed through to VennDiagram::draw.*.venn
#'
#' @examples
#' x=vennSets(list(A=c(1:5),B=c(3:10)),c("A"="red", "B"="blue"))
#' y=vennSets(list(A=c(1:5),B=c(3:10),C=c(8:20,1)),c("A"="red", "B"="blue", "C"="green"))
vennSets = function(sets, sets_col, ...) {
  count = length(sets)
  if(count==2) { vennPair(sets,sets_col, ...)}
  else if(count==3) { vennTriple(sets,sets_col, ...)}
  else if(count==4) { vennQuadSets(sets,sets_col, ...)}
  else if(count==5) { vennQintuple(sets,sets_col, ...)}
  else { stop("list of sets must contain 2-5 sets; yours contained ", count, " set(S)")}
}

#
#' 2 Set Venn
#'
#' Given a list of sets, draw the Venn Diagram depecting their overlap.
#' All parameters for VennDiagram will be passed trhough.
#' @export
#' @param sets List of sets
#' @param sets_col vector mapping set names to colors
#' @param ... passed through to VennDiagram::draw.pairwise.venn
#'
#' @examples
#' x=vennPair(list(A=c(1:5),B=c(3:10)),c("A"="red", "B"="blue"))
vennPair = function(sets, sets_col, ...) {
  draw.pairwise.venn(
    area1=length(sets[[1]])
    , area2=length(sets[[2]])
    , cross.area	= length(intersect(sets[[1]],sets[[2]])) # n12
    , category=names(sets)
    , fill=sets_col[names(sets)]
    , cat.col = sets_col[names(sets)]
    , cat.cex=1.5
    , ...
  )
}

#
#' 3 Set Venn (Euler)
#'
#' Given a list of sets, draw the Venn Diagram depecting their overlap.
#' All parameters for VennDiagram will be passed trhough.
#' @export
#' @param sets List of sets
#' @param sets_col vector mapping set names to colors
#' @param ... passed through to VennDiagram::draw.triple.venn
#'
#' @examples
#' x=vennTriple(list(A=c(1:5),B=c(3:10),C=c(8:20,1)),c("A"="red", "B"="blue", "C"="green"))
#
vennTriple = function(sets, sets_col,...) {
  overrideTriple=TRUE;
  draw.triple.venn(
    #print(c(
    area1=length(sets[[1]])
    , area2=length(sets[[2]])
    , area3=length(sets[[3]])
    , n12	= length(intersect(sets[[1]],sets[[2]]))
    , n13	= length(intersect(sets[[1]],sets[[3]]))
    , n23	= length(intersect(sets[[2]],sets[[3]]))
    , n123 = length(intersect(intersect(sets[[1]],sets[[2]]),sets[[3]]))
    , category=names(sets)
    , fill=sets_col[names(sets)]
    , cat.col = sets_col[names(sets)]
    , cat.cex=1.5
    ,...
  )
}


#
#' 4 Set Venn (Euler)
#'
#' Given a list of sets, draw the Venn Diagram depecting their overlap.
#' All parameters for VennDiagram will be passed through.
#' @export
#' @param sets List of sets
#' @param sets_col vector mapping set names to colors
#' @param ... passed through to VennDiagram::draw.quad.venn
#'
#' @examples
#'
#' x=vennQuadSets(list(A=c(1:5),B=c(3:10),C=c(8:20,1),D=c(1:2,3:4,15:20,99:100)),c("A"="red", "B"="blue", "C"="green", "D"="purple"))
#
vennQuadSets = function(sets, sets_col, ...) {
  draw.quad.venn(
    #print(c(
    area1=length(sets[[1]])
    , area2=length(sets[[2]])
    , area3=length(sets[[3]])
    , area4=length(sets[[4]])
    , n12	= length(intersect(sets[[1]],sets[[2]]))
    , n13	= length(intersect(sets[[1]],sets[[3]]))
    , n14	= length(intersect(sets[[1]],sets[[4]]))
    , n23	= length(intersect(sets[[2]],sets[[3]]))
    , n24	= length(intersect(sets[[2]],sets[[4]]))
    , n34	= length(intersect(sets[[3]],sets[[4]]))
    # The size of the intersection between the first, second and third sets
    , n123 = length(intersect(intersect(sets[[1]],sets[[2]]),sets[[3]]))
    , n124 = length(intersect(intersect(sets[[1]],sets[[2]]),sets[[4]]))
    , n134	= length(intersect(intersect(sets[[1]],sets[[3]]),sets[[4]]))
    , n234	= length(intersect(intersect(sets[[2]],sets[[3]]),sets[[4]]))
    , n1234	= length(intersect(intersect(sets[[1]],sets[[2]]),intersect(sets[[3]],sets[[4]])))
    , fill=sets_col[names(sets)]
    , cat.col = sets_col[names(sets)]
    , cat.cex=1.5
    #, category=names(sets)
    # Add set size counts to category labels - how to make this conditional
    #,category=paste(names(sets),"\nn=",lapply(sets, length),sep="")
    , ...

  )
}

#
#' 5 Set Venn (Euler)
#'
#' Given a list of sets, draw the Venn Diagram depecting their overlap.
#' All parameters for VennDiagram will be passed through.
#' @export
#' @param sets List of sets
#' @param sets_col vector mapping set names to colors
#' @param ... passed through to VennDiagram::draw.quintuple.venn
#'
#' @examples
#'
#' x=vennQintuple(list(A=c(1:5),B=c(3:10),C=c(8:20,1),D=c(1:2,3:4,15:20,99:100),E=(50:100)),c("A"="red", "B"="blue", "C"="green", "D"="purple", "E"="beige"))
#
vennQintuple = function(sets, sets_col, ...) {
  draw.quintuple.venn(
    #print(c(
    area1=length(sets[[1]])
    , area2=length(sets[[2]])
    , area3=length(sets[[3]])
    , area4=length(sets[[4]])
    , area5=length(sets[[5]])
    , n12	= length(intersect(sets[[1]],sets[[2]]))
    , n13	= length(intersect(sets[[1]],sets[[3]]))
    , n14	= length(intersect(sets[[1]],sets[[4]]))
    , n15	= length(intersect(sets[[1]],sets[[5]]))
    , n23	= length(intersect(sets[[2]],sets[[3]]))
    , n24	= length(intersect(sets[[2]],sets[[4]]))
    , n25	= length(intersect(sets[[2]],sets[[5]]))
    , n34	= length(intersect(sets[[3]],sets[[4]]))
    , n35	= length(intersect(sets[[3]],sets[[5]]))
    , n45	= length(intersect(sets[[4]],sets[[5]]))
    # The size of the intersection between the first, second and third sets
    , n123 = length(intersect(intersect(sets[[1]],sets[[2]]),sets[[3]]))
    , n124 = length(intersect(intersect(sets[[1]],sets[[2]]),sets[[4]]))
    , n125 = length(intersect(intersect(sets[[1]],sets[[2]]),sets[[5]]))
    , n134	= length(intersect(intersect(sets[[1]],sets[[3]]),sets[[4]]))
    , n135	= length(intersect(intersect(sets[[1]],sets[[3]]),sets[[5]]))
    , n145	= length(intersect(intersect(sets[[1]],sets[[4]]),sets[[5]]))
    , n234	= length(intersect(intersect(sets[[2]],sets[[3]]),sets[[4]]))
    , n235	= length(intersect(intersect(sets[[2]],sets[[3]]),sets[[5]]))
    , n245	= length(intersect(intersect(sets[[2]],sets[[4]]),sets[[5]]))
    , n345	= length(intersect(intersect(sets[[3]],sets[[4]]),sets[[5]]))
    , n1234	= length(intersect(intersect(sets[[1]],sets[[2]]),intersect(sets[[3]],sets[[4]])))
    , n1235	= length(intersect(intersect(sets[[1]],sets[[2]]),intersect(sets[[3]],sets[[5]])))
    , n1245	= length(intersect(intersect(sets[[1]],sets[[2]]),intersect(sets[[4]],sets[[5]])))
    , n1345	= length(intersect(intersect(sets[[1]],sets[[3]]),intersect(sets[[4]],sets[[5]])))
    , n2345	= length(intersect(intersect(sets[[2]],sets[[3]]),intersect(sets[[4]],sets[[5]])))
    , n12345	= length(intersect(sets[[1]],intersect(intersect(sets[[2]],sets[[3]]),intersect(sets[[4]],sets[[5]]))))
    , fill=sets_col[names(sets)]
    #, cat.col = sets_col[names(sets)]
    , cat.cex=1.0
    #, category=names(sets) - how to make this conditional
    # Add set size counts to category labels - how to make this conditional
    #,category=paste(names(sets),"\nn=",lapply(sets, length),sep="")
    , ...

  )
}
