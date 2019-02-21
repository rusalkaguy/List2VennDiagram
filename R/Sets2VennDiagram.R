#
# Take a list of sets, compute intesections and build VennDiagrams
#

#' Venn Diagram from a list of sets
#'
#' Given a list of 2 to 5 sets, draw the Venn Diagram depecting their overlap.
#' All parameters for VennDiagram will be passed trhough.
#' @export
#' @param sets A List of 2-5 sets. If names are set, they will be used as category names.
#' @param set.col A vector (length=sets) of colors for category fill and cat.col. If names are set, they will over-ride the sets names
#' @param set.counts A boolean, if TRUE, the total number items in each category is appended to the title. If a string, then that is used as the separator, instead of a newline.
#' @param category A vector (length=sets) of display names for the categories, which will override the names from sets and set.col.
#' @param cat.col A vector (length=sets) giving the colours of the category names
#' @param fill A vector (length=sets) giving the colours of the circles' areas
#' @param ... passed through to VennDiagram::draw.*.venn
#'
#' @examples
#' x=vennSets(list(c(1:5),c(3:10)))
#' y=vennSets(list(A=c(1:5),B=c(3:10),C=c(8:20,1)))
#' x=vennSets(list(A=c(1:5),B=c(3:10)),c("X"="red", "Y"="blue"), cat.counts=T)
#' y=vennSets(list(1:5,3:10,c(8:20,1)),
#'          set.col=c("red", "blue", "green"),
#'          category=paste("Set",1:3), set.counts=": members=")
#'
vennSets = function(sets,  set.col=NULL, set.counts=F, category=NULL, cat.col=NULL, fill=NULL, ...) {
  # valid number of sets
  n_sets = length(sets)
  if(n_sets < 2 || n_sets > 5 ) { stop("vennSets: sets list must contain 2-5 sets; yours contained ", n_sets, " set(S)") }
  # validate lengths of other non-null vectors
  if(length(set.col) && length(set.col)!=n_sets) { stop("set.col must be same length as sets:", length(set.col), "!=", n_sets) }
  if(length(category) && length(category)!=n_sets) { stop("category must be same length as sets:", length(category), "!=", n_sets) }
  if(length(cat.col) && length(cat.col)!=n_sets) { stop("cat.col must be same length as sets:", length(cat.col), "!=", n_sets) }
  if(length(fill) && length(fill)!=n_sets) { stop("fill must be same length as sets:", length(fill), "!=", n_sets) }

  # cateogry names
  # 1. category argument, if set
  # 2. set.col names, if set
  # 3. sets names, if set
  if(!length(category) && length(names(set.col))) { category=names(set.col) }
  if(!length(category) && length(names(sets)))     { category=names(sets)     }
  if(!length(category))  { category=rep("", n_sets) }  # default from VennDiagram

  # Add set size counts to category labels - how to make this conditional
  if(set.counts!=F) {
    sep = "\nn="; if(set.counts!=T) { sep = set.counts }
    category=paste(category,sep,lapply(sets, length),sep="")
  }

  # category and fill colors
  # 1. cat.col & fill args
  # 2. set.col values
  if(!length(cat.col) && length(set.col) ) { cat.col=set.col }
  if(!length(fill)    && length(set.col) ) { fill=   set.col }
  if(length(cat.col)==0) { cat.col=rep("black", n_sets) }  # default from VennDiagram

  # dispatch to correct underying intersect and draw function
  if(n_sets==2)      { vennSetPair(sets,category=category, fill=fill, cat.col=cat.col,...)}
  else if(n_sets==3) { vennSetTriple(sets,category=category, fill=fill, cat.col=cat.col, ...)}
  else if(n_sets==4) { vennSetQuad(sets,category=category, fill=fill, cat.col=cat.col, ...)}
  else if(n_sets==5) { vennSetQintuple(sets, category=category, fill=fill, cat.col=cat.col, ...)}
}


#' 2 Set Venn
#'
#' Given a list of sets, draw the Venn Diagram depecting their overlap.
#' All parameters for VennDiagram will be passed trhough.
#' @export
#' @param sets A List of 2 sets, one for each category of the diagram
#' @param ... passed through to VennDiagram::draw.pairwise.venn
#'
#' @examples
#' x=vennSetPair(list(A=c(1:5),B=c(3:10)))
# x=vennSetPair(list(A=c(1:5),B=c(3:10)),c("A"="red", "B"="blue"))
vennSetPair = function(sets, ...) {
  VennDiagram::draw.pairwise.venn(
    area1=length(sets[[1]])
    , area2=length(sets[[2]])
    , cross.area	= length(intersect(sets[[1]],sets[[2]])) # n12
    , ...
  )
}

#
#' 3 Set Venn (Euler)
#'
#' Given a list of sets, draw the Venn Diagram depecting their overlap.
#' All parameters for VennDiagram will be passed trhough.
#' @export
#' @param sets A List of 3 sets, one for each category of the diagram
#' @param ... passed through to VennDiagram::draw.triple.venn
#'
#' @examples
#' x=vennSetTriple(list(A=c(1:5),B=c(3:10),C=c(8:20,1)),c("A"="red", "B"="blue", "C"="green"))
#
vennSetTriple = function(sets,...) {
  overrideTriple=TRUE;
  VennDiagram::draw.triple.venn(
    area1=length(sets[[1]])
    , area2=length(sets[[2]])
    , area3=length(sets[[3]])
    , n12	= length(intersect(sets[[1]],sets[[2]]))
    , n13	= length(intersect(sets[[1]],sets[[3]]))
    , n23	= length(intersect(sets[[2]],sets[[3]]))
    , n123 = length(intersect(intersect(sets[[1]],sets[[2]]),sets[[3]]))
    ,...
  )
}


#
#' 4 Set Venn (Euler)
#'
#' Given a list of sets, draw the Venn Diagram depecting their overlap.
#' All parameters for VennDiagram will be passed through.
#' @export
#' @param sets A List of 4 sets, one for each category of the diagram
#' @param ... passed through to VennDiagram::draw.quad.venn
#'
#' @examples
#'
#' x=vennSetQuad(list(A=c(1:5),B=c(3:10),C=c(8:20,1),D=c(1:2,3:4,15:20,99:100)))
# x=vennSetQuad(list(A=c(1:5),B=c(3:10),C=c(8:20,1),D=c(1:2,3:4,15:20,99:100)),c("A"="red", "B"="blue", "C"="green", "D"="purple"))
#
vennSetQuad = function(sets, ...) {
  VennDiagram::draw.quad.venn(
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
    , ...

  )
}

#
#' 5 Set Venn (Euler)
#'
#' Given a list of sets, draw the Venn Diagram depecting their overlap.
#' All parameters for VennDiagram will be passed through.
#' @export
#' @param sets A List of 5 sets, one for each category of the diagram
#' @param ... passed through to VennDiagram::draw.quintuple.venn
#'
#' @examples
#'
#' x=vennSetQintuple(list(A=c(1:5),B=c(3:10),C=c(8:20,1),D=c(1:2,3:4,15:20,99:100),E=(50:100)))
# x=vennSetQintuple(list(A=c(1:5),B=c(3:10),C=c(8:20,1),D=c(1:2,3:4,15:20,99:100),E=(50:100)),c("A"="red", "B"="blue", "C"="green", "D"="purple", "E"="beige"))
#
vennSetQintuple = function(sets, ...) {
  VennDiagram::draw.quintuple.venn(
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
    , ...

  )
}

#-------------------
# debug/dev bits
#-------------------
#require(VennDiagram)  # install.packages("VennDiagram")
# usethis::use_package("VennDiagram")
# devtools::document() # update docs, NAMESPACE
# devtools::build()  # packages into a tar.gz file
#

