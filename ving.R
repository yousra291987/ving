#! /usr/bin/Rscript

RetreiveAndDestroy=function(opt,root,stem,regexp,SearchNames,Out,isValue,NbFound,StockNames) {
  Bool=lapply(paste(root,SearchNames,stem,sep=""),grepl,opt)
  names(Bool)=StockNames
  Pos=lapply(Bool,which)
  names(Pos)=StockNames
  disable=c()
  for(i in StockNames) {
    nbmatch=length(Pos[[i]])
    if(nbmatch>0) {
      NbFound[[i]]=NbFound[[i]]+nbmatch
      disable=c(disable,-1*Pos[[i]])
      if(is.null(Out[[i]])) {
	if(isValue[[i]]!=0) {
	  if(regexp=="next") {
	    Out[[i]]=opt[Pos[[i]]+1]
	    disable=c(disable,-1*(Pos[[i]]+1))
	  }
	  else {
	    Out[[i]]=sub(regexp,"\\1",opt[Pos[[i]]])
	  }
	}
	else {
	  Out[[i]]=TRUE
	}
      }
      else {
	if(isValue[[i]]!=0) {
	  if(regexp=="next") {
	    Out[[i]]=c(Out[[i]],opt[Pos[[i]]+1])
	    disable=c(disable,-1*(Pos[[i]]+1))
	  }
	  else {
	    Out[[i]]=c(Out[[i]],sub(regexp,"\\1",opt[Pos[[i]]]))
	  }
	}
	else {
	  Out[[i]]=c(Out[[i]],TRUE)
	}
      }
    }
  }
  if(length(disable)>0) {
    opt=opt[disable]
  }
  Out[["ARGUMENT"]]=list()
  Out[["ARGUMENT"]][["opt"]]=opt
  Out[["ARGUMENT"]][["NbFound"]]=NbFound
  return(Out)
}

getopt=function (spec=NULL,opt=commandArgs()) {
  FindArgs=which(opt=="--args")
  if(length(FindArgs)!=1) {
    stop(length(FindArgs)," --args found where 1 expected.",call.=F)
  }
  ExecName=sub("--file=","",opt[FindArgs-1])
  
  if(FindArgs<length(opt)) {
    opt=opt[(FindArgs+1):length(opt)]
  }
  else {
    opt=""
  }
  
  min.columns=5
  colNames=c("LongName","ShortName","Flag","Mod","Default")
  max.columns=6
  DimSpec=dim(spec)
  if(DimSpec[2]>min.columns) {
    colNames=c(colNames,"Description")
  }
  
  if(is.null(spec) | !is.matrix(spec) | (DimSpec[2]<min.columns | DimSpec[2]>max.columns)) {
    stop('argument "spec" is required and must be a matrix with 4|5 columns.',call.=F)
  }
  colnames(spec)=colNames
  
  spec=as.data.frame(spec,stringsAsFactors=F)
  #spec validation
  if(length(unique(c(spec$ShortName,"ARGUMENT","args")))!=DimSpec[1]+2 | length(unique(spec$LongName))!=DimSpec[1]) {
    stop('Long|Short names for flags must be unique (Long name : "ARGUMENT" and "args" forbidden).',
	"\n","List of duplicated :",
	"\n","Short: ",paste(spec$ShortName[duplicated(c(spec$ShortName,"ARGUMENT","args"))],collapse=" "),
	"\n","Long:  ",paste(spec$ShortName[duplicated(spec$LongName)],collapse=" "),call.=F)
  }
  if(length(which(nchar(spec$ShortName)>1))!=0) {
    stop('Short names flags can\'t be longer than 1 character.')
  }
  
  #initialize 
  Out=list()
  Short2Long=list()
  NbFound=list()
  isValue=list()
  for(i in 1:DimSpec[1]) {
    Short2Long[[spec$ShortName[i]]]=spec$LongName[i]
    NbFound[[spec$LongName[i]]]=0
    isValue[[spec$LongName[i]]]=spec$Flag[i]
  }
  
  #Map, retreive and suppress ARGUMENTs and arguments
  #Value ARGUMENT --example=value
  Out=RetreiveAndDestroy(opt,"^--","=.+$",".+=(.+)$",spec$LongName,Out,isValue,NbFound,spec$LongName)
  opt=Out[["ARGUMENT"]][["opt"]]
  NbFound=Out[["ARGUMENT"]][["NbFound"]]
  Out[["ARGUMENT"]]=NULL
  #boolean ARGUMENT --example
  Out=RetreiveAndDestroy(opt,"^--","$","$",spec$LongName,Out,isValue,NbFound,spec$LongName)
  opt=Out[["ARGUMENT"]][["opt"]]
  NbFound=Out[["ARGUMENT"]][["NbFound"]]
  Out[["ARGUMENT"]]=NULL
  #short name ARGUMENT -t value OR boolean -t
  Out=RetreiveAndDestroy(opt,"^-","$","next",spec$ShortName,Out,isValue,NbFound,spec$LongName)
  opt=Out[["ARGUMENT"]][["opt"]]
  NbFound=Out[["ARGUMENT"]][["NbFound"]]
  Out[["ARGUMENT"]]=NULL
  #Warn about non mapped ARGUMENTs
  if(length(opt)>0) {
    PosUnkArg=which(grepl("^-",opt))
    if(length(PosUnkArg)) {
      message("Error, argument unreconized :","\n",paste(opt[PosUnkArg],collapse="\n"),"\n\n")
    }
    if(length(PosUnkArg)>0) {
      opt=opt[PosUnkArg*-1]
    }
  }
  #Arguments
  Out[["ARGUMENT"]]=opt
  
  #Validation of ARGUMENTs
  for(i in 1:DimSpec[1]) {
    if(spec$Flag[i]=="0") {#verify boolean arguments
      NbValue=length(Out[[spec$LongName[i]]])
      if(NbValue>1) {
	message("Warning : ",spec$LongName[i]," found ",NbValue," times")
      }
    }
    if(length(Out[[spec$LongName[i]]])==0) {
      Out[[spec$LongName[i]]]=spec$Default[i]
    }
    library("methods")
    Out[[spec$LongName[i]]]=as(Out[[spec$LongName[i]]],spec$Mod[i])
  }
  
  return(Out)
}

## Converts the flag numbers into binary 
integer.base.b=function(x, b=2) {
  xi=as.integer(x)
  if(any(is.na(xi) | ((x-xi)!=0)))
    print(list(ERROR="x not integer", x=x))
  N=length(x)
  xMax=max(c(x,1))
  ndigits=11
  Base.b=array(NA, dim=c(N, ndigits))
  for(i in 1:ndigits) {#i=1
    Base.b[, ndigits-i+1]=(x %% b)
    x=(x %/% b)
  }
  Base.b
} 

## Checks if color used is an acceptable color
is.acceptable.color=function(character) {
  tmp=try(col2rgb(character),TRUE)
  return(class(tmp)!="try-error")
}

extractFromBam=function(file,which,what) {
  return(scanBam(file, param=ScanBamParam(which=which,what=what))[[1]][[1]])
}

## Returns a list from cigar expression
interpreteCIGAR=function(cigar) {
  cigar_un=strsplit(unique(cigar),split="")
  n_cigar_un=length(cigar_un)
  taille_cigar=list()
  analise_cigar=function(cigar_) {
    cigar_sortie=list()
    acc=""
    for(j in 1:length(cigar_)) {
      if(sum(cigar_[j]==as.character(0:9))==1) {
	acc=paste(acc,cigar_[j],sep="")
      }
      else {
	cigar_sortie[[length(cigar_sortie)+1]]=as.integer(acc)
	cigar_sortie[[length(cigar_sortie)+1]]=cigar_[j]
	acc=""
      }
    }
    return(cigar_sortie)
  }
  cigar_interprete=lapply(cigar_un,analise_cigar)
  names(cigar_interprete)=unique(cigar)

  return(cigar_interprete)
}

# prend un CIGAR splités et retourne la taille occupé par le read sur la séquence génomique (introns compris)
calcule_longueur_cigar=function(cigar_) {
  lon=0
  N=length(cigar_)
  for(j in seq(2,N,2)) {
    if(cigar_[[j]]!="I") {
      lon=lon+cigar_[[j-1]]
    }
  }
  return(lon)
}

# prend un CIGAR splités et retourne les positions 
calcule_junction_cigar=function(cigar_) {
  retour=list()
  lon=0
  N=length(cigar_)
  for(j in seq(2,N,2)) {
    if(cigar_[[j]]!="I") {
      lon=lon+cigar_[[j-1]]
    }
    if(cigar_[[j]]=="N") {
      retour[[length(retour)+1]]=c(lon-cigar_[[j-1]]+1,lon)
    }
  }
  return(retour)
}

## Returns a list of numbers of single read with their coordinates
compresse_coordonnees=function(debut,fin) {
  if(length(debut)==0) {
    return(list(numeric(),numeric(),numeric()))
  }
  else {
    tmp=sort(paste(debut,fin,sep="_"))
    tmp_rle=rle(tmp)
    poids=tmp_rle$lengths
    values_split=strsplit(tmp_rle$values,split="_")
    doit=function(j) {
      return(as.integer(values_split[[j]][1]))
    }
    debut_uni=sapply(1:length(poids),doit)
    doit=function(j) {
      return(as.integer(values_split[[j]][2]))
    }
    fin_uni=sapply(1:length(poids),doit)
    ordre_debut=order(debut_uni)
    return(list(debut_uni[ordre_debut],fin_uni[ordre_debut],poids[ordre_debut]))  
  }
}

RDataFileName=function(file) {
  return(paste(file,".RData",sep=""))
}

## Function converts and extracts the infos from bamfile 
readBam_=function(file_,insert_max_=2000,stranded_=TRUE,ncore_=1,libraryType_=c("standard","inverse"),fileNameRData_=NA,normalized_=NULL,chrName_=NULL,from_=1,to_=NULL) {
  suppressPackageStartupMessages(require("Rsamtools"))
  suppressPackageStartupMessages(require("GenomicRanges"))
## Declaration of variables
  flagstat=numeric(11)
  names(flagstat)=c("total","duplicates","mapped","paired","read1","read2","properly paired","itself and mate mapped","singletons","mate mapped on a different chr","QC-failed")
  genome_info=scanBamHeader(file_)[[1]]$targets
  noms_chromosomes=names(genome_info)
  longueur_chromosomes=as.integer(genome_info)
  nombre_chromosomes=length(noms_chromosomes)
  brin_F=list()
  brin_R=list()
  brin_F_junction=list()
  brin_R_junction=list()
  pas=c(1,2,6,7)
  i_zone=0
  if(is.null(chrName_)) {
    chrName__=noms_chromosomes
  }
  else {
    chrName__=chrName_
  }
## Fragments identification
  for(i in (1:nombre_chromosomes)) {
    i_zone=i_zone +1
    nom_chromo=noms_chromosomes[i]
    lon_chromo=longueur_chromosomes[i]
    
    if(!(nom_chromo %in% chrName__)) {
      brin_F[[i]]=list(numeric(),numeric(),numeric())
      brin_R[[i]]=list(numeric(),numeric(),numeric())
      brin_F_junction[[i]]=list(numeric(),numeric(),numeric())
      brin_R_junction[[i]]=list(numeric(),numeric(),numeric())
    }
    else {
      if(is.null(to_)) {
	to_i=lon_chromo
      }
      else {
	to_i=to_[min(i_zone,length(to_))]
      }
      from_i=from_[min(i_zone,length(from_))]
      
      commande=paste("RangesList(`",nom_chromo,"`=IRanges(",from_i,",",to_i,"))",sep="")
      expr=try(parse(text=commande),TRUE)
## Function used from GenomicRanges package
      which=eval(expr)
      what=c("flag","mpos","cigar","mrnm","isize")
      param=ScanBamParam(what=what, which=which)
## Case of no reads on the chromosome   
      start=extractFromBam(file=file_,which=which,what="pos")
      if(length(start)==0 ) {
	brin_F[[i]]=list(numeric(),numeric(),numeric())
	brin_R[[i]]=list(numeric(),numeric(),numeric())
	brin_F_junction[[i]]=list(numeric(),numeric(),numeric())
	brin_R_junction[[i]]=list(numeric(),numeric(),numeric())
      }
      else {
	strand=extractFromBam(file=file_,which=which,what="strand")
	flag=extractFromBam(file=file_,which=which,what="flag")
	mpos=extractFromBam(file=file_,which=which,what="mpos")
	cigar=extractFromBam(file=file_,which=which,what="cigar")
	mrnm=extractFromBam(file=file_,which=which,what="mrnm")
	isize=extractFromBam(file=file_,which=which,what="isize")
	
	first_read=integer.base.b(flag)[,5]==1
	strand[strand=="+" & !first_read ]="*"
	strand[strand=="-" & !first_read ]="+"
	strand[strand=="*" & !first_read ]="-"

## CIGAR's interpreter
	cigar_interprete=interpreteCIGAR(cigar)
	longueur_cigar=lapply(cigar_interprete,calcule_longueur_cigar)
	junction_cigar=lapply(cigar_interprete,calcule_junction_cigar)
	
	end=start+sapply(1:length(cigar),function(j) longueur_cigar[[cigar[j]]])

## Case of pairend reads
	is_on_same_chr=mrnm==nom_chromo
	is_on_same_chr[is.na(is_on_same_chr)]=FALSE
	is_paired=is_on_same_chr & abs(isize) <=insert_max_
	is_paired[first_read & strand=="+" & (isize<0 | isize>insert_max_)]=FALSE
	is_paired[!first_read & strand=="+" & (isize>0 | isize < -insert_max_)]=FALSE
	is_paired[first_read & strand=="-" & (isize>0 | isize < -insert_max_)]=FALSE
	is_paired[!first_read & strand=="-" & (isize<0 | isize>insert_max_)]=FALSE
	is_paired[is.na(is_paired)]=FALSE
	
	debut_fragment_paired_plus=mpos[!first_read & strand=="+" & is_paired]
	fin_fragment_paired_plus=end[!first_read & strand=="+" & is_paired]
	debut_fragment_paired_moins=mpos[first_read & strand=="-" & is_paired]
	fin_fragment_paired_moins=end[first_read & strand=="-" & is_paired]
	
## Case of single reads
	debut_fragment_singleton_plus=start[!is_paired & strand=="+"] 
	fin_fragment_singleton_plus=end[!is_paired & strand=="+"]
	debut_fragment_singleton_moins=start[!is_paired & strand=="-"] 
	fin_fragment_singleton_moins=end[!is_paired & strand=="-"]

## Fragments
	debut_frag_plus=c(debut_fragment_paired_plus,debut_fragment_singleton_plus)
	fin_frag_plus=c(fin_fragment_paired_plus,fin_fragment_singleton_plus)
	debut_frag_moins=c(debut_fragment_paired_moins,debut_fragment_singleton_moins)
	fin_frag_moins=c(fin_fragment_paired_moins,fin_fragment_singleton_moins)
	brin_F[[i]]=compresse_coordonnees(debut_frag_plus,fin_frag_plus)
	brin_R[[i]]=compresse_coordonnees(debut_frag_moins,fin_frag_moins)

## Junction read
	debut_junction=numeric()
	fin_junction=numeric()
	brin_junction=numeric()
	i_junction=0
	for(j in 1:length(cigar)) {
	  junctions_=junction_cigar[[cigar[j]]]
	  if(length(junctions_)) {
	    for(k in 1:length(junctions_)) {
	      i_junction=i_junction + 1
	      debut_junction[i_junction]=start[j] + junctions_[[k]][1] - 1
	      fin_junction[i_junction]=start[j] + junctions_[[k]][2] - 1
	      brin_junction[i_junction]=as.character(strand[j])
	    }
	  }
	}
	if(i_junction==0) {
	  brin_F_junction[[i]]=list(numeric(),numeric(),numeric())
	  brin_R_junction[[i]]=list(numeric(),numeric(),numeric())
	}
	else {
	  brin_F_junction[[i]]=compresse_coordonnees(debut_junction[brin_junction=="+"],fin_junction[brin_junction=="+"])
	  brin_R_junction[[i]]=compresse_coordonnees(debut_junction[brin_junction=="-"],fin_junction[brin_junction=="-"])
	}

## Flagstat interpreter
	flag_bits=integer.base.b(flag)#remplie les  données stat pour un flag donné

	## flagstat
	## total
	flagstat[1]=flagstat[1] + sum(flag_bits[,2]==0)
	## duplicates
	flagstat[2]=flagstat[2] + sum((flag_bits[,1]==1)&(flag_bits[,2]==0))
	## mapped
	flagstat[3]=flagstat[3] + sum((flag_bits[,9]==0)&(flag_bits[,2]==0))
	## paired
	flagstat[4]=flagstat[4] + sum((flag_bits[,11]==1)&(flag_bits[,2]==0))
	## read1
	flagstat[5]=flagstat[5] + sum((flag_bits[,5]==1)&(flag_bits[,2]==0))
	## read2
	flagstat[6]=flagstat[6] + sum((flag_bits[,4]==1)&(flag_bits[,2]==0))
	## iself and mate mapped
	flagstat[8]=flagstat[8] + sum((flag_bits[,11]==1)&(flag_bits[,9]==0)&(flag_bits[,8]==0)&(flag_bits[,2]==0))
	## singletons
	flagstat[9]=flagstat[9] + sum((flag_bits[,8]==1)&(flag_bits[,2]==0))
	## QC-failed
	flagstat[11]=flagstat[11] + sum(flag_bits[,2]==1)
	## flagstat
	## mate on a different chr
	flagstat[10]=flagstat[10] + sum((!is_on_same_chr)&(flag_bits[,11]==1)&(flag_bits[,9]==0)&(flag_bits[,8]==0)&(flag_bits[,2]==0))
	## flagstat
	## properly paired
	flagstat[7]=flagstat[7] + sum(is_paired)
      }
    }
  }
  
## Data storing
  names(brin_F)=noms_chromosomes
  names(brin_R)=noms_chromosomes
  names(brin_F_junction)=noms_chromosomes
  names(brin_R_junction)=noms_chromosomes

  bamHandler=list()
  if(libraryType_[1]=="inverse") {
    bamHandler[[1]]=brin_R
    bamHandler[[2]]=brin_F
  }
  else {
    bamHandler[[1]]=brin_F
    bamHandler[[2]]=brin_R
  }
  bamHandler[[3]]=longueur_chromosomes
  bamHandler[[4]]=flagstat
  bamHandler[[5]]=stranded_

  if(libraryType_[1]=="inverse") {
    bamHandler[[6]]=brin_R_junction
    bamHandler[[7]]=brin_F_junction
  }
  else {
    bamHandler[[6]]=brin_F_junction
    bamHandler[[7]]=brin_R_junction
  }
  bamHandler[[8]]=FALSE
  if(!is.null(normalized_)) {
    for( i in pas) {
      for(j in 1:nombre_chromosomes) {
	bamHandler[[i]][[j]][[4]]=normalized_*bamHandler[[i]][[j]][[3]]
      }
    }
    bamHandler[[8]]=TRUE
  }
    
  names(bamHandler)=c("F","R","chrLength","flagstat","stranded","junctions_F","junctions_R","norm")
  if((is.null(chrName_))&(from_==1)&(is.null(to_))) {
    if(is.null(fileNameRData_)|is.na(fileNameRData_)) {
      save(bamHandler,file=RDataFileName(file_))
    } 
    else {
      save(bamHandler,file=fileNameRData_)
    }
  }
  return(bamHandler)
}

## Returns the sum of two bamHandler objects
addBam=function(bamHandler1,bamHandler2) {
  if(class(bamHandler1)=="try-error"|class(bamHandler2)=="try-error") {
    if(class(bamHandler1)=="try-error") {
      return(bamHandler2)
    }
    else {
      return(bamHandler1)
    }
  }
  else {
    brin_F=list()
    junctions_brin_F=list()
    brin_R=list()
    junctions_brin_R=list()
    L=length(bamHandler1$F)
    bamHandler=list()
    if(bamHandler1$norm !=bamHandler2$norm ) {
      warning(expr="Two different bam files(normalized and non normalized)!!",immediate.=TRUE)
    }
    for(i in 1:L) {
      brin_F[[i]]=list(c(bamHandler1$F[[i]][[1]],bamHandler2$F[[i]][[1]]),c(bamHandler1$F[[i]][[2]],bamHandler2$F[[i]][[2]]),c(bamHandler1$F[[i]][[3]],bamHandler2$F[[i]][[3]]))
      junctions_brin_F[[i]]=list(c(bamHandler1$junctions_F[[i]][[1]],bamHandler2$junctions_F[[i]][[1]]),c(bamHandler1$junctions_F[[i]][[2]],bamHandler2$junctions_F[[i]][[2]]),c(bamHandler1$junctions_F[[i]][[3]],bamHandler2$junctions_F[[i]][[3]]))
      brin_R[[i]]=list(c(bamHandler1$R[[i]][[1]],bamHandler2$R[[i]][[1]]),c(bamHandler1$R[[i]][[2]],bamHandler2$R[[i]][[2]]),c(bamHandler1$R[[i]][[3]],bamHandler2$R[[i]][[3]]))
      junctions_brin_R[[i]]=list(c(bamHandler1$junctions_R[[i]][[1]],bamHandler2$junctions_R[[i]][[1]]),c(bamHandler1$junctions_R[[i]][[2]],bamHandler2$junctions_R[[i]][[2]]),c(bamHandler1$junctions_R[[i]][[3]],bamHandler2$junctions_R[[i]][[3]]))
      if(bamHandler1$norm & bamHandler2$norm) {
	brin_F[[i]][[4]]=c(bamHandler1$F[[i]][[4]],bamHandler2$F[[i]][[4]])
	junctions_brin_F[[i]][[4]]=c(bamHandler1$junctions_F[[i]][[4]],bamHandler2$junctions_F[[i]][[4]])
	brin_R[[i]][[4]]=c(bamHandler1$R[[i]][[4]],bamHandler2$R[[i]][[4]])
	junctions_brin_R[[i]][[4]]=c(bamHandler1$junctions_R[[i]][[4]],bamHandler2$junctions_R[[i]][[4]])
      }
    }
    names(brin_F)=names(bamHandler1$F)
    names(brin_R)=names(bamHandler2$R)
    names(junctions_brin_F)=names(bamHandler1$junctions_F)
    names(junctions_brin_R)=names(bamHandler2$junctions_R)
    bamHandler[[1]]=brin_F
    bamHandler[[2]]=brin_R
    bamHandler[[3]]=bamHandler1[[3]]
    bamHandler[[4]]=bamHandler1[[4]] + bamHandler2[[4]]
    bamHandler[[5]]=bamHandler1[[5]] & bamHandler2[[5]]
    bamHandler[[6]]=junctions_brin_F
    bamHandler[[7]]=junctions_brin_R 
    bamHandler[[8]]=bamHandler1$norm & bamHandler2$norm
    names(bamHandler)=c("F","R","chrLength","flagstat","stranded","junctions_F","junctions_R","norm")
    return(bamHandler)
  }
}

## Extracts the signal from bamHandler objects 
extractSignal=function(bamHandlerList,chrName,from=1, to=NULL,normalized_=FALSE) {
  forward=list()
  reverse=list()
  chr=which(names(bamHandlerList[[1]]$F)==chrName)
  if(is.null(to)) {
    to=bamHandlerList[[1]]$chrLength[chr]
  }
  for(i in 1:length(bamHandlerList)) {
    if(normalized_) {
      if(bamHandlerList[[i]]$norm) {
	end=4
      }
      else {
	end=3
      }
    }
    else {
      end=3
    }
    forward_=numeric(to-from+1)
    which_read=which((bamHandlerList[[i]]$F[[chrName]][[2]]>=from)&(bamHandlerList[[i]]$F[[chrName]][[1]]<=to))
    n_reads=length(which_read)
    if(n_reads>0) {
      for(k in which_read) {
	debut_read=max(1,bamHandlerList[[i]]$F[[chrName]][[1]][k]-from+1)
	fin_read=min(bamHandlerList[[i]]$F[[chrName]][[2]][k]-from+1,to-from+1)
	forward_[debut_read:fin_read]=forward_[debut_read:fin_read]+bamHandlerList[[i]]$F[[chrName]][[end]][k]
      }
    } 
    which_junctions=which((bamHandlerList[[i]]$junctions_F[[chrName]][[2]]>=from)&(bamHandlerList[[i]]$junctions_F[[chrName]][[1]]<=to))
    n_junctions=length(which_junctions)
    if(n_junctions>0) {
      for(k in which_junctions) {
	debut_junction=max(1,bamHandlerList[[i]]$junctions_F[[chrName]][[1]][k]-from+1)
	fin_junction=min(bamHandlerList[[i]]$junctions_F[[chrName]][[2]][k]-from+1,to-from+1)
	forward_[debut_junction:fin_junction]=forward_[debut_junction:fin_junction]-bamHandlerList[[i]]$junctions_F[[chrName]][[end]][k]
      }
    }
    reverse_=numeric(to-from+1) 
    which_read=which((bamHandlerList[[i]]$R[[chrName]][[2]]>=from)&(bamHandlerList[[i]]$R[[chrName]][[1]]<=to))
    n_reads=length(which_read)
    if(n_reads>0) {
      for(k in which_read) {
	debut_read=max(1,bamHandlerList[[i]]$R[[chrName]][[1]][k]-from+1)
	fin_read=min(bamHandlerList[[i]]$R[[chrName]][[2]][k]-from+1,to-from+1)
	reverse_[debut_read:fin_read]=reverse_[debut_read:fin_read]+bamHandlerList[[i]]$R[[chrName]][[end]][k]
      }
    } 
    which_junctions=which((bamHandlerList[[i]]$junctions_R[[chrName]][[2]]>=from)&(bamHandlerList[[i]]$junctions_R[[chrName]][[1]]<=to))
    n_junctions=length(which_junctions)
    if(n_junctions>0) {
      for(k in which_junctions) {
	debut_junction=max(1,bamHandlerList[[i]]$junctions_R[[chrName]][[1]][k]-from+1)
	fin_junction=min(bamHandlerList[[i]]$junctions_R[[chrName]][[2]][k]-from+1,to-from+1)
	reverse_[debut_junction:fin_junction]=reverse_[debut_junction:fin_junction]-bamHandlerList[[i]]$junctions_R[[chrName]][[end]][k]
      }
    }
    forward_[forward_<0]=0
    reverse_[reverse_<0]=0
    if(bamHandlerList[[i]]$stranded) {
      forward[[i]]=forward_
      reverse[[i]]=reverse_
    }
    else {
      forward[[i]]=forward_+reverse_
      reverse[[i]]=numeric(to-from+1)
    }
  }
  chr_=list()
  chr_$F=forward
  chr_$R=reverse
  return(chr_)
}

## Intern function for readGff function
my.read.lines2=function(fname) {
  s=file.info( fname )$size 
  buf=readChar( fname, s, useBytes=T)
  strsplit( buf,"\n",fixed=T,useBytes=T)[[1]]
}

## Extracts the annotation infos from Gff file
readGff=function(file_in, from=1, to=Inf, chr=NULL, infoName=c("ID","Name","Parent","gene","Alias","orf_classification","Ontology_term","Note","GO")) {
  tmp=try(my.read.lines2(file_in))
  if(!is.null(chr)) {
    tmp1=grep(chr, tmp, value=TRUE,useBytes=T)
  }
  else {
    tmp1=tmp
  }
  N=length(tmp1)
  Chr=array()
  Start=array()
  Stop=array()
  Strand=array()
  Type=array()
  info=list()
  for(i in 1:length(infoName)) info[[i]]=array()
  names(info)=infoName
  j=1
  for (i in 1:N) {
    if(substr(tmp1[i],1,1)!="#") {
      line_split=unlist(strsplit(tmp1[i],"\t",fixed=T,useBytes=T))
      if((as.integer(line_split[4])<=to) & (as.integer(line_split[5])>=from)) {
	Chr[j]=line_split[1]
	Start[j]=as.integer(line_split[4])
	Stop[j]=as.integer(line_split[5])
	Strand[j]=line_split[7]
	Type[j]=line_split[3]
	ninth=unlist(strsplit(line_split[9],";",fixed=T,useBytes=T))
	element_ninth_empty=rep(TRUE,length(infoName))
	for(element_ninth in ninth) {
	  element_ninth_split=unlist(strsplit(element_ninth,"=",fixed=T,useBytes=T))
	  if(length(element_ninth_split)==2) {
	    if(element_ninth_split[1] %in% infoName) {
	      info[[element_ninth_split[1]]][j]=element_ninth_split[2]
	      element_ninth_empty[infoName==element_ninth_split[1]]=FALSE
	    }
	  }
	}
	for(infoName_ in infoName[element_ninth_empty]) {
	  info[[infoName_]][j]="."
	}
	j=j+1
      }
    }
  } 
  retour=data.frame(Chr,Type,Start,Stop,Strand,info,stringsAsFactors=FALSE)
  return(retour)
}

## Returns the classic visualisation
plotRNAseq=function(forward,reverse,debut_vue=1,fin_vue=length(forward),chr=NULL,annot=NULL,style=NULL,top=NULL,bottom=NULL,x="",y="",titre="",repeated=FALSE,name_flags="",decal=0,ataxises=NULL,classic_plus_color="navyblue",classic_minus_color="mediumvioletred",stranded=TRUE) {
  if(repeated) {
    forward_=numeric(fin_vue)
    forward_[debut_vue:fin_vue]=forward
    reverse_=numeric(fin_vue)
    reverse_[debut_vue:fin_vue]=reverse
  }
  else {
    forward_=forward
    reverse_=reverse
  }
  if(is.null(top)) {
    top=max(forward_[debut_vue:fin_vue])
  }
  if(is.null(bottom)) {
    bottom=max(reverse_[debut_vue:fin_vue])
  }
  if(is.null(ataxises)) {
    plot(c(debut_vue,fin_vue)+decal,c(-bottom,top),ylim=c(-bottom,top),xlab=x,ylab=y,main=titre,col="white",xaxs="i",cex.main=2,yaxt="n",cex.lab=1.8)
  }
  else {
    plot(c(debut_vue,fin_vue)+decal,c(-bottom,top),ylim=c(-bottom,top),xlab=x,ylab=y,main=titre,col="white",xaxs="i",xaxt="n",cex.main=2,yaxt="n",cex.lab=1.8)
    ataxisesLabels=as.character(ataxises)
    ataxisesLabels[((1:length(ataxises))%%2)==0]=""
    ataxisesLabels[1]=""
    ataxisesLabels[length(ataxises)]=""
    lim=c(-bottom,top)
    ataxises_y=pretty(lim,n=4)
    ataxisesLabels_y=as.character(abs(ataxises_y))
    axis(1,at=ataxises,labels=FALSE,cex.axis=2)
    axis(1,at=ataxises,labels=ataxisesLabels,cex.axis=2,line=0.4,lwd=0)
    axis(2, at=ataxises_y,labels=FALSE,cex.axis=2)
    axis(2, at=ataxises_y,labels=ataxisesLabels_y,cex.axis=2,line=-0.4,lwd=0)
  }
  polygon(c(debut_vue,debut_vue:fin_vue,fin_vue)+decal,c(0,forward_[debut_vue:fin_vue],0),col=classic_plus_color,border=NA)
  if(stranded) {
    text(fin_vue+(fin_vue-debut_vue)*0.01,top/2,"+",xpd=NA,cex=3)
    text(fin_vue+(fin_vue-debut_vue)*0.01,-bottom/2,"-",xpd=NA,cex=3)
    text(fin_vue+(fin_vue-debut_vue)*0.025,(top-bottom)/2,"Strand",xpd=NA,cex=2,srt=-90)
    polygon(c(debut_vue,debut_vue:fin_vue,fin_vue)+decal,c(0,-reverse_[debut_vue:fin_vue],0),col=classic_minus_color,border=NA)
    abline(h=0,lwd=2)
    abline(h=0,col="white")
  }
  if(name_flags!="") {
    flags=try(get(name_flags),TRUE)
    if(class(flags)!="try-error") {
      f_=flags[(flags$Chr==chr)&(flags$Stop>=debut_vue)&(flags$Start<=fin_vue),]
      N=dim(f_)[1]
      points(f_$Start,rep(0,N),col=2,pch=19,cex=2)
    }
  }
  if(decal<=0) {
    lines(c(0,0),c(top,-bottom),col=1)
  }
}

## Intern function for heatmap visualisation
paletteFromColors=function(colMin="blue",colMax="red",n=300,method=c("hsv","rgb")) {
  colMinRGB=col2rgb(colMin)[,1]/255
  colMaxRGB=col2rgb(colMax)[,1]/255
  seqList=list()
  if(method[1]=="rgb") {
    for(i in 1:3) {
      seqList[[i]]=seq(colMinRGB[i],colMaxRGB[i],length.out=n)
    }
    return(rgb(seqList[[1]],seqList[[2]],seqList[[3]]))
  }
  else {
    colMinHSV=rgb2hsv(colMinRGB)
    colMaxHSV=rgb2hsv(colMaxRGB)
    for(i in 1:3) {
      seqList[[i]]=seq(colMinHSV[i],colMaxHSV[i],length.out=n)
    }
    return(hsv(seqList[[1]],s=seqList[[2]],v=seqList[[3]]))
  }
}

## Retuns the heatmap visualisation
myHeatMap=function(data,debut_vue,fin_vue,ataxises=NULL,lim=NULL,heatmap_max_color="#000055",heatmap_min_color="#FFFFAA",heatmap_palette_method="hsv",textOnLeftSide="") {
  palette=paletteFromColors(heatmap_min_color,heatmap_max_color,method=heatmap_palette_method)
  if(is.null(lim)) {
    image(debut_vue:fin_vue,1:dim(data)[1],t(data), col=palette,xlab="",ylab="",xaxt="n",yaxt="n")
  }
  else {
    image(debut_vue:fin_vue,1:dim(data)[1],t(data), col=palette,xlab="",ylab="",xaxt="n",yaxt="n",zlim=lim)
  }
  box()
  if(is.null(ataxises)) {
    axis(1)
  }
  else {
    if(sum(!is.na(ataxises))!=0) {
      axis(1,at=ataxises,labels=FALSE)
      axis(3,at=ataxises,labels=FALSE)
    }
  }
  if(sum(!is.na(ataxises))!=0) {
    axis(2,at=1:dim(data)[1],labels=rownames(data),las=2,cex.axis=1)
  }
  text(fin_vue+(fin_vue-debut_vue)*0.015,(dim(data)[1]+1)/2,textOnLeftSide,xpd=NA,cex=1,srt=-90)
}

## Returns the title of visualisation
plotTitle=function(debut_vue=1,fin_vue=length(listForward[[1]]),chr=NULL,style=NULL) {
  plot(c(0,1),c(0,1),cex=0,ylab="",xlab="",fg="white",axes=FALSE,xaxs="i",yaxs="i")
  text(0,0.5,paste(chr,":",debut_vue,"-",fin_vue,sep=""),cex=2.2,adj=0)
}

openGraphicalDevice=function(file,widthPixels,heightPixels,fileType=c("png","jpeg","tiff","bmp","pdf"),resolutionDPI=72) {
  widthInches=widthPixels/72
  heightInches=heightPixels/72
  doit=function(x) {
    switch(x,
	   png(file,widthInches,heightInches,units="in",res=resolutionDPI),
	   jpeg=jpeg(file,widthInches,heightInches,units="in",res=resolutionDPI),
	   tiff=tiff(file,widthInches,heightInches,units="in",res=resolutionDPI),
	   bmp=bmp(file,widthInches,heightInches,units="in",res=resolutionDPI),
	   pdf=pdf(file,widthInches,heightInches))
  }
  doit(fileType[1])
}
 
## The main function of visualisation
plotVisu=function(file,typeVisu="classic",listForward,listReverse,which=1:length(listForward),stranded=TRUE,
debut_vue=1,fin_vue=length(listForward[[1]]),chr=NULL,annot=NULL,style=NULL,tops=NULL,bottoms=NULL,marks=NULL,strandMarks=NULL,
titres="",repeated=FALSE,name_flags="",decal=0,log=TRUE,classic_plus_color="navyblue",classic_minus_color="deeppink3",
heatmap_max_color="#000055",heatmap_min_color="#FFFFAA",heatmap_palette_method="hsv",heatmap_lane_height=round(10+40/(1+10^((length(which)-12)/9))),
lines_samples_colors=c(1,3,4,2)[((0:length(listForward))%%4)+1],lines_samples_type_line=((0:length(listForward))%/%4)+1,
smoothLength=trunc((fin_vue-debut_vue)/1200),annotation_color_by_strand=FALSE,annotation_placed_by_strand=FALSE,display_name=NULL,initialize_label_sizes=NULL,
fileType="png",resolutionDPI=72) {
  if(fin_vue-debut_vue+1>1000000) {
    openGraphicalDevice(file,1200,400,fileType=fileType,resolutionDPI=resolutionDPI)
    plot(0:1,0:1,fg="white",col="white",axes=FALSE,frame=FALSE,xlab="",ylab="")
    text(0.5,0.5,"Window too long !",cex=5)
  }
  else {
  n_element_vue=length(which)
  i_data=which[1] 
  forward_matrice=NULL
  reverse_matrice=NULL
  all_stranded=data[[i_data]]$stranded
  if(!repeated) {
    for(i in 1:n_element_vue) {
      i_data=which[i]
      forward_matrice=rbind(forward_matrice,listForward[[i_data]][debut_vue:fin_vue])
      reverse_matrice=rbind(reverse_matrice,listReverse[[i_data]][debut_vue:fin_vue])
      all_stranded=all_stranded & data[[i_data]]$stranded
    }
  }
  else {
    for(i in 1:n_element_vue) {
      i_data=which[i]
      forward_matrice=rbind(forward_matrice,listForward[[i_data]])
      reverse_matrice=rbind(reverse_matrice,listReverse[[i_data]])      
      all_stranded=all_stranded & data[[i_data]]$stranded
    }
  }
  rownames(forward_matrice)=titres[which]
  rownames(reverse_matrice)=titres[which]
  for( i in 1:n_element_vue) {
    if(smoothLength>1) {
      lo=smooth(forward_matrice[i,],L=smoothLength)
      forward_matrice[i,]=lo
      los=smooth(reverse_matrice[i,],L=smoothLength)
      reverse_matrice[i,]=los
    }
  }
  if(is.null(annot)) {
    heights_ann=NULL
    annot_selec=NULL
  }
  else {
    annot_selec=annot[(annot$Chr==chr)&(annot$Stop>=debut_vue)&(annot$Start<=fin_vue),]
    heights_ann=sizePlotAnnotation(annot=annot_selec,chr=chr,debut=debut_vue,fin=fin_vue,annotation_placed_by_strand=annotation_placed_by_strand,display_name=display_name,typeVisu=typeVisu,initialize_label_sizes=initialize_label_sizes)*50
  }
  ataxises=pretty((debut_vue:fin_vue)+decal,n=14)
  if(log) {
    label_scal="log2 tag densities"
  }
  else {
    label_scal="tag densities"
  }
  if(debut_vue==1&decal==0) {
    ataxises[1]=1
  }## Classic visualisation
  if(typeVisu=="classic") {
    height_panels=c(40,rep(200,n_element_vue),heights_ann)
    openGraphicalDevice(file,1200,sum(height_panels),fileType=fileType,resolutionDPI=resolutionDPI)
    prev=par(no.readonly=TRUE)
    n_panels=length(height_panels)
    layout(matrix(1:n_panels,n_panels,1),heights=height_panels)
    par(mar=c(0, 5, 0, 4)+0.1)
    plotTitle(debut_vue=debut_vue,fin_vue=fin_vue,chr=chr)
    par(mar=c(2.5, 5, 2.5, 4)+0.1)
    for(element in 1:n_element_vue) {
      i_data=which[element]
      plotRNAseq(listForward[[i_data]],listReverse[[i_data]],debut_vue,fin_vue,chr,annot_selec,style,top=tops[min(element,length(tops))],bottom=bottoms[min(element,length(bottoms))],y=label_scal,titre=titres[min(i_data,length(titres))],name_flags=name_flags,repeated=repeated,decal=decal,ataxises=ataxises,classic_plus_color=classic_plus_color,classic_minus_color=classic_minus_color,stranded=stranded)
      if(!is.null(marks)) {
	if(is.null(tops)) {
	  top=max(listForward[[i_data]])
	}
	else {
	  top=tops[min(element,length(tops))]
	}
	if(is.null(bottoms)) {
	  bottom=max(listReverse[[i_data]])
	}
	else {
	  bottom=bottoms[min(element,length(bottoms))]
	}
	if(is.null(strandMarks)) {
	  segments(marks,top,y1=-bottom,col=2)
	}
	else {
	  if(strandMarks==0|!stranded) {
	    segments(marks,0,y1=top,col=2)
	  }
	  else {
	    if(strandMarks==1) {
	      segments(marks,0,y1=-bottom,col=2)
	    }
	    else {
	      segments(marks,top,y1=-bottom,col=2)
	    }
	  }
	}
      }
    }
    if(!is.null(annot_selec)) {
      par(mar=c(0,5,1,4)+0.1)
      plot_annotation(annot_selec,chr,debut=debut_vue,fin=fin_vue,style=style,textSize=1.5,annotation_color_by_strand=annotation_color_by_strand,annotation_placed_by_strand=annotation_placed_by_strand,display_name=display_name,typeVisu=typeVisu,initialize_label_sizes=initialize_label_sizes)
    }
  }
  else {## Heatmap visualisation
  if(typeVisu=="heatmap") {
    if(stranded) {
      height_panels=c(40,heatmap_lane_height*n_element_vue+28,heights_ann,heatmap_lane_height*n_element_vue+10,100)
    }
    else {
      height_panels=c(40,heatmap_lane_height*n_element_vue+28,heights_ann,100)
    }
    openGraphicalDevice(file,1200,sum(height_panels),fileType=fileType,resolutionDPI=resolutionDPI)
    prev=par(no.readonly=TRUE)
    n_panels=length(height_panels)
    layout(matrix(1:n_panels,n_panels,1),heights=height_panels)
    par(mar=c(0, 8, 0, 2)+0.1)
    plotTitle(debut_vue=debut_vue,fin_vue=fin_vue,chr=chr)
    limIntensity=c(0,max(forward_matrice,reverse_matrice,na.rm=TRUE))
    if(limIntensity[2]==0) {
      limIntensity[2]=0.01
    }
    ataxisesLabels=as.character(ataxises)
    ataxisesLabels[((1:length(ataxises))%%2)==0]=""
    rownames(forward_matrice)=titres[which]
    rownames(reverse_matrice)=titres[which]
    par(mar=c(1, 8, 1.8, 2)+0.1,cex=1.1)
    if(stranded) {
      tmp="Plus strand"
    }
    else {
      tmp="Both strands"
    }
    myHeatMap(forward_matrice[(dim(forward_matrice)[1]):1,],debut_vue+decal,fin_vue+decal,ataxises,lim=limIntensity,heatmap_max_color=heatmap_max_color,heatmap_min_color=heatmap_min_color,heatmap_palette_method=heatmap_palette_method,textOnLeftSide=tmp)
    axis(3,at=ataxises,labels=FALSE,cex.axis=1.2)
    axis(3,at=ataxises,labels=ataxisesLabels,cex.axis=1.2,line=-0.4,lwd=0)
    if(!is.null(marks)) {
      if(is.null(strandMarks)|!all_stranded) {
	segments(marks,-2,y1=dim(forward_matrice)[1]+2,col=2,lwd=2,xpd=TRUE)
      }
    else {
      if(strandMarks!=1) {
	segments(marks,-2,y1=dim(forward_matrice)[1]+2,col=2,lwd=2,xpd=TRUE)
      }
    }
    }
    if(name_flags!="") {
      flags=try(get(name_flags),TRUE)
      if(class(flags)!="try-error") {
	f_=flags[(flags$Chr==chr)&(flags$Stop>=debut_vue)&(flags$Start<=fin_vue),]
	N=dim(f_)[1]
	points(f_$Start,rep(0,N),col=2,pch=19,cex=2)
      }
    }
    if(!is.null(annot_selec)) {
      par(mar=c(0,8,0,2)+0.1)
      plot_annotation(annot_selec,chr,debut=debut_vue,fin=fin_vue,style=style,textSize=0.9,annotation_color_by_strand=annotation_color_by_strand,annotation_placed_by_strand=annotation_placed_by_strand,display_name=display_name,typeVisu=typeVisu,initialize_label_sizes=initialize_label_sizes)
    }
    if(stranded) {
      par(mar=c(1, 8, 0, 2)+0.1)
      myHeatMap(reverse_matrice[(dim(reverse_matrice)[1]):1,],debut_vue+decal,fin_vue+decal,ataxises,lim=limIntensity,heatmap_max_color=heatmap_max_color,heatmap_min_color=heatmap_min_color,heatmap_palette_method=heatmap_palette_method,textOnLeftSide="Minus strand")
      if(!is.null(marks)) {
	if(is.null(strandMarks)|!all_stranded) {
	  segments(marks,-1,y1=dim(forward_matrice)[1]+1,col=2,lwd=2,xpd=TRUE)
	}
	else {
	  if(strandMarks!=0) {
	    segments(marks,-1,y1=dim(forward_matrice)[1]+1,col=2,lwd=2,xpd=TRUE)
	  }
	}
      }
      axis(1,at=ataxises,labels=ataxisesLabels,cex.axis=1.2)
    }
    palette=matrix(seq(limIntensity[1],limIntensity[2],length.out=2000),1,2000)
    rownames(palette)="0"
    par(mar=c(2,18,3, 12)+0.1)
    myHeatMap(palette,1,2000,ataxises=NA,heatmap_max_color=heatmap_max_color,heatmap_min_color=heatmap_min_color,heatmap_palette_method=heatmap_palette_method)
    labelAxisHeatmapLegend=pretty(c(limIntensity[1],limIntensity[2]),n=7)
    atAxisHeatmapLegend=1+((labelAxisHeatmapLegend-limIntensity[1])/(limIntensity[2]-limIntensity[1]))*1999
    axis(1,at=atAxisHeatmapLegend,labels=FALSE,cex.axis=1.2)
    axis(1,at=atAxisHeatmapLegend,labels=labelAxisHeatmapLegend,cex.axis=1.2,line=-0.4,lwd=0)
    text(1000,2,label_scal,xpd=NA,font=2,cex=1.4)
   }## Lines visualisation
    else if(typeVisu=="lines") {
      legendSize=(floor(n_element_vue/2)+n_element_vue%%2)*40
      height_panels=c(40,legendSize,400,heights_ann)
      n_panels=length(height_panels)
      openGraphicalDevice(file,1200,sum(height_panels),fileType=fileType,resolutionDPI=resolutionDPI)
      prev=par(no.readonly=TRUE)
      par(mar=c(0, 5, 0,4)+0.1,cex=1.1)
      layout(matrix(c(1:n_panels),n_panels,4),heights=height_panels)
      par(mar=c(0, 5, 0,4)+0.1)
      plotTitle(debut_vue=debut_vue,fin_vue=fin_vue,chr=chr)
      lines_legend(n_element_vue=n_element_vue,which,titres,lines_samples_colors=lines_samples_colors,lines_samples_type_line=lines_samples_type_line)
      par(mar=c(3, 5, 0,4)+0.1)
      plotlines(forward_matrice,reverse_matrice,which=which,debut_vue, fin_vue , chr, annot, style, tops, bottoms,marks,strandMarks,titres, repeated,name_flags,decal,ataxises=ataxises,n_element_vue=n_element_vue,y=label_scal,lines_samples_colors=lines_samples_colors,lines_samples_type_line=lines_samples_type_line,stranded=stranded)
      if(!is.null(annot_selec)) {
	par(mar=c(0,5,0,4)+0.1)
	plot_annotation(annot_selec,chr,debut=debut_vue,fin=fin_vue,style=style,textSize=1.5,annotation_color_by_strand=annotation_color_by_strand,annotation_placed_by_strand=annotation_placed_by_strand,display_name=display_name,typeVisu=typeVisu,initialize_label_sizes=initialize_label_sizes) 
      }

      }
    }
  }
  invisible(dev.off())
}

## 
smooth=function(X,L=10) {
  x_smooth=filter(X,rep(1,L)/L)
  x_smooth[is.na(x_smooth)]=0
  return(x_smooth)
}

## Returns lines visualisation
plotlines=function(forward_matrice,reverse_matrice,which=1:length(forward), debut_vue=1,fin_vue=length(forward), chr=NULL, annot=NULL, style=NULL, tops=NULL,bottoms=NULL,marks=NULL,strandMarks=NULL, titres="", repeated=FALSE,name_flags="",decal=0,ataxises=NULL,n_element_vue=length(which),y="",lines_samples_colors=c(1,3,4,2)[((0:length(forward))%%4)+1],lines_samples_type_line=((0:length(forward))%%4)+1,stranded=TRUE) {
  limIntensity=c(-max(bottoms),max(tops))
  plot(c(debut_vue,fin_vue)+decal,limIntensity,col="white",ylab=y,main="",xaxs="i",xaxt="n",yaxt="n",cex.lab=1.2,xlab="",cex.lab=1.8)
  lty=lines_samples_type_line
  col=lines_samples_colors
  ataxises_y=pretty(limIntensity,n=8)
  for(i in ataxises) abline(v=i,lty=2,col="#808080")
  for(i in ataxises_y) abline(h=i,lty=2,col="#808080")
  for( i in 1:n_element_vue) {
    lo=forward_matrice[i,]
    lines((debut_vue:fin_vue)+decal,lo,type="l",lty=lty[i],col=col[i],lwd=2,xaxt="n")
    if(stranded) {
      los=-reverse_matrice[i,]
      lines((debut_vue:fin_vue)+decal,los,type="l",lty=lty[i],col=col[i],lwd=2,xaxt="n")
    }
  }
  ataxisesLabels=as.character(ataxises)
  ataxisesLabels[((1:length(ataxises))%%2)==0]=""
  ataxisesLabels_y=as.character(abs(ataxises_y))
  ataxisesLabels_y[((1:length(ataxises_y))%%2)==0]=""
  axis(1,at=ataxises,labels=FALSE,cex.axis=2)
  axis(1,at=ataxises,labels=ataxisesLabels,cex.axis=2,line=0.6,lwd=0)
  axis(2, at=ataxises_y,labels=FALSE,cex.axis=2)
  axis(2, at=ataxises_y,labels=ataxisesLabels_y,cex.axis=2,line=-0.4,lwd=0)
  if(stranded) {
    abline(h=0,lwd=6)
    abline(h=0,col="white",lwd=4)
    text(fin_vue+(fin_vue-debut_vue)*0.01,tops/2,"+",xpd=NA,cex=3)
    text(fin_vue+(fin_vue-debut_vue)*0.01,-bottoms/2,"-",xpd=NA,cex=3)
    text(fin_vue+(fin_vue-debut_vue)*0.025,(tops-bottoms)/2,"Strand",xpd=NA,cex=2,srt=-90)
  }
  if(!is.null(marks)) {
    if(is.null(tops)) {
      top=max(listForward[[i_data]])
    }
    else {
      top=max(tops)
    }
    if(is.null(bottoms)) {
      bottom=max(listReverse[[i_data]])
    }
    else {
      bottom=max(bottoms)
    }
    if(is.null(strandMarks)) {
      segments(marks,top,y1=-bottom,col=2)
    }
    else {
      if(strandMarks==0|!stranded) {
	segments(marks,0,y1=top,col=2)
      }
      else {
	if(strandMarks==1) {
	  segments(marks,0,y1=-bottom,col=2)
	}
	else {
	  segments(marks,top,y1=-bottom,col=2)
	}
      }
    }
  }
}

## Returns lines legend
lines_legend=function(n_element_vue,which,titres,lines_samples_colors=c(1,3,4,2)[((0:n_element_vue)%%4)+1],lines_samples_type_line=((0:n_element_vue)%%4)+1) {
  lty=lines_samples_type_line
  col=lines_samples_colors
  n_y=floor(n_element_vue/2)+n_element_vue%%2
  plot(c(0,4),c(0,-(n_y+1)),col="white", ylab="",xlab="",main="",fg="white",col.axis="white",yaxs="i")
  i_style=0
  for(i in 1:n_y) {
    i_style=i_style+1
    lines(c(1.5,2),-c(i,i),col=col[i_style],lty=lty[i_style],lwd=4)
    text(1.48,-i,titres[which[i_style]],cex=2.6,adj=1)
  }
  if(n_element_vue>1) {
    for(i in (n_y+1):n_element_vue) {
      i_style=i_style+1
      lines(c(3.5,4),-c(i,i)+n_y,col=col[i_style],lty=lty[i_style],lwd=4)
      text(3.48,-i+n_y,titres[which[i_style]],cex=2.6,adj=1)   
    }
  }
}

## Returns the shape of plain arrow for the annotation
plain_arrow=function(left,right,y,thickness=1,pickSize=(right-left)*0.1,pickSide=c("right","left","both","none"),col="blue") {
  middle=(left+right)/2
  if(pickSide[1]=="right") {
    pick_point=max(right - pickSize,middle)
    polygon(c(left,pick_point,right,pick_point,left),c(y-thickness/2,y-thickness/2,y,y+thickness/2,y+thickness/2),col=col)
  }
  if(pickSide[1]=="left") {
    pick_point=min(left + pickSize,middle)
    polygon(c(right,pick_point,left,pick_point,right),c(y-thickness/2,y-thickness/2,y,y+thickness/2,y+thickness/2),col=col)
  }
  if(pickSide[1]=="none") {
    polygon(c(right,left,left,right),c(y-thickness/2,y-thickness/2,y+thickness/2,y+thickness/2),col=col)
  }
  if(pickSide[1]=="both") {
    pick_point_1=min(left + pickSize,middle)
    pick_point_2=max(right - pickSize,middle)
    polygon(c(left,pick_point_1,pick_point_2,right,pick_point_2,pick_point_1),c(y,y-thickness/2,y-thickness/2,y,y+thickness/2,y+thickness/2),col=col)
  }
}

## Returns the size of the  panel of the annotation
sizePlotAnnotation=function(annot,chr,debut,fin,annotation_placed_by_strand=FALSE,display_name=NULL,typeVisu="classic",initialize_label_sizes=NULL) {
  left=c()
  right=c()
  labels=c()
  annot_chr=annot[annot$Chr==chr,]
  N=dim(annot_chr)[1]
  all_names=annot_chr[,c(display_name,"ID")]
  unique_ID=unique(annot_chr$ID)
  for(j in 1:length(unique_ID)) {
    left[j]=min(annot_chr$Start[annot_chr$ID==unique_ID[j]])
    right[j]=max(annot_chr$Stop[annot_chr$ID==unique_ID[j]])
    all_names=unlist(annot_chr[annot_chr$ID==unique_ID[j],c(display_name,"ID")])
    all_names=all_names[all_names!="."]
    labels[j]=all_names[1]
  }
  if(annotation_placed_by_strand) {
    y_plot=parking(left,right,debut,fin,annot_chr$Strand=="+",FALSE,labels=labels,typeVisu=typeVisu,initialize_label_sizes=initialize_label_sizes)
  }
  else {
    y_plot=parking(left,right,debut,fin,biggestOnesInside=FALSE,labels=labels,typeVisu=typeVisu,initialize_label_sizes=initialize_label_sizes)
  }
  return(max(y_plot)-min(y_plot)+1)
}

## Function to organise the annotation shapes to display
parking=function(left,right,debut,fin,plus=rep(TRUE,length(left)),labels=c(),biggestOnesInside=TRUE,typeVisu="classic",initialize_label_sizes=NULL) {
  if(length(labels)!=0) {
    if(!is.null(initialize_label_sizes)) {
      initialize_=initialize_label_sizes[[typeVisu]]
      for(i in 1:length(left)) {
	right[i]=max(right[i],left[i]+initialize_$size[initialize_$labels==labels[i]])+(fin-debut)/100
      }
    }
  }
  y=rep(0,length(left))
  if(sum(plus)>0) {
    left_plus=left[plus]
    right_plus=right[plus]
    y_plus=rep(0,sum(plus))
    lengths_plus=right_plus-left_plus
    for(i in order(lengths_plus,decreasing=TRUE)) {
      otherleft_plus=left_plus
      otherleft_plus[i]=NA
      otherright_plus=right_plus
      otherright_plus[i]=NA
      placed=FALSE
      y_plus[i]=0.5
      while(placed==FALSE) {
	placed=sum((right_plus[i]>otherleft_plus[y_plus==y_plus[i]])&(left_plus[i]<otherright_plus[y_plus==y_plus[i]]),na.rm=TRUE)==0
	if(placed==FALSE) {
	  y_plus[i]=y_plus[i]+1
	}
      }
    }
    if(biggestOnesInside) {
      y[plus]=y_plus
    }
    else {
      y[plus]=sapply(y_plus,function(i) sort(unique(y_plus))[i==sort(unique(y_plus),decreasing=TRUE)])
    }
  }
  if(sum(!plus)>0) {
    left_minus=left[!plus]
    right_minus=right[!plus]
    y_minus=rep(0,sum(!plus))
    lengths_minus=right_minus-left_minus
    for(i in order(lengths_minus,decreasing=TRUE)) {
      otherleft_minus=left_minus
      otherleft_minus[i]=NA
      otherright_minus=right_minus
      otherright_minus[i]=NA
      placed=FALSE
      y_minus[i]=-0.5
      while(placed==FALSE) {
	placed=sum((right_minus[i]>otherleft_minus[y_minus==y_minus[i]])&(left_minus[i]<otherright_minus[y_minus==y_minus[i]]),na.rm=TRUE)==0
	if(placed==FALSE) {
	  y_minus[i]=y_minus[i]-1
	}
      }
    }
    if(biggestOnesInside) {
      y[!plus]=y_minus
    }
    else {
      y[!plus]=sapply(y_minus,function(i) sort(unique(y_minus))[i==sort(unique(y_minus),decreasing=TRUE)])
    }
  }  
  return(y)
}

## Function to 
plot_annotation=function(annot,chr,debut,fin,style=NULL,textSize=par("cex"),annotation_color_by_strand=FALSE,annotation_placed_by_strand=FALSE,display_name=NULL,typeVisu="classic",initialize_label_sizes=NULL) {
  left=c()
  right=c()
  labels=c()
  annot_chr=annot[annot$Chr==chr,]
  N=dim(annot_chr)[1]
  if(N>0) {
    all_names=annot_chr[,c(display_name,"ID")]
    unique_ID=unique(annot_chr$ID)
    for(j in 1:length(unique_ID)) {
      left[j]=min(annot_chr$Start[annot_chr$ID==unique_ID[j]])
      right[j]=max(annot_chr$Stop[annot_chr$ID==unique_ID[j]])
      all_names=unlist(annot_chr[annot_chr$ID==unique_ID[j],c(display_name,"ID")])
      all_names=all_names[all_names!="."]
      labels[j]=all_names[1]
    }
    if(annotation_placed_by_strand) {
      y_plot=parking(left,right,debut,fin,annot_chr$Strand=="+",FALSE,labels=labels,typeVisu=typeVisu,initialize_label_sizes=initialize_label_sizes)
    }
    else {
      y_plot=parking(left,right,debut,fin,biggestOnesInside=FALSE,labels=labels,typeVisu=typeVisu,initialize_label_sizes=initialize_label_sizes)
    }
    plot(c(debut,fin),c(min(y_plot)-0.5,max(y_plot)+0.5),col="white",ylab="",xlab="",fg="white",col.axis="white",xaxs="i",yaxs="i")
    for(j in 1:length(unique_ID)) {
      annot_ID=annot_chr[annot_chr$ID==unique_ID[j],]
      x_text=Inf
      for(i_annot_ID in 1:dim(annot_ID)[1]) {
	if(annot_ID$Strand[i_annot_ID]=="-") {
	  orientation="left"
	}
	else {
	  orientation="right"
	}
	iDraw=FALSE
	if(annot_ID$Strand[i_annot_ID]==".") {
	  tmp="+"
	}
	else {
	  tmp=annot_ID$Strand[i_annot_ID]
	}
	style_demande=style[style$Type==annot_ID$Type[i_annot_ID]&style$Strand==tmp,]
	x_text=min(x_text,annot_ID$Start[i_annot_ID])
	if(style_demande$shape=="box") {
	  plain_arrow(annot_ID$Start[i_annot_ID],annot_ID$Stop[i_annot_ID],y_plot[j],thickness=0.5, pickSide=orientation,col=style_demande$col,pickSize=(fin-debut)*0.02)
	  iDraw=TRUE
	}
	if(style_demande$shape=="line") {
	  segments(annot_ID$Start[i_annot_ID],y_plot[j],annot_ID$Stop[i_annot_ID],y_plot[j],col=style_demande$col)
	  iDraw=TRUE
	}			
	if(style_demande$shape=="rectangle") {
	  plain_arrow(annot_ID$Start[i_annot_ID],annot_ID$Stop[i_annot_ID],y_plot[j],thickness=0.5, pickSide="none",col=style_demande$col)
	  iDraw=TRUE
	}
	if(style_demande$shape=="arrow") {
	  arrowHeads=pretty(debut:fin,n=50)
	  x=c(annot_ID$Start[i_annot_ID],arrowHeads[arrowHeads>annot_ID$Start[i_annot_ID]&arrowHeads<annot_ID$Stop[i_annot_ID]],annot_ID$Stop[i_annot_ID])
	  if(annot_ID$Strand[i_annot_ID]=="-") {
	    arrows(x[2:length(x)],y_plot[j],x[1:length(x)-1],col=style_demande$col,length=0.08)
	  }
	  else {
	    arrows(x[1:length(x)-1],y_plot[j],x[2:length(x)],col=style_demande$col,length=0.08)
	  }
	  iDraw=TRUE
	}
	if(iDraw==FALSE) {
	  segments(annot_ID$Start[i_annot_ID],y_plot[j],annot_ID$Stop[i_annot_ID],y_plot[j],col="black")
	}
      }
      text(max(x_text,debut),y_plot[j]+0.4,adj=0,labels=labels[j],cex=textSize)
    }
    if(annotation_placed_by_strand) {
      abline(h=0.125,lty=2,col="black")
    }
  }
}

html2rgb=function(codeHTML) {
  chiffres=c(0:9,"A","B","C","D","E","F")
  codeHTMLsplit=unlist(strsplit(codeHTML,split=""))
  red=(((which(chiffres==codeHTMLsplit[2])-1)*16)+(which(chiffres==codeHTMLsplit[3])-1))/255
  green=(((which(chiffres==codeHTMLsplit[4])-1)*16)+(which(chiffres==codeHTMLsplit[5])-1))/255
  blue=(((which(chiffres==codeHTMLsplit[6])-1)*16)+(which(chiffres==codeHTMLsplit[7])-1))/255
  return(c(red,green,blue))
}

rgb2hsv=function(vectRGB) {
  Cmax=max(vectRGB)
  Cmin=min(vectRGB)
  delta=Cmax-Cmin
  if(delta==0) {
    H=0
  }
  else {
    if(Cmax==vectRGB[1]) H=(1/6)*(((vectRGB[2]-vectRGB[3])/delta)%%6)
    if(Cmax==vectRGB[2]) H=(1/6)*(((vectRGB[3]-vectRGB[1])/delta)+2)
    if(Cmax==vectRGB[3]) H=(1/6)*(((vectRGB[1]-vectRGB[2])/delta)+4)
  }
  if(delta==0) {
    S=0
  }
  else {
    S=delta/Cmax
  }
  V=Cmax

  return(c(H,S,V))
}

### =========================================================================
### Ving's Main version beta 1.1
### -------------------------------------------------------------------------
###
{
if(version$major !="3" & version$minor !="0.2") {
  write("Ving has been developped on R 3.0.2,
therefore it might misbehave here.",stderr())
}
arg=commandArgs(TRUE)
if(length(arg)==0) {
  write("Ving for visualisation of RNA seq data: 
Usage:
    ./ving.R [options] <input> [<input2> <input3> ...]
  Options:
    -o/--output                     <string>     [default:./ouput.png]
    -F/--fileType                   <integer>    [default: same as -o] (png,jpeg,bmp,tiff,pdf)
    -R/--resolution                 <integer>    [default: 72        ]
    -v/--typeVisu                   <string>     [default: classic   ] (classic,lines,heatmap)
    -t/--description-data           <string>     [default: <filename>] 
    -n/--normalization-coefficient  <string>     [default: none      ]
    -i/--inverseStrand
    -u/--unstranded
    -c/--chromosome-name            <string>     [default: first one ]
    -S/--start                      <integer>    [default: 1         ]
    -E/--end                        <integer>    [default: chr length]
    -a/--annotation                 <filename>   
    -r/--typeTranscript             <string>     [default: <all>     ]
    -C/--annotation-colors          <string>     [default: fabulous  ] 
    -s/--annotation-shapes          <string>     [default: box       ] (box,line,rectangle,arrow)
    -l/--scale-log   
    -y/--symetric-scale
    --classic-plus-color            <string>     [default: navyblue  ]
    --classic-minus-color           <string>     [default: deeppink3 ]
    --heatmap-max-color             <string>     [default: 000055    ]
    --heatmap-min-color             <string>     [default: FFFFAA    ]
    --heatmap-palette-method        <string>     [default: hsv       ] (hsv,rgb)
    --lines-samples-colors          <string>     [default: black     ]
    --lines-samples-type-line       <string>     [default: plain     ]
    --annotation-color-by-strand    <logical>    [default: FALSE     ]
    --annotation-placed-by-strand   <logical>    [default: FALSE     ]
    -L/--smoothLength               <integer>    [default: NA        ]
    ",stdout())
  q("no")
}
else {
  tmp=suppressPackageStartupMessages(require("Rsamtools"))
  if(!tmp) {
    stop("Package Rsamtools required !!")
  }
  tmp=suppressPackageStartupMessages(require("GenomicRanges"))
  if(!tmp) {
    stop("Package GenomicRanges required !!")
  }
  optArgs=getopt(rbind(
	    c('output','o', 1, 'character',"output.png"),
	    c('fileType','F',1,'character',NA),
	    c('resolution','R',1,'numeric',72),
	    c('typeVisu', 'v', 1, 'character', "classic"),
	    c('description-data','t',1,'character',NA),
	    c('chromosome-name', 'c', 1, 'character', NA),
	    c('start', 'S', 1, 'numeric',1),
	    c('end', 'E', 1, 'numeric',NA),
	    c('annotation','a',1,'character',NA),
	    c('typeTranscript','r',1,'character',NA),
	    c('annotation-colors','C',1,'character',NA),
	    c('annotation-shapes','s','1','character',NA),  
	    c('normalization-coefficient','n','1','character',NA),
	    c('classic-plus-color',1,1,'character',"navyblue"),
	    c('classic-minus-color',2,1,'character',"deeppink3"),
	    c('heatmap-max-color',3,1,'character',"#000055"),
	    c('heatmap-min-color',4,1,'character',"#FFFFAA"),
	    c('heatmap-palette-method',7,1,'character',"hsv"),
	    c('lines-samples-colors',5,1,'character',NA),
	    c('lines-samples-type-line',6,1,'character',NA),
	    c('scale-log', 'l',0,'logical',FALSE),   
	    c('inverseStrand','i',0,'logical', FALSE),
	    c('unstranded','u',0,'logical', FALSE),
	    c('symetric-scale','y',0,'logical', FALSE),
	    c('annotation-color-by-strand',8,0,'logical',FALSE),
	    c('annotation-placed-by-strand',9,0,'logical',FALSE),
	    c('smoothLength','L',1,'numeric',NA)
	  ))
}

###################
## ARGUMENTS
#################################################################################
files=optArgs$ARGUMENT
## Case file doesn't exist
for( i in 1:length(files)) {
  if(!file.exists(files[i])) {
    stop(paste(files[i],"do not exist!","\n"))
  }
}
imagefile=optArgs$output
typeVisu=optArgs$typeVisu
description_data=optArgs$`description-data`
chrName=optArgs$`chromosome-name`[1]
start=optArgs$start[1]
end=optArgs$end[1]
annotation=optArgs$annotation
typeTranscript=optArgs$typeTranscript
colors=optArgs$`annotation-colors`
shape_data=optArgs$`annotation-shapes`
weight=optArgs$`normalization-coefficient`
classic_plus_color=optArgs$`classic-plus-color`
classic_minus_color=optArgs$`classic-minus-color`
heatmap_max_color=optArgs$`heatmap-max-color`
heatmap_min_color=optArgs$`heatmap-min-color`
heatmap_palette_method=optArgs$`heatmap-palette-method`
lines_samples_colors=optArgs$`lines-samples-colors`
lines_samples_type_line=optArgs$`lines-samples-type-line`
log=optArgs$`scale-log`
inverseStrand=optArgs$inverseStrand
unstranded=optArgs$unstranded
symetric=optArgs$`symetric-scale`
annotation_color_by_strand=optArgs$`annotation-color-by-strand`
annotation_placed_by_strand=optArgs$`annotation-placed-by-strand`
smoothLength=optArgs$`smoothLength`
resolution=optArgs$`resolution`
fileType=optArgs$`fileType`

###################
## MAIN
###################################################################################

genome_info=scanBamHeader(files[1])[[1]]$targets
noms_chromosomes=names(genome_info)
longueur_chromosomes=as.integer(genome_info)
nombre_chromosomes=length(noms_chromosomes)

## Case no chromosome specified
if(sum(is.na(chrName))) {
  chrName=noms_chromosomes[1]
  write(paste("No chromosome specified, processing chromosome :",chrName,"\n",sep=""),stderr())
}

## Case false chromosome name
if(!(chrName %in% noms_chromosomes)) {
  stop(paste("\"",chrName,"\" is not a proper chromosome name"))
}

if(is.na(end)) {
  end=longueur_chromosomes[chrName==noms_chromosomes]
}

if(start > end) {
  stop("The start is bigger than the end!")
}

## Case asked coordinates outside the chromosome
if(start<0|end>longueur_chromosomes[chrName==noms_chromosomes]) {
  stop("You are outside the chromosome")
}

if(sum(is.na(weight))>0) {
  normalized_data=NULL
  isnormalized=FALSE
}
else {
  isnormalized=TRUE
  normalized_data=unlist(strsplit(weight,split=","))
  if(length(files)!=length(normalized_data)) {
    stop("Different number of files and  weights ") 
  }
  else {
    normalized_data=as.numeric(normalized_data)
  }
}

if(inverseStrand) {
  libraryType="inverse"
}
else {
  libraryType="standard"
}
## Read the bam file and extract the infos
doit=function(i,libraryType) {
  try(readBam_(files[i], libraryType=libraryType, chrName_=chrName, from_=start, to_=end,normalized_=normalized_data[i]))
}
data=lapply(1:length(files),doit,libraryType=libraryType)
ctrl=unlist(lapply(data,class))=="try-error"

if(sum(ctrl)>0) {
  for(i in which(ctrl)) {
    write(paste("Problem with file :",files[i],"\n",sep=""),stderr())
  }
  stop("At least a file has problem")
}

## Read the GFF file and extract the infos
if(sum(is.na(annotation))==0) {
  gff=try(readGff(annotation[1],from=start,to=end,chr=chrName),TRUE)
  ctrl=class(gff)=="try-error"
  if(sum(ctrl)>0) {
      stop(paste("Problem with Gff file :",annotation,"\n"))	
  }
  if(length(annotation)>1) {
    for(i in 2:length(annotation)) {
      gff1=try(readGff(annotation[i],from=start,to=end,chr=chrName),TRUE)
      ctrl=class(gff1)=="try-error"
      if(sum(ctrl)>0) {
	stop(paste("Problem with Gff file :",gff1,"\n"))	
      }
      if(sum(is.na(gff1))==0) {
	gff=rbind(gff,gff1)
      }
    }
  }
}
else {
  gff=NA
}

if(sum(is.na(description_data))>0) {
  description_data=files
}

## Case of different number of files and description
if(length(description_data)!=length(files)) {
  stop("Different number of files and description")
}

## Pooling bam files (if necessary)
description_data_unique=unique(description_data)
data_pooled=list()
for(i_descri in description_data_unique) {
  i_data_voulu=which(description_data==i_descri)
  data_pooled_=data[[i_data_voulu[1]]]
  if(length(i_data_voulu)>1) {
    for(i_i_data_voulu in 2:length(i_data_voulu)) {
      data_pooled_=addBam(data_pooled_,data[[i_data_voulu[i_i_data_voulu]]])
    }
  }
  data_pooled[[i_descri]]=data_pooled_
}

if(sum(is.na(typeTranscript)>0)) {
  if(sum(is.na(annotation))==0) {
    typeTranscritSplit=unique(gff$Type)
  }
  else {
    typeTranscritSplit=NA
  }
}
else {
  typeTranscritSplit=unlist(strsplit(typeTranscript,split=","))
}

if((sum(is.na(gff))>0)|(sum(is.na(typeTranscritSplit))>0)) {
  annot=NULL
}
else {
  annot=gff[gff$Type %in% typeTranscritSplit,]
}

## Check the  colors
if(sum(is.na(colors)>0)) {
  nTypeTranscrit=length(typeTranscritSplit)
  if(annotation_color_by_strand) {
    colorsSplit=c(classic_plus_color,classic_minus_color)
  }
  else {
    colorsSplit=rainbow(nTypeTranscrit+1)[1:nTypeTranscrit]
  }
}
else {
  colorsSplit=unlist(strsplit(colors,split=","))
}
for(i in 1:length(colorsSplit)) {
  tmp=unlist(strsplit(colorsSplit[i],split=""))
  if(length(tmp)==6|length(tmp)==8) {
    if(sum(tmp %in% c(0:9,"A","B","C","D","E","F"))==length(tmp)) {
      colorsSplit[i]=paste("#",colorsSplit[i],sep="")
    }
  }
}

tmp=unlist(strsplit(classic_plus_color,split=""))
if(length(tmp)==6|length(tmp)==8) {
  if(sum(tmp %in% c(0:9,"A","B","C","D","E","F"))==length(tmp)) {
    classic_plus_color=paste("#",classic_plus_color,sep="")
  }
}
tmp=unlist(strsplit(classic_minus_color,split=""))
if(length(tmp)==6|length(tmp)==8) {
  if(sum(tmp %in% c(0:9,"A","B","C","D","E","F"))==length(tmp)) {
    classic_minus_color=paste("#",classic_minus_color,sep="")
  }
}
tmp=unlist(strsplit(heatmap_max_color,split=""))
if(length(tmp)==6|length(tmp)==8) {
  if(sum(tmp %in% c(0:9,"A","B","C","D","E","F"))==length(tmp)) {
    heatmap_max_color=paste("#",heatmap_max_color,sep="")
  }
} 
tmp=unlist(strsplit(heatmap_min_color,split=""))
if(length(tmp)==6|length(tmp)==8) {
  if(sum(tmp %in% c(0:9,"A","B","C","D","E","F"))==length(tmp)) {
    heatmap_min_color=paste("#",heatmap_min_color,sep="")
  }
} 
if(sum(is.na(lines_samples_colors))>0) {
  lines_samples_colors_split=c(1,3,4,2)[((0:(length(files)-1))%%4)+1]
}
else {
  lines_samples_colors_split=unlist(strsplit(lines_samples_colors,split=","))
  for(i in 1:length(lines_samples_colors_split)) {
    tmp=unlist(strsplit(lines_samples_colors_split[i],split=""))
    if(length(tmp)==6|length(tmp)==8) {
      if(sum(tmp %in% c(0:9,"A","B","C","D","E","F"))==length(tmp)) {
	lines_samples_colors_split[i]=paste("#",lines_samples_colors_split[i],sep="")
      }
    } 
  }
}
colorToCheck=c(colorsSplit,classic_plus_color,classic_minus_color,heatmap_max_color,heatmap_min_color,lines_samples_colors_split)
ctrl=sapply(colorToCheck,is.acceptable.color)
if(sum(!ctrl)>0) {
  for(i in which(!ctrl)) {
    write(paste("\"",colorToCheck[i],"\" is not a proper color name.\n",sep=""),stderr())
  }
  stop("At least one color has a problem")
}
if(annotation_color_by_strand) {
  if(length(colorsSplit)>2) {
    stop("You have to specify two and only two colors!")
  }
}
else {
  if(length(typeTranscritSplit)!=length(colorsSplit)) {
    stop("Please specify the same number of transcript types and colors")
  }
}
## Check the line types  
if(sum(is.na(lines_samples_type_line))>0) {
  lines_samples_type_line_split=((0:(length(files)-1))%/%4)+1
}
else {
  lines_samples_type_line_split=unlist(strsplit(lines_samples_type_line,split=","))
}
if(typeVisu=="lines") {
  ctrl=sapply(lines_samples_type_line_split,function(x) {
    tmp=suppressWarnings(as.numeric(x))
    if(!is.na(tmp)) {
      return((tmp==floor(tmp))&tmp>=1&tmp<=5)
    }
    else {
      return(FALSE)
    }
  })
  if(sum(!ctrl)>0) {
    for(i in which(!ctrl)) {
      write(paste("\"",lines_samples_type_line_split[i],"\" is not a proper line style.\n",sep=""),stderr())
    }
    stop("At least one line style has problem")
  }
  lines_samples_type_line_split=as.integer(lines_samples_type_line_split)
}
## Check the shapes
type_shape=rep(1,length(typeTranscritSplit))

if(sum(is.na(shape_data)>0)) {
  for( i in 1:length(typeTranscritSplit)) {
    type_shape[i]="box"
  }
}
else {
  shape=unlist(strsplit(shape_data,split=","))
  shape=as.array(shape)
  if(length(typeTranscritSplit)!=length(shape)) {
    stop("Please specify the same number of transcript types and shapes")
  }
  else {
    for( i in 1:length(typeTranscritSplit)) {
      type_shape[i]=shape[[i]]
    }
  }
}

## Style for the annotation
label=rep(1,length(typeTranscritSplit))
style=data.frame(Type=c(typeTranscritSplit,typeTranscritSplit),Strand=c(rep("+",length(typeTranscritSplit)),rep("-",length(typeTranscritSplit))),col=NA,shape=NA,label,stringsAsFactors=FALSE)
for(i in 1:length(typeTranscritSplit)) {
  style$shape[style$Type==typeTranscritSplit[i]]=type_shape[i]
}
if(annotation_color_by_strand) {
  style$col[style$Strand=="+"]=colorsSplit[1]
  style$col[style$Strand=="-"]=colorsSplit[2]
}
else {
  for(i in 1:length(typeTranscritSplit)) {
    style$col[style$Type==typeTranscritSplit[i]]=colorsSplit[i]
  }
}

## Main for visualisation
databychr=extractSignal(data_pooled,chrName,from=start,to=end,normalized_=isnormalized)

{
  reverse=list()
  forward=list()
  if(log) {
    for(i in 1:length(databychr$F)) {
      forward_=numeric()
      tmp=log2(1+databychr$F[[i]])
      forward_[1:length(tmp)]=tmp
      forward[[i]]=forward_
      reverse_=numeric()
      tmp=log2(1+databychr$R[[i]])
      reverse_[1:length(tmp)]=tmp
      reverse[[i]]=reverse_ 
    } 
  }
  else {
    forward=databychr$F
    reverse=databychr$R
  }
}

if(sum(is.na(smoothLength))>0) {
  smoothLength=max(trunc((end-start)/1200),1)
}
else {
  smoothLength=smoothLength[1]
}
if(unstranded) {
  for(i in 1:length(databychr$F)) {
    if(smoothLength>0) {
      lo=smooth(forward[[i]]+reverse[[i]],L=smoothLength)
    }
    else {
      lo=forward[[i]]+reverse[[i]]
    }
    forward[[i]]=lo
    los=rep(0,length(lo))
    reverse[[i]]=los
  }  
}
else {
  for(i in 1:length(databychr$F)) {
    if(smoothLength>0) {
      lo=smooth(forward[[i]],L=smoothLength)
    }
    else {
      lo=forward[[i]]
    }
    forward[[i]]=lo
    if(smoothLength>0) {
      los=smooth(reverse[[i]],L=smoothLength)
    }
    else {
      los=reverse[[i]]
    }
    reverse[[i]]=los
  }
}

group_maximum=rep(1,length(databychr$F))
max_forward=numeric(length(databychr$F))
max_reverse=numeric(length(databychr$F))
for(i_data in 1:length(databychr$F)) {
  max_forward[i_data]=max(forward[[i_data]],na.rm=TRUE)
  max_reverse[i_data]=max(reverse[[i_data]],na.rm=TRUE)
}
for(i_max in unique(group_maximum)) {
  max_forward[group_maximum==i_max]=max(max_forward[group_maximum==i_max],na.rm=TRUE)
  max_reverse[group_maximum==i_max]=max(max_reverse[group_maximum==i_max],na.rm=TRUE)
}

if(symetric) {
  for(i_data in 1:length(databychr$F)) {
    max_forward[i_data]=max(max_forward[i_data],max_reverse[i_data],na.rm=TRUE)
  }
  max_reverse=max_forward
}

if(is.na(fileType)) {
  imageFileSplit=unlist(strsplit(imagefile,split=".",fixed=TRUE))
  if(length(imageFileSplit)>=2) {
    fileType=imageFileSplit[length(imageFileSplit)]
  }
  else {
    fileType="png"
  }
}

plotVisu(imagefile,typeVisu=typeVisu,listForward=forward,listReverse=reverse,
debut_vue=start,fin_vue=end,chr=chrName,annot=annot,repeated=TRUE,
titres=description_data_unique,name_flags="",style=style,log=log,stranded=!unstranded,
tops=max_forward,bottoms=max_reverse,
classic_plus_color=classic_plus_color,classic_minus_color=classic_minus_color,
heatmap_max_color=heatmap_max_color,heatmap_min_color=heatmap_min_color,heatmap_palette_method=heatmap_palette_method,
lines_samples_colors=lines_samples_colors_split,lines_samples_type_line=lines_samples_type_line_split,
smoothLength=1,annotation_color_by_strand=annotation_color_by_strand,annotation_placed_by_strand=annotation_placed_by_strand,
fileType=fileType,resolutionDPI=resolution)

}
