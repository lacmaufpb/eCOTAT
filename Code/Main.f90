Program eCOTATplus

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! eCOTAT+ algorithm
!
! Improvement from COTAT+ algorithm:
! Paz, A.R.; Collischonn, W.; Silveira, A.L.L. Improvements in large scale drainage networks derived from digital elevation models. Water Resources Research, v. 42, p. W08502, 2006.
! https://doi.org/10.1029/2005WR004544

! Input files: 
! * DIRHIGH (.rst;.rdc)  high resolution flow directions: raster, Idrisi/TerrSet format, integer/binary (see codification below)
! * AREAHIGH (.rst;.rdc) high resolution flow accumulated area: raster, Idrisi/TerrSet format, real/binary, attributes in km2
! * INPUT_UPSCALING (.txt) upscaling configuration (ascii)
!
! Output files:
! * DIRLOW (.rst;.rdc)  low resolution flow directions: raster, Idrisi/TerrSet format, integer/binary (see codification below)
!
!   The names of the input files are defined along the code and could be changed.
!   The number of cores for the parallelization should be adjusted in the lines 368 and 610
!   in the 'num_threads()' argument prior to the two parallelized loops.
!


!>>>>>>>>>>>>>>>>>>>> VARIABLE DEFINITION >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

use vars_rdc
use OMP_LIB


implicit none

character(len=9) :: texto3,texto4  
character(len=8) :: texto5           
character(len=12) :: texto6          
integer :: ncolpix,nlinpix    
real :: respix,rescel     
integer :: linpix,colpix
integer*2,allocatable:: dirpix(:,:),exu(:,:)
real,allocatable:: aacpix(:,:)
character (len=40) :: texto7
character (len=13) :: texto8

integer :: fim,plin,pcol     
integer :: lincel,colcel
integer :: nlincel,ncolcel
integer*2,allocatable:: dircel(:,:),marcel(:,:),vizmarcel(:,:),histocel(:,:),histo(:,:)
integer*2,allocatable:: marcacel(:,:)
integer histomax

integer :: linpixini,colpixini,linpixfin,colpixfin
integer :: lincelini,colcelini,lincelfin,colcelfin

real MaiorArea
integer linexu,colexu
integer,allocatable:: linpixex(:,:)
integer,allocatable:: colpixex(:,:)

integer linaux,colaux
integer caminho
integer diraux
real auxl,auxc,auxl2,auxc2
integer lincelaux,colcelaux
real AreaInc,LimiteInc
integer deltalin,deltacol

integer foraviz,exut
integer dd
integer A,B,C,D,E,F,G,H
integer ArcView
integer LimiteCam

real LimiteCamkm,LimiteCamr

integer montante
integer foracel
real MaiorAreaExu
integer contacam
integer passo2
integer linaux2,colaux2
real MaiorArea2
integer linaux3,colaux3
real,allocatable:: AreaAux(:)
integer ex,nex,auxex
real AreaJus,AreaMont

integer cruzamento
integer dircelSE,dircelSD,dircelIE,dircelID
integer correcao
integer aacpixSE,aacpixSD,aacpixIE,aacpixID

integer :: dlin(128),dcol(128),ddaux(8),inv(128)
integer laux,caux
integer RespMar

integer dl, dc,invaux
integer lincelaux2,colcelaux2
integer caux99,laux99

integer cont,linexuH,colexuH

real respalta,respbaixa
real plinr,pcolr
real dx,Xmin,Xmax,Ymin,Ymax

character (len=40) :: texto1
character (len=13) :: texto2

integer metro



integer*2,allocatable :: lininicel(:,:),colinicel(:,:)
integer*2,allocatable :: linfincel(:,:),colfincel(:,:)

integer(4) resp
integer ilin,icol
integer*2 lixoi2
real lixor
integer*2,allocatable:: dirpix3(:,:)
real,allocatable:: aacpix3(:,:),aacexucel(:,:)
integer direxu
real aacexu,coordxaux,coordyaux
integer linexuabs,colexuabs,linauxabs,colauxabs
integer ilin4


integer numcel,Nnumcel
integer*2,allocatable :: Lcel(:),Ccel(:)
real ifileX,ifileY
real tempoCPU1,tempoCPU2,tempoCPU3,tempoCPU4,tempoCPU5,tempoCPU6

integer numbl,Nnumbl,tambl,jahresolvido
integer,allocatable :: lininibl(:),linfinbl(:),colinibl(:),colfinbl(:)
integer lininiblaux,linfinblaux,coliniblaux,colfinblaux,linbl,colbl
integer*2,allocatable :: blcel(:)

write(*,*) "---- eCOTAT+ ALGORITHM ---- "
write(*,*)

!>>>>>>>>>>>>>>>>>> PARAMETER DEFINITION AND INPUT FILES >>>>>>>>>>>>>>>>
!
write(*,*) "1. PARAMETER DEFINITION AND INPUT FILES..."
write(*,*)



!FLOW DIRECTION CODE

!   G  H  A             COTAT+:   64  128  1 
!   F  *  B                       32   *   2
!   E  D  C                       16   8   4

A=1   
B=2   
C=4   
D=8   
E=16  
F=32  
G=64  
H=128 

ddaux(1)=1
ddaux(2)=2
ddaux(3)=4
ddaux(4)=8
ddaux(5)=16
ddaux(6)=32
ddaux(7)=64
ddaux(8)=128

!relative line and column positions according to each flow direction
dlin(A)=-1
dcol(A)=1
dlin(B)=0
dcol(B)=1
dlin(C)=1
dcol(C)=1
dlin(D)=1
dcol(D)=0
dlin(E)=1
dcol(E)=-1
dlin(F)=0
dcol(F)=-1
dlin(G)=-1
dcol(G)=-1
dlin(H)=-1
dcol(H)=0

!opposite flow directions
inv(A)=E
inv(B)=F
inv(C)=G
inv(D)=H
inv(E)=A
inv(F)=B
inv(G)=C
inv(H)=D



	!define input file: flow direction - raster integer/binary, Idrisi/TerrSet format
	nomeRST='DIRHIGH.rst'
	nomeRDC='DIRHIGH.rdc'
	open(10,file=NomeRDC)
	read(10,'(A)') texto1
	read(10,'(A)') texto1
	read(10,'(A)') texto1
	read(10,'(A)') texto1
	read(10,'(A,1I10)') texto2,ncolpix
	read(10,'(A,1I10)') texto2,nlinpix
	read(10,'(A14,A7)') texto20,sistemaref
	read(10,'(A14,A3)') texto20,unidaderef3
	read(10,'(A)') texto1
	read(10,'(A,F)') texto2,Xmin
	read(10,'(A,F)') texto2,Xmax
	read(10,'(A,F)') texto2,Ymin
	read(10,'(A,F)') texto2,Ymax
	read(10,'(A)') texto1
	read(10,'(A,F)') texto2,dx
	close(10)    

if (unidaderef3=='deg') then 
     !reference system uses degrees - it is assumed as latlong for writing .rdc of output raster files
    metro=0
else
    !assuming reference system in meters
    metro=1
end if

!define input file: upscaling configuration 
open(21,file='input_upscaling.txt')
read(21,*)
read(21,*) respalta   !High resolution cell size (same units of the input raster)
read(21,*)
read(21,*) respbaixa  !Low resolution cell size (same units of the input raster)
read(21,*)
read(21,*) tambl      !Size of the tile in number of coarse resolution cells (1 tile = tambl.cells^2)
read(21,*)
read(21,*) limiteInc  !Parameter AT (km2)
read(21,*)
read(21,*) limitecamkm !Parameter MUFP (km)
close(21)


plinr=(respbaixa/respalta)
plin=anint(plinr)
pcol=plin


nlincel=nlinpix/plin
ncolcel=ncolpix/pcol


allocate (dircel(nlincel,ncolcel))
allocate (linpixex(nlincel,ncolcel))
allocate (colpixex(nlincel,ncolcel))
allocate (histocel(nlincel,ncolcel))
allocate (marcacel(nlincel,ncolcel))
allocate (lininicel(nlincel,ncolcel))
allocate (colinicel(nlincel,ncolcel))
allocate (aacexucel(nlincel,ncolcel))
dircel=0
linpixex=0
colpixex=0
areaaux=0
histocel=0
marcacel=0
lininicel=0
colinicel=0
aacexucel=0

if (metro==0) then
    LimiteCamR=LimiteCamkm/100.0/respalta !converting from km to number of pixels, assuming 1 degree = 100 km
    LimiteCam=int(LimiteCamR)
else
    LimiteCamR=LimiteCamkm*1000.0/respalta !converting from km to number of pixels
    LimiteCam=int(LimiteCamR)
end if



! initialization of variables
lincel=1
colcel=1
dircel=-99
linpixex=1
colpixex=1
foraviz=0
colpixini=1
colpixfin=pcol    
linpixini=1
linpixfin=plin
fim=0

write(*,*) "STARTING THE ALGORITHM..."

write(*,*)


!>>>>>>>>>>>>>>>>>>>>>>>> TILE DEFINITION >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

write(*,*) "2. DEFINING THE TILES..."

numcel=0
numbl=0
do colcel=1,ncolcel
     do lincel=1,nlincel
        if ((mod(colcel,tambl)==0).and.(mod(lincel,tambl)==0)) then
            numbl=numbl+1
            coliniblaux=colcel-tambl+1    
            lininiblaux=lincel-tambl+1
            colfinblaux=colcel
            linfinblaux=lincel
            do colbl=coliniblaux,colfinblaux
                do linbl=lininiblaux,linfinblaux
                    numcel=numcel+1
                end do
            end do
        end if
    end do
end do
Nnumcel=numcel
Nnumbl=numbl

allocate(colinibl(Nnumbl))
allocate(lininibl(Nnumbl))
allocate(colfinbl(Nnumbl))
allocate(linfinbl(Nnumbl))
allocate(blcel(Nnumcel))
allocate(Lcel(Nnumcel))
allocate(Ccel(Nnumcel))
Lcel=0
Ccel=0
blcel=0
colinibl=0
lininibl=0
colfinbl=0
linfinbl=0

numcel=0
numbl=0
do colcel=1,ncolcel
     do lincel=1,nlincel
        if ((mod(colcel,tambl)==0).and.(mod(lincel,tambl)==0)) then
            numbl=numbl+1
            colinibl(numbl)=colcel-tambl+1    
            lininibl(numbl)=lincel-tambl+1
            colfinbl(numbl)=colcel
            linfinbl(numbl)=lincel
            do colbl=colinibl(numbl),colfinbl(numbl)
                do linbl=lininibl(numbl),linfinbl(numbl)
                    numcel=numcel+1
                    blcel(numcel)=numbl
                    Lcel(numcel)=linbl
                    Ccel(numcel)=colbl
                end do
            end do
        end if
    end do
end do



!>>>>>>>>>>>>>>>>>>>>>>>> DETERMINING THE OUTLET PIXEL FOR EACH LOW RESOLUTION CELL >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

write(*,*) "3. DETERMINING THE OUTLET PIXEL FOR EACH LOW RESOLUTION CELL..."


!define the number of cores to parallelization in the 'num_threads()' argument
!$OMP PARALLEL DO NUM_THREADS(4) DEFAULT (SHARED) PRIVATE (numbl,numcel,lincel,colcel,linpixini,linpixfin, &
!$OMP colpixini,colpixfin,ilin,icol,lixoi2,lixor,linpix,colpix,dirpix,aacpix,MaiorArea,linexu, &
!$OMP colexu,montante,passo2,exut,AreaAux,nex,MaiorAreaExu,auxex,ex,linaux2,colaux2,contacam, &
!$OMP linaux,colaux,linaux3,colaux3,AreaJus,dl,dc,dd,invaux,i, &
!$OMP MaiorArea2,histo,histomax,linexuH,colexuH,caminho)	


do numbl=1,Nnumbl

    linpixini=(lininibl(numbl)-1)*plin+1
    linpixfin=linfinbl(numbl)*plin
    colpixini=(colinibl(numbl)-1)*plin+1
    colpixfin=colfinbl(numbl)*plin 
    ilin=0
    icol=0
    allocate(dirpix(plin*tambl,plin*tambl))
    allocate(aacpix(plin*tambl,plin*tambl))
    allocate(histo(plin*tambl,plin*tambl))
    allocate(areaaux(plin*plin*tambl*tambl))
    dirpix=0
    aacpix=0
    histo=0
    AreaAux=0            

    !read high resolution input files exclusively for the area located inside the low resolution cell
    open(numbl,file='DIRHIGH.rst',status='old',form='unformatted',access='direct',RECL=2*ncolpix)
    open(numbl+100000,file='AREAHIGH.rst',status='old',form='unformatted',access='direct',RECL=4*ncolpix)
    do linpix=linpixini,linpixfin
        ilin=ilin+1
        read(numbl,REC=linpix) (lixoi2, colpix=1,colpixini-1), (dirpix(ilin,colpix-colpixini+1),colpix=colpixini,colpixfin), (lixoi2, colpix=colpixfin+1,ncolpix)
        read(numbl+100000,REC=linpix) (lixor, colpix=1,colpixini-1), (aacpix(ilin,colpix-colpixini+1),colpix=colpixini,colpixfin), (lixor, colpix=colpixfin+1,ncolpix)
    end do
    close(numbl)
    close(numbl+100000)


   do numcel=(numbl-1)*tambl*tambl+1,numbl*tambl*tambl

        lincel=Lcel(numcel)
        colcel=Ccel(numcel)
        linpixini=(lincel-lininibl(numbl))*plin+1
        linpixfin=linpixini+plin-1
        colpixini=(colcel-colinibl(numbl))*plin+1
        colpixfin=colpixini+plin-1      
        
        
        ilin=0
        icol=0           

          MaiorArea=0
          linexu=0
          colexu=0
          montante=0
          passo2=0
          exut=0
          AreaAux=0

              do while (passo2==0)
               !loop for searching the pixel with largest accumulation area
	            if (exut==1) then      !a potential outlet pixel was disregarded
	              nex=nex+1
	              AreaAux(nex)=MaiorAreaExu
                  MaiorAreaExu=0
	            else                  !first time an outlet pixel is being searched
	              AreaAux=0
	              MaiorAreaExu=0
	              nex=0
	            end if

		            do linpix=linpixini,linpixfin
		              do colpix=colpixini,colpixfin
		               if ((linpix==linpixini).OR.(linpix==linpixfin).OR.(colpix==colpixini).OR.(colpix==colpixfin)) then !testa se estah na borda
			               auxex=0
			               if (aacpix(linpix,colpix)>=MaiorAreaExu) then
				             if (nex>0) then
				               do ex=1,nex
					             if (aacpix(linpix,colpix)<AreaAux(ex)) then
					               auxex=auxex+1
					             end if
				               end do
				               if (auxex==nex) then
					               MaiorAreaExu=aacpix(linpix,colpix)
   					               linexu=linpix
					               colexu=colpix
					               exut=1
				               end if
				             else
				               MaiorAreaExu=aacpix(linpix,colpix)
   				               linexu=linpix
				               colexu=colpix
				               exut=1
				             end if	 
			               end if
			            end if
		              end do
		            end do
		            !verifies the minimum upstream flow path criterion
		            linaux2=linexu
		            colaux2=colexu
		            contacam=0
		            linaux=linexu
		            colaux=colexu
		            linaux3=linexu
		            colaux3=colexu
		            montante=0
		            AreaJus=MaiorAreaExu
		            do while (montante==0)
		              MaiorArea=0
		              do linpix=linaux2-1,linaux2+1
			            do colpix=colaux2-1,colaux2+1
			              if ((linpix>=linpixini).AND.(linpix<=linpixfin).AND.(colpix>=colpixini).AND.(colpix<=colpixfin)) then
				            if ((linpix/=linaux2).OR.(colpix/=colaux2)) then
				              if ((linpix/=linaux3).OR.(colpix/=colaux3)) then
					            if (aacpix(linpix,colpix)>MaiorArea) then
					              if (aacpix(linpix,colpix)<AreaJus) then						            
						            dl=linpix-linaux2
						            dc=colpix-colaux2
						            dd=dirpix(linpix,colpix)
						            invaux=inv(dd)
						            do i=1,8					 
						              if (invaux==ddaux(i)) then			   
							            if ((dlin(invaux)==dl).AND.(dcol(invaux)==dc)) then					     
							              MaiorArea=aacpix(linpix,colpix)					
							              linaux=linpix
							              colaux=colpix								  
							            end if
						              end if
						            end do
					              end if
					            end if
				              end if
				            end if
			              end if
			            end do
		              end do
		              AreaJus=MaiorArea
		              linaux3=linaux2
		              colaux3=colaux2
            		  
            		  if ((linaux<linpixini).or.(linaux>linpixfin).or.(colaux<colpixini).or.(colaux>colpixfin)) then
   			            montante=1              !pixel outside the cell -> flow path ends
            		  else
			            montante=0
			            contacam=contacam+1     !pixel inside the cell -> flow path continues
			            linaux2=linaux
			            colaux2=colaux
			            MaiorArea2=MaiorArea           		        
            		  end if
            		  if (contacam>LimiteCam) then
			            montante=1              !criterion ok -> flow path ends
		              end if
		              

		            end do     

		            if (contacam>LimiteCam) then
		              linpixex(lincel,colcel)=linexu-plin+lininibl(numbl)*plin
		              colpixex(lincel,colcel)=colexu-plin+colinibl(numbl)*plin
		              passo2=1      !outlet pixel is ok according to minimum upstream flow path
		            else
		              !outlet pixel is not ok according to minimum upstream flow path
		                                    
		              !continues testing other potential outlet pixels in the descending order of flow accumulation area
			            do colpix=colpixini,colpixfin  	            
			              do linpix=linpixini,linpixfin
				            linaux=linpix
				            colaux=colpix
				            caminho=0
				            do while (caminho==0)
				              dd=dirpix(linaux,colaux)
				              linaux=linaux+dlin(dd)
				              colaux=colaux+dcol(dd)

				              if ((linaux<linpixini).OR.(linaux>linpixfin).OR.(colaux<colpixini).OR.(colaux>colpixfin)) then
					            caminho=1
				              else
						        caminho=0
						            if ((linaux==linpixini).OR.(linaux==linpixfin).OR.(colaux==colpixini).OR.(colaux==colpixfin)) then
						              histo(linaux,colaux)=histo(linaux,colaux)+1
						            end if
						      end if 
				            end do 
			              end do
			            end do
                
			            histomax=0
			            do colpix=colpixini,colpixfin
			              do linpix=linpixini,linpixfin
				            if (histo(linpix,colpix)>histomax) then
				              histomax=histo(linpix,colpix)
				              linexuH=linpix
				              colexuH=colpix
				            end if
			              end do
			            end do
                
			            if ((linexu==linexuH).AND.(colexu==colexuH)) then
		                  linpixex(lincel,colcel)=linexu-plin+lininibl(numbl)*plin
		                  colpixex(lincel,colcel)=colexu-plin+colinibl(numbl)*plin	              	              
			              histocel(lincel,colcel)=histomax
			              passo2=1
			            else
			              passo2=0
 				            do colpix=colpixini,colpixfin
				              do linpix=linpixini,linpixfin
				                 histo(linpix,colpix)=0
				              end do
				            end do
			            end if

		            end if
              end do             
   end do
   
    deallocate(dirpix)
    deallocate(aacpix)
    deallocate(histo)
    deallocate(Areaaux)

end do

!$OMP END PARALLEL DO



!>>>>>>>>>>>>>>>>>>>>>>>>> FLOW DIRECTION DETERMINATION >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

write(*,*) "4. FLOW DIRECTION DETERMINATION FOR EACH LOW RESOLUTION CELL"

!cells located in the border of the image drain out the image

do colcel=1,ncolcel
  dircel(1,colcel)=H
  dircel(nlincel,colcel)=D
  do lincel=1,nlincel
    dircel(lincel,1)=F
	dircel(lincel,ncolcel)=B
  end do
end do


!define the number of cores to parallelization in the 'num_threads()' argument
!$OMP PARALLEL DO NUM_THREADS(4) DEFAULT (SHARED) PRIVATE (numbl,lincel,colcel,i,linpixini,linpixfin,colpixini,colpixfin, &
!$OMP ilin,icol,linpix,lixoi2,lixor,colpix,dirpix3,aacpix3,linexuabs,colexuabs,linexu,colexu,aacexu,direxu, &
!$OMP caminho,linaux,colaux,exut,dd,diraux,foraviz,linauxabs,colauxabs,lincelaux,colcelaux,AreaInc,numcel, &
!$OMP deltacol,deltalin,jahresolvido)


do numbl=1,Nnumbl

    linpixini=(lininibl(numbl)-1)*plin+1
    linpixfin=(linfinbl(numbl))*plin
    colpixini=(colinibl(numbl)-1)*plin+1
    colpixfin=(colfinbl(numbl))*plin 

    linpixini=linpixini-plin
    linpixfin=linpixfin+plin
    colpixini=colpixini-plin
    colpixfin=colpixfin+plin    

    if (linpixini<1) then
        linpixini=1
    end if
    if (linpixfin>nlincel*plin) then
        linpixfin=nlincel*plin
    end if
    if (colpixini<1) then
        colpixini=1
    end if
    if (colpixfin>ncolcel*plin) then
        colpixfin=ncolcel*plin
    end if
    ilin=0
    icol=0

    allocate(dirpix3(plin*tambl+2*plin,plin*tambl+2*plin))
    allocate(aacpix3(plin*tambl+2*plin,plin*tambl+2*plin))
    dirpix3=0
    aacpix3=0
    
    
    
    !read high resolution input files exclusively for the area needed for solving the specific low resolution cell 
    open(numbl,file='DIRHIGH.rst',status='old',form='unformatted',access='direct',RECL=2*ncolpix)
    open(numbl+100000,file='AREAHIGH.rst',status='old',form='unformatted',access='direct',RECL=4*ncolpix)
    do linpix=linpixini,linpixfin
        ilin=ilin+1
        read(numbl,REC=linpix) (lixoi2, colpix=1,colpixini-1), (dirpix3(ilin,colpix-colpixini+1),colpix=colpixini,colpixfin), (lixoi2, colpix=colpixfin+1,ncolpix)
        read(numbl+100000,REC=linpix) (lixor, colpix=1,colpixini-1), (aacpix3(ilin,colpix-colpixini+1),colpix=colpixini,colpixfin), (lixor, colpix=colpixfin+1,ncolpix)
    end do
    close(numbl)
    close(numbl+100000)
    
    
    do numcel=(numbl-1)*tambl*tambl+1,numbl*tambl*tambl
            
       lincel=Lcel(numcel)
       colcel=Ccel(numcel)
       jahresolvido=0
       if ((lincel==1).or.(lincel==nlincel)) then
           jahresolvido=1
       end if
       if ((colcel==1).or.(colcel==ncolcel)) then
           jahresolvido=1
       end if      
       if (jahresolvido==0) then
            i=i+1              
            linpixini=(lincel-2)*plin+1
            linpixfin=(lincel+1)*plin
            colpixini=(colcel-2)*plin+1
            colpixfin=(colcel+1)*plin 
            if (linpixini<1) then
                linpixini=1
            end if
            if (linpixfin>nlincel*plin) then
                linpixfin=nlincel*plin
            end if
            if (colpixini<1) then
                colpixini=1
            end if
            if (colpixfin>ncolcel*plin) then
                colpixfin=ncolcel*plin
            end if
            ilin=0
            icol=0

            linexuabs=linpixex(lincel,colcel) 
	        colexuabs=colpixex(lincel,colcel)

            if (lininibl(numbl)==1) then
                linexu=linexuabs  
            else
                linexu=linexuabs+2*plin-lininibl(numbl)*plin
            end if 
            if (colinibl(numbl)==1) then
                colexu=colexuabs  
            else
                colexu=colexuabs+2*plin-colinibl(numbl)*plin
            end if 
      
	        aacexucel(lincel,colcel)=aacpix3(linexu,colexu)
	        aacexu=aacpix3(linexu,colexu)
	        direxu=dirpix3(linexu,colexu)

		    caminho=0
		    linaux=linexu
		    colaux=colexu
		    exut=0
		    dd=direxu
		    if (linexuabs==nlinpix) then 
		      if (colexuabs==ncolpix) then
			    dircel(lincel,colcel)=B
			    caminho=1
		      else
			    if (colexuabs==1) then
			      dircel(lincel,colcel)=F
			      caminho=1	
			    else
			      if ((dd==C).OR.(dd==D).OR.(dd==E).OR.(dd==H)) then
				    dircel(lincel,colcel)=D
	  			    caminho=1
			      end if
			    end if
		      end if
		    end if
		    if (linexuabs==1) then 
		      if (colexuabs==ncolpix) then
			    dircel(lincel,colcel)=B
		      else  	  
			    if (colexuabs==1) then
			      dircel(lincel,colcel)=F
			      caminho=1				
			    else
			      if ((dd==A).OR.(dd==D).OR.(dd==G).OR.(dd==H)) then
				    dircel(lincel,colcel)=H
				    caminho=1
			      end if
			    end if
		      end if
		    end if
		    if (colexuabs==ncolpix) then 
		      if ((linexuabs/=nlinpix).AND.(linexuabs/=1)) then	    
			    if ((dd==A).OR.(dd==B).OR.(dd==C).OR.(dd==F)) then
			      dircel(lincel,colcel)=B
			      caminho=1
			    end if
		      end if
		    end if
		    if (colexuabs==1) then 
		      if ((linexuabs/=nlinpix).AND.(linexuabs/=1)) then
			    if ((dd==B).OR.(dd==E).OR.(dd==F).OR.(dd==G)) then
			      dircel(lincel,colcel)=F
			      caminho=1
			    end if
		      end if
		    end if

		    ! if the cell does not drain out of the image,
		    ! the flow path downstream the outlet pixel of the cell is traced to define its flow direction
		    do while (caminho==0)
		      diraux=dirpix3(linaux,colaux)
		      foraviz=0
		      linaux=linaux+dlin(diraux)
		      colaux=colaux+dcol(diraux)
    		  
	           if (lininibl(numbl)==1) then
                    linauxabs=linaux  
                else
                    linauxabs=linaux-2*plin+lininibl(numbl)*plin
                end if 
                if (colinibl(numbl)==1) then
                    colauxabs=colaux  
                else           
                    colauxabs=colaux-2*plin+colinibl(numbl)*plin
                end if 
     
 
		         if (linauxabs<linpixini) then
		               lincelaux=lincel-2
		         else
		            if (linauxabs<(lincel-1)*plin+1) then
		                lincelaux=lincel-1
		            else
		                if (linauxabs<(lincel*plin)+1) then
		                    lincelaux=lincel
		                else
		                    if (linauxabs<(lincel+1)*plin) then
		                        lincelaux=lincel+1
		                    else
		                        lincelaux=lincel+2
		                    end if
		                end if
		            end if
		         end if 

    		 
    		 
		         if (colauxabs<colpixini) then
		               colcelaux=colcel-2
		         else
		            if (colauxabs<(colcel-1)*plin+1) then
		                colcelaux=colcel-1
		            else
		                if (colauxabs<(colcel*plin)+1) then
		                    colcelaux=colcel
		                else
		                    if (colauxabs<(colcel+1)*plin) then
		                        colcelaux=colcel+1
		                    else
		                        colcelaux=colcel+2
		                    end if
		                end if
		            end if
		         end if 

         
		 ! verifies if the flow path traced is out of the 3x3 neighbouring cells
		 ! (in this case, the flow direction of the cell is maintained, according to
		 !  i. the last outlet pixel found, if any has been found; ii. the neighbouring cell
		 !  last visited during flow path tracing) 
		     if ((lincelaux>lincel+1).OR.(colcelaux>colcel+1)) then
		       foraviz=1
		       caminho=1
		     end if
		     if ((lincelaux<lincel-1).OR.(colcelaux<colcel-1)) then
		       foraviz=1
		       caminho=1
		     end if

		     if ((foraviz==1).AND.(exut==0)) then  
    		                 
	   	     ! The flow path traced is out of the 3x3 neighbouring cells, without reaching any outlet pixel
             ! The flow direction of the cell is defined according to the neighbouring cell
		     ! last visited during flow path tracing
	
			     if (lincelaux>lincel+1) then  
			       if (colcelaux<=colcel-1) then 
				     dircel(lincel,colcel)=E
			       end if
			       if (colcelaux==colcel) then  
				     dircel(lincel,colcel)=D
			       end if
			       if (colcelaux>=colcel+1) then 
				     dircel(lincel,colcel)=C
			       end if
			     end if
			     if (lincelaux<lincel-1) then 
			       if (colcelaux<=colcel-1) then 
				     dircel(lincel,colcel)=G
			       end if
			       if (colcelaux==colcel) then 
				     dircel(lincel,colcel)=H
			       end if
			       if (colcelaux>=colcel+1) then 
				     dircel(lincel,colcel)=A
			       end if
			     end if
			     if (colcelaux>colcel+1) then 
			       if (lincelaux<=lincel-1) then 
				     dircel(lincel,colcel)=A
			       end if
			       if (lincelaux==lincel) then  
				     dircel(lincel,colcel)=B
			       end if
			       if (lincelaux>=lincel+1) then 
				     dircel(lincel,colcel)=C
			       end if
			     end if
			     if (colcelaux<colcel-1) then 
			       if (lincelaux<=lincel-1) then
				     dircel(lincel,colcel)=G  
			       end if
			       if (lincelaux==lincel) then
				     dircel(lincel,colcel)=F 
			       end if
			       if (lincelaux>=lincel+1) then
				     dircel(lincel,colcel)=E 
			       end if
			     end if
		      end if

		      if ((foraviz==1).AND.(exut==1)) then    
       		    !The flow path traced is out of the 3x3 neighbouring cells and at least one outlet pixel was reached 
	    	    !(nothing changes now) 
			    dircel(lincel,colcel)=dircel(lincel,colcel)
		      end if

         ! verifies if the flow path is out of the image regarding the current pixel being traced
	     if ((linauxabs<1).OR.(linauxabs>nlinpix)) then
		       foraviz=1
		       caminho=1
		       if (exut==0) then 
			     dircel(lincel,colcel)=direxu
		       else
			     dircel(lincel,colcel)=dircel(lincel,colcel)
		       end if
		     end if 
		     if ((colauxabs<1).OR.(colauxabs>ncolpix)) then
		       foraviz=1
		       caminho=1
		       if (exut==0) then
			     dircel(lincel,colcel)=direxu
		       else
			     dircel(lincel,colcel)=dircel(lincel,colcel)
		       end if
		     end if 

		     if (foraviz==0)then
                !verifies if the pixel reached is an outlet pixel
  		       if (linauxabs==linpixex(lincelaux,colcelaux)) then
			     if (colauxabs==colpixex(lincelaux,colcelaux)) then 
				     exut=1
			       !verifies the area threshold criterion
				     AreaInc=aacpix3(linaux,colaux)-aacexu		    
				     if (AreaInc>LimiteInc) then
				       caminho=1
				     end if
				     deltacol=colcelaux-colcel
				     deltalin=lincelaux-lincel
				     if (deltacol==1) then
				       if (deltalin==-1) then
					     dircel(lincel,colcel)=A
				       end if
				       if (deltalin==0) then
					     dircel(lincel,colcel)=B
				       end if
				       if (deltalin==1) then
					     dircel(lincel,colcel)=C
				       end if
				     end if
				     if (deltacol==0) then
				       if (deltalin==-1) then
					     dircel(lincel,colcel)=H
				       end if
				       if (deltalin==0) then
					     dircel(lincel,colcel)=0
				       end if
				       if (deltalin==1) then
					     dircel(lincel,colcel)=D
				       end if
				     end if
				     if (deltacol==-1) then
				       if (deltalin==-1) then
					     dircel(lincel,colcel)=G
				       end if
				       if (deltalin==0) then
					     dircel(lincel,colcel)=F
				       end if
				       if (deltalin==1) then
					     dircel(lincel,colcel)=E
				       end if
				     end if             
			     end if
		       end if
		     end if
		    end do 
        
	    end if 
	    
    end do 
    
    		    
	deallocate(dirpix3)
    deallocate(aacpix3)
    
end do 

!$OMP END PARALLEL DO

!>>>>>>>>>>>>>>>>>>> VERIFYING AND CORRECTING CROSSING PATHS >>>>>>>>>>>>
cruzamento=0
correcao=0
do while (correcao==0)  
  do lincel=2,nlincel-1
  
    do colcel=2,ncolcel-1
		  dircelSE=dircel(lincel,colcel)
		  dircelSD=dircel(lincel,colcel+1)
		  dircelIE=dircel(lincel+1,colcel)
		  dircelID=dircel(lincel+1,colcel+1)
		  aacpixSE=aacexucel(lincel,colcel)  
		  aacpixSD=aacexucel(lincel,colcel+1)
		  aacpixIE=aacexucel(lincel+1,colcel)
		  aacpixID=aacexucel(lincel+1,colcel+1) 
		  if ((dircelIE==A).AND.(dircelID==G)) then
			cruzamento=cruzamento+1
			if ((dircelSE/=D).AND.(aacpixIE<aacpixID)) then
			  dircelIE=H
			  dircel(lincel+1,colcel)=dircelIE
			else
			  dircelID=H
			  dircel(lincel+1,colcel+1)=dircelID
			end if
		  end if
		  if ((dircelSE==C).AND.(dircelSD==E)) then
			cruzamento=cruzamento+1
			if ((dircelIE/=H).AND.(aacpixSE<aacpixSD)) then
			  dircelSE=D
			  dircel(lincel,colcel)=dircelSE
			else
			  dircelSD=D
			  dircel(lincel,colcel+1)=dircelSD
			end if
		  end if
		  if ((dircelSD==E).AND.(dircelID==G)) then
			cruzamento=cruzamento+1
			if ((dircelSE/=B).AND.(aacpixSD<aacpixID)) then
			  dircelSD=F
			  dircel(lincel,colcel+1)=dircelSD
			else
			  dircelID=F
			  dircel(lincel+1,colcel+1)=dircelID
			end if
		  end if
		  if ((dircelSE==C).AND.(dircelIE==A)) then
			cruzamento=cruzamento+1
			if ((dircelSD/=F).AND.(aacpixSE<aacpixIE)) then
			  dircelSE=B
			  dircel(lincel,colcel)=dircelSE
			else
			  dircelIE=B
			  dircel(lincel+1,colcel)=dircelIE
			end if
		  end if
		end do
	   end do
	 
	  if (cruzamento==0) then
		correcao=1
	  else
		correcao=0
	  end if
	  cruzamento=0
end do  


!>>>>>>>>>>>>>>>>>>>> WRITING OUTPUT FILES >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

write(*,*) "5.  WRITING OUTPUT FILES..."
write(*,*)
write(*,*) "DirLow.rst - (integer/binary Idrisi/TerrSet raster format)"
write(*,*) " - low resolution flow directions"

metrordc=metro
    
open(60,file='DIRLOW.rst',status='unknown',form='unformatted',access='direct',RECL=2*ncolcel)
do lincel=1,nlincel
  write(60,REC=lincel) (dircel(lincel,colcel),colcel=1,ncolcel)
end do
close(60)

	dx3=respbaixa

	nlin3=nlincel
	ncol3=ncolcel
	tipodado=1
	tipoMM=2


	allocate(VarMM2(nlincel,ncolcel))
	varmm2=0


	varMM2=dircel
	i3=0
	xmin3=xmin
	xmax3=xmax
	ymin3=ymin
	ymax3=ymax
	nomeRST='DirLow.rst'
	call MinMax
	call EscreveRDC

	deallocate(varMM2)


write(*,*)
write(*,*)
write(*,*) "eCOTAT+ algorithm has finished (press enter)"
write(*,*)
pause

end Program eCOTATplus

