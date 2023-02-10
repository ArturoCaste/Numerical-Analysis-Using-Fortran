c     programa graficas
      program grafics2
      real x0,y0,t0,a,m,f2,x1,x2,f1,n1,n2
 
c     se debe ingresar un estimado incial x0
      open(1,file='ta1.dat',status='unknown')
      open(2,file='ta2.dat',status='unknown')
      open(3,file='ta3.dat',status='unknown')
      open(4,file='ta4.dat',status='unknown')
      open(5,file='ta5.dat',status='unknown')
      open(6,file='ta6.dat',status='unknown')
      open(7,file='ta7.dat',status='unknown')
c      
      open(9,file='ta9.dat',status='unknown')
      open(10,file='ta10.dat',status='unknown')
      open(11,file='ta11.dat',status='unknown')
c      
      open(13,file='ta13.dat',status='unknown')
      open(14,file='ta14.dat',status='unknown')
      open(15,file='ta15.dat',status='unknown')
     
c     write(*,*) 'Ingrese x0' 
c      read(*,*) x0
c     ingresamos una tolerancia
c     write(*,*) 'Ingrese la tolerancia' 
c     read(*,*) tol
      tol = 1e-16
      h = 0.01
c      write(*,*) '¿Cuál es el intervalo a graficar?'
c      read(*,*) a,b
   
    
   
      
      x0 =  0.2703
      y0 =  -0.9628
      x1 =  0.8719
      y1 =  0.4896
      
      t0 = -1
      m = 0.1
     
      
      do j= -3,-1,1
         t0 = -1
         y0 = m*j*(-0.9628)
         x0 = m*j*0.2703
         y1 = m*j*0.4896
         x1 = m*j*0.8719
         do i = 0,20,1
             n2 = f2(t0,x0,y0)
             n1 = f1(t0,x1,y1)
             t0 = t0 + m
             write( (-1*j) + 12,*) i,t0,n1,n2
         end do 
      end do 
      
      
      x0 =  0.2703
      y0 =  -0.9628
      x1 =  0.8719
      y1 =  0.4896
      
      
      do j= 0,3,1
         t0 = -1
         y0 = m*j*(-0.9628)
         x0 = -1*m*j*0.2703
         y1 = m*j*0.4896
         x1 = -1*m*j*0.8719       
         do i = 0,20,1
             n2 = f2(t0,x0,y0)
             n1 = f1(t0,x1,y1)
             t0 = t0 + m
             write(j,*) i,t0,n1,n2
         end do 
      end do 
      
      
      x0 =  0.2703
      y0 =  -0.9628
      x1 =  0.8719
      y1 =  0.4896
      
      
      do j= -3,-1,1
         t0 = -1
         y0 = m*j*(-0.9628)
         x0 =  -1*m*j*0.2703
         y1 = m*j*0.4896
         x1 =  -1*m*j*0.8719
         do i = 0,20,1
             n2 = f2(t0,x0,y0)
             n1 = f1(t0,x1,y1)
             t0 = t0 + m
             write(  (-1*j) + 8,*) i,t0,n1,n2
         end do 
      end do 
      
      
      x0 =  0.2703
      y0 =  -0.9628
      x1 =  0.8719
      y1 =  0.4896
      
      
      do j= 1,3,1
         t0 = -1
         y0 = m*j*(-0.9628)
         x0 = m*j*0.2703
         y1 = m*j*0.4896
         x1 = m*j*0.8719
         do i = 0,20,1
             n2 = f2(t0,x0,y0)
             n1 = f1(t0,x1,y1)
             t0 = t0 + m
             write(j + 4,*) i,t0,n1,n2
         end do 
      end do 
      
      
      

      stop
      close (1,status='keep')
      close (2,status='keep')
      close (3,status='keep')
      close (4,status='keep')
      close (5,status='keep')
      close (6,status='keep')
      close (7,status='keep')
      close (9,status='keep')
      close (10,status='keep')
      close (11,status='keep')
      close (13,status='keep')
      close (14,status='keep')
      close (15,status='keep')
      end program

c------------------------------------------------------------------------------------

c**********************************************************************************************************
      function f1(t0,x0,y0)
c**********************************************************************************************************
      real y0,x0,t0,f1
      
      f1 = x0*exp(-1.5616*t0) + y0*exp(2.5616*t0) 
   
      end function
c----------------------------------------------------------------------------------------------------------

c**********************************************************************************************************
      function f2(t0,x1,y1)
c**********************************************************************************************************
      real x1,y1,t0,f2
      f2 = x1*exp(-1.5616*t0) + y1*exp(2.5616*t0) 
      
      end function
c----------------------------------------------------------------------------------------------------------


