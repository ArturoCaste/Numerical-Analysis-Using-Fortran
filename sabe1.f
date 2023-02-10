c     programa graficas
      program grafics2
      real a,h,b,a1,a2,a3
      real c11,c12,c13,c14,c15
      real c21,c22,c23,c24,c25
      real c31,c32,c33,c34,c35
      real c51,c52,c53,c54,c55
      real f1,f2,f3
c     se debe ingresar un estimado incial x0
      open(1,file='tav.dat',status='unknown')
      
     
c     write(*,*) 'Ingrese x0' 
c      read(*,*) x0
c     ingresamos una tolerancia
c     write(*,*) 'Ingrese la tolerancia' 
c     read(*,*) tol
      tol = 1e-16
      h = 0.1
c      write(*,*) '¿Cuál es el intervalo a graficar?'
c      read(*,*) a,b
      a = 0 
      b= 21

      
      
      a1 = 5
      a2 = 5
      a3 = 5
      t0 = 0
      
      do while (t0.le.b)
c**************************************************************************************************	      	
        c11 = f1(t0,a1,a2,a3)
        c12 = f2(t0,a1,a2,a3)
        c13 = f3(t0,a1,a2,a3)
        c14 = f4(t0,a1,a2,a3)
        c15 = f5(t0,a1,a2,a3)
c**************************************************************************************************	
        c21 = f1(t0+0.5*h,a1+h*c11*0.5,a2+h*c12*0.5,a3+h*c13*0.5)
        c22 = f2(t0+0.5*h,a1+0.5*h*c11,a2+0.5*h*c12,a3+0.5*h*c13)
        c23 = f3(t0+0.5*h,a1+0.5*h*c11,a2+0.5*h*c12,a3+0.5*h*c13)
        c24 = f4(t0+0.5*h,a1+0.5*h*c11,a2+0.5*h*c12,a3+0.5*h*c13)
        c25 = f5(t0+0.5*h,a1+0.5*h*c11,a2+0.5*h*c12,a3+0.5*h*c13)
c**************************************************************************************************	
c	c31 = f1(t0 + 0.5*h , a1 + 0.5*h*c21, a2 + 0.5*h*c22, a3 + 0.5*h*c23 )
c	c32 = f2(t0 + 0.5*h , a1 + 0.5*h*c21, a2 + 0.5*h*c22, a3 + 0.5*h*c23 ) 
c	c33 = f3(t0 + 0.5*h , a1 + 0.5*h*c21, a2 + 0.5*h*c22, a3 + 0.5*h*c23 )
c	c34 = f4(t0 + 0.5*h , a1 + 0.5*h*c21, a2 + 0.5*h*c22, a3 + 0.5*h*c23 )
c	c35 = f5(t0 + 0.5*h , a1 + 0.5*h*c21, a2 + 0.5*h*c22, a3 + 0.5*h*c23 )
c**************************************************************************************************	
c	c41 = f1( t0 + h, a1 + h*c31, a2 + h*c32, a3 + h*c33 )
c	c42 = f2( t0 + h, a1 + h*c31, a2 + h*c32, a3 + h*c33 )
c	c43 = f3( t0 + h, a1 + h*c31, a2 + h*c32, a3 + h*c33 )
c	c44 = f4( t0 + h, a1 + h*c31, a2 + h*c32, a3 + h*c33 )
c	c45 = f5( t0 + h, a1 + h*c31, a2 + h*c32, a3 + h*c33 )
c***************************************************************************************************	
	write(1,*) t0,a1,a2,a3
c	
c	a1 = a1 + h*(c11 + 2*c21 + 2*c31 + c41)/6
c	a2 = a2 + h*(c12 + 2*c22 + 2*c32 + c42)/6
c	a3 = a3 + h*(c13 + 2*c23 + 2*c33 + c43)/6
c	t0 = t0 + h
      end do
      
      stop
      close (1,status='keep')
     
      end program


c**********************************************************************************************************
      function f1(t0,a1,a2,a3)
c**********************************************************************************************************
      real f1,a1,a2,a3
      
      f1 = - 10*a1 + 10*a3 
   
      end function
c----------------------------------------------------------------------------------------------------------

c**********************************************************************************************************
      function f2(t0,a1,a2,a3)
c**********************************************************************************************************
      real f2,a1,a2,a3
      f2 = 28*a1 - a2 - a1*a3  
      
      end function
c----------------------------------------------------------------------------------------------------------

c**********************************************************************************************************
      function f3(t0,a1,a2,a3)
c**********************************************************************************************************
      real f3,a1,a2,a3
      
      f3 = -2.6666667*a3 + a1*a2 
   
      end function
c----------------------------------------------------------------------------------------------------------



c**********************************************************************************************************
      function f4(t0,a1,a2,a3)
c**********************************************************************************************************
      real f4,a1,a2,a3

      f4 = -2.6666667*a3 + a1*a2

      end function
c----------------------------------------------------------------------------------------------------------

c**********************************************************************************************************
      function f5(t0,a1,a2,a3)
c**********************************************************************************************************
      real f5,a1,a2,a3

      f5 = -2.6666667*a3 + a1*a2

      end function
c----------------------------------------------------------------------------------------------------------






