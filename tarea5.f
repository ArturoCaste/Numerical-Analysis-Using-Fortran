c     programa graficas
      program grafics2
      double precision x0,y0,t0,a,b,z,Fg,dfob,fob,Fg1,erx,ery
      double precision k1,c1,k2,c2,k3,c3,k4,c4,f,g
c     se debe ingresar un estimado incial x0
      open(1,file='tablachida.dat',status='unknown')
      open(2,file='ta52.dat',status='unknown')
      open(3,file='ta53.dat',status='unknown')
     
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
      b= 1.1

      
      
      x0 =  1d0
      y0 =  2d0
      t0 = 0
      
      do while (t0.le.b)
      	Fg = dfob(t0)
      	Fg1 = fob(t0)
	erx = abs(x0-Fg)
	ery = abs(y0-Fg1)
	k1 = f(t0,x0,y0)
	c1 = g(t0,x0,y0)
	k2 = f(t0 + 0.5*h , x0 + 0.5*h*k1, y0 + 0.5*h*c1)
	c2 = g(t0 + 0.5*h , x0 + 0.5*h*k1, y0 + 0.5*h*c1)
	k3 = f(t0 + 0.5*h , x0 + 0.5*h*k2, y0 + 0.5*h*c2)
	c3 = g(t0 + 0.5*h , x0 + 0.5*h*k2, y0 + 0.5*h*c2)
	k4 = f(t0 + h, x0 + h*k3, y0 + h*c3)
	c4 = g(t0 + h, x0 + h*k3, y0 + h*c3)
	write(1,*) t0,x0,y0,Fg,Fg1,erx,ery
	write(2,*) t0,x0,Fg,erx
	write(3,*) t0,y0,Fg1,ery
	x0 = x0 + h*(k1 + 2*k2 + 2*k3 + k4)/6
	y0 = y0 + h*(c1 + 2*c2 + 2*c3 + c4)/6
	t0 = t0 + h
      end do
      

      stop
      close (1,status='keep')
      close (2,status='keep')
      close (3,status='keep')
      end program

c**********************************************************************************************************
      function fob(t0)
c**********************************************************************************************************
      double precision fob,t0
      
      fob = 3*exp(t0) - exp(-t0)
   
      end function
c----------------------------------------------------------------------------------------------------------

c**********************************************************************************************************
      function dfob(t0)
c**********************************************************************************************************
      double precision dfob,t0
      dfob = 3*exp(t0) - 2*exp(-t0)
      
      end function
c----------------------------------------------------------------------------------------------------------

c**********************************************************************************************************
      function f(t0,x0,y0)
c**********************************************************************************************************
      double precision f,x0,y0,t0
      
      f = - 3*x0 + 4*y0 
   
      end function
c----------------------------------------------------------------------------------------------------------

c**********************************************************************************************************
      function g(t0,x0,y0)
c**********************************************************************************************************
      double precision g,x0,y0,t0
      g = - 2*x0 + 3*y0 
      
      end function
c----------------------------------------------------------------------------------------------------------


