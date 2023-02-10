c     programa graficas
      program grafics2
      double precision x0,y0,Fg,pem,rk4,fob,pm,rk,eum,eu,er3,er2,er,z
c     se debe ingresar un estimado incial x0
      open(1,file='ta81.dat',status='unknown')
      open(2,file='ta82.dat',status='unknown')
      open(3,file='ta83.dat',status='unknown')
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
      b= 1
c     Creamos los datos para graficar las funciones
      z=(abs(a)+ abs(b))/1000
      x0=0
      y0 = -1
c      write(2,*)'	x		F(x)'
      do while (x0.LE.b)
      	Fg=fob(x0)
      	er = abs(1 - (y0/Fg))*100
      	write(1,*) y0,Fg,er
     	eum = eu(x0,y0)
      	y0 = eum 
      	x0= x0 + h
      end do
      
      x0=0d0
      y0 = -1d0
      
      do while (x0.LE.b)
      	Fg=fob(x0)
      	er2 = abs(1 - (y0/Fg))*100
      	write(2,*) y0,er2
     	pem = pm(x0,y0)
      	y0 = pem
      	x0= x0 + h 
      end do
      
      x0=0d0
      y0 = -1d0
      
      do while (x0.LE.b)
      	Fg=fob(x0)
      	er3 = abs(1 - (y0/Fg))*100
      	write(3,*) x0,y0
     	rk4 = rk(x0,y0)
      	y0 = rk4
      	
      	x0= x0 + h
      end do
   
     

      stop
      close (1,status='keep')
      close (2,status='keep')
      close (3,status='keep')

      end program

c**********************************************************************************************************
      function fob(x)
c**********************************************************************************************************
      double precision fob,x
      fob = exp(-x) + 2*x - 2
      
      end function
c----------------------------------------------------------------------------------------------------------


c**********************************************************************************************************
      function eu(x,y)
c**********************************************************************************************************
      double precision k1,k2,eu,x,y
      h=0.1
      k1 = 2*x - y
      k2 = 2*(x + h) - (y + k1*h)
      eu = y + (0.05)*(k1 + k2)
c      write(*,*) x,k1,k2
      end function
c----------------------------------------------------------------------------------------------------------
      
      
c**********************************************************************************************************
      function pm(x,y)
c**********************************************************************************************************
      double precision k1,k2,pm,x,y
      h=0.1
      k1 = 2*x - y
      k2 = 2*(x + 0.05) - y - k1*(0.05)
      pm = y + h*k2
c      write(*,*) k1,k2
      
      end function
c----------------------------------------------------------------------------------------------------------
      
      
c**********************************************************************************************************
      function rk(x,y)
c**********************************************************************************************************
      double precision k1,k2,k3,k4,rk,x,y
      h=0.1
      k1 = 2*x - y
      k2 = 2*(x + 0.05) - (y + k1*(0.05))
      k3 = 2*(x + 0.05) - (y + k2*(0.05))
      k4 = 2*(x + 0.1) - (y + k3*h)
      rk = y + (k1 + 2*k2 + 2*k3 + k4)/60
      write(*,*) k1,k2,k3,k4
      
      end function
c----------------------------------------------------------------------------------------------------------
      
  





