c     programa graficas
      program grafics
c     se debe ingresar un estimado incial x0
      real x,x0,y0,Ff,Fg,v,c,m
      
      pi = 3.141592654
      open(1,file='eul1.dat',status='unknown')
      open(2,file='eul2.dat',status='unknown')
      open(3,file='eul3.dat',status='unknown')
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
      z=(abs(a)+ abs(b))/10
      x0=a
c      write(2,*)'	x		F(x)'
      do while (x0.LE.b)
      	Fg=fob(x0)
     	write(2,*) x0,Fg
      	x0= x0 + h 
      end do
     
      y0 = -1
      x0 = 0
      
      
      do while (x0.LE.b)
      	Ff=  f(x0,y0)
      	y0 = Ff
      	Fe1  = fob(x0 + h)
      	er1 = abs(Ff - Fe1)
     	write(1,*) x0,Fe1,er1,Ff
      	x0=x0 + h
      end do
      
    
      

      stop
      close (1,status='keep')
      close (2,status='keep')
      close (3,status='keep')

      end program

c**********************************************************************************************************
      function fob(x)
c**********************************************************************************************************
      
      fob = exp(-x) + 2*x - 2
      
      end function
c----------------------------------------------------------------------------------------------------------

c**********************************************************************************************************
      function f(x,y)
c**********************************************************************************************************
      h=0.1
      
      f = y + h*(2*x - y)
      
      end function
c----------------------------------------------------------------------------------------------------------
      


