c     programa newton raphson
      program newton
c     se debe ingresar un estimado incial x0
      real m
      pi = 3.141592654
      open(1,file='sec24.dat',status='unknown')
      open(2,file='sec241.dat',status='unknown')
c     write(*,*) 'Ingrese x0' 
c      read(*,*) x0
      x0 = 0.37
      x1 = 0.34
c     ingresamos una tolerancia
c     write(*,*) 'Ingrese la tolerancia' 
c     read(*,*) tol
      tol = 1e-4
      N= 100
      write(*,*) '¿Cuál es el intervalo a graficar?'
      read(*,*) a,b

c     Creamos los datos para graficar la funcion
      z=(abs(a)+ abs(b))/100
      m=a
c      write(2,*)'	x		F(x)'
      do while (m.LE.b)
      	Fg=fob(m)
     	write(2,*) m,Fg
      	m=m+z
      end do
c     Evaluamos la función
      F0 = f(x0)
      F1 = f(x1)
      If (abs(F0).LT.tol.or.(abs(F1).LT.tol)) then
              write(1,*) 'Entonces x0 o x1 son la raíz del sistema'
              write(1,*) x0 , x1
              goto 2
      end if 
      dif = 1
      i=1
      write(1,*) '          n ','  x0    ','     x1 
     &',' x2 ',' Diferencia '
      do while (abs(dif).GT.tol)
        f0 = fob(x0)
        f1 = fob(x1)
        x2 = x1 - f1*(x1 - x0)/(f1 -f0)
        dif = abs(x2 - x1)       
        write(1,*) i,x0,x1,x2,dif
        if (i.EQ.100) then
          write(1,*) 'El método no converge'
          goto 2
        end if        
        i = i + 1 
        x0 = x1
        x1 = x2 
      end do   

2     stop
      close (1,status='keep')
      close (2,status='keep')

      end program

c**********************************************************************************************************
      function fob(x)
c**********************************************************************************************************
      
      fob = -1564000 + 1000000*exp(x) + (435000/x)*( exp(x) - 1 )
      
      end function
c----------------------------------------------------------------------------------------------------------

c**********************************************************************************************************
      function f(x)
c**********************************************************************************************************
      
      f = -1564000 + 1000000*exp(x) + (435000/x)*( exp(x) - 1 )
      
      end function
c----------------------------------------------------------------------------------------------------------


