c     programa newton raphson
      program newton
c     se debe ingresar un estimado incial x0
      real m
      pi = 3.141592654
      open(1,file='nw24.dat',status='unknown')
      open(2,file='nw241.dat',status='unknown')
c     write(*,*) 'Ingrese x0' 
c      read(*,*) x0
      x0 = -0.9
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
      F1 = fob(x0)
      If (abs(F1).LT.tol) then
              write(1,*) 'Entonces x0 es la solución del sistema'
              write(1,*) x0
              goto 2
      end if 
      dif = 1
      i=1
      write(1,*) '          n ','  Punto res   ','     Función (x0)
     &',' Función dev(p_n)',' Diferencia obtenida '
      do while (abs(dif).GT.tol)
        f = fob(x0)
        der = dfob(x0)
        x2 = x0 - (f/der)
        dif = abs(x2 - x0)       
        write(1,*) i,x0,f,der,dif
        if (i.EQ.100) then
          write(1,*) 'El método no converge'
          goto 2
        end if        
        i = i + 1 
        x0 = x2
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
      function dfob(x)
c**********************************************************************************************************
      
      dfob = + 1000000*exp(x) + (435000*x**(-2))*( exp(x)*(x - 1) +1)
      
      end function
c----------------------------------------------------------------------------------------------------------

