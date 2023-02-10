c     programa graficas
      program grafics2
      real x0,y0,Fg,pem,rk4,rk,eum,eu,er3,er2,er,z,f,dfob,fob,der,dif,tol
      
c     se debe ingresar un estimado incial x0
  
      open(2,file='ta82.dat',status='unknown')
     
c     write(*,*) 'Ingrese x0' 
c      read(*,*) x0
c     ingresamos una tolerancia
c     write(*,*) 'Ingrese la tolerancia' 
c     read(*,*) tol
      tol = 1e-5
      
c      write(*,*) '¿Cuál es el intervalo a graficar?'
c      read(*,*) a,b
      a = 1
      b= 11
c     Creamos los datos para graficar las funciones
      z=(abs(a)+ abs(b))/1000
      
c********************************  ************** 
      x0= 1d0
     
      i=1
      do while (abs(dif).GT.tol)
        f = fob(x0)
        der = dfob(x0)
        x2 = x0 - (f/der)
        dif = abs(x2 - x0)       
        write(*,*) i,x0,f,der,dif
        if (i.EQ.100) then
          write(2,*) 'El método no converge'
        end if        
        x0 = x2
        i = i + 1 
      end do   
     

      stop
  
      close (2,status='keep')
   

      end program



c**********************************************************************************************************
      function fob(x)
c**********************************************************************************************************
      real fob
      
      fob =  (1/x**2)*sin(1/x**2) + cos(1/x**2) - x
      
      end function
c----------------------------------------------------------------------------------------------------------

c**********************************************************************************************************
      function dfob(x)
c**********************************************************************************************************
      real  dfob,d
      d = 2* cos(1 /x**2)
      fob =  - (d/x**5) - 1
      
      end function
c----------------------------------------------------------------------------------------------------------

