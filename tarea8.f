c     programa graficas
      program grafics2
      double precision y1,y2,y3,x1,t0,a,b,dfob,fob,erx,gam1,gam2 
      double precision k1,c1,k2,c2,k3,c3,k4,c4,f,g,q
c     se debe ingresar un estimado incial x0
      open(7,file='ta7.dat',status='unknown')
      open(8,file='ta8.dat',status='unknown')
      open(9,file='ta9.dat',status='unknown')
     


      tol = 1e-6
      h = 0.1
      a = 0 
      beta = 2
      b= 3
      gam1 = 0.353854745
      y1 = 2**(0.5)
      x1 = 0.353854745
      t0 = 1
      
      
20    do while (t0.le.b)
        
	k1 = f(t0,y1,x1)
	c1 = g(t0,y1,x1)
	k2 = f(t0 + 0.5*h , y1 + 0.5*h*k1, x1 + 0.5*h*c1)
	c2 = g(t0 + 0.5*h , y1 + 0.5*h*k1, x1 + 0.5*h*c1)
	k3 = f(t0 + 0.5*h , y1 + 0.5*h*k2, x1 + 0.5*h*c2)
	c3 = g(t0 + 0.5*h , y1 + 0.5*h*k2, x1 + 0.5*h*c2)
	k4 = f(t0 + h, y1 + h*k3, x1 + h*c3)
	c4 = g(t0 + h, y1 + h*k3, x1 + h*c3)
	y1 = y1 + h*(k1 + 2*k2 + 2*k3 + k4)/6
	x1 = x1 + h*(c1 + 2*c2 + 2*c3 + c4)/6
	write(7,*) t0,y1
	t0 = t0 + h
	
      end do
c****************************************************************************************************      
      gam2 = 0.353551745
      x1 = 0.353551745
      y2 =  2**(0.5)
      t0 = 1
     
c****************************************************************************************************      
      
      
      do while (t0.le.b)
	k1 = f(t0,y2,x1)
	c1 = g(t0,y2,x1)
	k2 = f(t0 + 0.5*h , y2 + 0.5*h*k1, x1 + 0.5*h*c1)
	c2 = g(t0 + 0.5*h , y2 + 0.5*h*k1, x1 + 0.5*h*c1)
	k3 = f(t0 + 0.5*h , y2 + 0.5*h*k2, x1 + 0.5*h*c2)
	c3 = g(t0 + 0.5*h , y2 + 0.5*h*k2, x1 + 0.5*h*c2)
	k4 = f(t0 + h, y2 + h*k3, x1 + h*c3)
	c4 = g(t0 + h, y2 + h*k3, x1 + h*c3)
	y2 = y2 + h*(k1 + 2*k2 + 2*k3 + k4)/6
	x1 = x1 + h*(c1 + 2*c2 + 2*c3 + c4)/6
	write(8,*) t0,y2
	t0 = t0 + h
	
      end do
 
      
      dif1 = abs(y1 - 2)
      dif2 = abs(y2 - 2)
    
      gam3 = gam1 + (gam2 - gam1)*( (2 - y1)/(y2 - y1))
      gam1 = gam2
      gam2 = gam3
      y1 = gam1 
      y2 = gam2
      x1 = 1.4
      write(*,*) y1,y2
      t0 = 1
      
      
      do while (t0.le.b)
        dfob = fob(t0)
        write(9,*) t0,dfob
	
	t0 = t0 + h
	
      end do
      
      
      write (*,*) y1,y2
      
c****************************************************************************************************      

   
      
      stop
  
      close (7,status='keep')
      close (8,status='keep')
      close (9,status='keep')
     
      end program

c**********************************************************************************************************
      function fob(t0)
c**********************************************************************************************************
      double precision fob,t0
      
      fob = (1 + t0)**(0.5)
   
      end function
c----------------------------------------------------------------------------------------------------------

c**********************************************************************************************************
      function f(t0,x0,x1)
c**********************************************************************************************************
      double precision f,x0,x1,t0
      
      f = x1 
   
      end function
c----------------------------------------------------------------------------------------------------------

c**********************************************************************************************************
      function g(t0,x0,x1)
c**********************************************************************************************************
      double precision g,x0,x1,t0
      g =  -(x1**2)/x0 
      
      end function
c----------------------------------------------------------------------------------------------------------


