c      programa graficas
       program grafics2
       implicit none
       integer::i,j,n
       real:: ra
       real,allocatable::a(:),res(:),b(:),c(:),d(:)
       real,allocatable::p(:),y(:),q(:),r(:),po(:),inf(:)
       n=9
       allocate (a(n),res(n),b(n),c(n),d(n))
       allocate (p(n),y(n),q(n),r(n),po(n),inf(n))
       open(1,file='tom2.dat',status='unknown')
       y(0)=-1
       y(1)=1
       y(2)=1
       y(3)=1
       y(4)=1
       y(5)=1
       y(6)=1
       y(7)=1
       y(8)=1
       y(9)=1
       y(10)=0
c***********************************************************************************************
       do i=1,n
       p(i)=(1-(0.05*exp(0.1*i)))*y(i-1)
       y(i)=(-2+0.01*(-0.1*i))*y(i)
       q(i)=(1+0.05*exp(0.1*i))*y(i+1)
       r(i)=(0.01*(exp(-(0.1*i))*(-(0.1*i)**2+2*(0.1*i)-3)-(0.1*i)+2))
       end do

       po(1)=0
       po(2)=1
       po(3)=1
       po(4)=1
       po(5)=1
       po(6)=1
       po(7)=1
       po(8)=1
       po(9)=1

       do i=1,n
             inf(i)=(1-(0.05*exp(0.1*i)))*po(i)
       end do
c**************************************************************************************************   
c      Diagonal principal
       b(1)=y(1)
       b(2)=y(2)
       b(3)=y(3)
       b(4)=y(4)
       b(5)=y(5)
       b(6)=y(6)
       b(7)=y(7)
       b(8)=y(8)
       b(9)=y(9)

c      Diagonal superior
       c(1)=q(1)
       c(2)=q(2)
       c(3)=q(3)
       c(4)=q(4)
       c(5)=q(5)
       c(6)=q(6)
       c(7)=q(7)
       c(8)=q(8)
       c(9)=q(9)
      
c     Diagonal inferior
       a(1)=inf(1)
       a(2)=inf(2)
       a(3)=inf(3)
       a(4)=inf(4)
       a(5)=inf(5)
       a(6)=inf(6)
       a(7)=inf(7)
       a(8)=inf(8)
       a(9)=inf(9)
      
c     Matriz de respuestas
       d(1)=r(1)-p(1)
       d(2)=r(2)
       d(3)=r(3)
       d(4)=r(4)
       d(5)=r(5)
       d(6)=r(6)
       d(7)=r(7)
       d(8)=r(8)
       d(9)=r(9)
c*******************************************************************
       do j=2,n,1
           ra=a(j)/b(j-1)
           b(j)=b(j)-ra*c(j-1)
           d(j)=d(j)-ra*d(j-1)
       end do
       res(n)=d(n)/b(n)
       write(1,*)'Resultados'
       do j=n-1,1,-1
          res(j)=(d(j)-c(j)*res(j+1))/b(j)     
       end do
       
       write(1,*)res
    
       close (1,status='keep')
       end program



