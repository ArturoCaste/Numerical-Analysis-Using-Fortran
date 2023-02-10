       program thomas2
       implicit none
       integer::i,j,k,n
       real:: m
       double precision,allocatable::a(:),res(:),b(:),c(:),d(:),p(:),y(:),q(:),r(:),pe(:),s(:)
       open(1,file='2.dat',status='unknown')
       n=4
       allocate (a(n),res(n),b(n),c(n),d(n),x(n),p(n),y(n),q(n),r(n),pe(n),s(n))
       y(0)=-1
       y(1)=1
       y(2)=1
       y(3)=1
       y(4)=1
       y(5)=0

       do i=1,n
         p(i)=(1-(0.1*exp(0.2*i)))*y(i-1)
         y(i)=(-2+0.01*(-0.2*i))*y(i)
         q(i)=(1+0.1*exp(0.2*i))*y(i+1)
         r(i)=(0.01)*(exp(-(0.2*i))*(-(0.2*i)**2+2*(0.2*i)-3)-(0.2*i)+2)
       end do
       pe(1)=0
       pe(2)=1
       pe(3)=1
       pe(4)=1

       do i=1,n
          s(i)=(1-(0.1*exp(0.2*i)))*pe(i)
       end do
       
c      Diagonal principal
       b(1)=y(1)
       b(2)=y(2)
       b(3)=y(3)
       b(4)=y(4)

c      Diagonal superior
       c(1)=q(1)
       c(2)=q(2)
       c(3)=q(3)
       c(4)=q(4)

c      Diagonal inferior
       a(1)=s(1)
       a(2)=s(2)
       a(3)=s(3)
       a(4)=s(4)
 
c      Matriz respuestas 
       d(1)=r(1)-p(1)
       d(2)=r(2)
       d(3)=r(3)
       d(4)=r(4)

       do k=2,n,1
            m=a(k)/b(k-1)
            b(k)=b(k)-m*c(k-1)
            d(k)=d(k)-m*d(k-1)
       end do
       res(n)=d(n)/b(n)

       do k=n-1,1,-1
            res(k)=(d(k)-c(k)*res(k+1))/b(k)
       end do

       write(1,*)'Resultados'
       write(1,*)resultado
       stop
       close (1,status='keep')
       end program














