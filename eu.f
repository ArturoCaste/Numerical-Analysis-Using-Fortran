c	Programa para el metodo de Euler
	program euler
c INTRODUCIMOS LOS VALORES INICIALES
	a=0
	b=1
	N=10
	h=abs(b-a)/N
	yo=g(a)
	To= yo
	i=0
	write(*,*) 'i','a', '	Taylor k=0' , 'Taylor k=3', '	Analitica'
	do while (a.LE.b)
		ya=g(a)
		diff=abs(yo-ya)
		dif2= abs(to-ya)
		write(*,*) i, a, diff, dif2
		ym= yo + h*f(a,yo)
		tm= to + h*T3(a,to)
		to=tm 
		yo=ym
		a= a + h 
		i= i+ 1
	end do
	
	
	end program
	
	function g(t)
	g= exp(-t) + 2*t - 2
	end function
	
	function f(t,y)
	f= 2*t - y
	end function
	
	function T3(t,y)
	T3= 2*t - y + (0.433333)*(2*(1 - t)+ y)
	end function
