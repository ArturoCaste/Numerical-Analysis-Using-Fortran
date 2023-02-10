c 	Método de BAIRSTOW para el ejercicio 3 de la tarea 2
	program bairstow
        a4 = 1
        a3 = -0.22411958
        a2 = 0.011658361
        a1 = -5.422539e-4
        a0 = 1.251e-6
        r0 = -1.1
        so = -1.1
        EE = 1e-5
c------------------------------------------------------------------        
        r = r0
        s = s0
        Ear = 100
        Eas = 100
        a22 = a2
        do while (ear.gt.ee.and.eas.gt.ee)
                b4 = a4
                b3 = a3 + r*b4
                b2 = a2 + r*b3 + s*b4
                b1 = a1 + r*b2 + s*b3
                b0 = a0 + r*b1 + s*b2

        	c4 = b4
        	c3 = b3 + r*c4
        	c2 = b2 + r*c3 + s*c4
        	c1 = b1 + r*c2 + s*c3

c       Encontrar delta-r y delta-s
                det = c2*c2 - c1*c3
                dr = (b0*c3 - b1*c2)/det
                ds = (b1*c1 - b0*c2)/det
c                write(*,*)r,s
c       Se calcula el nuevo r y s
                r = r + dr
                s = s + ds
c       Calcular los errores asociados
                Ear = abs(dr/r)*100
                Eas = abs(ds/s)*100
        end do
c       Valores de las raíces
        dis = r**2 + 4*s
	if (dis.lt.0) then
		goto 60
	end if
	x1 = (r + sqrt(dis))/2
	x2 = (r - sqrt(dis))/2
	write(*,*)x1
	write(*,*)x2
	goto 50
60	write(*,*)r,dis
	write(*,*)r,dis
c----------------------------------------------------------
c	Se define el nuevo polinomio	
50	a2 = 1
	a1 = a3 + x1 + x2
	a0 = a22 + x2**2 + (x1 + x2)*(x1 + a3)
c	Se reinician valores
	r0 = -1.1
        so = -1.1
        r = r0
        s = s0
        Ear = 100
        Eas = 100
        do while (ear.gt.ee.and.eas.gt.ee)
                b2 = a2
                b1 = a1 + r*b2
                b0 = a0 + r*b1 + s*b2

        	c2 = b2
        	c1 = b1 + r*c2

c       Encontrar delta-r y delta-s
                det = c2**2 - c1*c3
                dr = (b0*c3 - b1*c2)/det
                ds = (b1*c1 - b0*c2)/det
c                write(*,*)r,s
c       Se calcula el nuevo r y s
                r = r + dr
                s = s + ds
c       Calcular los errores asociados
                Ear = abs(dr/r)*100
                Eas = abs(ds/s)*100
        end do	
        write(*,*)r,s
c       Valores de las raíces
        dis = r**2 + 4*s
	if (dis.lt.0) then
		goto 70
	end if
	x1 = (r + sqrt(dis))/2
	x2 = (r - sqrt(dis))/2
	write(*,*)x1
	write(*,*)x2
	goto 80
70	q = abs(dis)
	write(*,*)q
	re3 = r/2
	im3 = q/4
	im4 = -q/4
	write(*,*)re3,im3,im4
80 	stop	
        end program
			  
