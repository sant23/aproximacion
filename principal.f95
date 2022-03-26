program minimos
    use lineal
    use aproximacion 

    implicit none
    real(8), allocatable :: Xn(:), Yn(:), A(:,:), B(:), X(:),px(:)
    real(8)::e,s 
    character(15)::fichero 
    integer :: i,l,j,k,n,m
    

    
    
    print*, 'Introduzca el valor de m'
    read*, m 
  
    
    fichero = 'datos.dat'
    open(unit=10, file = fichero)
   
    read(10,*) n
   
    allocate (Xn(n),Yn(n), A(m+1,m+1), B(m+1), X(m+1),px(n))

    do i=1, n   
        read(10,*) Xn(i), Yn(i)
    ENDDO 

    call minimos_cuadrados(a,b,x,xn,yn)

    
    ! write(*,*) '-------b--------'
   !  write(*,*)   b  
    !write(*,*) '-------a--------'
    !write(*,*) a 


    !write (*,*) '------vector xn-------'
   ! write(*,*) xn
    !write (*,*) '------vector yn-------'
    !write(*,*) yn
    
    call error(Xn,Yn,px,x,e)

    open(unit=14,file='salidam.csv')
    write(14,*) 'x,   y,    px'
    e=0.d0
    do i=1, n 
        s=0.d0
        do l=0, m 
            s = s + x(l+1)*xn(i)**l 
        enddo 
        write(14,*) xn(i), ',', yn(i), ',', s 
        e = e + (yn(i)-s)**2 
    enddo 
    write(*,*)m, e


end program  minimos